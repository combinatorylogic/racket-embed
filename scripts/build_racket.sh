#!/bin/bash


## Build a Racket CS Unix-style system, avoiding pulling anything from a network in the process.
## A modified compatibility library removes the cascade dependencies it would have
## produced with compatibility-doc otherwise. Instead of pulling the existing PB boot files, we're building
## them locally using a racket-bc build step. Tested with Racket v8.6-v8.10.
##
## There is an option for using a tarball with PB boot files (e.g., prepared for a cross-compilation)
##
## This script is supposed to be execuded in a Docker environment (at least base_devenv:1.0.0 layer) available from
## https://github.com/combinatorylogic/devenv
##

## Additional packages:
#
# cext-lib, dynext-lib, scheme-lib, rackunit-lib, sandbox-lib, testing-util-lib, srfi-lite-lib, errortrace-lib, source-syntax


RACKET_VERSION=v8.10
RACKET_SOURCE=/workdir/racket
RACKET_PATCH=/root/racket-${RACKET_VERSION}.patch
TARBALL_PATH=/workdir/racket-pb-${RACKET_VERSION}.tar
PKGS_PATH=/workdir/racket-packages
RACKET_GIT_PREFIX=https://github.com/racket/

PKGS_LIST_MDLS="compatibility,cext-lib,rackunit,srfi,errortrace,typed-racket"
PKGS_LIST_OTHER="scheme-lib"
PKGS_LIST=${PKGS_LIST_MDLS},${PKGS_LIST_OTHER}

COMPATIBILITY_SOURCE=${PKGS_PATH}/compatibility
COMPATIBILITY_PATCH=/root/compatibility.patch

usage() {
    echo "usage: $0 [--no-bc TARBALL_PATH]"
}

set -e

## Prepare
mkdir -p $PKGS_PATH

cd $PKGS_PATH
for pkg in ${PKGS_LIST//,/ }
do
    echo "Fetching ${val}"
    if [ ! -d ${PKGS_PATH}/${pkg} ]
    then
       git clone ${RACKET_GIT_PREFIX}/${pkg}.git -b ${RACKET_VERSION}
    fi
done

cd /workdir
if [ ! -d $RACKET_SOURCE ]
then
    git clone ${RACKET_GIT_PREFIX}/racket.git $RACKET_SOURCE -b ${RACKET_VERSION}
fi

cd $COMPATIBILITY_SOURCE
git clean -fdx
git checkout .
git checkout ${RACKET_VERSION}
patch -p1 < $COMPATIBILITY_PATCH


setup_racket() {
    cd $RACKET_SOURCE
    git clean -fdx
    git checkout .
    git checkout ${RACKET_VERSION}
    rm -rf $RACKET_SOURCE/racket/src/ChezScheme/boot/pb
    patch -p1 < $RACKET_PATCH
    cd $PKGS_PATH
    for pkg in ${PKGS_LIST_MDLS//,/ }
    do
        cd ${PKGS_PATH}/${pkg}
        for mdl in *
        do
            if [ -d ${mdl} ]
            then
                ln -s ${PKGS_PATH}/${pkg}/${mdl} ${RACKET_SOURCE}/pkgs/${mdl}
            fi
        done
    done
    for pkg in ${PKGS_LIST_OTHER//,/ }
    do
        ln -s ${PKGS_PATH}/${pkg} ${RACKET_SOURCE}/pkgs/${pkg}
    done
}

NO_BC=0

while [ "$1" != "" ]; do
    case $1 in
        --no-bc )         shift
                          NO_BC=1
                          TARBALL_PATH=$1
                          ;;
        * )               usage
                          exit
                          ;;
    esac
    shift
done


setup_racket

if [ $NO_BC -eq 0 ]
then
    ## Build BC
    ##  generations and backtrace are disabled for a QEMU aarch64 bootstrap, we don't need BC
    ##    for anything but PB bootstrap files anyway
    cd $RACKET_SOURCE
    make PKGS="" bc CONFIGURE_ARGS="--disable-generations --disable-backtrace"

    ## Bootstrap files
    cd $RACKET_SOURCE/racket/src/ChezScheme/
    $RACKET_SOURCE/racket/bin/racketbc ../rktboot/main.rkt --machine pb
    (cd $RACKET_SOURCE; tar -cf $TARBALL_PATH racket/src/ChezScheme/boot/)
fi

## Clean up and setup again
if [ -f $TARBALL_PATH ]
then
    setup_racket
    cd $RACKET_SOURCE && tar -xf $TARBALL_PATH
fi

## Build RacketCS with these bootstrap files
cd $RACKET_SOURCE
make PKGS="cext-lib compatibility" PREFIX="/usr/" DESTDIR="/workdir/tmp" local-catalog CPUS=8
make PKGS="cext-lib compatibility" PREFIX="/usr/" DESTDIR="/workdir/tmp" unix-style CPUS=8

