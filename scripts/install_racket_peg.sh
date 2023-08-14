#!/bin/sh

# Like build_racket.sh, this script is supposed to be executed in a Docker environment (base_devenv:1.0.0 or layers on top of it)

cp racket-peg.patch /workdir
cd /workdir/

[[ -d racket-peg ]] || git clone https://github.com/rain-1/racket-peg.git
cd /workdir/racket-peg/ && git checkout 3ee1f69de9b79e75360798ea9bcd913e756292fd
git checkout . && patch -p1 < /workdir/racket-peg.patch
cd /workdir/racket-peg/ && raco pkg install -t dir --link `pwd`



