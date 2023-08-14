link_directories(/usr/local/lib)

execute_process(
  COMMAND git log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}
  OUTPUT_VARIABLE GIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(COMMAND git log -1 --format=%ad --date=local
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  OUTPUT_VARIABLE GIT_DATE
  ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE
)

execute_process(COMMAND git log -1 --format=%s
  WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
  OUTPUT_VARIABLE GIT_COMMIT_MSG
  ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE
)

set(RACKET_LIBRARIES rkt_version "-lracketcs" "-ldl" "-lpthread" "-lncurses")


add_custom_target(generate_prims_scm
  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/demo_common/nc/prims.scm")

add_custom_command(
  COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/demo_common/nc
  COMMAND racket ${CMAKE_SOURCE_DIR}/src/bind/bindings.scm  ${CMAKE_SOURCE_DIR}/src/bind/bindings.inc > ${CMAKE_CURRENT_BINARY_DIR}/demo_common/nc/prims.scm
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/bindings.scm ${CMAKE_CURRENT_BINARY_DIR}/demo_common/info.rkt
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/bindings-syntax.scm
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/bindings.inc
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/demo_common/nc/prims.scm
  COMMENT "Regenerating prims.scm"
  )

add_custom_target(install_racket_pkg ALL
  DEPENDS /root/.local/share/racket/8.4/pkgs/MARK
  )
#---------------------------------------------------------------------------------------------------

add_custom_command(
  COMMAND bash ${CMAKE_SOURCE_DIR}/scripts/raco_cleanup.sh
  COMMAND raco pkg install -t dir --link ${CMAKE_CURRENT_BINARY_DIR}/demo_common
  COMMAND raco setup
  COMMAND touch /root/.local/share/racket/8.4/pkgs/MARK
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/demo_common/nc/prims.scm
  OUTPUT /root/.local/share/racket/8.4/pkgs/MARK
  COMMENT "[re-]installing racket package demo_common")
  

add_custom_command(
  COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/demo_common/nc
  COMMAND cp -r ${CMAKE_SOURCE_DIR}/src/bind/demo_common/*.rkt ${CMAKE_CURRENT_BINARY_DIR}/demo_common/
  COMMAND cp -r ${CMAKE_SOURCE_DIR}/src/bind/demo_common/nc/*.rkt ${CMAKE_CURRENT_BINARY_DIR}/demo_common/nc/
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/demo_common/info.rkt
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/demo_common/main.rkt
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/demo_common/bindings.rkt
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/demo_common/toml.rkt
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/demo_common/toml_compiler.rkt
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/demo_common/nc/toml-syntax.rkt
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/demo_common/infix.rkt
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/demo_common/infix_compiler.rkt
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/demo_common/nc/infix-syntax.rkt
  DEPENDS ${CMAKE_SOURCE_DIR}/src/bind/demo_common/common.rkt
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/demo_common/main.rkt
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/demo_common/info.rkt
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/demo_common/bindings.rkt
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/demo_common/toml.rkt
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/demo_common/toml_compiler.rkt
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/demo_common/infix.rkt
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/demo_common/infix_compiler.rkt
  COMMENT "Installing demo_common package"
  )

add_custom_target(generate_base_h
  DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/zzzbase.h"
  )

add_custom_command(
  COMMAND raco ctool --c-mods "${CMAKE_CURRENT_BINARY_DIR}/zzzbase.h" ++lib racket/base ++lib compatibility/defmacro ++lib racket/load ++lib racket/match ++lib ffi/unsafe/vm ++lib demo_common
  DEPENDS /root/.local/share/racket/8.4/pkgs/MARK
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/demo_common/nc/prims.scm
  OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/zzzbase.h"
  COMMENT "Generate Racket binary modules wrapper")

add_library(rkt_version
    STATIC
    version.cpp
    )

target_compile_definitions(rkt_version PRIVATE
  GIT_HASH=${GIT_HASH}
  GIT_DATE="${GIT_DATE}"
  GIT_COMMIT_MSG="${GIT_COMMIT_MSG}")

add_library(bind
    STATIC
    ${CMAKE_SOURCE_DIR}/src/bind/bindings_racket.cpp
    ${CMAKE_SOURCE_DIR}/src/bind/racket_base.c
    )

target_include_directories(bind PRIVATE
    ${CMAKE_SOURCE_DIR}/src/
    ${CMAKE_INSTALL_PREFIX}/include/
    ${CMAKE_CURRENT_BINARY_DIR}/
    )


add_subdirectory(app)
add_subdirectory(test)

add_dependencies(bind generate_prims_scm)
add_dependencies(bind generate_base_h)
add_dependencies(generate_base_h install_racket_pkg)

target_link_libraries(bind
    ${RACKET_LIBRARIES}
  )


