
add_custom_target(test)

add_custom_target(run_demo)

add_custom_command(
  TARGET test
  COMMENT "Run Racket test script"
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/src/app
  POST_BUILD COMMAND ${CMAKE_CURRENT_BINARY_DIR}/../app/racket_cmd
  ARGS ${CMAKE_SOURCE_DIR}/src/app/test.scm)


add_custom_command(
  TARGET run_demo
  COMMENT "Run the demo app"
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/src/app
  POST_BUILD COMMAND ${CMAKE_CURRENT_BINARY_DIR}/../app/demo)

add_dependencies(test racket_cmd)
add_dependencies(run_demo demo)
