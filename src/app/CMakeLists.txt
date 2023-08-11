

add_executable(demo
    demo.cpp
    )

target_link_libraries(demo
    bind
    ${RACKET_LIBRARIES}
    )

target_include_directories(demo PRIVATE
    ${CMAKE_INSTALL_PREFIX}/include/
    ${CMAKE_SOURCE_DIR}/src/
    ${CMAKE_CURRENT_BINARY_DIR}/
    )

#--------------------------------------------------------------------------------------------------

add_executable(racket_cmd
    racket_cmd.cpp
    )

target_link_libraries(racket_cmd
    bind
    ${RACKET_LIBRARIES}
    )

target_include_directories(racket_cmd PRIVATE
    ${CMAKE_INSTALL_PREFIX}/include/
    ${CMAKE_SOURCE_DIR}/src/
    ${CMAKE_CURRENT_BINARY_DIR}/
    )
