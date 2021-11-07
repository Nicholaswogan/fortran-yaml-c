
cmake_minimum_required(VERSION 3.0)
project (test C)

add_subdirectory(libyaml)

add_executable(main main.c)
target_link_libraries(main yaml)
target_include_directories(main PUBLIC libyaml/include/)
