
cmake_minimum_required(VERSION "3.14")
project (FORTRAN_YAML_C VERSION "0.2.2" LANGUAGES C Fortran)

include(FortranCInterface)
FortranCInterface_VERIFY()

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules")

# dependencies
include(cmake/CPM.cmake)
CPMAddPackage(
  NAME libyaml 
  VERSION 0.2.5
  GITHUB_REPOSITORY "yaml/libyaml"
  GIT_TAG "0.2.5"
  EXCLUDE_FROM_ALL ON
)

add_subdirectory(src)
add_subdirectory(tests)
