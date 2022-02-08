
cmake_minimum_required(VERSION "3.14")
project (FORTRAN_YAML_C VERSION 0.1.1 LANGUAGES C Fortran)

include(FortranCInterface)
FortranCInterface_VERIFY()

include(cmake/CPM.cmake)
CPMAddPackage(
  NAME libyaml 
  VERSION 0.2.5
  GITHUB_REPOSITORY "yaml/libyaml"
  GIT_TAG "0.2.5"
  EXCLUDE_FROM_ALL ON
)

add_library(fortran-yaml-c yaml_types.f90 fortran_yaml_c.f90 libyaml_interface.c)
target_link_libraries(fortran-yaml-c yaml)

add_executable(test_yaml test_yaml.f90)
target_link_libraries(test_yaml fortran-yaml-c)

add_executable(example example.f90)
target_link_libraries(example fortran-yaml-c)
