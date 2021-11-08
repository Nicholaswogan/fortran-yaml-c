
cmake_minimum_required(VERSION 3.0)
project (fortran-yaml-c LANGUAGES C Fortran)

include(FortranCInterface)
FortranCInterface_VERIFY()

add_subdirectory(libyaml EXCLUDE_FROM_ALL)

add_library(fortran-yaml-c yaml_types.f90 yaml.f90 yaml.c)
target_link_libraries(fortran-yaml-c yaml)
target_include_directories(fortran-yaml-c PUBLIC libyaml/include)

add_executable(test_yaml test_yaml.f90)
target_link_libraries(test_yaml fortran-yaml-c)
