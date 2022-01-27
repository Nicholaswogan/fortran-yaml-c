
cmake_minimum_required(VERSION 3.4)
project (FORTRAN_YAML_C VERSION 0.1.0 LANGUAGES C Fortran)

include(FortranCInterface)
FortranCInterface_VERIFY()

add_subdirectory(libyaml EXCLUDE_FROM_ALL)

add_library(fortran-yaml-c yaml_types.f90 fortran_yaml_c.f90 libyaml_interface.c)
target_link_libraries(fortran-yaml-c yaml)
target_include_directories(fortran-yaml-c PUBLIC libyaml/include)

add_executable(test_yaml test_yaml.f90)
target_link_libraries(test_yaml fortran-yaml-c)

add_executable(example example.f90)
target_link_libraries(example fortran-yaml-c)
