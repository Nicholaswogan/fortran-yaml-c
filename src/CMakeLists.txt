add_library(libyaml_interface libyaml_interface.c)
target_link_libraries(libyaml_interface yaml)

add_library(fortran-yaml-c 
  yaml_types.f90 
  fortran_yaml_c.f90 
)
target_link_libraries(fortran-yaml-c libyaml_interface)

if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  target_compile_options(fortran-yaml-c PRIVATE -fimplicit-none)
  if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_compile_options(fortran-yaml-c PRIVATE -Wunused -fcheck=all)
  endif()
endif()