set(YAML_TESTS test_yaml example)

foreach(YAML_TEST ${YAML_TESTS})
  add_executable(${YAML_TEST} "${YAML_TEST}.f90")
  target_link_libraries(${YAML_TEST} fortran-yaml-c)
  target_include_directories(${YAML_TEST} PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
endforeach()