
cmake_minimum_required(VERSION "3.14")
project (FORTRAN_YAML_C VERSION "0.2.6" LANGUAGES C Fortran)

include(FortranCInterface)
FortranCInterface_VERIFY()

set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules")

option(DOWNLOAD_LIBYAML "If ON, then libyaml will be downloaded and 
built." ON)

# Find libyaml
if (DOWNLOAD_LIBYAML) 
  # use CPM to download and build libyaml
  include(cmake/CPM.cmake)
  CPMAddPackage(
    NAME libyaml 
    VERSION 0.2.5
    GITHUB_REPOSITORY "yaml/libyaml"
    GIT_TAG "release/0.2.5"
    EXCLUDE_FROM_ALL ON
  )
else()
  # Try to find libyaml using pkg-config
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(YAML REQUIRED IMPORTED_TARGET yaml-0.1)
  if (NOT (${YAML_VERSION} STREQUAL "0.2.5"))
    message(FATAL_ERROR "PkgConfig found yaml version ${YAML_VERSION} but version 0.2.5 is required")
  endif()
endif()

add_subdirectory(src)
add_subdirectory(tests)
