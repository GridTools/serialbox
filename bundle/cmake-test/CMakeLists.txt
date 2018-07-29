project(serialbox2-cmake-test C CXX Fortran)
enable_testing()
cmake_minimum_required(VERSION 3.8.1)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake")

set(YODA_ROOT CACHE PATH "path to yoda package")

if("${YODA_ROOT}" STREQUAL "")
  message(FATAL_ERROR "YODA_ROOT not found! Try specifying it in the environment via -DYODA_ROOT=<>")
endif()
list(APPEND CMAKE_MODULE_PATH "${YODA_ROOT}/cmake")
include(yodaInit)
yoda_init()

find_package(Serialbox CONFIG REQUIRED)

yoda_print_target_properties(sbox::SerialboxCoreStatic)
if(SERIALBOX_HAS_SHARED_LIBRARY)
  yoda_print_target_properties(sbox::SerialboxCoreShared)
endif(SERIALBOX_HAS_SHARED_LIBRARY)
yoda_print_target_properties(sbox::SerialboxCStatic)
if(SERIALBOX_HAS_SHARED_LIBRARY)
  yoda_print_target_properties(sbox::SerialboxCShared)
endif(SERIALBOX_HAS_SHARED_LIBRARY)
yoda_print_target_properties(sbox::SerialboxFortranStatic)
if(SERIALBOX_HAS_SHARED_LIBRARY)
  yoda_print_target_properties(sbox::SerialboxFortranShared)
endif(SERIALBOX_HAS_SHARED_LIBRARY)

#We need to install something
set(conf_bundle_filename ${CMAKE_BINARY_DIR}/gtclang-conf.bundle)

file(WRITE ${conf_bundle_filename} "conf")

install(
  FILES ${conf_bundle_filename}
  DESTINATION bundle_conf
)

