##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

include(CMakePackageConfigHelpers)

set(CMAKE_INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/cmake)

## Generate and install SerialboxConfig.cmake
configure_package_config_file(${CMAKE_SOURCE_DIR}/cmake/SerialboxConfig.cmake.in
    "${PROJECT_BINARY_DIR}/cmake/SerialboxConfig.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_DIR}
    PATH_VARS CMAKE_INSTALL_DIR
)
install(FILES "${PROJECT_BINARY_DIR}/cmake/SerialboxConfig.cmake" DESTINATION cmake)

## Install SerialboxTooling.cmake
install(FILES ${CMAKE_SOURCE_DIR}/cmake/SerialboxTooling.cmake DESTINATION cmake/)

# Generate and install SerialboxConfigVersion.cmake
write_basic_package_version_file(
  "${PROJECT_BINARY_DIR}/cmake/SerialboxConfigVersion.cmake"
  VERSION ${SERIALBOX_VERSION_STRING}
  COMPATIBILITY AnyNewerVersion
)
install(FILES "${PROJECT_BINARY_DIR}/cmake/SerialboxConfigVersion.cmake" DESTINATION cmake)


ExternalProject_Add(script-gen
   PREFIX script-gen
   SOURCE_DIR ${CMAKE_SOURCE_DIR}/cmake
   SOURCE_SUBDIR "script-gen"
   INSTALL_DIR "${CMAKE_INSTALL_PREFIX}"
   BUILD_ALWAYS 1
   CMAKE_ARGS -DSerialbox_DIR=${CMAKE_INSTALL_PREFIX}/cmake -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
)

