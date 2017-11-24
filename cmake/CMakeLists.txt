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

## Generate and install SerialboxConfig.cmake
configure_file(${CMAKE_SOURCE_DIR}/cmake/SerialboxConfig.cmake.in
    "${PROJECT_BINARY_DIR}/cmake/SerialboxConfig.cmake" @ONLY)
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

## This is a hack (use some variables which are set in SerialboxConfig.cmake.in):
include( ${PROJECT_BINARY_DIR}/cmake/SerialboxConfig.cmake )
configure_file(${CMAKE_SOURCE_DIR}/cmake/SerialboxConfig.sh.in "${PROJECT_BINARY_DIR}/cmake/SerialboxConfig.sh" @ONLY)
install(FILES "${PROJECT_BINARY_DIR}/cmake/SerialboxConfig.sh" DESTINATION cmake)
