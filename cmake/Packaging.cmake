##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# this registers the build-tree with a global CMake-registry
export(PACKAGE Serialbox)

include(CMakePackageConfigHelpers)

set(CMAKE_INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/cmake)
set(PYTHON_INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/python)

install(EXPORT SerialboxTargets
    FILE SerialboxTargets.cmake
    NAMESPACE Serialbox::
    DESTINATION ${CMAKE_INSTALL_DIR}
)

## Generate and install SerialboxConfig.cmake
configure_package_config_file(${PROJECT_SOURCE_DIR}/cmake/SerialboxConfig.cmake.in
    "${PROJECT_BINARY_DIR}/cmake/SerialboxConfig.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_DIR}
    PATH_VARS CMAKE_INSTALL_DIR PYTHON_INSTALL_DIR
)
install(FILES "${PROJECT_BINARY_DIR}/cmake/SerialboxConfig.cmake" DESTINATION cmake)

## Install SerialboxTooling.cmake
install(FILES ${PROJECT_SOURCE_DIR}/cmake/SerialboxTooling.cmake DESTINATION cmake/)

# Generate and install SerialboxConfigVersion.cmake
write_basic_package_version_file(
  "${PROJECT_BINARY_DIR}/cmake/SerialboxConfigVersion.cmake"
  VERSION ${Serialbox_VERSION_STRING}
  COMPATIBILITY AnyNewerVersion
)
install(FILES "${PROJECT_BINARY_DIR}/cmake/SerialboxConfigVersion.cmake" DESTINATION cmake)

## For build tree

set(CMAKE_INSTALL_DIR ${PROJECT_SOURCE_DIR}/cmake)
set(PYTHON_INSTALL_DIR ${PROJECT_SOURCE_DIR}/src/serialbox-python)


set( ${PROJECT_NAME}_DIR ${PROJECT_BINARY_DIR} CACHE STRING "" )

configure_package_config_file(${PROJECT_SOURCE_DIR}/cmake/SerialboxConfig.cmake.in
    ${PROJECT_BINARY_DIR}/SerialboxConfig.cmake
    INSTALL_DESTINATION ${PROJECT_BINARY_DIR}
    INSTALL_PREFIX ${PROJECT_BINARY_DIR}
    PATH_VARS CMAKE_INSTALL_DIR PYTHON_INSTALL_DIR
)
write_basic_package_version_file(
  ${PROJECT_BINARY_DIR}/SerialboxConfigVersion.cmake
  VERSION ${Serialbox_VERSION_STRING}
  COMPATIBILITY AnyNewerVersion
)
