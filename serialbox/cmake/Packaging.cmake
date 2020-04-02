##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# this registers the build-tree with a global CMake-registry
#export(PACKAGE Serialbox) TODO enable

include(CMakePackageConfigHelpers)

set(CMAKE_INSTALL_DIR ${CMAKE_INSTALL_PREFIX}/cmake)

if(BUILD_SHARED_LIBS)
    install(EXPORT SerialboxTargets
        FILE SerialboxSharedTargets.cmake
        NAMESPACE Serialbox::shared
        DESTINATION ${CMAKE_INSTALL_DIR}
    )
else()
    install(EXPORT SerialboxTargets
        FILE SerialboxStaticTargets.cmake
        NAMESPACE Serialbox::shared
        DESTINATION ${CMAKE_INSTALL_DIR}
    )
endif()

## Generate and install SerialboxConfig.cmake
configure_package_config_file(${PROJECT_SOURCE_DIR}/cmake/SerialboxConfig.cmake.in
    "${PROJECT_BINARY_DIR}/cmake/SerialboxConfig.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_DIR}
    PATH_VARS CMAKE_INSTALL_DIR
)
install(FILES "${PROJECT_BINARY_DIR}/cmake/SerialboxConfig.cmake" DESTINATION cmake)

## Install SerialboxTooling.cmake
# TODO
#install(FILES ${CMAKE_SOURCE_DIR}/cmake/SerialboxTooling.cmake DESTINATION cmake/)

# Generate and install SerialboxConfigVersion.cmake
write_basic_package_version_file(
  "${PROJECT_BINARY_DIR}/cmake/SerialboxConfigVersion.cmake"
  VERSION ${Serialbox_VERSION_STRING}
  COMPATIBILITY SameMajorVersion
)
install(FILES "${PROJECT_BINARY_DIR}/cmake/SerialboxConfigVersion.cmake" DESTINATION cmake)

## For build tree
#export(TARGETS SerialboxStatic SerialboxObjects SerialboxFilesytemTarget
#    FILE ${PROJECT_BINARY_DIR}/SerialboxTargets.cmake
#    NAMESPACE Serialbox::
#)
#if(SERIALBOX_HAS_NETCDF)
#    export(TARGETS NETCDF_TARGET
#        APPEND FILE ${PROJECT_BINARY_DIR}/SerialboxTargets.cmake
#        NAMESPACE Serialbox::
#    )
#endif()
#if(SERIALBOX_ENABLE_C)
#    export(TARGETS SerialboxCStatic SerialboxCObjects
#        APPEND FILE ${PROJECT_BINARY_DIR}/SerialboxTargets.cmake
#        NAMESPACE Serialbox::
#    )
#endif()
#if(SERIALBOX_ENABLE_FORTRAN)
#    export(TARGETS SerialboxFortranStatic SerialboxFortranObjects SerialboxFortranSerializeObjects
#        APPEND FILE ${PROJECT_BINARY_DIR}/SerialboxTargets.cmake
#        NAMESPACE Serialbox::
#    )
#endif()
#
#set(CMAKE_INSTALL_DIR ${PROJECT_SOURCE_DIR}/cmake)
#set(PYTHON_INSTALL_DIR ${PROJECT_SOURCE_DIR}/src/serialbox-python)
#configure_package_config_file(${CMAKE_SOURCE_DIR}/cmake/SerialboxConfig.cmake.in
#    ${PROJECT_BINARY_DIR}/SerialboxConfig.cmake
#    INSTALL_DESTINATION ${PROJECT_BINARY_DIR}
#    PATH_VARS CMAKE_INSTALL_DIR PYTHON_INSTALL_DIR
#)
#write_basic_package_version_file(
#  ${PROJECT_BINARY_DIR}/SerialboxConfigVersion.cmake
#  VERSION ${Serialbox_VERSION_STRING}
#  COMPATIBILITY AnyNewerVersion
#)
