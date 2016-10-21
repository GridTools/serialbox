##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

if(NOT(SERIALBOX_ENABLE_C))
  message(FATAL_ERROR "Python Interface requires the C Interface (set SERIALBOX_ENABLE_C=ON)")
endif()

if(NOT(SERIALBOX_BUILD_SHARED))
  message(FATAL_ERROR "Python Interface requires shared libraries (set SERIALBOX_BUILD_SHARED=ON)")
endif()

file(GLOB_RECURSE SERIALBOX_PYTHON_SOURCE 
     ${CMAKE_SOURCE_DIR}/src/serialbox-python/
     ${CMAKE_SOURCE_DIR}/src/serialbox-python/serialbox/*.py)

## Copy python code to python build dir
add_custom_target(SerialboxPython
                  COMMAND ${CMAKE_COMMAND} -E copy_directory
                  ${CMAKE_SOURCE_DIR}/src/serialbox-python/serialbox
                  ${CMAKE_BINARY_DIR}/src/serialbox-python/serialbox
                  DEPENDS "${SERIALBOX_PYTHON_SOURCE}")            
add_dependencies(SerialboxCLibraryShared SerialboxPython)

## Install python and create symlinks
install(DIRECTORY ${CMAKE_SOURCE_DIR}/src/serialbox-python/serialbox/ DESTINATION python/serialbox
        FILES_MATCHING PATTERN "*.py")

#install(CODE "
#  EXECUTE_PROCESS(COMMAND ln -sf ${CMAKE_INSTALL_PREFIX}/lib/$<TARGET_FILE_NAME:SerialboxCLibraryShared> ${CMAKE_INSTALL_PREFIX}/python/serialbox/$<TARGET_FILE_NAME:SerialboxCLibraryShared>)
#")
#add_custom_target(SerialboxPythonInstallScript
#                  COMMAND echo "ln -sf ${CMAKE_INSTALL_PREFIX}/lib/$<TARGET_FILE_NAME:SerialboxCLibraryShared> ${CMAKE_INSTALL_PREFIX}/python/serialbox/$<TARGET_FILE_NAME:SerialboxCLibraryShared>"
#)
#add_dependencies(SerialboxPythonInstallScript install SerialboxCLibraryShared)

#install(SCRIPT ${CMAKE_COMMAND} -E create_symlink $<TARGET_FILE:${target}Shared> ${path}/$<TARGET_FILE_NAME:${target}Shared>)        

