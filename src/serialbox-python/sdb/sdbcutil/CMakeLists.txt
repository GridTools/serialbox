##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

if(SERIALBOX_ENABLE_C)

  set(SYMLINKS ${CMAKE_SOURCE_DIR}/src/serialbox-python/sdb/sdbcutil)

  include_directories(${PYTHON_INCLUDE_DIR})

  serialbox_add_library(NAME sdbcutilC
                        TARGET sdbcutilCLibrary 
                        NATIVE_PYTHON_LIB
                        BUILD_SHARED
                        SYMLINKS ${SYMLINKS}
                        LIBRARIES ${PYTHON_LIBRARY}
                        SOURCES sdbcutilC.c)
endif()

