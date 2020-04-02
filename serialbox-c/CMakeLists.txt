##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##
##
## This is the master CMake file of the serialbox project.
##
##===------------------------------------------------------------------------------------------===##

set(CMAKE_INSTALL_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}/install" CACHE PATH "CMake install prefix")

cmake_policy(SET CMP0048 NEW)
file(STRINGS "../version.txt" __SERIALBOX_VERSION)
project(Serialbox VERSION ${__SERIALBOX_VERSION} LANGUAGES CXX C)
cmake_minimum_required(VERSION 3.12.4)

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_C_EXTENSIONS OFF)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake/modules") # TODO fix location

#---------------------------------------- Configure ------------------------------------------------

if(UNIX)
  set(SERIALBOX_ON_UNIX 1)
endif(UNIX)

if(WIN32)
  set(SERIALBOX_ON_WIN32 1)
endif(WIN32)

# Serialbox version
set(Serialbox_VERSION_STRING 
    "${Serialbox_VERSION_MAJOR}.${Serialbox_VERSION_MINOR}.${Serialbox_VERSION_PATCH}")


find_package(Boost 1.54 REQUIRED)
add_subdirectory(src/serialbox-c)

install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src/serialbox-c DESTINATION include 
    FILES_MATCHING PATTERN "*.h")

add_subdirectory(../serialbox serialbox)