##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

project(SerialboxSmagorinskyExample CXX)

#
# We use the Serialbox which was installed in install/. If this folder does not exists, or is empty,
# you have to build and install Serialbox first. To tell CMake where Serialbox is located, you have
# to define SERIALBOX_ROOT (either directly in CMake or pass it as -DSERIALBOX_ROOT=<> via 
# command-line).
#
set(SERIALBOX_ROOT "${CMAKE_SOURCE_DIR}/../../../install")

#
# Serialbox has a find_package-module which takes care of looking for the dependency libraries. 
# Usually you want to bundle this module with your own project and therefore we copied it into 
# "cmake/". We need to tell CMake about this.
#
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

#
# We call the find_package-module of Serialbox and gridtools and set the include directories
#
find_package(Serialbox REQUIRED)
find_package(GridTools REQUIRED)

include_directories(SYSTEM ${SERIALBOX_INCLUDE_DIRS})
include_directories(SYSTEM ${GRIDTOOLS_INCLUDE_DIRS})

#
# Serialbox requires C++11
#
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

#
# Finally, we add the executable and link against the Serialbox libraries
#
add_executable(smagorinsky smagorinsky.cpp smagorinsky_stencil.hpp smagorinsky_repository.hpp)
target_link_libraries(smagorinsky ${SERIALBOX_CXX_LIBRARIES})

