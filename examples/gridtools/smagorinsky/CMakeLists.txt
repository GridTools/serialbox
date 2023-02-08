##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

project(SerialboxSmagorinskyExample CXX)

#
# We use the Serialbox which was installed in install/. If this folder does not exists, or is empty,
# you have to build and install Serialbox first. To tell CMake where Serialbox is located, you have
# to define SERIALBOX_ROOT (either directly in CMake or pass it as -DSERIALBOX_ROOT=<> via 
# command-line).
#
if(NOT(DEFINED SERIALBOX_ROOT))
	set(SERIALBOX_ROOT "${PROJECT_SOURCE_DIR}/../../../install")
endif()

#
# Serialbox has a find_package-module which takes care of looking for the dependency libraries. 
# Usually you want to bundle this module with your own project and therefore we copied it into 
# "cmake/". We need to tell CMake about this.
#
list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

#
# We call the find_package-module of Serialbox and gridtools and set the include directories
#
find_package(Serialbox REQUIRED HINTS
    "${CMAKE_CURRENT_LIST_DIR}/../serialbox/install/cmake"
    "${SERIALBOX_ROOT}/cmake"
    "${SERIALBOX_DIR}/cmake"
    "$ENV{SERIALBOX_ROOT}/cmake")

find_package(GridTools REQUIRED)

include_directories(SYSTEM ${SERIALBOX_INCLUDE_DIRS})
include_directories(SYSTEM ${GRIDTOOLS_INCLUDE_DIRS})

#
# Serialbox requires C++17
#
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")

#
# Don't use Debug symbols, gcc takes to long otherwise ;)
#
string(REPLACE "-g" "" CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG}")

#
# Finally, we add the executables and link against the Serialbox libraries
#
add_executable(smagorinsky smagorinsky.cpp smagorinsky_stencil.hpp smagorinsky_repository.hpp)
target_link_libraries(smagorinsky ${SERIALBOX_CXX_LIBRARIES})

add_executable(smagorinsky-error smagorinsky.cpp smagorinsky_stencil.hpp smagorinsky_repository.hpp)
target_compile_definitions(smagorinsky-error PUBLIC -DERROR)
target_link_libraries(smagorinsky-error ${SERIALBOX_CXX_LIBRARIES})

