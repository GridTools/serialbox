##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

include(CMakeParseArguments)

## Add subdirectories
add_subdirectory(serialbox)
add_subdirectory(serialbox-c)
add_subdirectory(serialbox-fortran)
add_subdirectory(serialbox-python)

