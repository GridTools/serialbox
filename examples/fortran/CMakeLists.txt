##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

# Workaround for -rdynamic 
set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "")

add_subdirectory(simple)
