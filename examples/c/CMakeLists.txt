##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

serialbox_add_unittest_executable(
      NAME example-01-laplacian-c 
      SOURCES example-01-laplacian.c
      LIBRARIES SerialboxCStatic)
install(TARGETS example-01-laplacian-c DESTINATION bin)