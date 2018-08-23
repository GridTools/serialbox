##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

yoda_add_unittest(
      NAME example-01-laplacian-c 
      SOURCES example-01-laplacian.c
      LIBRARIES ${CMAKE_THREAD_LIBS_INIT}
      DEPENDS SerialboxCStatic
      INSTALL_DESTINATION bin)

