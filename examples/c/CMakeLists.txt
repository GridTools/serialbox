##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

add_executable(example-01-laplacian-c example-01-laplacian.c)
target_link_libraries(example-01-laplacian-c SerialboxCStatic)
install(TARGETS example-01-laplacian-c DESTINATION bin)
