##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

set(SHA256_SOURCE sha256.c)
set(SHA256_HEADER sha256.h)
add_library(sha256 OBJECT ${SHA256_SOURCE})
