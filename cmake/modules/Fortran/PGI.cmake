##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# General Flags (add to default)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Mpreprocess -Kieee -Mfree -Mdclchk")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Minform=warn -Munixlogical")

# Release
SET(CMAKE_Fortran_FLAGS_RELEASE "-O3 -fast -Mvect=noassoc")

# Debug Options (replace default)
set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -C -Mchkfpstk -Mchkptr -Ktrap=fp -traceback")

# https://cmake.org/pipermail/cmake/2010-November/040951.html
set(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS "-Mcuda")

