##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# General Flags (add to default)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=gnu")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -fno-fast-math -ffree-line-length-none")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-form -fno-backslash ")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fimplicit-none -finline-functions")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wunderflow -Wline-truncation -Waliasing")

# Release
SET(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ftree-vectorize -funroll-loops")

# Debug Options (replace default)
SET(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -fbacktrace -fdump-core")
SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -ffpe-trap=invalid,zero,overflow")
SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fbounds-check -fcheck-array-temporaries")
SET(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -finit-integer=-99999999 -finit-real=nan")

