##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

## Compile serialboxFortran
set(SOURCES m_serialize.f90 m_ser_perturb.f90 utils_ppser.f90)

serialbox_add_library(NAME SerialboxFortran 
                      TARGET SerialboxFortranLibrary 
                      BUILD_STATIC 
                      SOURCES ${SOURCES})
