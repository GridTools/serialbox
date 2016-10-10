##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

set(SOURCES 
  UnittestEnvironment.cpp
  FileUtility.h
  UnittestEnvironment.h
  GridTools.h
  Storage.h
  STELLA.h
  Serialbox.h
)

add_library(SerialboxUnittestUtility STATIC ${SOURCES})

if(DEFINED BUILD_GTEST)
  add_dependencies(SerialboxUnittestUtility gtest)
endif()

