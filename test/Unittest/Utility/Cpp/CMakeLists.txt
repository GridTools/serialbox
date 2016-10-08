##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

set(CPP_UNITTEST_UTILITY_SOURCES 
  UnittestEnvironment.cpp
  FileUtility.h
  UnittestEnvironment.h
  GridTools.h
  Storage.h
  STELLA.h
  Serialbox.h
)

add_library(SerialboxCppUnittestUtilityLibrary STATIC ${CPP_UNITTEST_UTILITY_SOURCES})

if(DEFINED BUILD_GTEST)
  add_dependencies(SerialboxCppUnittestUtilityLibrary gtest)
endif()

if(BUILD_GLOG)
  add_dependencies(SerialboxCppUnittestUtilityLibrary glog)
endif(BUILD_GLOG)
