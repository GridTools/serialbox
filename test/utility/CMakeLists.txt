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
  BenchmarkEnvironment.h
  BenchmarkEnvironment.cpp
  CInterfaceTestBase.h
  CInterfaceTestBase.cpp  
  FileUtility.h
  GridTools.h
  Storage.h
  STELLA.h
  Serialbox.h
  SerializerTestBase.h
  UnittestEnvironment.h
  UnittestEnvironment.cpp
)

yoda_create_library(
   TARGET SerialboxUnittestUtility
   SOURCES ${SOURCES}
   PRIVATE_BUILD_INCLUDES  ${CMAKE_SOURCE_DIR}/test
   LIBRARIES ${Boost_LIBRARIES} ${GTEST_LIBRARIES}
)

