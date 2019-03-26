##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

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

add_library(SerialboxUnittestUtility STATIC ${SOURCES})
set_target_properties(SerialboxUnittestUtility PROPERTIES VERSION ${Serialbox_VERSION_STRING})
target_include_directories(SerialboxUnittestUtility PUBLIC ${CMAKE_SOURCE_DIR}/test)
target_link_libraries(SerialboxUnittestUtility PUBLIC Boost::boost)
target_link_libraries(SerialboxUnittestUtility PUBLIC gtest_main) #TODO use target
target_link_libraries(SerialboxUnittestUtility PUBLIC SerialboxFilesytemTarget)
target_link_libraries(SerialboxUnittestUtility PUBLIC Serialbox)
if(GRIDTOOLS_FOUND)
    target_link_libraries(SerialboxUnittestUtility PUBLIC GridTools_TARGET) #TODO use the official GridTools target
endif()
