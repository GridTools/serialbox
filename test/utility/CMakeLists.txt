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

add_library(SerialboxUnittestUtilityObjects OBJECT ${SOURCES})
target_include_directories(SerialboxUnittestUtilityObjects PUBLIC ${PROJECT_SOURCE_DIR}/test)
target_link_libraries(SerialboxUnittestUtilityObjects PUBLIC Boost::boost)
target_link_libraries(SerialboxUnittestUtilityObjects PUBLIC gtest_main)
target_link_libraries(SerialboxUnittestUtilityObjects PUBLIC SerialboxFilesytemTarget)
target_link_libraries(SerialboxUnittestUtilityObjects PUBLIC SerialboxStatic)
if(GRIDTOOLS_FOUND)
    target_link_libraries(SerialboxUnittestUtilityObjects PUBLIC GridTools_TARGET)
endif()

add_library(SerialboxUnittestUtilityStatic STATIC $<TARGET_OBJECTS:SerialboxUnittestUtilityObjects>)
set_target_properties(SerialboxUnittestUtilityStatic PROPERTIES VERSION ${Serialbox_VERSION_STRING})
target_link_libraries(SerialboxUnittestUtilityStatic PUBLIC SerialboxUnittestUtilityObjects)
