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

# TODO doesn't make sense to build this as shared library, however yoda_create_library doesn't allow me to disable it
set(BUILD_SHARED_LIBS_TMP ${BUILD_SHARED_LIBS}) # TODO fix in yoda
set(BUILD_SHARED_LIBS OFF) # TODO fix in yoda

yoda_create_library(
   TARGET SerialboxUnittestUtility
   SOURCES ${SOURCES}
   #TODO the second one should come from DEPENDS Serialbox??? but it is exported from a different target...
   PRIVATE_BUILD_INCLUDES  ${CMAKE_SOURCE_DIR}/test ${CMAKE_SOURCE_DIR}/src
   LIBRARIES ${Boost_LIBRARIES}
   VERSION ${Serialbox_VERSION_STRING}
)
target_link_libraries(SerialboxUnittestUtilityObjects PUBLIC Boost::boost)
target_link_libraries(SerialboxUnittestUtilityObjects PUBLIC gtest_main)
target_link_libraries(SerialboxUnittestUtilityObjects PUBLIC SerialboxFilesytemTarget)
target_link_libraries(SerialboxUnittestUtilityStatic PUBLIC gtest_main)
target_link_libraries(SerialboxUnittestUtilityStatic PUBLIC Boost::boost)
set(BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS_TMP}) # TODO fix in yoda

