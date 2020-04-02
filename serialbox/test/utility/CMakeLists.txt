##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

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
target_include_directories(SerialboxUnittestUtility PUBLIC ${PROJECT_SOURCE_DIR}/test)
target_link_libraries(SerialboxUnittestUtility PUBLIC Boost::boost)
target_link_libraries(SerialboxUnittestUtility PUBLIC gtest_main)
target_link_libraries(SerialboxUnittestUtility PUBLIC SerialboxFilesytemTarget)
target_link_libraries(SerialboxUnittestUtility PUBLIC Serialbox)
if(GRIDTOOLS_FOUND)
    target_link_libraries(SerialboxUnittestUtility PUBLIC GridTools::gridtools)
endif()
