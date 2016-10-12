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
  UnittestArchive.cpp
  UnittestLogging.cpp
  UnittestFieldMetaInfo.cpp
  UnittestErrorHandling.cpp
  UnittestSavepoint.cpp
  UnittestSerializer.cpp
  UnittestMetaInfo.cpp
)

# Setup external libraries
set(EXTERNAL_LIBRARIES SerialboxCLibraryStatic 
                       ${Boost_LIBRARIES} 
                       ${SERIALBOX_EXTERNAL_LIBRARIES}
                       ${GTEST_LIBRARIES})
  
serialbox_add_unittest_executable(SerialboxCUnittest 
                                  "${EXTERNAL_LIBRARIES}"
                                  ${SOURCES} UnittestMain.cpp)

