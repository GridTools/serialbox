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
  UnittestArchive.cpp
  UnittestConfig.cpp
  UnittestLogging.cpp
  UnittestFieldMetaInfo.cpp
  UnittestErrorHandling.cpp
  UnittestSavepoint.cpp
  UnittestSerializer.cpp
  UnittestMetaInfo.cpp
  UnittestFortranWrapper.cpp
)

serialbox_add_unittest_executable(NAME SerialboxCUnittest 
                                  SOURCES ${SOURCES} UnittestMain.cpp
                                  LIBRARIES SerialboxCStatic
                                  )

