##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

set(UNITTEST_SOURCES UnittestMain.cpp
                     UnittestStorageView.cpp
                     UnittestType.cpp)
  
serialbox_add_unittest_executable(SerialboxUnittest "${UNITTEST_SOURCES}")
