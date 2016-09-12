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
                     UnittestBinaryArchive.cpp
                     UnittestStorage.cpp
                     UnittestStorageView.cpp
                     UnittestStorageViewGridTools.cpp
                     UnittestStorageViewSTELLA.cpp
                     UnittestType.cpp
                     UnittestTree.cpp

                     GridTools.h
                     Storage.h
                     STELLA.h)
  
serialbox_add_unittest_executable(SerialboxUnittest "${UNITTEST_SOURCES}")
