##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

set(CPP_UNITTEST_SOURCES 
  UnittestBinaryArchive.cpp
  UnittestStorage.cpp
  UnittestStorageView.cpp
  UnittestStorageViewGridTools.cpp
  UnittestStorageViewSTELLA.cpp
  UnittestType.cpp
  UnittestVersion.cpp
  UnittestException.cpp
  UnittestUnreachable.cpp
  UnittestMetaInfoMap.cpp
  UnittestMetaInfoValue.cpp
 
  # Utility
  UnittestEnvironment.cpp
  UnittestMain.cpp
  FileUtility.h
  UnittestEnvironment.h
  GridTools.h
  Storage.h
  STELLA.h
)
  
serialbox_add_cpp_unittest_executable(SerialboxCppUnittest 
                                      "${SERIALBOX_EXTERNAL_LIBRARIES}"
                                      ${CPP_UNITTEST_SOURCES} UnittestMain.cpp)
                                     
#set(CPP_COVERAGE_SOURCES 
#  UnittestBinaryArchive.cpp
#  UnittestStorage.cpp
#  UnittestStorageView.cpp
#  UnittestStorageViewGridTools.cpp
#  UnittestStorageViewSTELLA.cpp
#  UnittestType.cpp
#  UnittestVersion.cpp
#  UnittestException.cpp
#  UnittestUnreachable.cpp
#  UnittestMetaInfoMap.cpp
#)
#                                     
# serialbox_add_cpp_coverage_executable("${SERIALBOX_EXTERNAL_LIBRARIES}" ${CPP_COVERAGE_SOURCES})
#
