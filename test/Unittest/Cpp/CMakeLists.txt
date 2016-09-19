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
  UnittestFieldMap.cpp
  UnittestFieldMetaInfo.cpp
  UnittestStorage.cpp
  UnittestStorageView.cpp
  UnittestType.cpp
  UnittestVersion.cpp
  UnittestException.cpp
  UnittestUnreachable.cpp
  UnittestMetaInfoMap.cpp
  UnittestMetaInfoValue.cpp
  
  UnittestGridToolsStorageView.cpp
 
  UnittestSTELLAStorageView.cpp
)
  
serialbox_add_cpp_unittest_executable(SerialboxCppUnittest 
                                      "${SERIALBOX_EXTERNAL_LIBRARIES}"
                                      ${CPP_UNITTEST_SOURCES} UnittestMain.cpp)

## Add subdirectories
add_subdirectory(Utility)

