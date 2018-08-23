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
  UnittestArray.cpp
  UnittestException.cpp
  UnittestFieldMap.cpp
  UnittestFieldMetainfoImpl.cpp
  UnittestFieldID.cpp
  UnittestMetainfoMapImpl.cpp
  UnittestMetainfoValueImpl.cpp
  UnittestStorage.cpp
  UnittestStorageView.cpp
  UnittestSavepointImpl.cpp
  UnittestSavepointVector.cpp
  UnittestSerializerImpl.cpp
  UnittestSlice.cpp
  UnittestType.cpp
  UnittestUnreachable.cpp
  UnittestUpgradeArchive.cpp
  UnittestVersion.cpp
  
  # archive/  
  archive/UnittestArchiveFactory.cpp 
  archive/UnittestBinaryArchive.cpp
  archive/UnittestNetCDFArchive.cpp
  archive/UnittestMockArchive.cpp
  
  # frontend/gridtools/
  frontend/gridtools/UnittestStorageView.cpp
  frontend/gridtools/UnittestMetainfoMap.cpp
  frontend/gridtools/UnittestFieldMetaInfo.cpp  
  frontend/gridtools/UnittestSavepoint.cpp    
  frontend/gridtools/UnittestSerializer.cpp    
  
  # frontend/stella/
  frontend/stella/UnittestStorageView.cpp
  frontend/stella/UnittestMetainfoSet.cpp
  frontend/stella/UnittestException.cpp
  frontend/stella/UnittestSavepoint.cpp  
  frontend/stella/UnittestSerializer.cpp
  frontend/stella/UnittestDataFieldInfo.cpp
  frontend/stella/UnittestUtility.cpp  
)

set(EXTERNAL_LIBRARIES 
                       ${Boost_LIBRARIES}
                       ${GTEST_LIBRARIES} 
                       ${STELLA_LIBRARIES})
  
serialbox_add_unittest_executable(NAME SerialboxCppUnittest 
                                  LIBRARIES ${EXTERNAL_LIBRARIES}
                                  SOURCES ${SOURCES} UnittestMain.cpp
                                  DEPENDS SerialboxCoreStatic
                                  INCLUDE_DIRECTORIES ${CMAKE_SOURCE_DIR}/test
)

