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
  UnittestArray.cpp
  UnittestException.cpp
  UnittestFieldMap.cpp
  UnittestFieldMetaInfo.cpp
  UnittestFieldID.cpp
  UnittestMetaInfoMap.cpp
  UnittestMetaInfoValue.cpp
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
  
  # Archive/  
  Archive/UnittestArchiveFactory.cpp 
  Archive/UnittestBinaryArchive.cpp
  Archive/UnittestNetCDFArchive.cpp
  
  # Frontend/gridtools/
  Frontend/gridtools/UnittestStorageView.cpp
  Frontend/gridtools/UnittestMetaInfoMap.cpp
  Frontend/gridtools/UnittestFieldMetaInfo.cpp  
  Frontend/gridtools/UnittestSavepoint.cpp    
  Frontend/gridtools/UnittestSerializer.cpp    
  
  # Frontend/STELLA/
  Frontend/STELLA/UnittestStorageView.cpp
  Frontend/STELLA/UnittestMetainfoSet.cpp
  Frontend/STELLA/UnittestException.cpp
  Frontend/STELLA/UnittestSavepoint.cpp  
  Frontend/STELLA/UnittestSerializer.cpp
  Frontend/STELLA/UnittestDataFieldInfo.cpp
  Frontend/STELLA/UnittestUtility.cpp  
)

set(EXTERNAL_LIBRARIES SerialboxCoreLibraryStatic
                       ${Boost_LIBRARIES}
                       ${SERIALBOX_EXTERNAL_LIBRARIES} 
                       ${GTEST_LIBRARIES} 
                       ${SERIALBOX_OLD_LIBRARIES}
                       ${STELLA_LIBRARIES})
  
serialbox_add_unittest_executable(SerialboxCppUnittest 
                                  "${EXTERNAL_LIBRARIES}"
                                  ${SOURCES} UnittestMain.cpp)

