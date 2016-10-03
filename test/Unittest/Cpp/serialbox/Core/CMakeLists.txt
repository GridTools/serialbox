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
  UnittestType.cpp
  UnittestUnreachable.cpp
  UnittestUpgradeArchive.cpp
  UnittestVersion.cpp
  
  # Archive/  
  Archive/UnittestArchiveFactory.cpp 
  Archive/UnittestBinaryArchive.cpp
  
  # Frontend/gridtools/
  Frontend/gridtools/UnittestStorageView.cpp
 
  # Frontend/STELLA/
  Frontend/STELLA/UnittestStorageView.cpp
  Frontend/STELLA/UnittestMetainfoSet.cpp
  Frontend/STELLA/UnittestException.cpp
  Frontend/STELLA/UnittestSavepoint.cpp  
  Frontend/STELLA/UnittestSerializer.cpp
  Frontend/STELLA/UnittestDataFieldInfo.cpp
)

# Setup external libraries
set(CPP_EXTERNAL_LIBRARIES ${Boost_LIBRARIES} 
                           ${GTEST_LIBRARIES} 
                           ${GLOG_LIBRARIES} 
                           ${SHA256_LIBRARIES}
                           ${SERIALBOX_OLD_LIBRARIES}
                           ${STELLA_LIBRARIES})
  
serialbox_add_cpp_unittest_executable(SerialboxCppUnittest 
                                      "${CPP_EXTERNAL_LIBRARIES}"
                                      ${CPP_UNITTEST_SOURCES} UnittestMain.cpp)

