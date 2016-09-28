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
  UnittestArchiveFactory.cpp 
  UnittestBinaryArchive.cpp
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
  
  # gridtools
  UnittestGridToolsStorageView.cpp
 
  # STELLA
  UnittestSTELLAStorageView.cpp
  UnittestSTELLAMetainfoSet.cpp
  UnittestSTELLAException.cpp
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

## Add subdirectories
add_subdirectory(Utility)

