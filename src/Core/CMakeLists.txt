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
  FieldMap.cpp
  FieldMetaInfo.cpp
  FieldID.cpp
  Logging.cpp
  MetaInfoMap.cpp
  MetaInfoValue.cpp
  SavepointImpl.cpp
  Savepoint.cpp
  SavepointVector.cpp
  SerializerImpl.cpp
  StorageView.cpp
  StorageViewIterator.cpp
  SHA256.cpp
  Type.cpp
  Unreachable.cpp
  
  Archive/BinaryArchive.cpp
)

set(EXTERNAL_LIBRARIES ${BOOST_LIBRARIES} ${GLOG_LIBRARIES} ${SHA256_LIBRARIES})

serialbox_add_library(SerialboxCore "${EXTERNAL_LIBRARIES}" ${SOURCES})
