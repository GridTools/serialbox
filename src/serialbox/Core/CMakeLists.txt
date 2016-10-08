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
  SavepointVector.cpp
  SerializerImpl.cpp
  StorageView.cpp
  SHA256.cpp
  Type.cpp
  Unreachable.cpp
  
  Archive/ArchiveFactory.cpp
  Archive/BinaryArchive.cpp
  
  Frontend/STELLA/MetainfoSet.cpp
  Frontend/STELLA/Savepoint.cpp
  Frontend/STELLA/Serializer.cpp
  Frontend/STELLA/DataFieldInfo.cpp
)

set(EXTERNAL_LIBRARIES ${Boost_LIBRARIES} ${GLOG_LIBRARIES})

serialbox_add_library(SerialboxCore 
                      SerialboxCoreLibrary 
                      ""
                      TRUE
                      FALSE
                      "${EXTERNAL_LIBRARIES}"
                      ${SOURCES})
