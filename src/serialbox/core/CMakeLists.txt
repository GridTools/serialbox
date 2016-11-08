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
  FieldMetainfoImpl.cpp
  FieldID.cpp
  Logging.cpp
  MetainfoMapImpl.cpp
  MetainfoValueImpl.cpp
  SavepointImpl.cpp
  SavepointVector.cpp
  SerializerImpl.cpp
  StorageView.cpp
  Type.cpp
  Unreachable.cpp
  
  hash/HashFactory.cpp
  hash/SHA256.cpp
  hash/MD5.cpp
  
  archive/ArchiveFactory.cpp
  archive/BinaryArchive.cpp
  archive/NetCDFArchive.cpp
  archive/MockArchive.cpp
  
  frontend/stella/MetainfoSet.cpp
  frontend/stella/Savepoint.cpp
  frontend/stella/Serializer.cpp
  frontend/stella/DataFieldInfo.cpp
)

set(EXTERNAL_LIBRARIES ${Boost_LIBRARIES} ${SERIALBOX_EXTERNAL_LIBRARIES})

serialbox_add_library(NAME SerialboxCore 
                      TARGET SerialboxCoreLibrary 
                      BUILD_STATIC
                      BUILD_SHARED
                      LIBRARIES ${EXTERNAL_LIBRARIES}
                      SOURCES ${SOURCES})
