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
  Logging.cpp
  MetaInfoMap.cpp
  MetaInfoValue.cpp
  SerializerImpl.cpp
  StorageView.cpp
  StorageViewIterator.cpp
  Type.cpp
  Unreachable.cpp
  SHA256.cpp
  
  Archive/BinaryArchive.cpp
)

set(EXTERNAL_LIBRARIES ${BOOST_LIBRARIES} ${GLOG_LIBRARIES} ${SHA256_LIBRARIES})

serialbox_add_library(SerialboxCore "${EXTERNAL_LIBRARIES}" ${SOURCES})
