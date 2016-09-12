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
  StorageView.cpp
  StorageViewIterator.cpp
  Type.cpp
  Unreachable.cpp
  
  Archive/BinaryArchive.cpp
)

set(EXTERNAL_LIBRARIES ${BOOST_LIBRARIES} ${GLOG_LIBRARIES})

serialbox_add_library(SerialboxCore "${SOURCES}" "${EXTERNAL_LIBRARIES}")
