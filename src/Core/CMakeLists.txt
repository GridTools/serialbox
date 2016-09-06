##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

set(CORE_SOURCES 
  # Support
  Logging.cpp
  Type.cpp
  StorageView.cpp
  StorageViewIterator.cpp
  Unreachable.cpp
    
  # Serializer
  
  # Archive
)

set(CORE_EXTERNAL_LIBRARIES)

serialbox_add_library(SerialboxCore "${CORE_SOURCES}" "${CORE_EXTERNAL_LIBRARIES}")
