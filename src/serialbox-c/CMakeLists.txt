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
  Serialbox.cpp
)

set(EXTERNAL_LIBRARIES ${Boost_LIBRARIES})

serialbox_add_library(SerialboxC 
                      SerialboxCLibrary 
                      SerialboxCoreLibraryObjects
                      TRUE
                      TRUE
                      "${EXTERNAL_LIBRARIES}"
                      ${SOURCES})