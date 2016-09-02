##===------------------------------------------------------------------------------------------===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

set(SUPPORT_SOURCES Logging.cpp
                    Type.cpp
                    StorageView.cpp)
set(SUPPORT_EXTERNAL_LIBRARIES)

serialbox_add_library(SerialboxSupport "${SUPPORT_SOURCES}" "${SUPPORT_EXTERNAL_LIBRARIES}")
