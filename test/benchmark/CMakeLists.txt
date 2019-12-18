##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

set(SOURCES 
  BenchmarkOldSerialbox.cpp
  BenchmarkSerialbox.cpp
)

# Setup external libraries
set(EXTERNAL_LIBRARIES ${Boost_LIBRARIES} 
                       ${GTEST_LIBRARIES}
                       ${SERIALBOX_OLD_LIBRARIES}
                       ${STELLA_LIBRARIES})

serialbox_add_unittest_executable(NAME SerialboxBenchmarkUnittest 
                                  LIBRARIES ${EXTERNAL_LIBRARIES}
                                  SOURCES ${SOURCES} BenchmarkMain.cpp 
                                  DEPENDS SerialboxCStatic SerialboxStatic
)

