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
  BenchmarkOldSerialbox.cpp
  BenchmarkBinaryArchive.cpp
)

# Setup external libraries
set(EXTERNAL_LIBRARIES SerialboxCLibraryStatic 
                       SerialboxCoreLibraryStatic 
                       ${Boost_LIBRARIES} 
                       ${GTEST_LIBRARIES}
                       ${SERIALBOX_OLD_LIBRARIES}
                       ${STELLA_LIBRARIES})

serialbox_add_unittest_executable(SerialboxBenchmarkUnittest 
                                  "${EXTERNAL_LIBRARIES}"
                                  ${SOURCES} BenchmarkMain.cpp)

