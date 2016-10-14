##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

## \brief Add a C++ unittest executable and register it within CMake
##
## The exectuable will be linked against SerialboxCore and SerialboxUnittest.
##
## \param name          Name of the executable (i.e the <target>)
## \param libraries     Libraries to link against
## \param ARGN          List of source files
##
## Note: To pass in a list of elements you have to *quote* the variable i.e call 
##    serialbox_add_cpp_unittest_executable(Executable "${Libs}" SourceA.cpp SourceB.cpp)
##
function(serialbox_add_unittest_executable name libraries)
  add_executable(${name} ${ARGN} ${SERIALBOX_HEADERS})
  target_link_libraries(${name} ${libraries}  SerialboxUnittestUtility)
  
  if(DEFINED SERIALBOX_VERBOSE_WARNINGS)
    set_target_properties(${name} PROPERTIES COMPILE_FLAGS -Wall)
  endif()

  if(DEFINED BUILD_GTEST)
    add_dependencies(${name} gtest)
  endif()
  
  if(DEFINED BUILD_SERIALBOX_OLD)
    add_dependencies(${name} serialbox-old)
  endif()
  
  if(DEFINED BUILD_STELLA)
    add_dependencies(${name} stella)
  endif()

  if(DEFINED COVERAGE_SUPPORTED)
    set_property(TARGET ${name} PROPERTY COMPILE_FLAGS ${CMAKE_CXX_FLAGS_COVERAGE})
    set_property(TARGET ${name} PROPERTY LINK_FLAGS ${CMAKE_EXE_LINKER_FLAGS_COVERAGE})
    set_property(TARGET ${name} PROPERTY RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
    add_dependencies(coverage ${name})
  endif()

  add_test(NAME ${name} COMMAND $<TARGET_FILE:${name}> --gtest_color=yes)
endfunction(serialbox_add_unittest_executable)

## Add subdirectories
add_subdirectory(Utility)
  
add_subdirectory(serialbox)

if(SERIALBOX_ENABLE_C)
  add_subdirectory(serialbox-c)
endif()

if(SERIALBOX_TESTING_PYTHON)
  add_subdirectory(serialbox-python)
endif()

if(SERIALBOX_BENCHMARKING)
  add_subdirectory(Benchmark)
endif(SERIALBOX_BENCHMARKING)

