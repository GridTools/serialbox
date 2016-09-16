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
## The exectuable will be linked against SerialboxCore and SerialboxCppUnittestUtility.
##
## \param name          Name of the executable (i.e the <target>)
## \param libraries     Libraries to link against
## \param ARGN          List of source files
##
## Note: To pass in a list of elements you have to *quote* the variable i.e call 
##    serialbox_add_cpp_unittest_executable(Executable "${Libs}" SourceA.cpp SourceB.cpp)
##
function(serialbox_add_cpp_unittest_executable name libraries)
  add_executable(${name} ${ARGN} ${SERIALBOX_HEADERS})
  target_link_libraries(${name} ${libraries} 
                                SerialboxCppUnittestUtility 
                                SerialboxCore)
  
  if(SERIALBOX_VERBOSE_WARNINGS)
    set_target_properties(${name} PROPERTIES COMPILE_FLAGS -Wall)
  endif(SERIALBOX_VERBOSE_WARNINGS)

  if(BUILD_GTEST)
    add_dependencies(${name} gtest)
  endif(BUILD_GTEST)

  if(BUILD_GLOG)
    add_dependencies(${name} glog)
  endif(BUILD_GLOG)
  
  add_test(NAME test_${name} COMMAND $<TARGET_FILE:${name}> --gtest_color=yes)
endfunction(serialbox_add_cpp_unittest_executable)

## \brief Add a C++ coverage executable and register it within CMake
##
## For each source file an executable will be generated and linked against SerialboxCore
## and SerialboxCppUnittestUtility.
##
## \param libraries       Libraries to link against
## \param main_executable Main exectuble to link against
## \param ARGN            List of source files
##
function(serialbox_add_cpp_coverage_executable libraries main_executable)
  if(DEFINED COVERAGE_SUPPORTED)
    add_library(SerialboxCoverageMain OBJECT ${main_executable})

    foreach(file ${ARGN})
      string(REPLACE ".cpp" ".coverage" test ${file})
      set(test SerialboxCpp${test})

      serialbox_add_cpp_unittest_executable(${test} "${libraries}" 
                                            $<TARGET_OBJECTS:SerialboxCoverageMain> ${file})

      set_property(TARGET ${test} PROPERTY COMPILE_FLAGS ${CMAKE_CXX_FLAGS_COVERAGE})
      set_property(TARGET ${test} PROPERTY LINK_FLAGS ${CMAKE_EXE_LINKER_FLAGS_COVERAGE})
      set_property(TARGET ${test} PROPERTY RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/test")

      add_test(NAME coverage_${test} 
               WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/test" 
               COMMAND $<TARGET_FILE:${test}>)
      add_dependencies(coverage ${test})

    endforeach()
  endif()
endfunction()

## Add subdirectories
add_subdirectory(Cpp)

