##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

## serialbox_add_unittest_executable
## ---------------------------------
##
## Add a unittest executable and register it within CMake
## 
##    NAME:STRING=<>           - Name of the unitests
##    LIBRARIES:STRING=<>      - Specify external libraries or flags to be use when linking the 
##                               target (SerialboxUnittestUtility will be added).
##    SOURCES:STRING=<>        - List of source files
##
function(serialbox_add_unittest_executable)                 
  cmake_parse_arguments(serialbox_add_unittest_executable "" "NAME" "LIBRARIES;SOURCES" ${ARGN})
  
  set(name ${serialbox_add_unittest_executable_NAME})
  set(libraries ${serialbox_add_unittest_executable_LIBRARIES} SerialboxUnittestUtility)
  set(sources ${serialbox_add_unittest_executable_SOURCES})
  
  if(serialbox_add_unittest_executable_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR 
            "unparsed arguments: ${serialbox_add_unittest_executable_UNPARSED_ARGUMENTS}")
  endif()

  add_executable(${name} ${sources} ${SERIALBOX_HEADERS})
  target_link_libraries(${name} ${libraries})
  
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

  serialbox_add_test(TARGET ${name} 
                     EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/${name})
endfunction(serialbox_add_unittest_executable)

## Add subdirectories
add_subdirectory(utility)
add_subdirectory(serialbox)

if(SERIALBOX_ENABLE_C)
  add_subdirectory(serialbox-c)
endif()

if(SERIALBOX_TESTING_PYTHON)
  add_subdirectory(serialbox-python)
endif()

if(SERIALBOX_BENCHMARKING)
  add_subdirectory(benchmark)
endif(SERIALBOX_BENCHMARKING)

