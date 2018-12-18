##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

## serialbox_add_unittest_executable
## ---------------------------------
##
## Add a unittest executable and register it within CMake
## 
##    NAME:STRING=<>                - Name of the unitests
##    LIBRARIES:STRING=<>           - Specify external libraries or flags to be use when linking the 
##                                    target (SerialboxUnittestUtility will be added).
##    SOURCES:STRING=<>             - List of source files
##    DEPENDS:STRING=<>             - Dependency on other targets
function(serialbox_add_unittest_executable)                 
  set(one_value_args NAME)
  set(multi_value_args LIBRARIES SOURCES DEPENDS)
  cmake_parse_arguments(ARG "${options}" "${one_value_args}" "${multi_value_args}" ${ARGN})

  set(libraries ${ARG_LIBRARIES} SerialboxUnittestUtility)
  set(sources ${ARG_SOURCES})
  set(depends ${ARG_DEPENDS})

  if(ARG_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR 
            "unparsed arguments: ${ARG_UNPARSED_ARGUMENTS}")
  endif()

  set(opt_arg)
  if(ARG_LIBRARIES)
    set(opt_arg ${opt_arg} LIBRARIES ${ARG_LIBRARIES})
  endif()
  add_executable(${ARG_NAME} ${sources})
  target_link_libraries(${ARG_NAME} SerialboxUnittestUtilityStatic ${depends} ${ARG_LIBRARIES})
                                    
  if(DEFINED SERIALBOX_VERBOSE_WARNINGS)
    set_target_properties(${ARG_NAME} PROPERTIES COMPILE_FLAGS -Wall)
  endif()

  if(DEFINED BUILD_SERIALBOX_OLD)
    add_dependencies(${ARG_NAME} serialbox-old)
  endif()
  
  if(DEFINED BUILD_STELLA)
    add_dependencies(${ARG_NAME} stella)
  endif()

  if(DEFINED COVERAGE_SUPPORTED)
    set_property(TARGET ${ARG_NAME} PROPERTY COMPILE_FLAGS ${CMAKE_CXX_FLAGS_COVERAGE})
    set_property(TARGET ${ARG_NAME} PROPERTY LINK_FLAGS ${CMAKE_EXE_LINKER_FLAGS_COVERAGE})
    set_property(TARGET ${ARG_NAME} PROPERTY RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
    add_dependencies(coverage ${ARG_NAME})
  endif()

  serialbox_add_test(TARGET ${ARG_NAME} 
                     EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/${ARG_NAME})
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

if(SERIALBOX_TESTING_FORTRAN)
  add_subdirectory(serialbox-fortran)
endif()

if(SERIALBOX_BENCHMARKING)
  add_subdirectory(benchmark)
endif(SERIALBOX_BENCHMARKING)
