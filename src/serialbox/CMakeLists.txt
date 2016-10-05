##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

## \brief Add a library to the project using the specified source files
## 
## \param name          Name of the library (e.g SerialboxCore)
## \param target        Target used by CMkae (e.g SerialboxCoreLibrary)
## \param libraries     Specify libraries or flags to be use when linking the target
## \param ARGN          List of source files
##
## Note: To pass in a list of elements you have to *quote* the variable i.e call 
##    serialbox_add_library(Library "${ExternalLibs}" SourceA.cpp SourceB.cpp)
##
function(serialbox_add_library name target libraries)
  add_library(${target}Objects OBJECT ${ARGN} ${SERIALBOX_HEADERS})
  
  if(SERIALBOX_VERBOSE_WARNINGS)
    set_target_properties(${target}Objects PROPERTIES COMPILE_FLAGS -Wall)
  endif(SERIALBOX_VERBOSE_WARNINGS)

  if(BUILD_GLOG)
    add_dependencies(${target}Objects glog)
  endif(BUILD_GLOG)

  if(DEFINED COVERAGE_SUPPORTED)
    set_property(TARGET ${target}Objects PROPERTY COMPILE_FLAGS ${CMAKE_CXX_FLAGS_COVERAGE})
    set_property(TARGET ${target}Objects PROPERTY LINK_FLAGS ${CMAKE_SHARED_LINKER_FLAGS_COVERAGE})
    set_property(TARGET ${target}Objects PROPERTY RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
  endif()

  # Build static library
  add_library(${target}Static STATIC $<TARGET_OBJECTS:${target}Objects> $<TARGET_OBJECTS:sha256>)
  set_target_properties(${target}Static PROPERTIES OUTPUT_NAME ${name})
  target_link_libraries(${target}Static ${libraries})
  install(TARGETS ${target}Static DESTINATION lib)

  # Build shared library
  if(SERIALBOX_BUILD_SHARED)
    add_library(${target}Shared SHARED $<TARGET_OBJECTS:${target}Objects> $<TARGET_OBJECTS:sha256>)
    set_target_properties(${target}Shared PROPERTIES OUTPUT_NAME ${name})
    target_link_libraries(${target}Shared ${libraries})
    install(TARGETS ${target}Shared DESTINATION lib)
  endif()

endfunction(serialbox_add_library)

## Add subdirectories
add_subdirectory(external)
add_subdirectory(Core)

