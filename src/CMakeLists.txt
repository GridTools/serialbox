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
## \param name          Name of the library (i.e the <target>)
## \param libraries     Specify libraries or flags to be use when linking the target
## \param ARGN          List of source files
##
## Note: To pass in a list of elements you have to *quote* the variable i.e call 
##    serialbox_add_library(Library "${ExternalLibs}" SourceA.cpp SourceB.cpp)
##
function(serialbox_add_library name libraries)
  add_library(${name}Objects OBJECT ${ARGN} ${SERIALBOX_HEADERS})

  # Build static library
  add_library(${name} STATIC $<TARGET_OBJECTS:${name}Objects>)
  set_target_properties(${name} PROPERTIES OUTPUT_NAME ${name})
  target_link_libraries(${name} ${libraries})

  # Build shared library
#  add_library(${name}Shared SHARED $<TARGET_OBJECTS:${name}Objects>)
#  set_target_properties(${name}Shared PROPERTIES OUTPUT_NAME ${name})
#  target_link_libraries(${name}Shared ${libraries})
  
  if(SERIALBOX_VERBOSE_WARNINGS)
    set_target_properties(${name}Objects PROPERTIES COMPILE_FLAGS -Wall)
  endif(SERIALBOX_VERBOSE_WARNINGS)

  add_dependencies(${name}Objects sha256)

  if(BUILD_GLOG)
    add_dependencies(${name}Objects glog)
  endif(BUILD_GLOG)

endfunction(serialbox_add_library)

## Add subdirectories
add_subdirectory(Core)

