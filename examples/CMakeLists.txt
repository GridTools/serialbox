##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

## \brief Create C example
##
## \param name    STRING: Name of target and executable
## \param ARGN    LIST OF STRINGS: List of source files
function(serialbox_add_c_example name)
  add_executable(${name} ${ARGN} ${SERIALBOX_C_HEADERS})
  target_link_libraries(${name} 
                        SerialboxCLibraryStatic 
                        ${Boost_LIBRARIES} 
                        ${SERIALBOX_EXTERNAL_LIBRARIES})
endfunction(serialbox_add_c_example)

## Build examples
if(SERIALBOX_ENABLE_C)
  add_subdirectory(C)
endif()

