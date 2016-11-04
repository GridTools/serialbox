##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

## Create an install a C example
##
## name    
##    STRING: Name of target and executable
## ARGN    
##    LIST OF STRINGS: List of source files
function(serialbox_add_c_example name)
  add_executable(${name} ${ARGN} ${SERIALBOX_C_HEADERS})
  target_link_libraries(${name} 
                        SerialboxCLibraryStatic 
                        ${Boost_LIBRARIES} 
                        ${SERIALBOX_EXTERNAL_LIBRARIES}
                        ${CMAKE_THREAD_LIBS_INIT})
  install(TARGETS ${name} DESTINATION bin)
endfunction(serialbox_add_c_example)

## Create and install a gridtools example
##
## name    
##    STRING: Name of target and executable
## ARGN    
##    LIST OF STRINGS: List of source files
function(serialbox_add_gridtools_example name)
  add_executable(${name} ${ARGN} ${SERIALBOX_HEADERS})
  target_link_libraries(${name} 
                        SerialboxCoreLibraryStatic 
                        ${Boost_LIBRARIES} 
                        ${SERIALBOX_EXTERNAL_LIBRARIES}
                        ${CMAKE_THREAD_LIBS_INIT})
  install(TARGETS ${name} DESTINATION bin)
endfunction(serialbox_add_gridtools_example)

## Create and install a Fortran example
##
## name    
##    STRING: Name of target and executable
## ARGN    
##    LIST OF STRINGS: List of source files
function(serialbox_add_fortran_example name)
  add_executable(${name} ${ARGN} ${SERIALBOX_HEADERS})
  target_link_libraries(${name} 
                        SerialboxFortranLibraryStatic 
                        SerialboxCLibraryStatic
                        ${Boost_LIBRARIES} 
                        ${SERIALBOX_EXTERNAL_LIBRARIES}
                        ${CMAKE_THREAD_LIBS_INIT})
  target_include_directories(${name} PUBLIC ${CMAKE_BINARY_DIR}/src/serialbox-fortran)
  link_directories(${CMAKE_BINARY_DIR}/src/serialbox-fortran)
  install(TARGETS ${name} DESTINATION bin)
endfunction(serialbox_add_fortran_example)

## Install a python example
##
## ARGN    
##    LIST OF STRINGS: List of source files
function(serialbox_add_python_example)
  install(FILES ${ARGN} DESTINATION bin)
endfunction(serialbox_add_python_example)

## Build examples
if(SERIALBOX_ENABLE_C)
  add_subdirectory(C)
endif()

if(SERIALBOX_ENABLE_FORTRAN)
  add_subdirectory(fortran)
endif()


if(SERIALBOX_ENABLE_PYTHON)
  add_subdirectory(python)
endif()

if(SERIALBOX_TESTING_GRIDTOOLS)
  add_subdirectory(gridtools)
endif()

