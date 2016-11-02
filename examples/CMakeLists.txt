##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

## \brief Create an install a C example
##
## \param name    STRING: Name of target and executable
## \param ARGN    LIST OF STRINGS: List of source files
function(serialbox_add_c_example name)
  add_executable(${name} ${ARGN} ${SERIALBOX_C_HEADERS})
  target_link_libraries(${name} 
                        SerialboxCLibraryStatic 
                        ${Boost_LIBRARIES} 
                        ${SERIALBOX_EXTERNAL_LIBRARIES}
                        ${CMAKE_THREAD_LIBS_INIT})
  install(TARGETS ${name} DESTINATION bin)
endfunction(serialbox_add_c_example)

## \brief Create and install a gridtools example
##
## \param name    STRING: Name of target and executable
## \param ARGN    LIST OF STRINGS: List of source files
function(serialbox_add_gridtools_example name)
  add_executable(${name} ${ARGN} ${SERIALBOX_HEADERS})
  target_link_libraries(${name} 
                        SerialboxCoreLibraryStatic 
                        ${Boost_LIBRARIES} 
                        ${SERIALBOX_EXTERNAL_LIBRARIES}
                        ${CMAKE_THREAD_LIBS_INIT})
  install(TARGETS ${name} DESTINATION bin)
endfunction(serialbox_add_gridtools_example)

## \brief Install a python example
##
## \param ARGN    LIST OF STRINGS: List of source files
function(serialbox_add_python_example)
  install(FILES ${ARGN} DESTINATION bin)
endfunction(serialbox_add_python_example)

## Build examples
if(SERIALBOX_ENABLE_C)
  add_subdirectory(C)
endif()

if(SERIALBOX_ENABLE_FORTRAN)
  add_subdirectory(Fortran)
endif()


if(SERIALBOX_ENABLE_PYTHON)
  add_subdirectory(python)
endif()

if(SERIALBOX_TESTING_GRIDTOOLS)
  add_subdirectory(gridtools)
endif()

