##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

## Find bash
find_program(BASH_EXECUTABLE bash)
if(NOT BASH_EXECUTABLE)
  message(FATAL_ERROR "Cannot find bash to execute python test script")
endif()

## Location of python modules
set(SERIALOBX_PYTHON_MODULE ${PROJECT_SOURCE_DIR}/src/serialbox-python)

## Configure the test script
set(PYTHON_TEST_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(PYTHON_TEST_SCRIPT_IN ${CMAKE_CURRENT_SOURCE_DIR}/run_python_tests.sh.cmake)
set(PYTHON_TEST_SCRIPT ${CMAKE_CURRENT_BINARY_DIR}/run_python_tests.sh)
configure_file(${PYTHON_TEST_SCRIPT_IN} ${PYTHON_TEST_SCRIPT})

## Register test
serialbox_add_test(EXECUTABLE ${BASH_EXECUTABLE} ${PYTHON_TEST_SCRIPT})

