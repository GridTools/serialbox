##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

enable_language(Fortran)

file(GLOB SOURCES 
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.pf)
string(REPLACE ".pf" "" SOURCES "${SOURCES}")

message(STATUS "sources: ${SOURCES}")

# Setup external libraries
set(EXTERNAL_LIBRARIES SerialboxFortranLibraryStatic
                       SerialboxCLibraryStatic 
                       ${Boost_LIBRARIES} 
                       ${SERIALBOX_EXTERNAL_LIBRARIES}
                       ${GTEST_LIBRARIES})

set(generated_src_dir ${PROJECT_BINARY_DIR}/generated)
file(MAKE_DIRECTORY ${generated_src_dir})
file(WRITE ${generated_src_dir}/testSuites.inc "")
  
#SerialboxFortran mod files
#TODO: is there a better way to do this
include_directories(${CMAKE_BINARY_DIR}/src/serialbox-fortran)
include_directories(${generated_src_dir} ${PFUNIT_INCLUDE_DIRS})
  
set(_test_sources)
foreach(_test ${SOURCES})
  message(STATUS "source: ${_test}")
  if(DEFINED BUILD_PFUNIT)
    set(test_dependency pfunit SerialboxFortranLibraryObjects ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.pf)
  else()
    set(test_dependency SerialboxFortranLibraryObjects ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.pf)
  endif()
  
  message(STATUS "dep ${test_dependency}")
    
  add_custom_command(
    OUTPUT ${generated_src_dir}/${_test}.F90
    COMMAND ${PYTHON_EXECUTABLE} ${PFUNIT_BIN_DIR}/pFUnitParser.py ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.pf ${generated_src_dir}/${_test}.F90
    DEPENDS ${test_dependency}
    )
  set(_test_sources ${_test_sources} ${generated_src_dir}/${_test}.F90)
  file(APPEND ${generated_src_dir}/testSuites.inc "ADD_TEST_SUITE(${_test}_suite)\n")
endforeach(_test)

message(STATUS "test sources: ${_test_sources}")

add_executable(SerialboxFortranUnittest ${_test_sources} ${PFUNIT_DRIVER})
target_link_libraries(SerialboxFortranUnittest ${PFUNIT_LIBRARIES})
target_link_libraries(SerialboxFortranUnittest ${EXTERNAL_LIBRARIES})

serialbox_add_test(TARGET SerialboxFortranUnittest 
                   EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/SerialboxFortranUnittest)
