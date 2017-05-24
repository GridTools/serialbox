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

function(serialbox_add_unittest_executable_fortran)                 
  cmake_parse_arguments(serialbox_add_unittest_executable_fortran "" "NAME" "LIBRARIES;SOURCES" ${ARGN})
  
  set(name ${serialbox_add_unittest_executable_fortran_NAME})
  set(libraries ${serialbox_add_unittest_executable_fortran_LIBRARIES} SerialboxUnittestUtility)
  set(sources ${serialbox_add_unittest_executable_fortran_SOURCES})
  
  set(current_test_dir ${PROJECT_BINARY_DIR}/${name}/generated)
  message(STATUS "libraries: ${libraries}" )
  file(MAKE_DIRECTORY ${current_test_dir})
  file(WRITE ${current_test_dir}/testSuites.inc "")
  
  include_directories(${current_test_dir} ${PFUNIT_INCLUDE_DIRS})
  
  #SerialboxFortran mod files
  #TODO: is there a better way to do this
  include_directories(${CMAKE_BINARY_DIR}/src/serialbox-fortran)
  
  set(_test_sources)
  message(STATUS "in sources: ${sources}")
  foreach(_test ${sources})
    message(STATUS "source: ${_test}")
    if(DEFINED BUILD_PFUNIT)
      set(test_dependency pfunit SerialboxFortranLibraryObjects ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.pf)
    else()
      set(test_dependency SerialboxFortranLibraryObjects ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.pf)
    endif()
    
    message(STATUS "using file: ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.F90")
    
    add_custom_command(
      OUTPUT ${current_test_dir}/${_test}.F90
      COMMAND ${PYTHON_EXECUTABLE} ${PFUNIT_BIN_DIR}/pFUnitParser.py ${CMAKE_CURRENT_SOURCE_DIR}/${_test}.pf ${current_test_dir}/${_test}.F90
      DEPENDS ${test_dependency}
      )
    set(_test_sources ${_test_sources} ${current_test_dir}/${_test}.F90)
    file(APPEND ${current_test_dir}/testSuites.inc "ADD_TEST_SUITE(${_test}_suite)\n")
  endforeach()
  
  if(serialbox_add_unittest_executable_fortran_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR 
            "unparsed arguments: ${serialbox_add_unittest_executable_fortran_UNPARSED_ARGUMENTS}")
  endif()

  message( STATUS "test sources: ${_test_sources}")
  add_executable(${name} ${_test_sources} ${PFUNIT_DRIVER})
  target_link_libraries(${name} ${PFUNIT_LIBRARIES})
  target_link_libraries(${name} ${libraries})
  
#  if(DEFINED SERIALBOX_VERBOSE_WARNINGS)
#    set_target_properties(${name} PROPERTIES COMPILE_FLAGS -Wall)
#  endif()
#
#  if(DEFINED BUILD_GTEST)
#    add_dependencies(${name} gtest)
#  endif()
#  
#  if(DEFINED BUILD_SERIALBOX_OLD)
#    add_dependencies(${name} serialbox-old)
#  endif()
#  
#  if(DEFINED BUILD_STELLA)
#    add_dependencies(${name} stella)
#  endif()
#
#  if(DEFINED COVERAGE_SUPPORTED)
#    set_property(TARGET ${name} PROPERTY COMPILE_FLAGS ${CMAKE_CXX_FLAGS_COVERAGE})
#    set_property(TARGET ${name} PROPERTY LINK_FLAGS ${CMAKE_EXE_LINKER_FLAGS_COVERAGE})
#    set_property(TARGET ${name} PROPERTY RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
#    add_dependencies(coverage ${name})
#  endif()

  serialbox_add_test(TARGET ${name} 
                     EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/${name})
endfunction(serialbox_add_unittest_executable_fortran)



set(SOURCES 
  serialbox_test
)

# Setup external libraries
set(EXTERNAL_LIBRARIES SerialboxFortranLibraryStatic
                       SerialboxCLibraryStatic 
                       ${Boost_LIBRARIES} 
                       ${SERIALBOX_EXTERNAL_LIBRARIES}
                       ${GTEST_LIBRARIES})
  
serialbox_add_unittest_executable_fortran(NAME SerialboxFortranUnittest 
                                  LIBRARIES ${EXTERNAL_LIBRARIES}
                                  SOURCES ${SOURCES})

