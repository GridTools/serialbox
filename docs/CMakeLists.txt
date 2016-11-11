##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

set(DOC_DIR ${PROJECT_SOURCE_DIR}/docs)

## Find doxygen
find_package(Doxygen)
if(NOT DOXYGEN_FOUND)
  message(FATAL_ERROR "Doxygen is needed to build the documentation.")
endif()

set(doxyfile_in ${CMAKE_CURRENT_SOURCE_DIR}/doxygen/Doxyfile.in)
set(doxyfile ${CMAKE_CURRENT_BINARY_DIR}/doxygen/Doxyfile)
configure_file(${doxyfile_in} ${doxyfile} @ONLY)

add_custom_target(doxygen
  COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Generating doxygen documentation" VERBATIM
)

## Find sphinx
find_package(Sphinx)
if(NOT SPHINX_FOUND)
  message(FATAL_ERROR "Sphinx is needed to build the documentation.")
endif()

add_custom_target(sphinx
  COMMAND ${SPHINX_EXECUTABLE} -b html -d ${CMAKE_CURRENT_SOURCE_DIR}/sphinx/_build/doctrees 
          ${CMAKE_CURRENT_SOURCE_DIR}/sphinx/ ${CMAKE_CURRENT_SOURCE_DIR}/sphinx/_build/html
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Generating sphinx documentation" VERBATIM
)

## Generate documentation
add_custom_target(docs
  COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
  COMMAND ${SPHINX_EXECUTABLE} -b html -d ${CMAKE_CURRENT_SOURCE_DIR}/sphinx/_build/doctrees 
          ${CMAKE_CURRENT_SOURCE_DIR}/sphinx/ ${CMAKE_CURRENT_SOURCE_DIR}/sphinx/_build/html
  COMMAND ${CMAKE_COMMAND} -E copy_directory
          ${CMAKE_SOURCE_DIR}/docs/doxygen/html
          ${CMAKE_SOURCE_DIR}/docs/sphinx/_build/html/_doxygen/html
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMENT "Generating documentation" VERBATIM
)
