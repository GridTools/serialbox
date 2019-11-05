##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

#.rst:
# SerialboxTooling
# ----------------
# 
# This module contains the `serialbox_run_pp_ser` function which is used to preprocess Fortran
# source code using the pp_ser.py script.
#
# Function arguments::
#
#   SOURCES       - Sources to preprocess
#   OUTPUT_DIR    - Output directory of the the source files. If nothing is specified 
#                   `${PROJECT_BINARY_DIR}/pp` is used. 
#

include(CMakeParseArguments)

function(serialbox_run_pp_ser)
  #===-------------------------------------------------------------------------------------------===
  # Parse arguments
  #===-------------------------------------------------------------------------------------------===
  set(options)
  set(oneValueArgs OUTPUT_DIR)
  set(multiValueArgs SOURCES)
  
  cmake_parse_arguments(serialbox_add_serialized_executable 
                        "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  
  set(output_dir ${serialbox_add_serialized_executable_OUTPUT_DIR})
  set(sources ${serialbox_add_serialized_executable_SOURCES})
  
  set(unparsed ${serialbox_add_serialized_executable_UNPARSED_ARGUMENTS})
  if(unparsed)
    message(WARNING "serialbox_run_pp_ser: unparsed arguments: ${unparsed}")
  endif()
  
  if(NOT(sources))
    message(FATAL_ERROR "serialbox_run_pp_ser: no source files provided")
  endif()
  
  if(NOT(output_dir))
    # If output_dir is not set, we place them in ${PROJECT_BINARY_DIR}/pp
    set(output_dir ${PROJECT_BINARY_DIR}/pp)  
  endif()
  
  # Create directory if it does not exist
  file(MAKE_DIRECTORY ${output_dir})
  
  #===-------------------------------------------------------------------------------------------===
  # Run pp_ser
  #===-------------------------------------------------------------------------------------------===
  set(pp_ser_input)
  set(pp_ser_output)
  foreach(file ${sources})
    get_filename_component(file_path ${file} ABSOLUTE)
    get_filename_component(file_name ${file} NAME)

    set(input_file ${file_path})
    set(output_file ${output_dir}/${file_name})
    
    # Output file is out-dated, preprocess again
    if(NOT(EXISTS(output_file) AND output_file IS_NEWER_THAN input_file))
      list(APPEND pp_ser_input ${input_file})
      list(APPEND pp_ser_output ${output_file})
    endif()
  endforeach()
  
  # Check for pp_ser
  if(NOT(DEFINED SERIALBOX_PPSER))
    message(FATAL_ERROR "Set \"SERIALBOX_PPSER\" to the location of the pp_ser.py script")
  endif()

  # Check for Python
  if(NOT(DEFINED PYTHONINTERP_FOUND))
    find_package(PythonInterp REQUIRED)
  endif()

  if(NOT(PYTHONINTERP_FOUND))
    message(FATAL_ERROR "pp_ser.py requires a python interpreter")
  endif()
  
  if(pp_ser_input)
    add_custom_command(OUTPUT ${pp_ser_output}
                       COMMAND ${PYTHON_EXECUTABLE} ${SERIALBOX_PPSER} --verbose 
                                                                       --ignore-identical 
                                                                       --output-dir=${output_dir} 
                                                                       ${pp_ser_input}
                       DEPENDS ${pp_ser_input}
                       COMMENT "Preprocessing for serialization")
  endif()
endfunction(serialbox_run_pp_ser)

