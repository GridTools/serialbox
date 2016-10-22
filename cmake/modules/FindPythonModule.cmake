# Try to find a specific python module.
#
# Usage of this module as follows:
#
#   find_python_module(PyQt4 REQUIRED)
#
# The module makes use of the ${PYTHON_EXECUTABLE} which needs to be set before calling the 
# find_python_module function.
#
# Variables defined by this cmake module:
#
#   PY_${module}_FOUND   Python module is avialable where ${module} is the python module to search 
#                        for in all uppercase (i.e for the example above it would be PY_PYQT4_FOUND)
#   PY_${module}_PATH    Path to the module where ${module} is the python module to search for in 
#                        all uppercase (i.e for the example above it would be PY_PYQT4_PATH)
#

function(find_python_module module)
  string(TOUPPER ${module} module_upper)

  if(NOT PY_${module_upper}_FOUND)
    # Check if module is required
    if(ARGC GREATER 1 AND ARGV1 STREQUAL "REQUIRED")
      set(${module}_FIND_REQUIRED TRUE)
    endif()

    # A module's location is usually a directory, but for binary modules it's a .so file.
    execute_process(COMMAND "${PYTHON_EXECUTABLE}" "-c" 
      "import re, ${module}; print(re.compile('/__init__.py.*').sub('',${module}.__file__))"
      RESULT_VARIABLE _${module}_status 
      OUTPUT_VARIABLE _${module}_location
      ERROR_QUIET 
      OUTPUT_STRIP_TRAILING_WHITESPACE)

    # Set result
    if(NOT _${module}_status)
      message(STATUS "Found python module \"${module}\" for python: ${PYTHON_EXECUTABLE}")
      set(PY_${module_upper}_PATH ${_${module}_location} 
          CACHE STRING "Location of Python module ${module}")
      set(PY_${module_upper}_FOUND TRUE
          CACHE STRING "Python module ${module} found")
      mark_as_advanced(PY_${module_upper}_PATH PY_${module_upper}_FOUND)
    else()
      set(PY_${module_upper}_FOUND FALSE)
        if(${module}_FIND_REQUIRED)
          message(FATAL_ERROR "Could NOT find module ${module} for python ${PYTHON_EXECUTABLE}")
        else()
          message(STATUS "Could NOT find \"${module}\" for python: ${PYTHON_EXECUTABLE}")
        endif()
    endif()
  endif(NOT PY_${module_upper}_FOUND)
endfunction(find_python_module)

