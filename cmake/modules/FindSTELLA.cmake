# Try to find STELLA libraries and headers.
#
# Usage of this module as follows:
#
#   find_package(STELLA)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
#   STELLA_ROOT          Set this variable to the root installation of
#                        STELLA if the module has problems finding the
#                        proper installation path.
#
# Variables defined by this module:
#
#   STELLA_FOUND         System has STELLA libraries and headers
#   STELLA_INCLUDE_DIRS  The location of STELLA headers
#   STELLA_LIBRARIES     The location of STELLA libraries

find_path(STELLA_ROOT NAMES include/STELLA)

# Look for Serialbox libraries
function(stella_find_library name)
  find_library(${name} NAMES ${ARGN} HINTS $ENV{STELLA_ROOT}/lib ${STELLA_ROOT}/lib)
  mark_as_advanced(${name})
endfunction()

stella_find_library(STELLA_LIBRARY_SHARED_INFRASTRUCTURE SharedInfrastructure)
stella_find_library(STELLA_LIBRARY_STELLA Stella)
stella_find_library(STELLA_LIBRARY_STELLA_UTILS StellaUtils)

set(STELLA_LIBRARIES
    ${STELLA_LIBRARY_SHARED_INFRASTRUCTURE}
    ${STELLA_LIBRARY_STELLA}
    ${STELLA_LIBRARY_STELLA_UTILS}
)

# Look for STELLA headers
find_path(STELLA_INCLUDE_DIRS
    NAMES Stencil.h
    HINTS ${STELLA_ROOT}/include/STELLA
          $ENV{STELLA_ROOT}/include/STELLA
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(STELLA DEFAULT_MSG STELLA_INCLUDE_DIRS STELLA_LIBRARIES)

if(STELLA_FOUND)
  mark_as_advanced(STELLA_INCLUDE_DIRS STELLA_LIBRARIES)
  message(STATUS "STELLA found at ${STELLA_ROOT}")
else()
  # If the package was required we abort the process
  if(${STELLA_FIND_REQUIRED})
    message(FATAL_ERROR "Could NOT find STELLA. (Try setting STELLA_ROOT in the env)")
  endif(${STELLA_FIND_REQUIRED})
endif()

