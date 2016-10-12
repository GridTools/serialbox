##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##
#
# Try to find Serialbox headers and libraries.
#
# Usage of this module as follows:
#
#   find_package(Serialbox)
#
# Variables used by this module, they can change the default behaviour and need to be set before 
# calling find_package:
#
#   SERIALBOX_ROOT      Set this variable to the root installation of Serialbox if the module has 
#                       problems finding the proper installation path.
#
# Variables defined by this module:
#
#   SERIALBOX_FOUND           System has Serialbox libraries and headers
#   SERIALBOX_LIBRARIES       The Serialbox library
#   SERIALBOX_INCLUDE_DIRS    The location of Serialbox headers
#   SERIALBOX_VERSION         Version of Serialbox
#
##===------------------------------------------------------------------------------------------===##

include(FindPackageHandleStandardArgs)

set(SERIALBOX_ROOT_ENV $ENV{SERIALBOX_ROOT})
if(SERIALBOX_ROOT_ENV)
  set(SERIALBOX_ROOT ${SERIALBOX_ROOT_ENV} CACHE "Serialbox install path.")
endif()

if(NOT(DEFINED SERIALBOX_ROOT))
  find_path(SERIALBOX_ROOT NAMES include/serialbox/Core/Config.h)
else()
  get_filename_component(_SERIALBOX_ROOT_ABSOLUTE ${SERIALBOX_ROOT} ABSOLUTE)
  set(SERIALBOX_ROOT ${_SERIALBOX_ROOT_ABSOLUTE} CACHE PATH "Serialbox install path.")
endif()

# Check for specific version
if(NOT Serialbox_FIND_VERSION)
  if(NOT Serialbox_FIND_VERSION_MAJOR)
    set(Serialbox_FIND_VERSION_MAJOR 2)
  endif(NOT Serialbox_FIND_VERSION_MAJOR)
  if(NOT Serialbox_FIND_VERSION_MINOR)
    set(Serialbox_FIND_VERSION_MINOR 0)
  endif(NOT Serialbox_FIND_VERSION_MINOR)
  if(NOT Serialbox_FIND_VERSION_PATCH)
    set(Serialbox_FIND_VERSION_PATCH 0)
  endif(NOT Serialbox_FIND_VERSION_PATCH)
  set(Serialbox_FIND_VERSION 
  "${Serialbox_FIND_VERSION_MAJOR}.${Serialbox_FIND_VERSION_MINOR}.${Serialbox_FIND_VERSION_PATCH}")
endif(NOT Serialbox_FIND_VERSION)

macro(_serialbox_read_config)
  file(READ ${SERIALBOX_ROOT}/include/serialbox/Core/Config.h _CONFIG_FILE)
  
  # Read version
  string(REGEX MATCH "define[ \t]+SERIALBOX_VERSION_MAJOR[ \t]+([0-9]+)" _MAJOR "${_CONFIG_FILE}")
  set(SERIALBOX_MAJOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+SERIALBOX_VERSION_MINOR[ \t]+([0-9]+)" _MINOR "${_CONFIG_FILE}")
  set(SERIALBOX_MINOR_VERSION "${CMAKE_MATCH_1}")
  string(REGEX MATCH "define[ \t]+SERIALBOX_VERSION_PATCH[ \t]+([0-9]+)" _PATCH "${_CONFIG_FILE}")
  set(SERIALBOX_PATCH_VERSION "${CMAKE_MATCH_1}")

  set(SERIALBOX_VERSION 
      "${SERIALBOX_MAJOR_VERSION}.${SERIALBOX_MINOR_VERSION}.${SERIALBOX_PATCH_VERSION}")
      
  if(${SERIALBOX_VERSION} VERSION_LESS ${Serialbox_FIND_VERSION})
    set(SERIALBOX_VERSION_OK FALSE)
  else()
    set(SERIALBOX_VERSION_OK TRUE)
  endif()
      
  if(NOT SERIALBOX_VERSION_OK)
    message(STATUS "Serialbox version ${SERIALBOX_VERSION} found in ${SERIALBOX_ROOT}, "
                   "but at least version ${Serialbox_FIND_VERSION} is required")
  endif(NOT SERIALBOX_VERSION_OK)
  
endmacro(_serialbox_read_config)

# Read config file
_serialbox_read_config()

# We need thread support
find_package(Threads REQUIRED)

# Set include directory
find_path(SERIALBOX_INCLUDE_DIRS NAMES serialbox/Core/Config.h HINTS ${SERIALBOX_ROOT}/include)

# Set libraries
find_library(_SERIALBOX_CORE NAMES SerialboxCore HINTS ${SERIALBOX_ROOT}/lib)
mark_as_advanced(_SERIALBOX_CORE)

set(SERIALBOX_LIBRARIES ${_SERIALBOX_CORE} ${CMAKE_THREAD_LIBS_INIT})

find_package_handle_standard_args(SERIALBOX 
                                  SERIALBOX_LIBRARIES SERIALBOX_INCLUDE_DIRS SERIALBOX_VERSION_OK)

if(SERIALBOX_FOUND)
  message(STATUS "Serialbox version: ${SERIALBOX_VERSION}") 
  mark_as_advanced(SERIALBOX_LIBRARIES SERIALBOX_INCLUDE_DIRS SERIALBOX_VERSION_OK)
else()
  # If the package was required we abort the process
  if(${Serialbox_FIND_REQUIRED})
    message(FATAL_ERROR "Could NOT find Serialbox. (Try setting SERIALBOX_ROOT in the env)")
  endif(${Serialbox_FIND_REQUIRED})
endif()

