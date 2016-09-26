# Try to find old Serialbox headers and libraries.
#
# Usage of this module as follows:
#
#   find_package(SerialboxOld)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
#   SERIALBOX_OLD_ROOT      Set this variable to the root installation of
#                           Serialbox if the module has problems finding the
#                           proper installation path.
#
# Variables defined by this module:
#
#   SERIALBOX_OLD_FOUND         System has Serialbox libraries and headers
#   SERIALBOX_OLD_LIBRARIES     The Serialbox library
#   SERIALBOX_OLD_INCLUDE_DIRS  The location of Serialbox headers

find_path(SERIALBOX_OLD_ROOT NAMES include/Serialbox/Savepoint.h)

# Look for Serialbox libraries
function(_serialbox_old_find_library _name)
    find_library(${_name}
        NAMES ${ARGN}
        HINTS $ENV{SERIALBOX_OLD_ROOT}/lib
              ${SERIALBOX_OLD_ROOT}/lib
    )
    mark_as_advanced(${_name})
endfunction()

_serialbox_old_find_library(SERIALBOX_LIBRARY_CORE    Serialbox)
_serialbox_old_find_library(SERIALBOX_LIBRARY_UTILS   Utils)
_serialbox_old_find_library(SERIALBOX_LIBRARY_JSON    json)
_serialbox_old_find_library(SERIALBOX_LIBRARY_SHA256  sha256)

set(SERIALBOX_OLD_LIBRARIES 
    ${SERIALBOX_OLD_LIBRARY_CORE}
    ${SERIALBOX_OLD_LIBRARY_UTILS}
    ${SERIALBOX_OLD_LIBRARY_JSON}
    ${SERIALBOX_OLD_LIBRARY_SHA256}
)

# Look for Serialbox headers
find_path(SERIALBOX_OLD_INCLUDE_DIRS
    NAMES Serialbox/Savepoint.h
    HINTS ${SERIALBOX_OLD_ROOT}/include
          $ENV{SERIALBOX_OLD_ROOT}/include
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(SERIALBOX_OLD
    DEFAULT_MSG
    SERIALBOX_OLD_LIBRARIES SERIALBOX_OLD_INCLUDE_DIRS
)

if(SERIALBOX_OLD_FOUND)
    mark_as_advanced(SERIALBOX_OLD_LIBRARIES SERIALBOX_OLD_INCLUDE_DIRS)
else()
    # If the package was required we abort the process
    if(${SerialboxOld_FIND_REQUIRED})
        message(FATAL_ERROR 
                "Could NOT find old Serialbox. (Try setting SERIALBOX_OLD_ROOT in the env)")
    endif(${SerialboxOld_FIND_REQUIRED})
endif()

