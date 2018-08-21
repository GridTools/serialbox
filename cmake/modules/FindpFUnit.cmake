# Try to find pFUnit framwork.
#
# Usage of this module as follows:
#
#   find_package(pFUnit)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#
#   PFUNIT_ROOT             Set this variable to the root installation of
#                           pFUnit if the module has problems finding the
#                           proper installation path.
#
# Variables defined by this module:
#
#   PFUNIT_FOUND            System has pFUnit libraries and headers
#   PFUNIT_INCLUDE_DIRS     The location of pFUnit headers
#   PFUNIT_DRIVER           The location of driver.F90
#   PFUNIT_LIBRARIES        The location of pFUnit libraries
#   PFUNIT_BIN_DIR          The location of pFUnit binaries

if(NOT(DEFINED PFUNIT_ROOT))
    find_path(PFUNIT_ROOT NAMES lib/libpfunit.a)
endif()


# Look for pFUnit headers
find_path(PFUNIT_INCLUDE_DIRS_INC
    NAMES TestUtil.F90
    HINTS ${PFUNIT_ROOT}/include
          $ENV{PFUNIT_ROOT}/include
)

set(PFUNIT_DRIVER "${PFUNIT_INCLUDE_DIRS_INC}/driver.F90")
find_path(PFUNIT_INCLUDE_DIRS_MOD
    NAMES test_mod.mod
    HINTS
            ${PFUNIT_ROOT}/mod
            ${PFUNIT_ROOT}/include
            $ENV{PFUNIT_ROOT}/mod
            $ENV{PFUNIT_ROOT}/include
)
set( PFUNIT_INCLUDE_DIRS "${PFUNIT_INCLUDE_DIRS_INC}" "${PFUNIT_INCLUDE_DIRS_MOD}" )

# Look for lib
find_library(PFUNIT_LIBRARIES NAMES pfunit HINTS ${PFUNIT_ROOT}/lib)

# Look for bin dir
find_path(PFUNIT_BIN_DIR
    NAMES pFUnitParser.py
    HINTS ${PFUNIT_ROOT}/bin
          $ENV{PFUNIT_ROOT}/bin
)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(PFUNIT DEFAULT_MSG
    PFUNIT_INCLUDE_DIRS_INC
    PFUNIT_INCLUDE_DIRS_MOD
    PFUNIT_LIBRARIES
    PFUNIT_BIN_DIR)

if(PFUNIT_FOUND)
    message(STATUS "pFUnit found at ${PFUNIT_ROOT}")
else()
    # If the package was required we abort the process
    if(${pFUnit_FIND_REQUIRED})
        message(FATAL_ERROR "Could NOT find pFUnit. (Try setting PFUNIT_ROOT in the env)")
    endif(${pFUnit_FIND_REQUIRED})
endif()

