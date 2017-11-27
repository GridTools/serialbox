# Try to find NetCDF C ibraries
#
# Usage of this module:
#
#  find_package(NetCDF)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#  NETCDF_ROOT          Base directory where all NetCDF components are found
#
# The following are set after configuration is done: 
#  NETCDF_FOUND           NetCDF C was found
#  NETCDF_INCLUDES        Include directories of NetCDF C libraries
#  NETCDF_LIBRARIES       Libraries of NetCDF C

if(NETCDF_INCLUDES AND NETCDF_LIBRARIES)
  set(NETCDF_FIND_QUIETLY TRUE)
endif(NETCDF_INCLUDES AND NETCDF_LIBRARIES)

set(NETCDF_ROOT_ENV $ENV{NETCDF_ROOT})
if(NETCDF_ROOT_ENV)
  set(NETCDF_ROOT ${NETCDF_ROOT_ENV} CACHE PATH "NetCDF install path.")
endif()

if(NOT(DEFINED NETCDF_ROOT))
  find_path(NETCDF_ROOT NAMES include/netcdf.h)
else()
  get_filename_component(_NETCDF_ROOT_ABSOLUTE ${NETCDF_ROOT} ABSOLUTE)
  set(NETCDF_ROOT ${_NETCDF_ROOT_ABSOLUTE} CACHE PATH "NetCDF install path.")
endif()

find_path(NETCDF_INCLUDES netcdf.h HINTS ${NETCDF_ROOT}/include)
find_library(NETCDF_LIBRARIES NAMES netcdf HINTS ${NETCDF_ROOT}/lib)

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (NetCDF DEFAULT_MSG NETCDF_LIBRARIES NETCDF_INCLUDES)

if(NetCDF_FOUND)
  mark_as_advanced(NETCDF_LIBRARIES NETCDF_INCLUDES)
else()
  # If the package was required we abort the process
  if(${NetCDF_FIND_REQUIRED})
    message(FATAL_ERROR "Could NOT find NetCDF. (Try setting NETCDF_ROOT in the env)")
  endif(${NetCDF_FIND_REQUIRED})
endif()

