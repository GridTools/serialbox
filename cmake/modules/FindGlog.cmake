# Try to find Glog
#
# Usage of this module:
#
#  find_package(Glog)
#
# Variables used by this module, they can change the default behaviour and need
# to be set before calling find_package:
#  GLOG_ROOT            Base directory where all GLOG components are found
#
# The following are set after configuration is done: 
#  GLOG_FOUND           Glog was found
#  GLOG_INCLUDE_DIRS    Include directories of glog
#  GLOG_LIBRARIES       Libraries of glog

include(FindPackageHandleStandardArgs)

set(GLOG_ROOT "" CACHE PATH "Folder containing Google's glog")

find_path(GLOG_INCLUDE_DIR glog/logging.h PATHS ${GLOG_ROOT} ${GLOG_ROOT}/include)
find_library(GLOG_LIBRARY glog PATHS ${GLOG_ROOT} PATH_SUFFIXES lib lib64)

find_package_handle_standard_args(GLOG DEFAULT_MSG GLOG_INCLUDE_DIR GLOG_LIBRARY)

if(GLOG_FOUND)
  set(GLOG_INCLUDE_DIRS ${GLOG_INCLUDE_DIR})
  set(GLOG_LIBRARIES ${GLOG_LIBRARY})
else()
  if(${GLOG_FIND_REQUIRED})
    message(FATAL_ERROR "Could NOT find Glog. (Try setting GLOG_ROOT in the env)")
  endif()
endif()