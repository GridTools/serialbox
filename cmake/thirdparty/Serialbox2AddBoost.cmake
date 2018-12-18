include(yodaSetDownloadDir)
include(yodaFindPackage)

# Set the default download directory (define SERIALBOX2_ALL_DOWNLOAD_DIR)
yoda_set_download_dir()

#
# Boost
#
set(boost_min_version 1.58.0)
set(_v 63)
set(boost_download_version 1.${_v}.0)

yoda_find_package(
  PACKAGE Boost
  PACKAGE_ARGS ${boost_min_version} 
  COMPONENTS ${boost_components}
  REQUIRED_VARS BOOST_ROOT
  ADDITIONAL
  DOWNLOAD_DIR ${YODA_DOWNLOAD_DIR}
    URL "http://sourceforge.net/projects/boost/files/boost/1.${_v}.0/boost_1_${_v}_0.tar.gz/download"
    URL_MD5 "7b493c08bc9557bbde7e29091f28b605" 
    BUILD_VERSION ${boost_download_version}
)


