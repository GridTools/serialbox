if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING 
      "Choose the type of build, options are: Debug Release RelWithDebInfo." FORCE)
endif()

if(NOT BUILD_SHARED_LIBS)
  set(BUILD_SHARED_LIBS ON CACHE BOOL "Build shared libraries." FORCE)
endif()

if(NOT CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "${CMAKE_SOURCE_DIR}/install" CACHE STRING
      "Install path prefix, prepended onto install directories." FORCE)
endif()

project(serialbox2-all C CXX Fortran)
enable_testing()
cmake_minimum_required(VERSION 3.8.1)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake")

include(serialbox2CloneRepository)

set(YODA_GIT_URL "https:///github.com/cosunae/yoda.git" 
    CACHE PATH "URL of the yoda git repository to clone")
set(YODA_GIT_BRANCH "create_library" CACHE STRING "Branch of the yoda git repository to clone")

serialbox2_clone_repository(NAME yoda URL ${YODA_GIT_URL} BRANCH ${YODA_GIT_BRANCH} SOURCE_DIR YODA_SOURCE_DIR )

list(APPEND CMAKE_MODULE_PATH "${YODA_SOURCE_DIR}/cmake")
include(yodaInit)

yoda_init()

include(yodaAddTargetCleanAll)

# Add custom targets
yoda_add_target_clean_all(
  "${CMAKE_BINARY_DIR}/prefix"
  "${CMAKE_BINARY_DIR}/thirdparty"  
  "${CMAKE_BINARY_DIR}/yoda-cmake"
  "${CMAKE_BINARY_DIR}/boost"
  "${CMAKE_BINARY_DIR}/boost-prefix"
)

include(yodaFindPackage)
include(yodaSetDownloadDir)

# Set the default download directory (define YODA_DOWNLOAD_DIR)
yoda_set_download_dir()
include("thirdparty/Serialbox2AddBoost")

set(serialbox2_cmake_args -DBOOST_ROOT=${BOOST_ROOT} -DSERIALBOX_ENABLE_FORTRAN=ON -DSERIALBOX_BUILD_SHARED=ON)
set(serialbox2_dependencies "boost")

yoda_find_package(
  PACKAGE serialbox2
  NO_DEFAULT_PATH
  FORWARD_VARS 
    BINARY_DIR serialbox2_binary_dir
  DEPENDS ${serialbox2_dependencies}
  ADDITIONAL
    SOURCE_DIR "${CMAKE_SOURCE_DIR}/../"
    YODA_ROOT "${YODA_SOURCE_DIR}"
    CMAKE_ARGS 
        ${serialbox2_cmake_args}
)

ExternalProject_Add(serialbox-test
   PREFIX serialbox-test
   SOURCE_DIR ${CMAKE_SOURCE_DIR}
   SOURCE_SUBDIR "cmake-test"
   INSTALL_DIR "${CMAKE_INSTALL_PREVIX}"
   BUILD_ALWAYS 1
   CMAKE_ARGS -DSerialbox_DIR=${CMAKE_INSTALL_PREFIX}/cmake -DYODA_ROOT=${YODA_SOURCE_DIR} -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
#   CMAKE_ARGS ${} 
)


if(NOT(serialbox2_binary_dir))
	message(FATAL_ERROR "serialbox2 binary dir was not defined by External_Serialbox2")
endif()

add_test(NAME gtclang-tests 
  COMMAND  ${CMAKE_COMMAND} --build ${gtclang_binary_dir} --target test
)

#We need to install something
set(conf_bundle_filename ${CMAKE_BINARY_DIR}/gtclang-conf.bundle)

file(WRITE ${conf_bundle_filename} "conf")

install(
  FILES ${conf_bundle_filename}
  DESTINATION bundle_conf
)

