##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license.
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##
##
## This is the master CMake file of the serialbox project.
##
##===------------------------------------------------------------------------------------------===##

set(CMAKE_INSTALL_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}/install" CACHE PATH "CMake install prefix")

cmake_policy(SET CMP0048 NEW)
cmake_minimum_required(VERSION 3.12.0)
# note: version should not be changed manually, use bump-my-version instead:
# install: pip install bump-my-version
# example: bump-my-version bump patch
project(Serialbox LANGUAGES C CXX VERSION 2.6.2)

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_C_EXTENSIONS OFF)

list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake/modules")
include(ExternalProject)
include(CMakeParseArguments)
include(CheckCXXCompilerFlag)
include(SerialboxInstallTargets)

#---------------------------------------- Configure ------------------------------------------------

if(UNIX)
  set(SERIALBOX_ON_UNIX 1)
endif(UNIX)

if(WIN32)
  set(SERIALBOX_ON_WIN32 1)
endif(WIN32)

# Serialbox version
set(Serialbox_VERSION_STRING
    "${Serialbox_VERSION_MAJOR}.${Serialbox_VERSION_MINOR}.${Serialbox_VERSION_PATCH}")

message(STATUS "Serialbox version: ${Serialbox_VERSION_STRING}")

#---------------------------------------- User options ---------------------------------------------

option(SERIALBOX_BUILD_SHARED "Build shared libraries" ON)
option(SERIALBOX_ENABLE_C "Build C Interface" ON)
option(SERIALBOX_ENABLE_PYTHON "Build Python Interface" ${SERIALBOX_ENABLE_C})
option(SERIALBOX_ENABLE_FORTRAN "Build Fortran Interface" OFF)
option(SERIALBOX_ENABLE_FTG "Build FortranTestGenerator frontend" OFF)
option(SERIALBOX_ENABLE_SDB "Install the stencil-debugger (sdb)" ON)

option(SERIALBOX_LOGGING "Enable logging" ON)
option(SERIALBOX_ASYNC_API "Enable the asynchronous API" ON)
option(SERIALBOX_EXAMPLES "Build example exectuables" ON)

option(SERIALBOX_USE_OPENSSL "Use OpenSSL library" OFF)
option(SERIALBOX_USE_NETCDF "Use NetCDF library" OFF)

option(SERIALBOX_TESTING "Build unittest executables" OFF)
option(SERIALBOX_TESTING_GRIDTOOLS "Build gridtools unitests and examples" OFF)
option(SERIALBOX_TESTING_DEATH_TESTS "Run death-tests" OFF)
option(SERIALBOX_TESTING_LARGE_FILE_TESTS "Run large file (>4GB) tests" OFF)
option(SERIALBOX_TESTING_FORTRAN "Build tests for the Fortran interface")

option(SERIALBOX_BENCHMARKING "Build benchmark exectuables" OFF)
option(SERIALBOX_DOCUMENTATION "Build and install the documentation" OFF)

option(SERIALBOX_CODE_COVERAGE "Generate code coverage" OFF)
option(SERIALBOX_VERBOSE_WARNINGS "Enable verbose warnings (-Wall)" OFF)

#---------------------------------------- CMake options --------------------------------------------

## Perform checks to make sure we support the current compiler
include(SerialboxCheckCxxCompilerSupport)

## Set C++ standard to C++17
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Compiler specific
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-sign-compare")
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Intel")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -wd488")
endif()

# Set build type to Release if nothing was specified (instead of Debug)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING
      "Choose the type of build, options are: Debug Release RelWithDebInfo" FORCE)
  message(STATUS "Setting build type to 'Release' as none was specified")
endif(NOT CMAKE_BUILD_TYPE)

# Clear all cmake generated files
if( NOT TARGET clean-all )
  add_custom_target(clean-all
                    COMMAND ${CMAKE_MAKE_PROGRAM} clean
                    COMMAND ${CMAKE_COMMAND} -P
                    "${PROJECT_SOURCE_DIR}/cmake/modules/SerialboxCleanAll.cmake")
endif()

# We need thread support
find_package(Threads REQUIRED)

# Set script directory
set(SCRIPT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/tools)

# If we build the Fortran Interface, we need a Fortran compiler
if(SERIALBOX_ENABLE_FORTRAN)
  enable_language(Fortran)
  include(SerialboxFortranCompilerFlags)
endif()

if(SERIALBOX_BUILD_SHARED AND CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
  message(WARNING "Cray Fortran compiler detected, disabling shared libraries")
  set(SERIALBOX_BUILD_SHARED OFF)
  set(SERIALBOX_ENABLE_SDB OFF)
  set(SERIALBOX_ENABLE_PYTHON OFF)
endif()

# Set shared library flags
if(SERIALBOX_BUILD_SHARED)
  set(BUILD_SHARED_LIBS ON)

  # Use, i.e. don't skip the full RPATH for the build tree
  set(CMAKE_SKIP_BUILD_RPATH FALSE)

  # When building, don't use the install RPATH already (but later on when installing)
  set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)

  set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
  set(CMAKE_MACOSX_RPATH ON)

  # Add the automatically determined parts of the RPATH which point to directories outside the
  # build tree to the install RPATH
  set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

  # The RPATH to be used when installing, but only if it's not a system directory
  list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
  if("${isSystemDir}" STREQUAL "-1")
    SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
  endif("${isSystemDir}" STREQUAL "-1")
  if(SKBUILD_PROJECT_NAME)
    if(APPLE)
      set(CMAKE_INSTALL_RPATH "@loader_path")
    else()
      set(CMAKE_INSTALL_RPATH "$ORIGIN")
    endif()
  endif()
endif()

if(NOT(SERIALBOX_LOGGING))
  add_definitions(-DSERIALBOX_DISABLE_LOGGING)
else()
  set(SERIALBOX_HAS_LOGGING 1)
endif()

if(SERIALBOX_ASYNC_API)
  add_definitions(-DSERIALBOX_ASYNC_API)
endif(SERIALBOX_ASYNC_API)

## Third party libraries
set(SERIALBOX_EXTERNAL_LIBRARIES)

## Enable testing if one ore more of the tests is requested
if(SERIALBOX_TESTING_GRIDTOOLS OR
   SERIALBOX_TESTING_DEATH_TESTS OR
   SERIALBOX_TESTING_FORTRAN)
 set(SERIALBOX_TESTING ON)
endif()

## IDE Support
include(IDESupport)

#---------------------------------------- GTest ----------------------------------------------------
if(SERIALBOX_TESTING)
  set(tmp_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS})
  set(BUILD_SHARED_LIBS OFF)
  include(FetchGoogletest)
  fetch_googletest()
  set(BUILD_SHARED_LIBS ${tmp_BUILD_SHARED_LIBS})

  if(SERIALBOX_ENABLE_C)
    set(SERIALBOX_HAS_C 1)
  endif(SERIALBOX_ENABLE_C)
endif(SERIALBOX_TESTING)

#---------------------------------------- pFUnit ---------------------------------------------------
if(SERIALBOX_TESTING_FORTRAN)
  if(SERIALBOX_ENABLE_FORTRAN)
    set(PFUNIT_ROOT_ENV "$ENV{PFUNIT_ROOT}")
    find_package(PFUNIT 4.4.1 REQUIRED)
  else()
    message(WARNING "You need to enable Fortran (-DSERIALBOX_ENABLE_FORTRAN=ON) to build the Fortran tests.")
  endif()
endif()


#---------------------------------------- OpenSSL --------------------------------------------------
if(${SERIALBOX_USE_OPENSSL})
  find_package(OpenSSL REQUIRED)
  if(NOT(${OpenSSL_FOUND}))
    message(FATAL_ERROR "OpenSSL library not found!")
  endif()
  include_directories(SYSTEM ${OPENSSL_INCLUDE_DIR})
  set(SERIALBOX_EXTERNAL_LIBRARIES ${SERIALBOX_EXTERNAL_LIBRARIES} ${OPENSSL_LIBRARIES})
  set(SERIALBOX_HAS_OPENSSL 1)
endif()

#---------------------------------------- NetCDF ---------------------------------------------------
if(${SERIALBOX_USE_NETCDF})
  find_package(NetCDF REQUIRED)
  if(NOT(${NetCDF_FOUND}))
    message(FATAL_ERROR "NetCDF library not found!")
  endif()
  set(SERIALBOX_HAS_NETCDF 1)
  serialbox_install_targets( TARGETS NETCDF_TARGET )
endif()

#---------------------------------------- Python ---------------------------------------------------
if(SERIALBOX_ENABLE_PYTHON)
  find_package(Python3 COMPONENTS Interpreter)

  # Python tests are enabled by default if we can find "nose" and "numpy"
  set(ENABLE_PYTHON_TESTS OFF)
  if(SERIALBOX_TESTING AND PYTHONINTERP_FOUND)
    include(FindPythonModule)
    find_python_module(nose)
    find_python_module(numpy)
    if(PY_NOSE_FOUND AND PY_NUMPY_FOUND)
      set(ENABLE_PYTHON_TESTS ON)
    endif()
  endif()

  option(SERIALBOX_TESTING_PYTHON "Run Python tests" ${ENABLE_PYTHON_TESTS})
  mark_as_advanced(ENABLE_PYTHON_TESTS)

endif(SERIALBOX_ENABLE_PYTHON)

#---------------------------------------- Filesystem -----------------------------------------------
# link with stdc++fs for gcc < 9.0 (in gcc 9 it's part of the runtime)
link_libraries( "$<$<AND:$<CXX_COMPILER_ID:GNU>,$<VERSION_LESS:$<CXX_COMPILER_VERSION>,9.0>>:-lstdc++fs>" )

#---------------------------------------- GridTools ------------------------------------------------
if(SERIALBOX_TESTING_GRIDTOOLS)
  find_package(GridTools QUIET REQUIRED)
  set(SERIALBOX_HAS_GRIDTOOLS 1)
endif(SERIALBOX_TESTING_GRIDTOOLS)

find_package(Boost 1.54 REQUIRED) # only Boost headers
set(SERIALBOX_BOOST_VERSION ${Boost_VERSION})

#---------------------------------------- ClangTools -----------------------------------------------

find_package(ClangTools)
if("$ENV{CMAKE_EXPORT_COMPILE_COMMANDS}" STREQUAL "1" OR CLANG_TIDY_FOUND)
  set(CMAKE_EXPORT_COMPILE_COMMANDS 1)
endif()

if(CLANG_FORMAT_BIN)
  # Runs clang format and updates files in place.
  add_custom_target(format
                    COMMAND ${SCRIPT_DIR}/run-clang-format.sh
                    ${CMAKE_CURRENT_SOURCE_DIR}
                    ${CLANG_FORMAT_BIN}
                    1
                    `find ${CMAKE_CURRENT_SOURCE_DIR}/src
                          -name \*.h -print -o -name \*.cpp -print`)

  # Runs clang format and exits with a non-zero exit code if any files need to be reformatted
  add_custom_target(check-format
                    COMMAND ${SCRIPT_DIR}/run-clang-format.sh
                    ${CMAKE_CURRENT_SOURCE_DIR}
                    ${CLANG_FORMAT_BIN}
                    0
                    `find ${CMAKE_CURRENT_SOURCE_DIR}/src
                          -name \*.h -print -o -name \*.cpp -print`)
endif()

if(CLANG_TIDY_BIN)
  # Runs clang-tidy and attempts to fix any warning automatically
  add_custom_target(clang-tidy
                    COMMAND ${SCRIPT_DIR}/run-clang-tidy.sh
                    ${CLANG_TIDY_BIN}
                    ${CMAKE_BINARY_DIR}/compile_commands.json
                    1
                    `find ${CMAKE_CURRENT_SOURCE_DIR}/src
                          -name \*.h -print -o -name \*.cpp -print`)

  # Runs clang-tidy and exits with a non-zero exit code if any errors are found.
  add_custom_target(check-clang-tidy
                    COMMAND ${SCRIPT_DIR}/run-clang-tidy.sh
                    ${CLANG_TIDY_BIN}
                    ${CMAKE_BINARY_DIR}/compile_commands.json
                    0
                    `find ${CMAKE_CURRENT_SOURCE_DIR}/src
                          -name \*.h -print -o -name \*.cpp -print`)
endif()

# --------------------------------------- Code Coverage --------------------------------------------
if(SERIALBOX_CODE_COVERAGE)
  include(SerialboxCoverage)
  serialbox_enable_coverage(${CMAKE_BINARY_DIR})

  set(SERIALBOX_EXTERNAL_LIBRARIES ${SERIALBOX_EXTERNAL_LIBRARIES}
                                   ${CMAKE_SHARED_LINKER_FLAGS_COVERAGE})
endif(SERIALBOX_CODE_COVERAGE)

#---------------------------------------- Compilation ----------------------------------------------

# Generate serialbox/core/Config.h
set(SERIALBOX_CONFIG_FILE_DISCLAIMER "WARNING! All changes made in this file will be lost!")
set(SERIALBOX_CXX_CONFIG_FILE_IN ${PROJECT_SOURCE_DIR}/src/serialbox/core/Config.h.cmake)
set(SERIALBOX_CXX_CONFIG_FILE ${PROJECT_SOURCE_DIR}/src/serialbox/core/Config.h)
configure_file(${SERIALBOX_CXX_CONFIG_FILE_IN} ${SERIALBOX_CXX_CONFIG_FILE})
install(FILES ${SERIALBOX_CXX_CONFIG_FILE}
        DESTINATION ${CMAKE_INSTALL_PREFIX}/include/serialbox/core)

# Install serialbox headers
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src/serialbox DESTINATION include
    FILES_MATCHING PATTERN "*.h" PATTERN "*.hpp")

if(SERIALBOX_ENABLE_C)
    install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src/serialbox-c DESTINATION include
        FILES_MATCHING PATTERN "*.h")
endif()

## Build Serialbox
add_subdirectory(src)

## Build unittests
if(SERIALBOX_TESTING)
  enable_testing()
  include(SerialboxTestScript)
  serialbox_test_init()

  if(SERIALBOX_TESTING_DEATH_TESTS)
    add_definitions(-DSERIALBOX_RUN_DEATH_TESTS)
  endif()

  if(SERIALBOX_TESTING_LARGE_FILE_TESTS)
    add_definitions(-DSERIALBOX_RUN_LARGE_FILE_TESTS)
  endif()

  # Generate utility/Config.h
  set(SERIALBOX_CPP_CONFIG_FILE_IN ${PROJECT_SOURCE_DIR}/test/utility/Config.h.cmake)
  set(SERIALBOX_CPP_CONFIG_FILE ${PROJECT_BINARY_DIR}/test/utility/Config.h)
  configure_file(${SERIALBOX_CPP_CONFIG_FILE_IN} ${SERIALBOX_CPP_CONFIG_FILE})
  include_directories(${PROJECT_BINARY_DIR}/test)

  add_subdirectory(test)
endif(SERIALBOX_TESTING)

## Build examples
if(SERIALBOX_EXAMPLES)
  add_subdirectory(examples)
endif(SERIALBOX_EXAMPLES)

## Build documentation
if(SERIALBOX_DOCUMENTATION)
  add_subdirectory(docs)
endif()

if(SERIALBOX_TESTING)
  serialbox_test_end()
endif(SERIALBOX_TESTING)

include(cmake/Packaging.cmake)
