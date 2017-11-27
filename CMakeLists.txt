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

project(serialbox2 C CXX)
cmake_minimum_required(VERSION 3.1)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/modules")

include(ExternalProject)
include(CMakeParseArguments)
include(CheckCXXCompilerFlag)

#---------------------------------------- Configure ------------------------------------------------

if(UNIX)
  set(SERIALBOX_ON_UNIX 1)
endif(UNIX)

if(WIN32)
  set(SERIALBOX_ON_WIN32 1)
endif(WIN32)

if("${CMAKE_CXX_COMPILER_ID}" MATCHES "MSVC")
  set(SERIALBOX_COMPILER_MSVC 1)
endif()

# Serialbox version
set(SERIALBOX_VERSION_MAJOR 2)
set(SERIALBOX_VERSION_MINOR 1)
set(SERIALBOX_VERSION_PATCH 1)
set(SERIALBOX_VERSION_STRING 
    "${SERIALBOX_VERSION_MAJOR}.${SERIALBOX_VERSION_MINOR}.${SERIALBOX_VERSION_PATCH}")
    
message(STATUS "Serialbox version: ${SERIALBOX_VERSION_STRING}")

#---------------------------------------- User options ---------------------------------------------

option(SERIALBOX_BUILD_SHARED "Build shared libraries" ON)
option(SERIALBOX_ENABLE_C "Build C Interface" ON)
option(SERIALBOX_ENABLE_PYTHON "Build Python Interface" ${SERIALBOX_ENABLE_C})
option(SERIALBOX_ENABLE_FORTRAN "Build Fortran Interface" OFF)
option(SERIALBOX_ENABLE_SDB "Install the stencil-debugger (sdb)" ON)

option(SERIALBOX_LOGGING "Enable logging" ON)
option(SERIALBOX_ASYNC_API "Enable the asynchronous API" ON)
option(SERIALBOX_EXAMPLES "Build example exectuables" ON)

option(SERIALBOX_USE_OPENSSL "Use OpenSSL library" OFF)
option(SERIALBOX_USE_NETCDF "Use NetCDF library" OFF)

option(SERIALBOX_TESTING "Build unittest executables" OFF)
option(SERIALBOX_TESTING_GRIDTOOLS "Build gridtools unitests and examples" OFF)
option(SERIALBOX_TESTING_STELLA "Build STELLA unitests" OFF)
option(SERIALBOX_TESTING_OLD_SERIALBOX "Build compatiblity unitests against old serialbox" OFF)
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
 
## Set C++ standard to C++11
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

if(NOT(SERIALBOX_COMPILER_MSVC))
  
  # Shared architecture flags
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -march=native")

  # Compiler specific
  if(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-sign-compare")            
  elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  elseif(CMAKE_CXX_COMPILER_ID MATCHES "Intel")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -wd488")
  endif()

else()
  
endif()
  
# Set build type to Release if nothing was specified (instead of Debug)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING 
      "Choose the type of build, options are: Debug Release RelWithDebInfo" FORCE)
  message(STATUS "Setting build type to 'Release' as none was specified")
endif(NOT CMAKE_BUILD_TYPE)

# Clear all cmake generated files
add_custom_target(clean-all
                  COMMAND ${CMAKE_MAKE_PROGRAM} clean
                  COMMAND ${CMAKE_COMMAND} -P  
                  "${CMAKE_SOURCE_DIR}/cmake/modules/SerialboxCleanAll.cmake")
                  
# We need thread support
find_package(Threads REQUIRED)

# Set script directory
set(SCRIPT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/tools)

# If we build the C Interface we require shared libraries
if(SERIALBOX_ENABLE_C AND NOT(SERIALBOX_BUILD_SHARED))
  message(STATUS "SERIALBOX_ENABLE_C requires shared libraries, setting SERIALBOX_BUILD_SHARED=ON")
  set(SERIALBOX_BUILD_SHARED ON)
endif()

# Set shared library flags
if(SERIALBOX_BUILD_SHARED)
  set(BUILD_SHARED_LIBS ON)
  
  if(NOT(SERIALBOX_COMPILER_MSVC))
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fPIC")
  endif()
  
  # Use, i.e. don't skip the full RPATH for the build tree
  set(CMAKE_SKIP_BUILD_RPATH FALSE)

  # When building, don't use the install RPATH already (but later on when installing)
  set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE) 

  set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
  set(CMAKE_MACOSX_RPATH "${CMAKE_INSTALL_RPATH}")

  # Add the automatically determined parts of the RPATH which point to directories outside the 
  # build tree to the install RPATH
  set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

  # The RPATH to be used when installing, but only if it's not a system directory
  list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
  if("${isSystemDir}" STREQUAL "-1")
     SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
  endif("${isSystemDir}" STREQUAL "-1")
endif()

# If we build the Fortran Interface, we need a Fortran compiler
if(SERIALBOX_ENABLE_FORTRAN)
  enable_language(Fortran)
  include(SerialboxFortranCompilerFlags)
  
  if(SERIALBOX_BUILD_SHARED)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
  endif(SERIALBOX_BUILD_SHARED)
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
   SERIALBOX_TESTING_STELLA OR 
   SERIALBOX_TESTING_OLD_SERIALBOX OR 
   SERIALBOX_TESTING_DEATH_TESTS OR
   SERIALBOX_TESTING_FORTRAN)
 set(SERIALBOX_TESTING ON)
endif()

## Sanity check that we are not building in the source tree
if( CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR AND NOT MSVC_IDE )
  message(FATAL_ERROR "In-source builds are not allowed.
Please create a build directory and run cmake from there, passing the path
to this source directory as the last argument.
This process created the file `CMakeCache.txt' and the directory `CMakeFiles'.
Please delete them.")
endif()

## IDE Support
include(IDESupport)

#---------------------------------------- GTest ----------------------------------------------------
if(SERIALBOX_TESTING)
  if(APPLE AND NOT(CMAKE_CXX_COMPILER_ID MATCHES "GNU"))
    add_definitions(-DGTEST_USE_OWN_TR1_TUPLE)
  endif()
  
  # If GTEST_ROOT is provided we will use it
  set(GTEST_ROOT_ENV "$ENV{GTEST_ROOT}")
  if(NOT(GTEST_ROOT) AND NOT(GTEST_ROOT_ENV))
    set(BUILD_GTEST "ON" BOOL)
    set(GTEST_INSTALL_DIR "${CMAKE_BINARY_DIR}/external/gtest/install")
    
    # Forward toolchain
    set(CMAKE_EXTERNAL_ARGS -Wno-dev
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} 
      -DCMAKE_GENERATOR=${CMAKE_GENERATOR}
      -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
      -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
      -DCMAKE_INSTALL_PREFIX=${GTEST_INSTALL_DIR}
    )

    # Build Gtest
    ExternalProject_Add(
        gtest
        URL        "${CMAKE_CURRENT_SOURCE_DIR}/external/gtest"
        PREFIX     "external/gtest"
        CMAKE_ARGS ${CMAKE_EXTERNAL_ARGS}
    )
    message(STATUS "Building Gtest from: ${CMAKE_CURRENT_SOURCE_DIR}/external/gtest")

    set(GTEST_INCLUDE_DIRS "${GTEST_INSTALL_DIR}/include")
    set(GTEST_LIBRARIES "${GTEST_INSTALL_DIR}/lib/libgtest.a"
                        "${GTEST_INSTALL_DIR}/lib/libgmock.a"
                        "${CMAKE_THREAD_LIBS_INIT}")
  else()
    find_package(GTest REQUIRED)
  endif()
  include_directories(SYSTEM ${GTEST_INCLUDE_DIRS})
  
  if(SERIALBOX_ENABLE_C)
    set(SERIALBOX_HAS_C 1)
  endif(SERIALBOX_ENABLE_C)
endif(SERIALBOX_TESTING)

#---------------------------------------- pFUnit ---------------------------------------------------
if(SERIALBOX_TESTING_FORTRAN)
  if(SERIALBOX_ENABLE_FORTRAN)
    set(PFUNIT_ROOT_ENV "$ENV{PFUNIT_ROOT}")
    if(NOT(PFUNIT_ROOT) AND NOT(PFUNIT_ROOT_ENV))
      set(BUILD_PFUNIT "ON" BOOL)
      set(PFUNIT_INSTALL_DIR "${CMAKE_BINARY_DIR}/external/pfunit/install")
    
      # Forward toolchain
      set(CMAKE_EXTERNAL_ARGS -Wno-dev
        -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} 
        -DCMAKE_GENERATOR=${CMAKE_GENERATOR}
        -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
        -DCMAKE_INSTALL_PREFIX=${PFUNIT_INSTALL_DIR}
      )

      # Build pFUnit
      ExternalProject_Add(
          pfunit
          URL        "${CMAKE_CURRENT_SOURCE_DIR}/external/pfunit"
          PREFIX     "external/pfunit"
          CMAKE_ARGS ${CMAKE_EXTERNAL_ARGS}
      )
      message(STATUS "Building pFUnit from: ${CMAKE_CURRENT_SOURCE_DIR}/external/pfunit")  

      # make the driver available in the binary dir
      set(PFUNIT_DRIVER "${CMAKE_BINARY_DIR}/external/pfunit/include/driver.F90")
      configure_file("${CMAKE_CURRENT_SOURCE_DIR}/external/pfunit/include/driver.F90" "${PFUNIT_DRIVER}" COPYONLY)
      
      set(PFUNIT_INCLUDE_DIRS "${PFUNIT_INSTALL_DIR}/include" "${PFUNIT_INSTALL_DIR}/mod")
      set(PFUNIT_LIBRARIES "${PFUNIT_INSTALL_DIR}/lib/libpfunit.a")
      set(PFUNIT_BIN_DIR "${PFUNIT_INSTALL_DIR}/bin")
    else()
      find_package(pFUnit REQUIRED)
    endif()
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
  include_directories(SYSTEM ${NETCDF_INCLUDES})
  set(SERIALBOX_EXTERNAL_LIBRARIES ${SERIALBOX_EXTERNAL_LIBRARIES} ${NETCDF_LIBRARIES})  
  set(SERIALBOX_HAS_NETCDF 1)
endif()

#---------------------------------------- Python ---------------------------------------------------
if(SERIALBOX_ENABLE_PYTHON)
  find_package(PythonInterp 3.4)
  
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
  
#---------------------------------------- GridTools ------------------------------------------------
if(SERIALBOX_TESTING_GRIDTOOLS)

  # If GRIDTOOLS_ROOT is not provided, we try to find it in external/
  set(GRIDTOOLS_ROOT_ENV "$ENV{GRIDTOOLS_ROOT}")
  if(NOT(GRIDTOOLS_ROOT) AND NOT(GRIDTOOLS_ROOT_ENV) AND 
     EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/external/gridtools/CMakeLists.txt" )
    message(STATUS "Using GridTools from: ${CMAKE_CURRENT_SOURCE_DIR}/external/gridtools")
    set(GRIDTOOLS_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/external/gridtools")
  endif()

  find_package(GridTools QUIET REQUIRED)
  include_directories(SYSTEM ${GRIDTOOLS_INCLUDE_DIRS})
  set(SERIALBOX_HAS_GRIDTOOLS 1)
  
endif(SERIALBOX_TESTING_GRIDTOOLS)

#---------------------------------------- std::experimental::filesystem ----------------------------
find_library( STD_FILESYSTEM_LIB NAMES stdc++fs PATHS
    /usr/lib/gcc/x86_64-linux-gnu/5
    /usr/lib/gcc/x86_64-linux-gnu/6
    /usr/lib/gcc/x86_64-linux-gnu/7
    /usr/lib64/gcc/x86_64-suse-linux/5
    /usr/lib64/gcc/x86_64-suse-linux/6
    /usr/lib64/gcc/x86_64-suse-linux/7
    /usr/lib/x86_64-linux-gnu/
    /usr/local/lib64/
    /usr/lib64/
    /usr/lib/
    ENV LD_LIBRARY_PATH)
if( STD_FILESYSTEM_LIB )
  message(STATUS "Using std::experimental::filesystem (no dependency on compiled boost libs)")
  set(SERIALBOX_EXTERNAL_LIBRARIES ${SERIALBOX_EXTERNAL_LIBRARIES} 
                                   ${STD_FILESYSTEM_LIB})
  add_definitions(-DSERIALBOX_USE_EXPERIMENTAL_FILESYSTEM)
else()
  message(STATUS "std::experimental::filesystem not found, using boost::filesystem")
endif()

#---------------------------------------- Boost ----------------------------------------------------

if( NOT STD_FILESYSTEM_LIB )
  set(REQUIRED_BOOST_COMPONENTS filesystem system)
endif()

find_package(Boost 1.54 REQUIRED ${REQUIRED_BOOST_COMPONENTS})

if(NOT(${Boost_FOUND}))
  message(FATAL_ERROR "Boost not found or insufficient version.")
endif(NOT(${Boost_FOUND}))

include_directories(SYSTEM ${Boost_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIRS})
set(SERIALBOX_BOOST_VERSION ${Boost_VERSION})

#---------------------------------------- STELLA ---------------------------------------------------
if(SERIALBOX_TESTING_STELLA)

  # If STELLA_ROOT is not provided, we try to find it in external/
  set(STELLA_ROOT_ENV "$ENV{STELLA_ROOT}")
  if(NOT(STELLA_ROOT) AND NOT(STELLA_ROOT_ENV) AND 
     EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/external/stella/CMakeLists.txt")
   
    set(STELLA_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/external/stella")
    
    set(BUILD_STELLA "ON" BOOL)
    set(STELLA_INSTALL_DIR "${CMAKE_BINARY_DIR}/external/stella/install")
    
    # Forward toolchain
    set(CMAKE_EXTERNAL_ARGS -Wno-dev
      -DSTELLA_ENABLE_BENCHMARK=OFF
      -DSTELLA_ENABLE_COMMUNICATION=OFF
      -DSTELLA_ENABLE_SERIALIZATION=OFF
      -DSTELLA_ENABLE_TESTING=OFF
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} 
      -DCMAKE_GENERATOR=${CMAKE_GENERATOR}
      -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
      -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
      -DCMAKE_INSTALL_PREFIX=${STELLA_INSTALL_DIR}
    )

    # Build STELLA
    ExternalProject_Add(
        stella
        URL        "${CMAKE_CURRENT_SOURCE_DIR}/external/stella"
        PREFIX     "external/stella"
        CMAKE_ARGS ${CMAKE_EXTERNAL_ARGS}
    )
    message(STATUS "Building STELLA from: ${CMAKE_CURRENT_SOURCE_DIR}/external/stella")

    set(STELLA_INCLUDE_DIRS "${STELLA_INSTALL_DIR}/include/STELLA")
    set(STELLA_LIBRARIES "${STELLA_INSTALL_DIR}/lib/libSharedInfrastructure.a"
                         "${STELLA_INSTALL_DIR}/lib/libStella.a"
                         "${STELLA_INSTALL_DIR}/lib/libStellaUtils.a")
  else()
    find_package(STELLA QUIET REQUIRED)
  endif()
  
  include_directories(SYSTEM ${STELLA_INCLUDE_DIRS})
  set(SERIALBOX_HAS_STELLA 1)

endif(SERIALBOX_TESTING_STELLA)

#---------------------------------------- Serialbox (old) ------------------------------------------
if(SERIALBOX_TESTING_OLD_SERIALBOX)

  # If SERIALBOX_OLD_ROOT is not provided, we try to find it in external/
  set(SERIALBOX_OLD_ROOT_ENV "$ENV{SERIALBOX_OLD_ROOT}")
  if(NOT(SERIALBOX_OLD_ROOT) AND NOT(SERIALBOX_OLD_ROOT_ENV) AND 
     EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/external/serialbox/CMakeLists.txt")
   
    set(SERIALBOX_OLD_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/external/serialbox")
    
    set(BUILD_SERIALBOX_OLD "ON" BOOL)
    set(SERIALBOX_OLD_INSTALL_DIR "${CMAKE_BINARY_DIR}/external/serialbox/install")
    
    # Forward toolchain
    set(CMAKE_EXTERNAL_ARGS -Wno-dev
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} 
      -DCMAKE_GENERATOR=${CMAKE_GENERATOR}
      -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
      -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
      -DCMAKE_INSTALL_PREFIX=${SERIALBOX_OLD_INSTALL_DIR}
    )
    
    # Propagate Boost if it was set via BOOST_ROOT
    if(BOOST_ROOT)
        list(APPEND CMAKE_EXTERNAL_ARGUMENTS "-DBOOST_ROOT=${BOOST_ROOT}")
    endif(BOOST_ROOT)

    # Build old serialbox
    ExternalProject_Add(
        serialbox-old
        URL        "${CMAKE_CURRENT_SOURCE_DIR}/external/serialbox"
        PREFIX     "external/serialbox"
        CMAKE_ARGS ${CMAKE_EXTERNAL_ARGS}
    )
    message(STATUS 
            "Building old SERIALBOX (0.1.1) from: ${CMAKE_CURRENT_SOURCE_DIR}/external/serialbox")

    set(SERIALBOX_OLD_INCLUDE_DIRS "${SERIALBOX_OLD_INSTALL_DIR}/include")
    set(SERIALBOX_OLD_LIBRARIES "${SERIALBOX_OLD_INSTALL_DIR}/lib/libSerialbox.a"
                                "${SERIALBOX_OLD_INSTALL_DIR}/lib/libUtils.a"
                                "${SERIALBOX_OLD_INSTALL_DIR}/lib/libjson.a"
                                "${SERIALBOX_OLD_INSTALL_DIR}/lib/libsha256.a")
  else()
    find_package(SerialboxOld QUIET REQUIRED)
  endif()
  
  include_directories(SYSTEM ${SERIALBOX_OLD_INCLUDE_DIRS})
  set(SERIALBOX_HAS_SERIALBOX_OLD 1)
  
endif(SERIALBOX_TESTING_OLD_SERIALBOX)

#---------------------------------------- ClangTools -----------------------------------------------
find_package(ClangTools)
if("$ENV{CMAKE_EXPORT_COMPILE_COMMANDS}" STREQUAL "1" OR CLANG_TIDY_FOUND)
  # Generate a Clang compile_commands.json "compilation database" file for use
  # with various development tools, such as Vim's YouCompleteMe plugin.
  # See http://clang.llvm.org/docs/JSONCompilationDatabase.html
  set(CMAKE_EXPORT_COMPILE_COMMANDS 1)
endif()

if(${CLANG_FORMAT_FOUND})
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

if(${CLANG_TIDY_FOUND})
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
set(SERIALBOX_CXX_CONFIG_FILE ${CMAKE_BINARY_DIR}/src/serialbox/core/Config.h)
configure_file(${SERIALBOX_CXX_CONFIG_FILE_IN} ${SERIALBOX_CXX_CONFIG_FILE})
install(FILES ${SERIALBOX_CXX_CONFIG_FILE} 
        DESTINATION ${CMAKE_INSTALL_PREFIX}/include/serialbox/core)
include_directories(${CMAKE_BINARY_DIR}/src)

# Setup include directories and glob the headers (for IDEs)
set(SERIALBOX_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/src)
include_directories(${SERIALBOX_INCLUDE_DIRS})

file(GLOB_RECURSE SERIALBOX_HEADERS 
     ${CMAKE_CURRENT_SOURCE_DIR}/src
     ${CMAKE_CURRENT_SOURCE_DIR}/src/serialbox/*.h*)
   
# Install serialbox headers
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src/serialbox DESTINATION include 
        FILES_MATCHING PATTERN "*.h" PATTERN "*.hpp")
      
if(SERIALBOX_ENABLE_C)
  file(GLOB_RECURSE SERIALBOX_C_HEADERS 
       ${CMAKE_CURRENT_SOURCE_DIR}/src
       ${CMAKE_CURRENT_SOURCE_DIR}/src/serialbox-c/*.h)
     
  install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src/serialbox-c DESTINATION include 
          FILES_MATCHING PATTERN "*.h")
endif()

## Build Serialbox
add_subdirectory(src)

## Install FindSerialbox.cmake
add_subdirectory(cmake)

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
  
  # Unittest Utility library
  include_directories(${PROJECT_SOURCE_DIR}/test)
  
  # Generate utility/Config.h
  set(SERIALBOX_CPP_CONFIG_FILE_IN ${PROJECT_SOURCE_DIR}/test/utility/Config.h.cmake)
  set(SERIALBOX_CPP_CONFIG_FILE ${CMAKE_BINARY_DIR}/test/utility/Config.h)
  configure_file(${SERIALBOX_CPP_CONFIG_FILE_IN} ${SERIALBOX_CPP_CONFIG_FILE})
  include_directories(${CMAKE_BINARY_DIR}/test)
  
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

