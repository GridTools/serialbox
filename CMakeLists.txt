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

project(serialbox C CXX)
cmake_minimum_required(VERSION 3.1)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/modules")

include(CheckCXXCompilerFlag)
include(ExternalProject)
include(CMakeParseArguments)
include(GetGitRevisionDescription)

#---------------------------------------- Configure ------------------------------------------------

if(UNIX)
  set(SERIALBOX_ON_UNIX 1)
endif(UNIX)

if(WIN32)
  set(SERIALBOX_ON_WIN32 1)
endif(WIN32)

# Serialbox version
set(SERIALBOX_VERSION_MAJOR 2)
set(SERIALBOX_VERSION_MINOR 0)
set(SERIALBOX_VERSION_PATCH 1)
set(SERIALBOX_VERSION_STRING 
    "${SERIALBOX_VERSION_MAJOR}.${SERIALBOX_VERSION_MINOR}.${SERIALBOX_VERSION_PATCH}")
    
# Current head revision
get_git_head_revision(GIT_REFSPEC GIT_SHA1)

message(STATUS "Serialbox version: ${SERIALBOX_VERSION_STRING}")
message(STATUS "Git HEAD revision: ${GIT_SHA1}")

#---------------------------------------- User options ---------------------------------------------

option(SERIALBOX_TESTING "Build unit-test executables" ON)
option(SERIALBOX_LOGGING "Enable logging" ON)
option(SERIALBOX_VERBOSE_WARNINGS "Enable verbose warnings (-Wall)" ON)

option(SERIALBOX_ENABLE_GRIDTOOLS "Build GridTools Unitests" OFF)
option(SERIALBOX_ENABLE_STELLA "Build STELLA Unitests" OFF)
option(SERIALBOX_ENABLE_FORTRAN "Build Fortran Unitests" OFF)

option(SERIALBOX_CODE_COVERAGE "Generate code coverage" OFF)
  
#---------------------------------------- CMake options --------------------------------------------
 
# Set C++ standard
if("${CMAKE_CXX_COMPILER_ID}" MATCHES "Cray")
  set(CXX11_FLAG "-h std=c++11")
else()
  set(CXX11_FLAG "-std=c++11")
endif()

CHECK_CXX_COMPILER_FLAG("${CXX11_FLAG}" COMPILER_SUPPORTS_CXX11)
if(NOT(COMPILER_SUPPORTS_CXX11))
  message(FATAL_ERROR "The compiler ${CMAKE_CXX_COMPILER} has no C++11 support.")
endif(NOT(COMPILER_SUPPORTS_CXX11))
set(CMAKE_CXX_FLAGS "${CXX11_FLAG} ${CMAKE_CXX_FLAGS}")

# Set build type to RelWithDebInfo if nothing was specified (instead of Debug)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "RelWithDebInfo " CACHE STRING 
      "Choose the type of build, options are: Debug Release RelWithDebInfo " FORCE)
  message(STATUS "Setting build type to 'RelWithDebInfo' as none was specified")
endif(NOT CMAKE_BUILD_TYPE)

# Clear all cmake generated files
add_custom_target(clean-all
                  COMMAND ${CMAKE_MAKE_PROGRAM} clean
                  COMMAND ${CMAKE_COMMAND} -P  "${CMAKE_SOURCE_DIR}/cmake/modules/clean-all.cmake")
                  
# We need thread support
find_package(Threads REQUIRED)

# Set script directory
set(SCRIPT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/scripts)

#---------------------------------------- sha256 ---------------------------------------------------
set(SHA256_INSTALL_DIR "${CMAKE_BINARY_DIR}/external/sha256/install")
    
# Forward toolchain
set(CMAKE_EXTERNAL_ARGS -Wno-dev
  -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} 
  -DCMAKE_GENERATOR=${CMAKE_GENERATOR}
  -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
  -DCMAKE_INSTALL_PREFIX=${SHA256_INSTALL_DIR}
)

# Build libsha256
ExternalProject_Add(
  sha256
  URL        "${CMAKE_CURRENT_SOURCE_DIR}/external/sha256"
  PREFIX     "external/sha256"
  CMAKE_ARGS ${CMAKE_EXTERNAL_ARGS}
)
message(STATUS "Building sh256 from: ${CMAKE_CURRENT_SOURCE_DIR}/external/sh256")
    
set(SHA256_INCLUDE_DIRS "${SHA256_INSTALL_DIR}/include")
set(SHA256_LIBRARIES "${SHA256_INSTALL_DIR}/lib/libsha256.a")
include_directories(${SHA256_INCLUDE_DIRS})

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
    set(GTEST_MAIN_LIBRARIES "${GTEST_INSTALL_DIR}/lib/libgtest_main.a" "${CMAKE_THREAD_LIBS_INIT}")                    
                        
  else()
    find_package(GTest REQUIRED)
  endif()
  include_directories(SYSTEM ${GTEST_INCLUDE_DIRS})
endif(SERIALBOX_TESTING)

#---------------------------------------- GLog -----------------------------------------------------
if(SERIALBOX_LOGGING)
  
  # If GLOG_ROOT is provided we will use it
  set(GLOG_ROOT_ENV "$ENV{GLOG_ROOT}")
  if(NOT(GLOG_ROOT) AND NOT(GLOG_ROOT_ENV))
    set(BUILD_GLOG "ON" BOOL)
    set(GLOG_INSTALL_DIR "${CMAKE_BINARY_DIR}/external/glog/install")
    
    # Forward toolchain
    set(CMAKE_EXTERNAL_ARGS -Wno-dev
      -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE} 
      -DCMAKE_GENERATOR=${CMAKE_GENERATOR}
      -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
      -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
      -DCMAKE_INSTALL_PREFIX=${GLOG_INSTALL_DIR}
    )

    # Build Glog
    ExternalProject_Add(
        glog
        URL        "${CMAKE_CURRENT_SOURCE_DIR}/external/glog"
        PREFIX     "external/glog"
        CMAKE_ARGS ${CMAKE_EXTERNAL_ARGS}
    )
    message(STATUS "Building Glog from: ${CMAKE_CURRENT_SOURCE_DIR}/external/glog")

    set(GLOG_INCLUDE_DIRS "${GLOG_INSTALL_DIR}/include")
    set(GLOG_LIBRARIES "${GLOG_INSTALL_DIR}/lib/libglog.a" 
                       "${CMAKE_THREAD_LIBS_INIT}")
  else()
    find_package(GLog REQUIRED)
  endif()
  include_directories(SYSTEM ${GLOG_INCLUDE_DIRS})
else()
  add_definitions(-DSERIALBOX_DISABLE_LOGGING)
endif()

#---------------------------------------- Boost ----------------------------------------------------
find_package(Boost 1.54 REQUIRED system chrono filesystem)

if(NOT(${Boost_FOUND}))
  message(FATAL_ERROR "Boost not found or insufficient version.")
endif(NOT(${Boost_FOUND}))

include_directories(SYSTEM ${Boost_INCLUDE_DIRS})
link_directories(${Boost_LIBRARY_DIRS})


#---------------------------------------- JSON -----------------------------------------------------
message(STATUS "Using json from: ${CMAKE_CURRENT_SOURCE_DIR}/external/json")
include_directories(SYSTEM ${CMAKE_CURRENT_SOURCE_DIR}/external/json/include)

#---------------------------------------- GridTools ------------------------------------------------
if(SERIALBOX_ENABLE_GRIDTOOLS)

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
  
endif(SERIALBOX_ENABLE_GRIDTOOLS)

#---------------------------------------- STELLA ---------------------------------------------------
if(SERIALBOX_ENABLE_STELLA)

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

endif(SERIALBOX_ENABLE_STELLA)

#--------------------------------------- Fortran ---------------------------------------------------
if(SERIALBOX_ENABLE_FORTRAN)

  set(SERIALBOX_HAS_FORTRAN 1)

endif(SERIALBOX_ENABLE_FORTRAN)

#---------------------------------------- Doxygen --------------------------------------------------
find_package(Doxygen)
option(SERIALBOX_DOCUMENTATION 
       "Create and install the HTML based API documentation (requires Doxygen)" ${DOXYGEN_FOUND})
set(DOC_DIR ${PROJECT_SOURCE_DIR}/docs)

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
                          ${CMAKE_CURRENT_SOURCE_DIR}/include 
                          -name \*.h -print -o -name \*.cpp -print`)

  # Runs clang format and exits with a non-zero exit code if any files need to be reformatted
  add_custom_target(check-format 
                    COMMAND ${SCRIPT_DIR}/run-clang-format.sh 
                    ${CMAKE_CURRENT_SOURCE_DIR} 
                    ${CLANG_FORMAT_BIN} 
                    0
                    `find ${CMAKE_CURRENT_SOURCE_DIR}/src 
                          ${CMAKE_CURRENT_SOURCE_DIR}/include 
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
                          ${CMAKE_CURRENT_SOURCE_DIR}/include 
                          -name \*.h -print -o -name \*.cpp -print`)

  # Runs clang-tidy and exits with a non-zero exit code if any errors are found.
  add_custom_target(check-clang-tidy 
                    COMMAND ${SCRIPT_DIR}/run-clang-tidy.sh 
                    ${CLANG_TIDY_BIN} 
                    ${CMAKE_BINARY_DIR}/compile_commands.json
                    0
                    `find ${CMAKE_CURRENT_SOURCE_DIR}/src 
                          ${CMAKE_CURRENT_SOURCE_DIR}/include 
                          -name \*.h -print -o -name \*.cpp -print`)
endif()

# --------------------------------------- Code Coverage --------------------------------------------
if(SERIALBOX_CODE_COVERAGE)
  include(Coverage)
  enable_coverage(${CMAKE_BINARY_DIR}/test)
endif(SERIALBOX_CODE_COVERAGE)

#---------------------------------------- Compilation ----------------------------------------------

# Generate serialbox/Support/Config.h
set(SERIALBOX_CONFIG_FILE_IN ${PROJECT_SOURCE_DIR}/include/serialbox/Core/Config.h.cmake)
set(SERIALBOX_CONFIG_FILE ${CMAKE_BINARY_DIR}/include/serialbox/Core/Config.h)
set(SERIALBOX_CONFIG_FILE_DISCLAIMER "WARNING! All changes made in this file will be lost!")
configure_file(${SERIALBOX_CONFIG_FILE_IN} ${SERIALBOX_CONFIG_FILE})
include_directories(${CMAKE_BINARY_DIR}/include)

enable_testing()

# Setup include directories and glob the headers (for IDEs)
set(SERIALBOX_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(${SERIALBOX_INCLUDE_DIRS})

file(GLOB_RECURSE SERIALBOX_HEADERS 
     ${CMAKE_CURRENT_SOURCE_DIR}/include
     ${CMAKE_CURRENT_SOURCE_DIR}/include/serialbox/*.h)
     
     
# Setup external libraries
set(SERIALBOX_EXTERNAL_LIBRARIES ${Boost_LIBRARIES} 
                                 ${GTEST_LIBRARIES} 
                                 ${GLOG_LIBRARIES} 
                                 ${SHA256_LIBRARIES})

if(SERIALBOX_HAS_STELLA)
  set(SERIALBOX_EXTERNAL_LIBRARIES ${SERIALBOX_EXTERNAL_LIBRARIES} ${STELLA_LIBRARIES})
endif(SERIALBOX_HAS_STELLA)

# Build Serialbox
add_subdirectory(src)

# Build unittests
if(SERIALBOX_TESTING)
  
  # ------------------ C++ ------------------
  
  # Cpp Unittest Utility library
  include_directories(${PROJECT_SOURCE_DIR}/test/Unittest/Cpp)
  
  # Generate Cpp/Utility/CppConfig.h
  set(SERIALBOX_CPP_CONFIG_FILE_IN ${PROJECT_SOURCE_DIR}/test/Unittest/Cpp/Utility/CppConfig.h.cmake)
  set(SERIALBOX_CPP_CONFIG_FILE ${CMAKE_BINARY_DIR}/test/Unittest/Cpp/Utility/CppConfig.h)
  configure_file(${SERIALBOX_CPP_CONFIG_FILE_IN} ${SERIALBOX_CPP_CONFIG_FILE})
  include_directories(${CMAKE_BINARY_DIR}/test/Unittest/Cpp/)
  
  add_subdirectory(test)
endif(SERIALBOX_TESTING)


