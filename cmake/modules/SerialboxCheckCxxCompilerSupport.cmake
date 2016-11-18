##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# Copyright Louis Dionne 2013-2016
# Copyright Markus J. Weber 2015
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)
#
#
# This CMake module checks whether the current compiler is supported, and
# provides friendly hints to the user.

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    if (${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS "3.4")
        message(WARNING "
    ### You appear to be using Clang ${CMAKE_CXX_COMPILER_VERSION}, which is known
    ### to be unable to compile Serialbox. Consider switching to
    ### Clang >= 3.4. If it is already installed on your
    ### system, you can tell CMake about it with
    ###
    ###     cmake .. -DCMAKE_CXX_COMPILER=/path/to/clang
        ")
    endif()
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
    if (${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS "6.1.0")
        message(WARNING "
    ### You appear to be using Apple's Clang ${CMAKE_CXX_COMPILER_VERSION}, which is
    ### shipped with Xcode < 6.3. Unfortunately, only Apple's Clang
    ### >= 6.1.0 (shipped with Xcode >= 6.3) is supported by Serialbox.
    ### You should consider updating to Xcode >= 6.3 (requires Yosemite)
    ### or using a non-Apple Clang >= 3.4, which can be installed via
    ### Homebrew with
    ###
    ###     brew install llvm --with-clang
    ###
    ### You can then tell CMake to use that non-system Clang with
    ###
    ###     cmake .. -DCMAKE_CXX_COMPILER=/path/to/clang
        ")
    endif()
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Intel")
    if (${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS "17")
        message(WARNING "
    ### You appear to be using Intel's ICC ${CMAKE_CXX_COMPILER_VERSION}, which is known
    ### to be unable to compile Serialbox. Consider switching to
    ### ICC >= 17.0. If it is already installed on your
    ### system, you can tell CMake about it with
    ###
    ###     cmake .. -DCMAKE_CXX_COMPILER=/path/to/icpc
        ")
    endif()
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    if (${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS "4.9")
        message(WARNING "
    ### You appear to be using GCC ${CMAKE_CXX_COMPILER_VERSION}, which is known to be
    ### unable to compile Serialbox. Only GCC >= 4.9 is supported.
    ### Consider using a more recent GCC or switching to Clang.
    ### If a more recent compiler is already installed on your
    ### system, you can tell CMake to use it with
    ###
    ###     cmake .. -DCMAKE_CXX_COMPILER=/path/to/g++
        ")
    endif()
elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
    if (${CMAKE_CXX_COMPILER_VERSION} VERSION_LESS "19.0")
        message(WARNING "
    ### You appear to be using Visual Studio ${CMAKE_CXX_COMPILER_VERSION}, 
    ### which is known to be unable to compile Serialbox. 
    ### Only Visual Studio 14 2015 is currently supported.
        ")
    endif()
else()
    message(WARNING "
    ### You appear to be using a compiler that is not yet tested with Serialbox 2.
    ")
endif()
