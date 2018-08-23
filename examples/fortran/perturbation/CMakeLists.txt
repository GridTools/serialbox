##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##
##
## This is an example CMake file which shows a standalone Fortran example.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

#
# First, we setup the project and look for Serialbox. This is a pure Fortran project, however we 
# still require the C++ compiler for linking (i.e we depend on libstdc++ or libc++).
#
project(SerialboxPerturbationExample CXX Fortran)

#
# We use the Serialbox which was installed in install/. If this folder does not exists, or is empty,
# you have to build and install Serialbox first. To tell CMake where Serialbox is located, you have
# to define SERIALBOX_ROOT (either directly in CMake or pass it as -DSERIALBOX_ROOT=<> via 
# command-line).
#
if(NOT(DEFINED SERIALBOX_ROOT))
	set(SERIALBOX_ROOT "${CMAKE_SOURCE_DIR}/../../../install")
endif()

#
# Serialbox has a find_package-module. Usually you want to bundle this module with your own project
# and therefore we copied it into "cmake/". We need to tell CMake about this.
#

#
# Now, we call the find_package-module and specifically request the Fortran interface. Note that 
# this obviously required you to compile Serialbox with SERIALBOX_FORTRAN_ENABLE=ON.
#
find_package(Serialbox REQUIRED COMPONENTS Fortran HINTS
    "${CMAKE_CURRENT_LIST_DIR}/../serialbox2/install/cmake"
    "${SERIALBOX_ROOT}/cmake"
    "${SERIALBOX_DIR}/cmake"
    "$ENV{SERIALBOX_ROOT}/cmake")


#
# Set some Fortran specific compiler flags (e.g enable preprocessing) and set the corresponding 
# flags for serialization.
#
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -ffree-form -ffree-line-length-180 -J.")
add_definitions(-DSERIALIZE)

#
# Here we preprocess all the source files, using the preprocesser script pp_ser.py, and store them 
# in ${CMAKE_SOURCE_DIR}/pp. The necessary CMake function `serialbox_run_pp_ser` is located in 
# SerialboxTooling.cmake which is installed in ${SERIALBOX_ROOT}/cmake.
#
list(APPEND CMAKE_MODULE_PATH "${SERIALBOX_ROOT}/cmake")
include(SerialboxTooling)

set(PP_OUTPUT ${CMAKE_SOURCE_DIR}/pp)
serialbox_run_pp_ser(SOURCES main_producer.f90  
                             main_consumer.f90
                             main_consumer_perturb.f90
                             m_ser.f90 
                     OUTPUT_DIR ${PP_OUTPUT})

#
# Now, we add the executables. But first, we need to tell CMake where to find the *.mod files.
#
include_directories(${SERIALBOX_INCLUDE_DIRS})

# The source files of `add_executable` are the preprocessed files from before.
# The variable `SERIALBOX_FORTRAN_LIBRARIES` contains all the necessary external libraries (Boost, 
# OpenSSL, NetCDF) and the libSerialboxFortran itself. As already mentioned, we need a C++ linker 
# which adds the correct STL library (libstdc++ or libc++) to the LD flags.
#

## Producer
add_executable(fortran_producer ${PP_OUTPUT}/main_producer.f90 ${PP_OUTPUT}/m_ser.f90)
target_link_libraries(fortran_producer ${SERIALBOX_FORTRAN_LIBRARIES})
set_target_properties(fortran_producer PROPERTIES LINKER_LANGUAGE CXX)

## Consumer
add_executable(fortran_consumer ${PP_OUTPUT}/main_consumer.f90 ${PP_OUTPUT}/m_ser.f90)
target_link_libraries(fortran_consumer ${SERIALBOX_FORTRAN_LIBRARIES})
set_target_properties(fortran_consumer PROPERTIES LINKER_LANGUAGE CXX)

## Perturbed Consumer
add_executable(fortran_consumer_perturb ${PP_OUTPUT}/main_consumer_perturb.f90 ${PP_OUTPUT}/m_ser.f90)
target_link_libraries(fortran_consumer_perturb ${SERIALBOX_FORTRAN_LIBRARIES})
set_target_properties(fortran_consumer_perturb PROPERTIES LINKER_LANGUAGE CXX)

#
# Finally, copy our run script to the build directory
#
file(COPY ${CMAKE_SOURCE_DIR}/run.sh DESTINATION ${CMAKE_BINARY_DIR})

