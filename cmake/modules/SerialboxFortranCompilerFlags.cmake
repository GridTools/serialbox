##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

#
# Set the correct Fortran flags
#

if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
  include(Fortran/Cray)
endif()
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "PGI")
  include(Fortran/PGI)
endif()
if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
  include(Fortran/GNU)
endif()

