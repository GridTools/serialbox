##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license.
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# General Flags (add to default)
set(CMAKE_Fortran_FLAGS
    "${CMAKE_Fortran_FLAGS} -ffree -N255 -ec -eC -eI -eF -hflex_mp=conservative -Ofp1 -hadd_paren -ra")

if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 9.0.0)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -hnosecond_underscore ")
endif()

# OpenACC flags
set(OpenACC_FLAGS "-hnoacc")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenACC_FLAGS}")

# Release
set(CMAKE_Fortran_FLAGS_RELEASE "")

# Debug Options (replace default)
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -eD -Rb -Rc -Rd -Rp -Rs")
