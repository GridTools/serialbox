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
    "${CMAKE_Fortran_FLAGS} -ffree -eZ -N255 -ec -eC -eI -eF -hnosecond_underscore -hflex_mp=conservative -Ofp1 -hadd_paren -ra")

# OpenACC flags
set(OpenACC_FLAGS "-hnoacc")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenACC_FLAGS}")

# Release
set(CMAKE_Fortran_FLAGS_RELEASE "")

# Debug Options (replace default)
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -eZ -eD -Rb -Rc -Rd -Rp -Rs")
