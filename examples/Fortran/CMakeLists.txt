##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

serialbox_add_fortran_example(fortran_producer main_producer.f90 m_ser.f90)
serialbox_add_fortran_example(fortran_consumer main_consumer.f90 m_ser.f90)

