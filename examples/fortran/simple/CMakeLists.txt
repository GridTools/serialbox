##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

serialbox_add_fortran_example(fortran_producer main_producer.F90 m_ser.F90)
set_property(TARGET fortran_producer PROPERTY LINKER_LANGUAGE CXX)
serialbox_add_fortran_example(fortran_consumer main_consumer.F90 m_ser.F90)
set_property(TARGET fortran_consumer PROPERTY LINKER_LANGUAGE CXX)

