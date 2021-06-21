##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

add_library(simple_m_ser STATIC m_ser.F90)
target_link_libraries( simple_m_ser
                        PUBLIC 
                        SerialboxFortranStatic 
                        ${CMAKE_THREAD_LIBS_INIT}
                        )
target_include_directories(simple_m_ser PUBLIC ${PROJECT_BINARY_DIR}/src/serialbox-fortran)
set_target_properties( simple_m_ser PROPERTIES LINKER_LANGUAGE Fortran)

add_executable(fortran_producer main_producer.F90)
target_link_libraries(fortran_producer simple_m_ser)
install(TARGETS fortran_producer DESTINATION bin)
set_target_properties(fortran_producer PROPERTIES LINKER_LANGUAGE Fortran)

add_executable(fortran_consumer main_consumer.F90)
target_link_libraries(fortran_consumer simple_m_ser)
install(TARGETS fortran_consumer DESTINATION bin)
set_target_properties(fortran_consumer PROPERTIES LINKER_LANGUAGE Fortran)
