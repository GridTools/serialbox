##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

add_library(simple_m_ser m_ser.F90)
target_link_libraries( simple_m_ser
                        PUBLIC 
                        SerialboxFortranStatic 
                        ${CMAKE_THREAD_LIBS_INIT}
                        )
target_include_directories(simple_m_ser PUBLIC ${CMAKE_BINARY_DIR}/src/serialbox-fortran)
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI" OR ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    set_target_properties( simple_m_ser PROPERTIES LINKER_LANGUAGE Fortran)
endif()

add_executable(fortran_producer main_producer.F90)
target_link_libraries(fortran_producer simple_m_ser)
install(TARGETS fortran_producer DESTINATION bin)
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI" OR ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    set_target_properties(fortran_producer PROPERTIES LINKER_LANGUAGE Fortran)
endif()

add_executable(fortran_consumer main_consumer.F90)
target_link_libraries(fortran_consumer simple_m_ser)
install(TARGETS fortran_consumer DESTINATION bin)
if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI" OR ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    set_target_properties(fortran_consumer PROPERTIES LINKER_LANGUAGE Fortran)
endif()

