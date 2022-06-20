##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

enable_language(Fortran)

set(SOURCES serialbox_test.pf)
if (SERIALBOX_ENABLE_FTG)
  set(SOURCES ${SOURCES} ser_ftg_test.pf ser_ftg_cmp_test.pf)
endif()

# Setup external libraries
set(EXTERNAL_LIBRARIES SerialboxFortranStatic
                       SerialboxCStatic 
                       ${Boost_LIBRARIES} 
                       ${SERIALBOX_EXTERNAL_LIBRARIES}
                       )

set(generated_src_dir ${PROJECT_BINARY_DIR}/generated)
file(MAKE_DIRECTORY ${generated_src_dir})
file(WRITE ${generated_src_dir}/testSuites.inc "")
  
#SerialboxFortran mod files
#TODO: is there a better way to do this
include_directories(${PROJECT_BINARY_DIR}/src/serialbox-fortran)
include_directories(${generated_src_dir} ${PFUNIT_INCLUDE_DIRS})
  
add_pfunit_ctest(SerialboxFortranUnittest TEST_SOURCES ${SOURCES} LINK_LIBRARIES ${EXTERNAL_LIBRARIES})
set_target_properties(SerialboxFortranUnittest PROPERTIES LINKER_LANGUAGE Fortran) # required for nvhpc
