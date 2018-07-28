##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

if(SERIALBOX_ENABLE_FORTRAN)

  set(SOURCES m_serialize.f90 m_ser_perturb.f90 utils_ppser.f90)
  if (SERIALBOX_ENABLE_FTG)
    set(SOURCES ${SOURCES} m_ser_ftg.f90)
  endif()
  
  yoda_create_library(TARGET SerialboxFortran
                      DEPENDS SerialboxCoreStatic SerialboxCStatic
                      SOURCES ${SOURCES}
                      PUBLIC_BUILD_INCLUDES ${CMAKE_SOURCE_DIR}/src
                      PUBLIC_INSTALL_INCLUDES include/
                      VERSION ${SERIALBOX_VERSION_STRING}
  )


  # Install mod files
  foreach(source ${SOURCES})
    get_filename_component(module_name ${source} NAME_WE)

    if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
      string(TOUPPER ${module_name} module_name)
    endif()
    
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.mod" DESTINATION include)
  endforeach()

endif()
