##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

if(SERIALBOX_ENABLE_FORTRAN)

  set(SOURCES_FORTRAN_SERIALIZE m_serialize.f90 m_ser_perturb.f90)
  yoda_add_library( NAME SerialboxFortranSerialize
                    SOURCES ${SOURCES_FORTRAN_SERIALIZE}
                    DEPENDS SerialboxCoreStatic SerialboxCStatic 
                    OBJECT
  )
  if(BUILD_SHARED_LIBS)
    set_property(TARGET SerialboxFortranSerializeObjects PROPERTY POSITION_INDEPENDENT_CODE 1)
  endif()
  
  install(TARGETS SerialboxFortranSerializeObjects
    EXPORT SerialboxTargets
  )

  set(SOURCES_FORTRAN utils_ppser.f90 )
  if (SERIALBOX_ENABLE_FTG)
    set(SOURCES_FORTRAN ${SOURCES_FORTRAN} m_ser_ftg.f90)
  endif()

  yoda_create_library(TARGET SerialboxFortran
                      SOURCES ${SOURCES_FORTRAN}
                      DEPENDS SerialboxFortranSerializeObjects SerialboxCoreStatic SerialboxCStatic 
                      PUBLIC_BUILD_INCLUDES ${CMAKE_SOURCE_DIR}/src
                      PUBLIC_INSTALL_INCLUDES include/
                      VERSION ${SERIALBOX_VERSION_STRING}
                      TARGET_GROUP SerialboxTargets
                      TARGET_NAMESPACE sbox::
  )
  target_link_libraries(SerialboxFortranObjects SerialboxFortranSerializeObjects)
  #TODO: propagate this fix to yoda
  #(note that this is compiling with -fPic even for static libs which can have performance implication)
  if(BUILD_SHARED_LIBS)
    set_property(TARGET SerialboxFortranObjects PROPERTY POSITION_INDEPENDENT_CODE 1)
  endif()

  # Install mod files
  foreach(source ${SOURCES_FORTRAN} ${SOURCES_FORTRAN_SERIALIZE})
    get_filename_component(module_name ${source} NAME_WE)

    if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
      string(TOUPPER ${module_name} module_name)
    endif()
    
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.mod" DESTINATION include)
  endforeach()

endif()
