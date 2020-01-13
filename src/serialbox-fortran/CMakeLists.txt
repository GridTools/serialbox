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
  add_library(SerialboxFortranSerializeObjects OBJECT ${SOURCES_FORTRAN_SERIALIZE})
  target_include_directories(SerialboxFortranSerializeObjects
    PUBLIC
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
        $<INSTALL_INTERFACE:include>
    )
  set(SOURCES_FORTRAN utils_ppser.f90 utils_ppser_kbuff.f90)

  if (SERIALBOX_ENABLE_FTG)
    set(SOURCES_FORTRAN ${SOURCES_FORTRAN} m_ser_ftg.f90 m_ser_ftg_cmp.f90)
  endif()

  add_library(SerialboxFortranObjects OBJECT ${SOURCES_FORTRAN})
  target_link_libraries(SerialboxFortranObjects SerialboxFortranSerializeObjects)
  target_include_directories(SerialboxFortranObjects
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # for .mod files
        $<INSTALL_INTERFACE:include>
    )

  add_library(SerialboxFortranStatic STATIC $<TARGET_OBJECTS:SerialboxFortranSerializeObjects> $<TARGET_OBJECTS:SerialboxFortranObjects>)
  set_target_properties(SerialboxFortranStatic PROPERTIES OUTPUT_NAME SerialboxFortran)
  target_link_libraries(SerialboxFortranStatic PUBLIC SerialboxFortranObjects)
  target_link_libraries(SerialboxFortranStatic PUBLIC SerialboxFortranSerializeObjects)
  target_link_libraries(SerialboxFortranStatic PUBLIC SerialboxCStatic)
  set_target_properties(SerialboxFortranStatic PROPERTIES VERSION ${Serialbox_VERSION_STRING})


  if(BUILD_SHARED_LIBS)
    set_target_properties( SerialboxFortranSerializeObjects PROPERTIES POSITION_INDEPENDENT_CODE ON )
    set_target_properties( SerialboxFortranObjects PROPERTIES POSITION_INDEPENDENT_CODE ON )
    add_library(SerialboxFortranShared SHARED $<TARGET_OBJECTS:SerialboxFortranSerializeObjects> $<TARGET_OBJECTS:SerialboxFortranObjects>)
    set_target_properties(SerialboxFortranShared PROPERTIES OUTPUT_NAME SerialboxFortran)
    target_link_libraries(SerialboxFortranShared PUBLIC SerialboxFortranObjects)
    target_link_libraries(SerialboxFortranShared PUBLIC SerialboxFortranSerializeObjects)
    target_link_libraries(SerialboxFortranShared PUBLIC SerialboxCShared)
    set_target_properties(SerialboxFortranShared PROPERTIES VERSION ${Serialbox_VERSION_STRING})
  endif()

  serialbox_install_targets( TARGETS
    SerialboxFortranStatic
    SerialboxFortranShared
    SerialboxFortranObjects
    SerialboxFortranSerializeObjects
  )


  # Install mod files
  foreach(source ${SOURCES_FORTRAN} ${SOURCES_FORTRAN_SERIALIZE})
    get_filename_component(module_name ${source} NAME_WE)

    if("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Cray")
      string(TOUPPER ${module_name} module_name)
    endif()

    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${module_name}.mod" DESTINATION include)
  endforeach()

endif()
