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
        $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/src>
        $<INSTALL_INTERFACE:include>
    )
  if(BUILD_SHARED_LIBS)
    set_property(TARGET SerialboxFortranSerializeObjects PROPERTY POSITION_INDEPENDENT_CODE 1)
  endif()
  
  set(SOURCES_FORTRAN utils_ppser.f90 )
  if (SERIALBOX_ENABLE_FTG)
    set(SOURCES_FORTRAN ${SOURCES_FORTRAN} m_ser_ftg.f90)
  endif()

  add_library(SerialboxFortranObjects OBJECT ${SOURCES_FORTRAN})
  target_link_libraries(SerialboxFortranObjects SerialboxFortranSerializeObjects)
  target_include_directories(SerialboxFortranObjects
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # for .mod files
        $<INSTALL_INTERFACE:include>
    )
  if(BUILD_SHARED_LIBS)
    set_property(TARGET SerialboxFortranObjects PROPERTY POSITION_INDEPENDENT_CODE 1)
  endif()

  add_library(SerialboxFortranStatic STATIC $<TARGET_OBJECTS:SerialboxFortranSerializeObjects> $<TARGET_OBJECTS:SerialboxFortranObjects>)
  target_link_libraries(SerialboxFortranStatic PUBLIC SerialboxFortranObjects)
  target_link_libraries(SerialboxFortranStatic PUBLIC SerialboxFortranSerializeObjects)
  target_link_libraries(SerialboxFortranStatic PUBLIC SerialboxCStatic)
  set_target_properties(SerialboxFortranStatic PROPERTIES VERSION ${Serialbox_VERSION_STRING})

  install(TARGETS SerialboxFortranStatic SerialboxFortranObjects SerialboxFortranSerializeObjects
    EXPORT SerialboxTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    )

  if(BUILD_SHARED_LIBS)
    add_library(SerialboxFortranShared $<TARGET_OBJECTS:SerialboxFortranSerializeObjects> $<TARGET_OBJECTS:SerialboxFortranObjects>)
    target_link_libraries(SerialboxFortranShared PUBLIC SerialboxFortranObjects)
    target_link_libraries(SerialboxFortranShared PUBLIC SerialboxFortranSerializeObjects)
    target_link_libraries(SerialboxFortranShared PUBLIC SerialboxCShared)
    set_target_properties(SerialboxFortranShared PROPERTIES VERSION ${Serialbox_VERSION_STRING})
    
    install(TARGETS SerialboxFortranShared
      EXPORT SerialboxTargets
      LIBRARY DESTINATION lib
      ARCHIVE DESTINATION lib
      )
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
