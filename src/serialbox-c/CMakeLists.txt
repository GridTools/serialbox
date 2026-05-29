##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license.
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

if(SERIALBOX_ENABLE_C)
  # Generate serialbox-c/Config.h
  set(SERIALBOX_CXX_COMPILER ${CMAKE_C_COMPILER})
  set(SERIALBOX_CXX_FLAGS ${CMAKE_CXX_FLAGS})
  if(CMAKE_BUILD_TYPE MATCHES "Release")
    set(SERIALBOX_CXX_FLAGS "${SERIALBOX_CXX_FLAGS} ${CMAKE_CXX_FLAGS_RELEASE}")
  elseif(CMAKE_BUILD_TYPE MATCHES "RelWithDebInfo")
    set(SERIALBOX_CXX_FLAGS "${SERIALBOX_CXX_FLAGS} ${CMAKE_CXX_FLAGS_RELWITHDEBINFO}")
  elseif(CMAKE_BUILD_TYPE MATCHES "MinSizeRel")
    set(SERIALBOX_CXX_FLAGS "${SERIALBOX_CXX_FLAGS} ${CMAKE_CXX_FLAGS_MINSIZEREL}")
  else(CMAKE_BUILD_TYPE MATCHES "Debug")
    set(SERIALBOX_CXX_FLAGS "${SERIALBOX_CXX_FLAGS} ${CMAKE_CXX_FLAGS_DEBUG}")
  endif()

  set(SERIALBOX_C_CONFIG_FILE_IN ${PROJECT_SOURCE_DIR}/src/serialbox-c/Config.h.cmake)
  set(SERIALBOX_C_CONFIG_FILE ${PROJECT_SOURCE_DIR}/src/serialbox-c/Config.h)
  set(SERIALBOX_C_CONFIG_FILE_DISCLAIMER "WARNING! All changes made in this file will be lost!")
  configure_file(${SERIALBOX_C_CONFIG_FILE_IN} ${SERIALBOX_C_CONFIG_FILE})
  install(FILES ${SERIALBOX_C_CONFIG_FILE} DESTINATION ${CMAKE_INSTALL_PREFIX}/include/serialbox-c)

  # Compile serialboxC
  set(SOURCES
    Archive.cpp
    Archive.h
    Array.cpp
    Array.h
    ConfigOptions.cpp
    ConfigOptions.h
    ErrorHandling.cpp
    ErrorHandling.h
    FieldMetainfo.cpp
    FieldMetainfo.h
    Logging.cpp
    Logging.h
    Metainfo.cpp
    Metainfo.h
    Savepoint.cpp
    Savepoint.h
    Serializer.cpp
    Serializer.h
    FortranWrapper.cpp
    FortranWrapper.h
  )

  set(EXTERNAL_LIBRARIES ${SERIALBOX_EXTERNAL_LIBRARIES})

  add_library(SerialboxCObjects OBJECT ${SOURCES})
  target_include_directories(SerialboxCObjects
    PUBLIC
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
        $<INSTALL_INTERFACE:include>
    )
  target_link_libraries(SerialboxCObjects PUBLIC Boost::boost)

  add_library(SerialboxCStatic STATIC $<TARGET_OBJECTS:SerialboxCObjects>)
  set_target_properties(SerialboxCStatic PROPERTIES OUTPUT_NAME SerialboxC)
  set_target_properties(SerialboxCStatic PROPERTIES VERSION ${Serialbox_VERSION_STRING})
  target_link_libraries(SerialboxCStatic PUBLIC SerialboxCObjects)
  target_link_libraries(SerialboxCStatic PUBLIC SerialboxStatic)
  if(BUILD_SHARED_LIBS)
    add_library(SerialboxCShared $<TARGET_OBJECTS:SerialboxCObjects>)
    set_target_properties(SerialboxCShared PROPERTIES OUTPUT_NAME SerialboxC)
    set_target_properties(SerialboxCShared PROPERTIES VERSION ${Serialbox_VERSION_STRING})
    target_link_libraries(SerialboxCShared PUBLIC SerialboxCObjects)
    target_link_libraries(SerialboxCShared PUBLIC SerialboxShared)
  endif()

  if(BUILD_SHARED_LIBS)
    set_property(TARGET SerialboxCObjects PROPERTY POSITION_INDEPENDENT_CODE 1)
  endif()

  serialbox_install_targets( TARGETS
    SerialboxCStatic
    SerialboxCShared
    SerialboxCObjects
  )

endif()
