##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

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
  set(SERIALBOX_C_CONFIG_FILE ${CMAKE_BINARY_DIR}/src/serialbox-c/Config.h)
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

  set(EXTERNAL_LIBRARIES ${Boost_LIBRARIES} ${SERIALBOX_EXTERNAL_LIBRARIES})

  yoda_create_library(
                        TARGET SerialboxC
                        DEPENDS SerialboxCoreStatic 
                        SOURCES ${SOURCES}
                        PUBLIC_BUILD_INCLUDES ${CMAKE_SOURCE_DIR}/src/  
                        PUBLIC_INSTALL_INCLUDES include/
                        VERSION ${SERIALBOX_VERSION_STRING}
                        TARGET_GROUP SerialboxTargets
                        TARGET_NAMESPACE sbox::
  )

endif()

