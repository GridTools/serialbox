##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

include(yodaCreateLibrary)
cmake_minimum_required(VERSION 3.12)

set(SOURCES 
  FieldMap.cpp
  FieldMap.h
  FieldMetainfoImpl.cpp
  FieldMetainfoImpl.h
  FieldID.cpp
  FieldID.h
  Logging.cpp
  Logging.h
  MetainfoMapImpl.cpp
  MetainfoMapImpl.h
  MetainfoValueImpl.cpp
  MetainfoValueImpl.h
  SavepointImpl.cpp
  SavepointImpl.h
  SavepointVector.cpp
  SavepointVector.h
  SerializerImpl.cpp
  SerializerImpl.h
  StorageView.cpp
  StorageView.h
  Type.cpp
  Type.h
  Unreachable.cpp
  Unreachable.h
  
  hash/HashFactory.cpp
  hash/HashFactory.h
  hash/SHA256.cpp
  hash/SHA256.h
  hash/MD5.cpp
  hash/MD5.h
  
  archive/ArchiveFactory.cpp
  archive/ArchiveFactory.h
  archive/BinaryArchive.cpp
  archive/BinaryArchive.h
  archive/NetCDFArchive.cpp
  archive/NetCDFArchive.h
  archive/MockArchive.cpp
  archive/MockArchive.h
  
  frontend/stella/MetainfoSet.cpp
  frontend/stella/MetainfoSet.h
  frontend/stella/Savepoint.cpp
  frontend/stella/Savepoint.h
  frontend/stella/Serializer.cpp
  frontend/stella/Serializer.h
  frontend/stella/DataFieldInfo.cpp
  frontend/stella/DataFieldInfo.h
)

add_library(SerialboxObjects OBJECT ${SOURCES})
target_include_directories(SerialboxObjects
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/src>
        $<INSTALL_INTERFACE:include>
    )
target_link_libraries(SerialboxObjects PUBLIC Boost::boost)
target_link_libraries(SerialboxObjects PUBLIC SerialboxFilesytemTarget)
target_link_libraries(SerialboxObjects PUBLIC ${CMAKE_THREAD_LIBS_INIT}) # TODO target based

if(GRIDTOOLS_FOUND)
    target_link_libraries(SerialboxObjects PUBLIC GridTools_TARGET)
endif()

add_library(SerialboxStatic $<TARGET_OBJECTS:SerialboxObjects>)
set_target_properties(SerialboxStatic PROPERTIES VERSION ${Serialbox_VERSION_STRING})
target_link_libraries(SerialboxStatic PUBLIC SerialboxObjects)
if(BUILD_SHARED_LIBS)
    add_library(SerialboxShared $<TARGET_OBJECTS:SerialboxObjects>)
    set_target_properties(SerialboxShared PROPERTIES VERSION ${Serialbox_VERSION_STRING})
    target_link_libraries(SerialboxShared PUBLIC SerialboxObjects)
endif()

if(SERIALBOX_USE_NETCDF)
    target_link_libraries(SerialboxStatic PRIVATE NETCDF_TARGET)
    if(BUILD_SHARED_LIBS)
        target_link_libraries(SerialboxShared PUBLIC NETCDF_TARGET)
    endif()
    #TODO try to make private once we protect our headers
    target_include_directories(SerialboxObjects PUBLIC SYSTEM ${NETCDF_INCLUDES})
endif()

if(BUILD_SHARED_LIBS)
    set_property(TARGET SerialboxObjects PROPERTY POSITION_INDEPENDENT_CODE 1)
endif()

install(TARGETS SerialboxStatic SerialboxObjects
    EXPORT SerialboxTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    )

if(BUILD_SHARED_LIBS)
    install(TARGETS SerialboxShared
        EXPORT SerialboxTargets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        )
endif()