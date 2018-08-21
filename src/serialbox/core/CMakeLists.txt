##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

include(yodaCreateLibrary)
cmake_minimum_required(VERSION 3.1)

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

set(EXTERNAL_LIBRARIES ${Boost_LIBRARIES} ${STD_FILESYSTEM_LIB} ${CMAKE_THREAD_LIBS_INIT})
yoda_create_library(TARGET SerialboxCore
                      LIBRARIES ${EXTERNAL_LIBRARIES}
                      SOURCES ${SOURCES}
                      PUBLIC_BUILD_INCLUDES ${CMAKE_SOURCE_DIR}/src SYSTEM ${Boost_INCLUDE_DIRS}
                      PUBLIC_INSTALL_INCLUDES include/
                      VERSION ${SERIALBOX_VERSION_STRING}
                      TARGET_GROUP SerialboxTargets
                      TARGET_NAMESPACE sbox::
)
if(SERIALBOX_USE_NETCDF)
  target_link_libraries(SerialboxCoreStatic PRIVATE NETCDF_TARGET)
endif()

#TODO: propagate this fix to yoda
#(note that this is compiling with -fPic even for static libs which can have performance implication)
if(BUILD_SHARED_LIBS)
  set_property(TARGET SerialboxCoreObjects PROPERTY POSITION_INDEPENDENT_CODE 1)
endif()
