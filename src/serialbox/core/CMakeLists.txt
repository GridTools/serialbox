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
  FieldMapSerializer.h
  FieldMapSerializer.cpp
  FieldMetainfoImpl.cpp
  FieldMetainfoImpl.h
  FieldMetainfoImplSerializer.cpp
  FieldMetainfoImplSerializer.h
  FieldID.cpp
  FieldID.h
  Logging.cpp
  Logging.h
  MetainfoMapImpl.cpp
  MetainfoMapImpl.h
  MetainfoMapImplSerializer.cpp
  MetainfoMapImplSerializer.h
  MetainfoValueImpl.cpp
  MetainfoValueImpl.h
  SavepointImpl.cpp
  SavepointImpl.h
  SavepointImplSerializer.cpp
  SavepointImplSerializer.h
  SavepointVector.cpp
  SavepointVector.h
  SavepointVectorSerializer.cpp
  SavepointVectorSerializer.h
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

# we use "-lpthread" instead of ${CMAKE_THREAD_LIBS_INIT} on purpose, because ${CMAKE_THREAD_LIBS_INIT}
# contains "-pthread" instead of "-lpthread" which is a problem if it propagates to nvcc via the target
set(EXTERNAL_LIBRARIES ${Boost_LIBRARIES} ${STD_FILESYSTEM_LIB} -lpthread)
yoda_create_library(TARGET Serialbox
                      LIBRARIES ${EXTERNAL_LIBRARIES}
                      SOURCES ${SOURCES}
                      PUBLIC_BUILD_INCLUDES ${CMAKE_SOURCE_DIR}/src
                      PUBLIC_INSTALL_INCLUDES include/
                      VERSION ${Serialbox_VERSION_STRING}
                      TARGET_GROUP SerialboxTargets
                      TARGET_NAMESPACE Serialbox::
)
target_link_libraries(SerialboxObjects PUBLIC Boost::boost)

if(SERIALBOX_USE_NETCDF)
  target_link_libraries(SerialboxStatic PRIVATE NETCDF_TARGET)
  if(BUILD_SHARED_LIBS)
    target_link_libraries(SerialboxShared PUBLIC NETCDF_TARGET)
  endif()
  #TODO try to make private once we protect our headers
  target_include_directories(SerialboxObjects PUBLIC SYSTEM ${NETCDF_INCLUDES})
endif()

#TODO: propagate this fix to yoda
#(note that this is compiling with -fPic even for static libs which can have performance implication)
if(BUILD_SHARED_LIBS)
  set_property(TARGET SerialboxObjects PROPERTY POSITION_INDEPENDENT_CODE 1)
endif()
