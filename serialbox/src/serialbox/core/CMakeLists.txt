##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

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

add_library(Serialbox ${SOURCES})
set_target_properties(Serialbox PROPERTIES VERSION ${Serialbox_VERSION_STRING})
target_include_directories(Serialbox
    PUBLIC
        $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src>
        $<INSTALL_INTERFACE:include>
    )
target_link_libraries(Serialbox PRIVATE Boost::boost)
target_link_libraries(Serialbox PUBLIC SerialboxFilesytemTarget)
# we use "-lpthread" instead of ${CMAKE_THREAD_LIBS_INIT} on purpose, because ${CMAKE_THREAD_LIBS_INIT}
# contains "-pthread" instead of "-lpthread" which is a problem if it propagates to nvcc via the target
# TODO use the workaround from GridTools
target_link_libraries(Serialbox PUBLIC pthread)

if(SERIALBOX_USE_NETCDF)
    target_link_libraries(Serialbox PRIVATE NETCDF_TARGET)
    #TODO try to make private once we protect our headers
    target_include_directories(Serialbox SYSTEM PUBLIC ${NETCDF_INCLUDES})
endif()

install(TARGETS Serialbox
    EXPORT SerialboxTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    )