##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

include(CMakeParseArguments)

## serialbox_add_library
## ---------------------
##
## Create an object library with target "<TARGET>Objects" and convert them to a static and/or 
## shared library with name "<NAME>.a" and <NAME>.so" respectively.
## 
##    NAME:STRING=<>           - Name of the library (e.g SerialboxCore)
##    TARGET:STRING=<>         - Target used by CMake (e.g SerialboxCoreLibrary)
##    OBJECTS:STRING=<>        - Object libraries to include (e.g SerialboxCoreLibraryObjects)
##    BUILD_STATIC:BOOL=<>     - Convert object library to static library
##    BUILD_SHARED:BOOL=<>     - Convert object library to shared library
##    SYMLINKS:STRING=<>       - Add symlinks of the shared library to the specified paths
##    LIBRARIES:=<>            - Specify external libraries or flags to be use when linking the 
##                               target
##    SOURCES:STRING=<>        - List of source files
##
function(serialbox_add_library)

  #
  # Parse arguments
  #
  set(options BUILD_SHARED BUILD_STATIC)
  set(oneValueArgs NAME TARGET)
  set(multiValueArgs OBJECTS SYMLINKS LIBRARIES SOURCES)
  cmake_parse_arguments(serialbox_add_library "${options}" "${oneValueArgs}" "${multiValueArgs}" 
                        ${ARGN})
                        
  set(name ${serialbox_add_library_NAME})
  set(target ${serialbox_add_library_TARGET})
  set(objects ${serialbox_add_library_OBJECTS})
  set(buildStatic ${serialbox_add_library_BUILD_STATIC})
  set(buildShared ${serialbox_add_library_BUILD_SHARED})
  set(addSymlinksTo ${serialbox_add_library_SYMLINKS})
  set(libraries ${serialbox_add_library_LIBRARIES})
  set(sources ${serialbox_add_library_SOURCES})
  
  if(NOT("${serialbox_add_library_UNPARSED_ARGUMENTS}" STREQUAL ""))
    message(FATAL_ERROR "invalid argument ${serialbox_add_library_UNPARSED_ARGUMENTS}")
  endif()

  #
  # Add object library
  #
  add_library(${target}Objects OBJECT ${sources} ${SERIALBOX_HEADERS} ${SERIALBOX_C_HEADERS})

  if(SERIALBOX_VERBOSE_WARNINGS)
    set_target_properties(${target}Objects PROPERTIES COMPILE_FLAGS -Wall)
  endif(SERIALBOX_VERBOSE_WARNINGS)

  if(NOT("${objects}" STREQUAL ""))
    set(OBJECT_SOURCES)
    foreach(object ${objects})
      list(APPEND OBJECT_SOURCES $<TARGET_OBJECTS:${object}>)
    endforeach()
  endif()

  if(DEFINED SERIALBOX_COMPILER_MSVC)
    if(buildShared)
      add_definitions(-DSERIALBOX_DLL)
    endif()
  endif()
  
  #
  # Set properties for code-coverage
  #
  if(DEFINED COVERAGE_SUPPORTED)
    set_property(TARGET ${target}Objects PROPERTY COMPILE_FLAGS ${CMAKE_CXX_FLAGS_COVERAGE})
    set_property(TARGET ${target}Objects PROPERTY LINK_FLAGS ${CMAKE_SHARED_LINKER_FLAGS_COVERAGE})
    set_property(TARGET ${target}Objects PROPERTY RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
  endif()

  #
  # Build static library
  #
  if(buildStatic)
    add_library(${target}Static STATIC $<TARGET_OBJECTS:${target}Objects> ${OBJECT_SOURCES})
    
    set_target_properties(${target}Static PROPERTIES OUTPUT_NAME ${name})
    
    target_link_libraries(${target}Static ${libraries})
    install(TARGETS ${target}Static DESTINATION lib)
    
    if(DEFINED COVERAGE_SUPPORTED)
      set_property(TARGET ${target}Static PROPERTY LINK_FLAGS ${CMAKE_SHARED_LINKER_FLAGS_COVERAGE})
    endif()
  endif()
  
  #
  # Build shared library
  #
  if(buildShared)

    if(NOT(SERIALBOX_BUILD_SHARED))
      message(WARNING "SERIALBOX_BUILD_SHARED is \"OFF\", cannot add shared library ${name}")
    endif(NOT(SERIALBOX_BUILD_SHARED))

    add_library(${target}Shared SHARED $<TARGET_OBJECTS:${target}Objects> ${OBJECT_SOURCES})
    
    set_target_properties(${target}Shared PROPERTIES OUTPUT_NAME ${name})

    target_link_libraries(${target}Shared ${libraries})
    install(TARGETS ${target}Shared DESTINATION lib)
    
    if(DEFINED COVERAGE_SUPPORTED)
      set_property(TARGET ${target}Shared PROPERTY LINK_FLAGS ${CMAKE_SHARED_LINKER_FLAGS_COVERAGE})
    endif()
  endif()
  
  #
  # Add symlinks of the shared library to the paths in "addSymlinksTo"
  #
  if(NOT("${addSymlinksTo}" STREQUAL ""))
  
    if(NOT(buildShared))
      message(WARNING 
      "Cannot create symlinks of shared library of ${name} (set builShared=ON in the function call")
    endif()

    if(UNIX)
      set(LINK_OR_COPY create_symlink)
    else()
      set(LINK_OR_COPY copy)
    endif()
  
    foreach(path ${addSymlinksTo})
      add_custom_command(TARGET ${target}Shared POST_BUILD
         COMMAND ${CMAKE_COMMAND} -E ${LINK_OR_COPY}
         $<TARGET_FILE:${target}Shared> 
         ${path}/$<TARGET_FILE_NAME:${target}Shared>)
    endforeach()
  endif()

endfunction(serialbox_add_library)

## Add subdirectories
add_subdirectory(serialbox)
add_subdirectory(serialbox-c)
add_subdirectory(serialbox-fortran)
add_subdirectory(serialbox-python)

