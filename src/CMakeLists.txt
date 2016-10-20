##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

## \brief Create an object library with target "<target>Objects" and convert them to a static and/or 
## shared library with name "<name>.a" and <name>.so" respectively
## 
## \param name          STRING: Name of the library (e.g SerialboxCore)
## \param target        STRING: Target used by CMkae (e.g SerialboxCoreLibrary)
## \param objects       LIST OF STRINGS: Object libraries to include (e.g 
##                      SerialboxCoreLibraryObjects)
## \param buildStatic   BOOL: Convert object library to static library
## \param buildShared   BOOL: Convert object library to shared library (requires 
##                      SERIALBOX_BUILD_SHARED)
## \param addSymlinksTo LIST OF STRING: Add symlinks of the shared library to the specified paths
## \param libraries     LIST OF STRINGS: Specify external libraries or flags to be use when linking 
##                      the target
## \param ARGN          LIST OF STRINGS: List of source files
##
## Note: To pass in a list of elements you have to *quote* the variable i.e call 
##
## set(EXTERNAL_LIBRARIES ${libA} ${libB})
## serialbox_add_library(Serialbox 
##                       SerialboxLibrary 
##                       ""
##                       TRUE
##                       FALSE
##                       ""
##                       "${EXTERNAL_LIBRARIES}"
##                       SourceA.cpp SoureB.cpp SourceC.cpp)
##
function(serialbox_add_library 
         name 
         target 
         objects
         buildStatic
         buildShared
         addSymlinksTo
         libraries)

  add_library(${target}Objects OBJECT ${ARGN} ${SERIALBOX_HEADERS} ${SERIALBOX_C_HEADERS})

  if(SERIALBOX_VERBOSE_WARNINGS)
    set_target_properties(${target}Objects PROPERTIES COMPILE_FLAGS -Wall)
  endif(SERIALBOX_VERBOSE_WARNINGS)

  if(NOT("${objects}" STREQUAL ""))
    set(OBJECT_SOURCES)
    foreach(object ${objects})
      list(APPEND OBJECT_SOURCES $<TARGET_OBJECTS:${object}>)
    endforeach()
  endif()
  
  ## Set properties for code-coverage
  if(DEFINED COVERAGE_SUPPORTED)
    set_property(TARGET ${target}Objects PROPERTY COMPILE_FLAGS ${CMAKE_CXX_FLAGS_COVERAGE})
    set_property(TARGET ${target}Objects PROPERTY LINK_FLAGS ${CMAKE_SHARED_LINKER_FLAGS_COVERAGE})
    set_property(TARGET ${target}Objects PROPERTY RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
  endif()

  ## Build static library
  if(buildStatic)
    add_library(${target}Static STATIC $<TARGET_OBJECTS:${target}Objects> ${OBJECT_SOURCES})
    
    set_target_properties(${target}Static PROPERTIES OUTPUT_NAME ${name})
    set_target_properties(${target}Static PROPERTIES INTERPROCEDURAL_OPTIMIZATION True)  
    
    target_link_libraries(${target}Static ${libraries})
    install(TARGETS ${target}Static DESTINATION lib)
    
    if(DEFINED COVERAGE_SUPPORTED)
      set_property(TARGET ${target}Static PROPERTY LINK_FLAGS ${CMAKE_SHARED_LINKER_FLAGS_COVERAGE})
    endif()
  endif()

  ## Build shared library
  if(buildShared)

    if(NOT(SERIALBOX_BUILD_SHARED))
      message(WARNING "SERIALBOX_BUILD_SHARED is \"OFF\", cannot add shared library ${name}")
    endif(NOT(SERIALBOX_BUILD_SHARED))

    add_library(${target}Shared SHARED $<TARGET_OBJECTS:${target}Objects> ${OBJECT_SOURCES})
    
    set_target_properties(${target}Shared PROPERTIES OUTPUT_NAME ${name})
    set_target_properties(${target}Shared PROPERTIES INTERPROCEDURAL_OPTIMIZATION True)  
    
    target_link_libraries(${target}Shared ${libraries})
    install(TARGETS ${target}Shared DESTINATION lib)
    
    if(DEFINED COVERAGE_SUPPORTED)
      set_property(TARGET ${target}Shared PROPERTY LINK_FLAGS ${CMAKE_SHARED_LINKER_FLAGS_COVERAGE})
    endif()
  endif()
  
  ## Add symlinks of the shared library to the path "addSymlinksTo"
  if(NOT("${addSymlinksTo}" STREQUAL ""))
  
    if(NOT(buildShared))
      message(WARNING 
      "Cannot create symlinks of shared library of ${name} (set builShared=ON in the function call")
    endif()
  
    foreach(path ${addSymlinksTo})
      add_custom_command(TARGET ${target}Shared POST_BUILD
         COMMAND ${CMAKE_COMMAND} -E create_symlink
         $<TARGET_FILE:${target}Shared> 
         ${path}/$<TARGET_FILE_NAME:${target}Shared>)
    endforeach()
  endif()
  
endfunction(serialbox_add_library)

## Add subdirectories
add_subdirectory(serialbox)

if(SERIALBOX_ENABLE_C)
  add_subdirectory(serialbox-c)
endif()

if(SERIALBOX_ENABLE_PYTHON)
  add_subdirectory(serialbox-python)
endif()

