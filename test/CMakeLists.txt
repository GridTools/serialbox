##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.1)

## \brief Add a unittest executable
##
## The exectuable will be linked against all Serialbox libraries as well as the external libraries 
## (Boost, GTest and GLog). 
##
## \param name          Name of the executable (i.e the <target>)
## \param sources       List of source files
##
## Note: To pass in a list of elements you have to *quote* the variable i.e call 
##    serialbox_add_unittest_executable(MyExecutable "${MySources}")
##
function(serialbox_add_unittest_executable name sources)
  add_executable(${name} ${sources} ${SERIALBOX_HEADERS})

  target_link_libraries(${name} SerialboxSupport
                                ${SERIALBOX_EXTERNAL_LIBRARIES})
  
  # Compile with warnings?
  if(SERIALBOX_VERBOSE_WARNINGS)
    set_target_properties(${name} PROPERTIES COMPILE_FLAGS -Wall)
  endif(SERIALBOX_VERBOSE_WARNINGS)

  # Depend on GTest?
  if(BUILD_GTEST)
    add_dependencies(${name} gtest)
  endif(BUILD_GTEST)

  # Depend on GLog?
  if(BUILD_GLOG)
    add_dependencies(${name} glog)
  endif(BUILD_GLOG)
  
  add_test(RunSerialboxUnittest ${name} --gtest_color=yes)

endfunction(serialbox_add_unittest_executable)

## Add subdirectories
add_subdirectory(Unittest)
