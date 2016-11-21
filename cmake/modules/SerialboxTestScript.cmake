##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

include(CMakeParseArguments)

## serialbox_test_init
## -------------------
##
## Setup the test_script.
##
function(serialbox_test_init)
  set(SERIALBOX_TEST_SCRIPT ${CMAKE_BINARY_DIR}/run_tests.sh CACHE PATH "Test script path")
  file(WRITE ${SERIALBOX_TEST_SCRIPT} "#!/bin/bash\n")
  file(APPEND ${SERIALBOX_TEST_SCRIPT} "res=0\n")
endfunction(serialbox_test_init)

## serialbox_add_test
## ------------------
## 
## Add a unittest to CTest and the test_script.
##
##    TARGET:STRINGS=<>          - The first string in the list will be treated as the CMake target 
##                                 to run i.e the file $<TARGET_FILE:target> will be added to CTest
##                                 while the remaining elements in the list are passed as 
##                                 command-line arguments.
##    EXECUTABLE:STRINGS=<>      - The first string in the list will be treated as the exectuable 
##                                 to run while the remaining elements in the list are passed as 
##                                 command-line arguments.
##
function(serialbox_add_test)
  cmake_parse_arguments(serialbox_add_test "" "" "TARGET;EXECUTABLE" ${ARGN})
  
  set(target_list ${serialbox_add_test_TARGET})
  set(exectuable_list ${serialbox_add_test_EXECUTABLE})

  if(target_list)
    list(GET target_list 0 target)
    
    set(args ${target_list})
    list(REMOVE_AT args 0)
    
    set(flat_args "")
    foreach(arg ${args})
      set(flat_args "${flat_args} ${arg}")
    endforeach()
    
    add_test(NAME ${target} COMMAND $<TARGET_FILE:${target}> ${flat_args})
  endif()
  
  if(exectuable_list)
    list(GET exectuable_list 0 exectuable)
    
    set(args ${exectuable_list})
    list(REMOVE_AT args 0)
    
    set(flat_args "")
    foreach(arg ${args})
      set(flat_args "${flat_args} ${arg}")
    endforeach()
  
    file(APPEND ${SERIALBOX_TEST_SCRIPT} "\n${exectuable} ${flat_args}\n")
    file(APPEND ${SERIALBOX_TEST_SCRIPT} "if [ $? -ne 0 ] ; then\n echo \"Error: problem found in Unittest\"\nfi\n")
    file(APPEND ${SERIALBOX_TEST_SCRIPT} "res=$((res || $? ))\n")
  endif()

endfunction(serialbox_add_test)


## serialbox_test_end
## ------------------
##
## Append last statements to the test_script.
##
function(serialbox_test_end)
  file(APPEND ${SERIALBOX_TEST_SCRIPT} 
       "\nif [ $res -ne 0 ]; then\n printf \"\\n  TESTS FAILED\\n\\n\"\nelse\nprintf \"\\n  ALL TESTS PASSED\\n\\n\"\nfi\n")
  file(INSTALL ${SERIALBOX_TEST_SCRIPT} DESTINATION ${CMAKE_BINARY_DIR}/install
       FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ)
endfunction(serialbox_test_end)

