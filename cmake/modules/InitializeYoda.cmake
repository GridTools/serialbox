##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

# Downloads and initializes yoda

set(YODA_REPOSITORY_URL "https://github.com/cosunae/yoda.git")
set(YODA_REPOSITORY_BRANCH "create_library")

if(NOT("${YODA_ROOT}" STREQUAL ""))
    message("Using predefined yoda")
    set(YODA_SOURCE_DIR "${YODA_ROOT}")
else()
    set(YODA_INSTALL_PATH "${CMAKE_CURRENT_BINARY_DIR}/yoda")

    # Check if repository exists
    if(EXISTS "${YODA_INSTALL_PATH}")
        message(STATUS "yoda found at ${YODA_INSTALL_PATH}")
    else()
        find_program(git_executable "git")
        if(NOT(git_executable))
            message(FATAL_ERROR "ERROR: git not found, cannot install yoda!")
        endif()
        mark_as_advanced(git_executable)

        macro(run_git)
            unset(result)
            unset(out_var)
            unset(error_var)
            set(cmd "${git_executable}")
            foreach(arg ${ARGN})
                set(cmd ${cmd} "${arg}")
            endforeach()
    
            execute_process(
                COMMAND ${cmd}
                WORKING_DIRECTORY "${CMAKE_CURRENT_BUILD_DIR}"
                RESULT_VARIABLE result
                OUTPUT_VARIABLE out_var
                ERROR_VARIABLE error_var
            )
        
            if(NOT("${result}" STREQUAL "0"))
                string(REPLACE ";" " " cmd_str "${cmd}")
                message(FATAL_ERROR "${error_var}\n\nERROR: failed to run\n\"${cmd_str}\"\n\n${error_var}")
            endif()
        endmacro()

        # Clone repository
        message(STATUS "Cloning yoda from ${YODA_REPOSITORY_URL} with branch/tag ${YODA_REPOSITORY_BRANCH}")
        run_git("clone" "${YODA_REPOSITORY_URL}" "--branch=${YODA_REPOSITORY_BRANCH}" ${YODA_INSTALL_PATH})
        message(STATUS "Successfully cloned yoda")
    
    endif()
    
    list(APPEND CMAKE_MODULE_PATH "${YODA_INSTALL_PATH}/cmake")
    include(yodaInit)
    yoda_init()
    
endif()

