include(CMakeParseArguments)

# serialbox2_clone_repository
# ----------------------------
#
# This will make sure the repository NAME exists and, if not, will clone the branch BRANCH 
# from the git repository given by URL.
#
#    NAME:STRING=<>        - Name of the repository
#    URL:STRING=<>         - Version of the package
#    BRANCH:STRING=<>      - Do we use the system version of the package?
#    VAR_SOURCE_DIR:STRING - Output variable that will be set to the path where repository was cloned
#
function(serialbox2_clone_repository)
  set(options)
  set(one_value_args NAME URL BRANCH SOURCE_DIR)
  set(multi_value_args)
  cmake_parse_arguments(ARG "${options}" "${one_value_args}" "${multi_value_args}" ${ARGN})

  if(NOT("${ARG_UNPARSED_ARGUMENTS}" STREQUAL ""))
    message(FATAL_ERROR "invalid argument ${ARG_UNPARSED_ARGUMENTS}")
  endif()

  if(NOT(DEFINED ARG_NAME)) 
    message(FATAL_ERROR "name not specified")
  endif()

  if(NOT(DEFINED ARG_SOURCE_DIR))
    message(FATAL_ERROR "SOURCE_DIR not specified")
  endif()


  string(TOUPPER ${ARG_NAME} upper_name)
  set(source_dir "${CMAKE_SOURCE_DIR}/${ARG_NAME}")

  # Check if repository exists
  if(EXISTS "${source_dir}")
    set("${ARG_SOURCE_DIR}" "${source_dir}" PARENT_SCOPE)
    message(STATUS "Setting path to ${ARG_NAME} in: ${source_dir}}")
  else()
    find_program(git_executable "git")
    if(NOT(git_executable))
      message(FATAL_ERROR "ERROR: git not FOUND!")
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
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
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
    message(STATUS "Cloning ${ARG_NAME} from git: ${ARG_URL} ...")
    run_git("clone" "${ARG_URL}" "--branch=${ARG_BRANCH}" ${source_dir})
    message(STATUS "Successfully cloned ${ARG_NAME}")

    set("${ARG_SOURCE_DIR}" "${source_dir}" PARENT_SCOPE)
  endif()
endfunction()
