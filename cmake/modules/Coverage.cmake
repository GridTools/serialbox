#
# Setup code coverage using gcov and lcov and generate HTLM output using genhtml
#

function(enable_coverage root)
  set(EXCLUDE_PATTERN # System
                      '/usr/*'
                      '/opt/*'

                      # Unittests and external libraries
                      '*test/*'
                      '*external/*'
                      '*External/*' 
                      
                      # Not fully testable
                      '*/Unreachable.cpp'
                      '*/Logging.cpp'
                      '*/SHA256.cpp'
                      '*/MD5.cpp'
  )

  add_custom_target(coverage
    COMMAND ${CMAKE_COMMAND} -E remove coverage.info coverage.info.cleaned
    COMMAND ${LCOV_PATH} --directory . --zerocounters
    COMMAND ctest
    COMMAND ${LCOV_PATH} --directory . --capture --output-file coverage.info
    COMMAND ${LCOV_PATH} --remove coverage.info ${EXCLUDE_PATTERN} -o coverage.info.cleaned
    WORKING_DIRECTORY ${root}
    COMMENT "Processing code coverage counters"
  )

  if(DEFINED GENHTML_PATH)
    add_custom_target(coverage2html
      COMMAND ${GENHTML_PATH} coverage.info.cleaned -o ${root}/coverage
      WORKING_DIRECTORY ${root}
      COMMENT "Generating HTML coverage output in ${root}/coverage"
    )
    add_dependencies(coverage2html coverage)
  endif()
endfunction()

if(NOT CMAKE_COMPILER_IS_GNUCXX)
  message(STATUS "Coverage disabled - compiler does not support gcov!")
  return()
endif()

find_program(GCOV_PATH gcov)
find_program(LCOV_PATH lcov)
find_program(GENHTML_PATH genhtml)

if(NOT GCOV_PATH)
  message(STATUS "Coverage disabled - gcov not found!")
  return()
endif()

if(NOT LCOV_PATH)
  message(STATUS "Coverage disabled - lcov not found!")
  return()
endif()

if(NOT GENHTML_PATH)
  message(STATUS "Generation of HTML coverage output - genhtml not found!")
  return()
endif()

set(CMAKE_CXX_FLAGS_COVERAGE "-g --coverage -fprofile-arcs -ftest-coverage"
    CACHE STRING "Flags used by the C++ compiler during coverage builds." FORCE)
  
set(CMAKE_C_FLAGS_COVERAGE "-g --coverage -fprofile-arcs -ftest-coverage"
    CACHE STRING "Flags used by the C compiler during coverage builds." FORCE)
  
set(CMAKE_EXE_LINKER_FLAGS_COVERAGE "-lgcov -fprofile-arcs"
    CACHE STRING "Flags used for linking binaries during coverage builds." FORCE)
  
set(CMAKE_SHARED_LINKER_FLAGS_COVERAGE "-lgcov -fprofile-arcs"
    CACHE STRING "Flags used by the shared libraries linker during coverage builds." FORCE)

mark_as_advanced(CMAKE_CXX_FLAGS_COVERAGE
                 CMAKE_C_FLAGS_COVERAGE
                 CMAKE_EXE_LINKER_FLAGS_COVERAGE
                 CMAKE_SHARED_LINKER_FLAGS_COVERAGE)

set(COVERAGE_SUPPORTED TRUE)
message(STATUS "Code coverage enabled")
message(STATUS "  gcov: ${GCOV_PATH}")
message(STATUS "  lcov: ${LCOV_PATH}")
if(DEFINED GENHTML_PATH)
  message(STATUS "  genhtml: ${GENHTML_PATH}")
endif()

