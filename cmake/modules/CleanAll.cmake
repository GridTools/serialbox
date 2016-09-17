# Remove of all CMake related files
#
# Usage of this module:
#
#  add_custom_target(clean-all
#                    COMMAND ${CMAKE_MAKE_PROGRAM} clean
#                    COMMAND ${CMAKE_COMMAND} -P <PATH_TO_THIS_SCRIPT>
#  )
#

set(cmake_generated ${CMAKE_BINARY_DIR}/CMakeCache.txt
                    ${CMAKE_BINARY_DIR}/CTestTestfile.cmake
                    ${CMAKE_BINARY_DIR}/cmake_install.cmake
                    ${CMAKE_BINARY_DIR}/Makefile
                    ${CMAKE_BINARY_DIR}/CMakeFiles
                    ${CMAKE_BINARY_DIR}/external
)

foreach(file ${cmake_generated})
  if(EXISTS ${file})
     file(REMOVE_RECURSE ${file})
  endif()
endforeach(file)
