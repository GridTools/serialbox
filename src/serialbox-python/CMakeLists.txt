##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license.
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

cmake_minimum_required(VERSION 3.12)

## Install ppser
install(
  FILES ${PROJECT_SOURCE_DIR}/src/serialbox-python/pp_ser/pp_ser.py
  DESTINATION python/pp_ser
  PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

## Install compare
install(
  FILES ${PROJECT_SOURCE_DIR}/src/serialbox-python/compare/compare.py
  DESTINATION python/compare
  PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
)

## Install serialbox
if(SERIALBOX_ENABLE_PYTHON)
  if(NOT(SERIALBOX_ENABLE_C))
    message(FATAL_ERROR
            "Python Interface requires the C Interface (set SERIALBOX_ENABLE_C=ON)")
  endif()

  if(NOT(SERIALBOX_BUILD_SHARED))
    message(FATAL_ERROR
            "Python Interface requires shared libraries (set SERIALBOX_BUILD_SHARED=ON)")
  endif()

  file(GLOB_RECURSE SERIALBOX_PYTHON_SOURCE
       ${PROJECT_SOURCE_DIR}/src/serialbox-python/
       ${PROJECT_SOURCE_DIR}/src/serialbox-python/*.py
       ${PROJECT_SOURCE_DIR}/src/serialbox-python/serialbox/*.py)

  set(REMOVE_OBSOLETE_SYMLINKS_OR_NOP
            find -L ${PROJECT_SOURCE_DIR}/src/serialbox-python/serialbox -type l -delete)

  add_custom_target(SerialboxPython
                    COMMAND ${REMOVE_OBSOLETE_SYMLINKS_OR_NOP}
                    COMMAND ${CMAKE_COMMAND} -E copy_directory
                    ${PROJECT_SOURCE_DIR}/src/serialbox-python/serialbox
                    ${PROJECT_BINARY_DIR}/src/serialbox-python/serialbox
                    DEPENDS "${SERIALBOX_PYTHON_SOURCE}")
  add_dependencies(SerialboxCShared SerialboxPython)

  # Install python source
  install(DIRECTORY ${PROJECT_SOURCE_DIR}/src/serialbox-python/serialbox/
          DESTINATION python/serialbox
          FILES_MATCHING PATTERN "*.py"
          PATTERN "*__pycache__*" EXCLUDE)

  install(CODE "
      EXECUTE_PROCESS(COMMAND
        ln -sf
        ${CMAKE_INSTALL_PREFIX}/lib/libSerialboxC${CMAKE_SHARED_LIBRARY_SUFFIX}
        ${CMAKE_INSTALL_PREFIX}/python/serialbox/libSerialboxC${CMAKE_SHARED_LIBRARY_SUFFIX})
  ")

endif()

## Install sdb
if(SERIALBOX_ENABLE_SDB)

  if(NOT(SERIALBOX_ENABLE_PYTHON))
    message(FATAL_ERROR
            "sdb requires the Python interface (set SERIALBOX_ENABLE_PYTHON=ON)")
  endif()

  file(GLOB_RECURSE SDB_PYTHON_SOURCE
       ${PROJECT_SOURCE_DIR}/src/serialbox-python/
       ${PROJECT_SOURCE_DIR}/src/serialbox-python/*.py
       ${PROJECT_SOURCE_DIR}/src/serialbox-python/sdb/*.py)

  add_custom_target(sdbPython
                    COMMAND ${CMAKE_COMMAND} -E copy_directory
                    ${PROJECT_SOURCE_DIR}/src/serialbox-python/sdb
                    ${PROJECT_BINARY_DIR}/src/serialbox-python/sdb
                    DEPENDS "${SDB_PYTHON_SOURCE}")
  add_dependencies(SerialboxPython sdbPython)

  # Install python source
  install(DIRECTORY ${PROJECT_SOURCE_DIR}/src/serialbox-python/sdb/
          DESTINATION python/sdb
          FILES_MATCHING PATTERN "*.py" PATTERN "*.png"
          PATTERN "*__pycache__*" EXCLUDE)

endif()
