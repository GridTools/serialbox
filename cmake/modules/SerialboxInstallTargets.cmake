##===------------------------------------------------------------------------------*- CMake -*-===##
##
##                                   S E R I A L B O X
##
## This file is distributed under terms of BSD license. 
## See LICENSE.txt for more information.
##
##===------------------------------------------------------------------------------------------===##

#
# serialbox_install_targets( TARGETS [ target1 [ target2 ... ] )
#
#  Arguments:
#
#       TARGETS [target1 [ target2 ...] ]   Targets to be exported and installed.
#
function( serialbox_install_targets )
  cmake_parse_arguments( serialbox_install_targets "" "" "TARGETS" ${ARGN})
  
  set(target_list ${serialbox_install_targets_TARGETS})

  foreach( target ${target_list} )
    if( TARGET ${target} )
      install(TARGETS ${target}
        EXPORT SerialboxTargets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
      )
      export( TARGETS ${target}
        APPEND FILE ${PROJECT_BINARY_DIR}/SerialboxTargets.cmake
        NAMESPACE Serialbox::
      )
      add_library( Serialbox::${target} ALIAS ${target} )
    endif()
  endforeach()
endfunction()
