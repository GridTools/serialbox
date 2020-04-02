# TODO I just copy snippets to this location from the old main CMakeLists.txt


# If we build the Fortran Interface, we need a Fortran compiler
if(SERIALBOX_ENABLE_FORTRAN)
  enable_language(Fortran)
  include(SerialboxFortranCompilerFlags)
endif()

# CRAY does not want to build shared libraries
if("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "Cray")
  message(STATUS "Disabling shared libraries for Cray compiler")
  set(SERIALBOX_BUILD_SHARED OFF)
  set(BUILD_SHARED_LIBS OFF)
  message(STATUS "Disabling python")
  set(SERIALBOX_ENABLE_PYTHON OFF)
  set(SERIALBOX_ENABLE_SDB OFF)
endif()




#---------------------------------------- pFUnit ---------------------------------------------------
if(SERIALBOX_TESTING_FORTRAN)
  if(SERIALBOX_ENABLE_FORTRAN)
    set(PFUNIT_ROOT_ENV "$ENV{PFUNIT_ROOT}")
    find_package(pFUnit REQUIRED)
  else()
    message(WARNING "You need to enable Fortran (-DSERIALBOX_ENABLE_FORTRAN=ON) to build the Fortran tests.")
  endif()
endif()