project(serialbox2-cmake-test C CXX Fortran)
enable_testing()
cmake_minimum_required(VERSION 3.8.1)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/../cmake")

find_package(Serialbox CONFIG REQUIRED)

set(libs SerialboxCoreShared SerialboxCShared SerialboxFortranShared)

foreach(slib ${libs})
  if(TARGET sbox::${slib})
    file(GENERATE OUTPUT ${PROJECT_BINARY_DIR}/target${slib}.sh CONTENT "export SERIALBOX_CXX_LIBRARIES=\"-L $<TARGET_FILE_DIR:sbox::${slib}> -l$<TARGET_FILE_NAME:sbox::${slib}>\"")
  else()
    file(APPEND ${PROJECT_BINARY_DIR}/target${slib}.sh "")
  endif()
  install(FILES "${PROJECT_BINARY_DIR}/target${slib}.sh" DESTINATION cmake)

endforeach()

install(FILES ${CMAKE_SOURCE_DIR}/targetsSerialbox.sh DESTINATION cmake)

