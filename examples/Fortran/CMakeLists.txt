include_directories("${CMAKE_CURRENT_BINARY_DIR}/../../src/serialbox-fortran")
link_directories("${CMAKE_CURRENT_BINARY_DIR}/../../src/serialbox-fortran")

add_executable(fortran_producer
    m_ser.f90
    main_producer.f90)

target_link_libraries(fortran_producer
    SerialboxFortranLibraryStatic
    ${Boost_LIBRARIES}
    ${SERIALBOX_EXTERNAL_LIBRARIES}
    ${CMAKE_THREAD_LIBS_INIT})

add_executable(fortran_consumer
    m_ser.f90
    main_consumer.f90
)
target_link_libraries(fortran_consumer
    SerialboxFortranLibraryStatic
    ${Boost_LIBRARIES}
    ${SERIALBOX_EXTERNAL_LIBRARIES}
    ${CMAKE_THREAD_LIBS_INIT})

