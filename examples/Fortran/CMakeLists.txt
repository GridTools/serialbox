add_executable(
    fortran_producer
    m_ser.f90
    main_producer.f90
    ${SerialboxFortranStatic}
    ${Boost_LIBRARIES}
    ${SERIALBOX_EXTERNAL_LIBRARIES}
    ${CMAKE_THREAD_LIBS_INIT})

add_executable(fortran_consumer
    m_ser.f90
    main_consumer.f90
    ${SerialboxFortranStatic}
    ${Boost_LIBRARIES}
    ${SERIALBOX_EXTERNAL_LIBRARIES}
    ${CMAKE_THREAD_LIBS_INIT})

