PROGRAM main_consumer
  USE m_ser
  IMPLICIT NONE
  REAL(KIND=8), DIMENSION(5,5,5) :: a

  a = 0.0
  PRINT*,'Before read from serializer: sum(a)=', sum(a)
  CALL deserialize(a)
  PRINT*,'After read from serializer: sum(a)=', sum(a)
END PROGRAM main_consumer
