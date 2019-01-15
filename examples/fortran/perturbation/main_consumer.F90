!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license. 
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------

PROGRAM main_consumer
  USE m_ser
  IMPLICIT NONE
  REAL(KIND=8), DIMENSION(5,5,5) :: a

  a = 0.0
  PRINT*,'Before read from serializer: sum(a)=', sum(a)
  CALL deserialize(a)
  PRINT*,'After read from serializer: sum(a)=', sum(a)
  
  IF (sum(a) .NE. 625.0 ) THEN
    PRINT*,'Expected ', 625.0, ', got ', sum(a)
    call EXIT(1)
  ELSE
    PRINT*,'Got expected result.'
  END IF
END PROGRAM main_consumer
