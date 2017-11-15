!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license. 
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------

PROGRAM main_consumer_perturb
  USE m_ser
  IMPLICIT NONE
  REAL(KIND=8), DIMENSION(5,5,5) :: a

  a = 0.0
  PRINT*,'Before read from serializer: sum(a)=', sum(a)
  CALL deserialize_with_perturb(a)
  PRINT*,'After read from serializer: sum(a)=', sum(a)
END PROGRAM main_consumer_perturb
