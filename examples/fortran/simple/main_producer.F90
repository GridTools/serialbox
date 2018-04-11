!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license. 
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------

#define ACC_PREFIX !$acc
PROGRAM main_producer
  USE m_ser
  IMPLICIT NONE
  REAL(KIND=8), DIMENSION(5,5,5) :: a

  a = 5.0
  PRINT *, 'CALL serialize with sum(a)=', sum(a)
  CALL serialize(a)
END PROGRAM main_producer
