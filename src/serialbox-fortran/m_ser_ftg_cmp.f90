!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license.
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------
!
!+ This module contains the FortranTestGenerator (FTG) frontend of Serialbox2.
!+ For FTG see https://github.com/fortesg/fortrantestgenerator
!
!------------------------------------------------------------------------------

MODULE m_ser_ftg_cmp

!------------------------------------------------------------------------------
!
! Description:
!
!   This module contains subroutines to compare a given field with stored values. 
!   To be used by the FortranTestGenerator (https://github.com/fortesg/fortrantestgenerator)
!
! Current Code Owner: Christian Hovy, Universitaet Hamburg
!  email:  hovy@informatik.uni-hamburg.de
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!============================================================================

USE iso_c_binding
USE m_ser_ftg

IMPLICIT NONE

PUBLIC :: ftg_cmp_default_tolerance, ftg_cmp_max_print_deviations, ftg_cmp_message_prefix, ftg_compare

PRIVATE

CHARACTER(LEN=*), PARAMETER :: module_name = 'm_ser_ftg_cmp'

INTERFACE ftg_cmp_print_deviations
  MODULE PROCEDURE &
    ftg_cmp_print_deviations_logical_0d, &
    ftg_cmp_print_deviations_logical_1d, &
    ftg_cmp_print_deviations_logical_2d, &
    ftg_cmp_print_deviations_logical_3d, &
    ftg_cmp_print_deviations_logical_4d, &
    ftg_cmp_print_deviations_bool_0d, &
    ftg_cmp_print_deviations_bool_1d, &
    ftg_cmp_print_deviations_bool_2d, &
    ftg_cmp_print_deviations_bool_3d, &
    ftg_cmp_print_deviations_bool_4d, &
    ftg_cmp_print_deviations_int_0d, &
    ftg_cmp_print_deviations_int_1d, &
    ftg_cmp_print_deviations_int_2d, &
    ftg_cmp_print_deviations_int_3d, &
    ftg_cmp_print_deviations_int_4d, &
    ftg_cmp_print_deviations_long_0d, &
    ftg_cmp_print_deviations_long_1d, &
    ftg_cmp_print_deviations_long_2d, &
    ftg_cmp_print_deviations_long_3d, &
    ftg_cmp_print_deviations_long_4d, &
    ftg_cmp_print_deviations_float_0d, &
    ftg_cmp_print_deviations_float_1d, &
    ftg_cmp_print_deviations_float_2d, &
    ftg_cmp_print_deviations_float_3d, &
    ftg_cmp_print_deviations_float_4d, &
    ftg_cmp_print_deviations_double_0d, &
    ftg_cmp_print_deviations_double_1d, &
    ftg_cmp_print_deviations_double_2d, &
    ftg_cmp_print_deviations_double_3d, &
    ftg_cmp_print_deviations_double_4d
END INTERFACE ftg_cmp_print_deviations

INTERFACE ftg_compare
  MODULE PROCEDURE &
    ftg_compare_logical_0d, &
    ftg_compare_logical_1d, &
    ftg_compare_logical_2d, &
    ftg_compare_logical_3d, &
    ftg_compare_logical_4d, &
    ftg_compare_bool_0d, &
    ftg_compare_bool_1d, &
    ftg_compare_bool_2d, &
    ftg_compare_bool_3d, &
    ftg_compare_bool_4d, &
    ftg_compare_int_0d, &
    ftg_compare_int_1d, &
    ftg_compare_int_2d, &
    ftg_compare_int_3d, &
    ftg_compare_int_4d, &
    ftg_compare_long_0d, &
    ftg_compare_long_1d, &
    ftg_compare_long_2d, &
    ftg_compare_long_3d, &
    ftg_compare_long_4d, &
    ftg_compare_float_0d, &
    ftg_compare_float_1d, &
    ftg_compare_float_2d, &
    ftg_compare_float_3d, &
    ftg_compare_float_4d, &
    ftg_compare_double_0d, &
    ftg_compare_double_1d, &
    ftg_compare_double_2d, &
    ftg_compare_double_3d, &
    ftg_compare_double_4d
END INTERFACE ftg_compare

REAL              :: ftg_cmp_default_tolerance = 0.0
INTEGER           :: ftg_cmp_max_print_deviations = 10
CHARACTER(len=64) :: ftg_cmp_message_prefix = 'FTG Compare ***'

CONTAINS

!=============================================================================
!=============================================================================

FUNCTION ftg_cmp_size(fieldname, actual_shape, fieldname_print)

  CHARACTER(LEN=*), INTENT(IN) :: fieldname, fieldname_print
  INTEGER, INTENT(IN)          :: actual_shape(:)
  INTEGER                      :: rank, expected_shape(4), i
  LOGICAL                      :: ftg_cmp_size
  
  rank = SIZE(actual_shape)
  expected_shape = ftg_get_size(fieldname)
  ftg_cmp_size =  ALL(actual_shape == expected_shape(:rank))
  IF (.NOT. ftg_cmp_size) THEN
    WRITE (*,'(A,A,A,A)',advance="no") TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Size doesn't match, expected: ("
    DO i = 1, rank
      IF (i > 1) THEN
        WRITE(*,'(A)',advance="no") ', '
      END IF
      WRITE (*,'(I0)',advance="no") expected_shape(i)
    END DO
    WRITE (*,'(A)',advance="no") "), actual: ("
    DO i = 1, rank
      IF (i > 1) THEN
        WRITE(*,'(A)',advance="no") ', '
      END IF
      WRITE (*,'(I0)',advance="no") actual_shape(i)
    END DO
    WRITE (*,'(A)') ")"
  END IF

END FUNCTION ftg_cmp_size

!=============================================================================
!TODO UBOUND und LBOUND uebergeben und Indizes justieren
!=============================================================================

SUBROUTINE ftg_cmp_print_deviations_logical_0d(expected, actual, fieldname_print)
  LOGICAL, INTENT(IN)          :: expected, actual
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  
  IF (expected .NEQV. actual) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(L0)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_logical_0d

SUBROUTINE ftg_cmp_print_deviations_bool_0d(expected, actual, fieldname_print)
  LOGICAL(KIND=C_BOOL), INTENT(IN) :: expected, actual
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  
  IF (expected .NEQV. actual) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(L0)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_bool_0d

SUBROUTINE ftg_cmp_print_deviations_int_0d(expected, actual, fieldname_print)
  INTEGER, INTENT(IN)          :: expected, actual
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  
  IF (expected /= actual) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(I0)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_int_0d

SUBROUTINE ftg_cmp_print_deviations_long_0d(expected, actual, fieldname_print)
  INTEGER(KIND=C_LONG), INTENT(IN) :: expected, actual
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  
  IF (expected /= actual) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(I19)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_long_0d

SUBROUTINE ftg_cmp_print_deviations_float_0d(expected, actual, fieldname_print)
  REAL(KIND=C_FLOAT), INTENT(IN) :: expected, actual
  CHARACTER(LEN=*), INTENT(IN)   :: fieldname_print
  
  IF (expected /= actual) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(F0.14)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_float_0d

SUBROUTINE ftg_cmp_print_deviations_double_0d(expected, actual, fieldname_print)
  REAL(KIND=C_DOUBLE), INTENT(IN) :: expected, actual
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname_print
  
  IF (expected /= actual) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(F0.14)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_double_0d

SUBROUTINE ftg_cmp_print_deviations_logical_1d(expected, actual, fieldname_print)
  LOGICAL, INTENT(IN)          :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  LOGICAL, ALLOCATABLE         :: mask(:)
  INTEGER                      :: i, counter
  
  mask = expected .NEQV. actual
  counter = 1
  
  outer: DO i = 1, SIZE(mask, 1)
          IF (mask(i)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i
            WRITE (*,'(A)',advance="no") "), expected: "
            WRITE (*,'(L0)',advance="no") expected(i)
            WRITE (*,'(A)',advance="no") ", actual: "
            WRITE (*,'(L0)') actual(i)
            counter = counter + 1
            IF (counter > ftg_cmp_max_print_deviations) THEN
              EXIT outer
            END IF
          END IF
  END DO outer
  
END SUBROUTINE ftg_cmp_print_deviations_logical_1d

SUBROUTINE ftg_cmp_print_deviations_logical_2d(expected, actual, fieldname_print)
  LOGICAL, INTENT(IN)          :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  LOGICAL, ALLOCATABLE         :: mask(:,:)
  INTEGER                      :: i, j, counter
  
  mask = expected .NEQV. actual
  counter = 1
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
          IF (mask(i, j)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j
            WRITE (*,'(A)',advance="no") "), expected: "
            WRITE (*,'(L0)',advance="no") expected(i, j)
            WRITE (*,'(A)',advance="no") ", actual: "
            WRITE (*,'(L0)') actual(i, j)
            counter = counter + 1
            IF (counter > ftg_cmp_max_print_deviations) THEN
              EXIT outer
            END IF
          END IF
      END DO
  END DO outer
  
END SUBROUTINE ftg_cmp_print_deviations_logical_2d

SUBROUTINE ftg_cmp_print_deviations_logical_3d(expected, actual, fieldname_print)
  LOGICAL, INTENT(IN)          :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  LOGICAL, ALLOCATABLE         :: mask(:,:,:)
  INTEGER                      :: i, j, k, counter
  
  mask = expected .NEQV. actual
  counter = 1
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
      DO k = 1, SIZE(mask, 3)
          IF (mask(i, j, k)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") k
            WRITE (*,'(A)',advance="no") "), expected: "
            WRITE (*,'(L0)',advance="no") expected(i, j, k)
            WRITE (*,'(A)',advance="no") ", actual: "
            WRITE (*,'(L0)') actual(i, j, k)
            counter = counter + 1
            IF (counter > ftg_cmp_max_print_deviations) THEN
              EXIT outer
            END IF
          END IF
          END DO
      END DO
  END DO outer
  
END SUBROUTINE ftg_cmp_print_deviations_logical_3d

SUBROUTINE ftg_cmp_print_deviations_logical_4d(expected, actual, fieldname_print)
  LOGICAL, INTENT(IN)          :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  LOGICAL, ALLOCATABLE         :: mask(:,:,:,:)
  INTEGER                      :: i, j, k, l, counter
  
  mask = expected .NEQV. actual
  counter = 1
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
      DO k = 1, SIZE(mask, 3)
        DO l = 1, SIZE(mask, 4)
          IF (mask(i, j, k, l)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") k
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") l
            WRITE (*,'(A)',advance="no") "), expected: "
            WRITE (*,'(L0)',advance="no") expected(i, j, k, l)
            WRITE (*,'(A)',advance="no") ", actual: "
            WRITE (*,'(L0)') actual(i, j, k, l)
            counter = counter + 1
            IF (counter > ftg_cmp_max_print_deviations) THEN
              EXIT outer
            END IF
          END IF
        END DO
          END DO
      END DO
  END DO outer
  
END SUBROUTINE ftg_cmp_print_deviations_logical_4d

SUBROUTINE ftg_cmp_print_deviations_bool_1d(expected, actual, fieldname_print)
  LOGICAL(KIND=C_BOOL), INTENT(IN) :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  LOGICAL, ALLOCATABLE             :: mask(:)
  INTEGER                          :: i, counter
  
  mask = expected .NEQV. actual
  counter = 1
  
  outer: DO i = 1, SIZE(mask, 1)
          IF (mask(i)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i
            WRITE (*,'(A)',advance="no") "), expected: "
            WRITE (*,'(L0)',advance="no") expected(i)
            WRITE (*,'(A)',advance="no") ", actual: "
            WRITE (*,'(L0)') actual(i)
            counter = counter + 1
            IF (counter > ftg_cmp_max_print_deviations) THEN
              EXIT outer
            END IF
          END IF
  END DO outer
  
END SUBROUTINE ftg_cmp_print_deviations_bool_1d

SUBROUTINE ftg_cmp_print_deviations_bool_2d(expected, actual, fieldname_print)
  LOGICAL(KIND=C_BOOL), INTENT(IN) :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  LOGICAL, ALLOCATABLE             :: mask(:,:)
  INTEGER                          :: i, j, counter
  
  mask = expected .NEQV. actual
  counter = 1
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
          IF (mask(i, j)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j
            WRITE (*,'(A)',advance="no") "), expected: "
            WRITE (*,'(L0)',advance="no") expected(i, j)
            WRITE (*,'(A)',advance="no") ", actual: "
            WRITE (*,'(L0)') actual(i, j)
            counter = counter + 1
            IF (counter > ftg_cmp_max_print_deviations) THEN
              EXIT outer
            END IF
          END IF
      END DO
  END DO outer
  
END SUBROUTINE ftg_cmp_print_deviations_bool_2d

SUBROUTINE ftg_cmp_print_deviations_bool_3d(expected, actual, fieldname_print)
  LOGICAL(KIND=C_BOOL), INTENT(IN) :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  LOGICAL, ALLOCATABLE             :: mask(:,:,:)
  INTEGER                          :: i, j, k, counter
  
  mask = expected .NEQV. actual
  counter = 1
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
      DO k = 1, SIZE(mask, 3)
          IF (mask(i, j, k)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") k
            WRITE (*,'(A)',advance="no") "), expected: "
            WRITE (*,'(L0)',advance="no") expected(i, j, k)
            WRITE (*,'(A)',advance="no") ", actual: "
            WRITE (*,'(L0)') actual(i, j, k)
            counter = counter + 1
            IF (counter > ftg_cmp_max_print_deviations) THEN
              EXIT outer
            END IF
          END IF
          END DO
      END DO
  END DO outer
  
END SUBROUTINE ftg_cmp_print_deviations_bool_3d

SUBROUTINE ftg_cmp_print_deviations_bool_4d(expected, actual, fieldname_print)
  LOGICAL(KIND=C_BOOL), INTENT(IN) :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  LOGICAL, ALLOCATABLE             :: mask(:,:,:,:)
  INTEGER                          :: i, j, k, l, counter
  
  mask = expected .NEQV. actual
  counter = 1
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
      DO k = 1, SIZE(mask, 3)
        DO l = 1, SIZE(mask, 4)
          IF (mask(i, j, k, l)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") k
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") l
            WRITE (*,'(A)',advance="no") "), expected: "
            WRITE (*,'(L0)',advance="no") expected(i, j, k, l)
            WRITE (*,'(A)',advance="no") ", actual: "
            WRITE (*,'(L0)') actual(i, j, k, l)
            counter = counter + 1
            IF (counter > ftg_cmp_max_print_deviations) THEN
              EXIT outer
            END IF
          END IF
        END DO
          END DO
      END DO
  END DO outer
  
END SUBROUTINE ftg_cmp_print_deviations_bool_4d

SUBROUTINE ftg_cmp_print_deviations_int_1d(expected, actual, fieldname_print)
  INTEGER, INTENT(IN)          :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  LOGICAL, ALLOCATABLE         :: mask(:)
  INTEGER, ALLOCATABLE         :: deltas(:)
  INTEGER                      :: indices(1), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(I0)',advance="no") expected(indices(1))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(I0)') actual(indices(1))
      mask(indices(1)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_int_1d

SUBROUTINE ftg_cmp_print_deviations_int_2d(expected, actual, fieldname_print)
  INTEGER, INTENT(IN)          :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  LOGICAL, ALLOCATABLE         :: mask(:,:)
  INTEGER, ALLOCATABLE         :: deltas(:,:)
  INTEGER                      :: indices(2), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(I0)',advance="no") expected(indices(1), indices(2))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(I0)') actual(indices(1), indices(2))
      mask(indices(1), indices(2)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_int_2d

SUBROUTINE ftg_cmp_print_deviations_int_3d(expected, actual, fieldname_print)
  INTEGER, INTENT(IN)          :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  LOGICAL, ALLOCATABLE         :: mask(:,:,:)
  INTEGER, ALLOCATABLE         :: deltas(:,:,:)
  INTEGER                      :: indices(3), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(I0)',advance="no") expected(indices(1), indices(2), indices(3))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(I0)') actual(indices(1), indices(2), indices(3))
      mask(indices(1), indices(2), indices(3)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_int_3d

SUBROUTINE ftg_cmp_print_deviations_int_4d(expected, actual, fieldname_print)
  INTEGER, INTENT(IN)          :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  LOGICAL, ALLOCATABLE         :: mask(:,:,:,:)
  INTEGER, ALLOCATABLE         :: deltas(:,:,:,:)
  INTEGER                      :: indices(4), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(4)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(I0)',advance="no") expected(indices(1), indices(2), indices(3), indices(4))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(I0)') actual(indices(1), indices(2), indices(3), indices(4))
      mask(indices(1), indices(2), indices(3), indices(4)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_int_4d

SUBROUTINE ftg_cmp_print_deviations_long_1d(expected, actual, fieldname_print)
  INTEGER(KIND=C_LONG), INTENT(IN)  :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname_print
  LOGICAL, ALLOCATABLE              :: mask(:)
  INTEGER(KIND=C_LONG), ALLOCATABLE :: deltas(:)
  INTEGER                           :: indices(1), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(I19)',advance="no") expected(indices(1))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(I19)') actual(indices(1))
      mask(indices(1)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_long_1d

SUBROUTINE ftg_cmp_print_deviations_long_2d(expected, actual, fieldname_print)
  INTEGER(KIND=C_LONG), INTENT(IN)  :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname_print
  LOGICAL, ALLOCATABLE              :: mask(:,:)
  INTEGER(KIND=C_LONG), ALLOCATABLE :: deltas(:,:)
  INTEGER                           :: indices(2), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(I19)',advance="no") expected(indices(1), indices(2))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(I19)') actual(indices(1), indices(2))
      mask(indices(1), indices(2)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_long_2d

SUBROUTINE ftg_cmp_print_deviations_long_3d(expected, actual, fieldname_print)
  INTEGER(KIND=C_LONG), INTENT(IN)  :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname_print
  LOGICAL, ALLOCATABLE              :: mask(:,:,:)
  INTEGER(KIND=C_LONG), ALLOCATABLE :: deltas(:,:,:)
  INTEGER                           :: indices(3), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(I19)',advance="no") expected(indices(1), indices(2), indices(3))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(I19)') actual(indices(1), indices(2), indices(3))
      mask(indices(1), indices(2), indices(3)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_long_3d

SUBROUTINE ftg_cmp_print_deviations_long_4d(expected, actual, fieldname_print)
  INTEGER(KIND=C_LONG), INTENT(IN)  :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname_print
  LOGICAL, ALLOCATABLE              :: mask(:,:,:,:)
  INTEGER(KIND=C_LONG), ALLOCATABLE :: deltas(:,:,:,:)
  INTEGER                           :: indices(4), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(4)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(I19)',advance="no") expected(indices(1), indices(2), indices(3), indices(4))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(I19)') actual(indices(1), indices(2), indices(3), indices(4))
      mask(indices(1), indices(2), indices(3), indices(4)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_long_4d

SUBROUTINE ftg_cmp_print_deviations_float_1d(expected, actual, fieldname_print)
  REAL(KIND=C_FLOAT), INTENT(IN)  :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname_print
  LOGICAL, ALLOCATABLE            :: mask(:)
  REAL(KIND=C_FLOAT), ALLOCATABLE :: deltas(:)
  INTEGER                         :: indices(1), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(F0.14)',advance="no") expected(indices(1))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(F0.14)') actual(indices(1))
      mask(indices(1)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_float_1d

SUBROUTINE ftg_cmp_print_deviations_float_2d(expected, actual, fieldname_print)
  REAL(KIND=C_FLOAT), INTENT(IN)  :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname_print
  LOGICAL, ALLOCATABLE            :: mask(:,:)
  REAL(KIND=C_FLOAT), ALLOCATABLE :: deltas(:,:)
  INTEGER                         :: indices(2), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(F0.14)',advance="no") expected(indices(1), indices(2))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(F0.14)') actual(indices(1), indices(2))
      mask(indices(1), indices(2)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_float_2d

SUBROUTINE ftg_cmp_print_deviations_float_3d(expected, actual, fieldname_print)
  REAL(KIND=C_FLOAT), INTENT(IN)  :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname_print
  LOGICAL, ALLOCATABLE            :: mask(:,:,:)
  REAL(KIND=C_FLOAT), ALLOCATABLE :: deltas(:,:,:)
  INTEGER                         :: indices(3), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(F0.14)',advance="no") expected(indices(1), indices(2), indices(3))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(F0.14)') actual(indices(1), indices(2), indices(3))
      mask(indices(1), indices(2), indices(3)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_float_3d

SUBROUTINE ftg_cmp_print_deviations_float_4d(expected, actual, fieldname_print)
  REAL(KIND=C_FLOAT), INTENT(IN)  :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname_print
  LOGICAL, ALLOCATABLE            :: mask(:,:,:,:)
  REAL(KIND=C_FLOAT), ALLOCATABLE :: deltas(:,:,:,:)
  INTEGER                         :: indices(4), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(4)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(F0.14)',advance="no") expected(indices(1), indices(2), indices(3), indices(4))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(F0.14)') actual(indices(1), indices(2), indices(3), indices(4))
      mask(indices(1), indices(2), indices(3), indices(4)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_float_4d

SUBROUTINE ftg_cmp_print_deviations_double_1d(expected, actual, fieldname_print)
  REAL(KIND=C_DOUBLE), INTENT(IN)  :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  LOGICAL, ALLOCATABLE             :: mask(:)
  REAL(KIND=C_DOUBLE), ALLOCATABLE :: deltas(:)
  INTEGER                          :: indices(1), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(F0.14)',advance="no") expected(indices(1))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(F0.14)') actual(indices(1))
      mask(indices(1)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_double_1d

SUBROUTINE ftg_cmp_print_deviations_double_2d(expected, actual, fieldname_print)
  REAL(KIND=C_DOUBLE), INTENT(IN)  :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  LOGICAL, ALLOCATABLE             :: mask(:,:)
  REAL(KIND=C_DOUBLE), ALLOCATABLE :: deltas(:,:)
  INTEGER                          :: indices(2), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(F0.14)',advance="no") expected(indices(1), indices(2))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(F0.14)') actual(indices(1), indices(2))
      mask(indices(1), indices(2)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_double_2d

SUBROUTINE ftg_cmp_print_deviations_double_3d(expected, actual, fieldname_print)
  REAL(KIND=C_DOUBLE), INTENT(IN)  :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  LOGICAL, ALLOCATABLE             :: mask(:,:,:)
  REAL(KIND=C_DOUBLE), ALLOCATABLE :: deltas(:,:,:)
  INTEGER                          :: indices(3), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(F0.14)',advance="no") expected(indices(1), indices(2), indices(3))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(F0.14)') actual(indices(1), indices(2), indices(3))
      mask(indices(1), indices(2), indices(3)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_double_3d

SUBROUTINE ftg_cmp_print_deviations_double_4d(expected, actual, fieldname_print)
  REAL(KIND=C_DOUBLE), INTENT(IN)  :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  LOGICAL, ALLOCATABLE             :: mask(:,:,:,:)
  REAL(KIND=C_DOUBLE), ALLOCATABLE :: deltas(:,:,:,:)
  INTEGER                          :: indices(4), i, j
  
  mask = expected /= actual
  deltas = expected - actual
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3)
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(4)
      WRITE (*,'(A)',advance="no") "), expected: "
      WRITE (*,'(F0.14)',advance="no") expected(indices(1), indices(2), indices(3), indices(4))
      WRITE (*,'(A)',advance="no") ", actual: "
      WRITE (*,'(F0.14)') actual(indices(1), indices(2), indices(3), indices(4))
      mask(indices(1), indices(2), indices(3), indices(4)) = .FALSE.
    ELSE
      EXIT
    END IF    
  END DO
  
END SUBROUTINE ftg_cmp_print_deviations_double_4d

!=============================================================================
!TODO Ausgabe fuer Skalare
!TODO NaN beruecksichtigen
!=============================================================================

SUBROUTINE ftg_compare_logical_0d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL, INTENT(IN)                    :: field
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL, ALLOCATABLE                   :: stored_field
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
  IF (field .NEQV. stored_field) THEN
    result = .FALSE.
    WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
    IF (ftg_cmp_max_print_deviations > 0) THEN
      CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
    END IF 
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_logical_0d

SUBROUTINE ftg_compare_logical_1d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL, INTENT(IN)                    :: field(:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL, ALLOCATABLE                   :: stored_field(:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field .NEQV. stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_logical_1d

SUBROUTINE ftg_compare_logical_2d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL, INTENT(IN)                    :: field(:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL, ALLOCATABLE                   :: stored_field(:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field .NEQV. stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_logical_2d

SUBROUTINE ftg_compare_logical_3d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL, INTENT(IN)                    :: field(:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL, ALLOCATABLE                   :: stored_field(:,:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field .NEQV. stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_logical_3d

SUBROUTINE ftg_compare_logical_4d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL, INTENT(IN)                    :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL, ALLOCATABLE                   :: stored_field(:,:,:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field .NEQV. stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_logical_4d

SUBROUTINE ftg_compare_bool_0d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN)       :: field
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL(KIND=C_BOOL), ALLOCATABLE      :: stored_field
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
  IF (field .NEQV. stored_field) THEN
    result = .FALSE.
    WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
    IF (ftg_cmp_max_print_deviations > 0) THEN
      CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
    END IF 
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_bool_0d

SUBROUTINE ftg_compare_bool_1d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN)       :: field(:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL(KIND=C_BOOL), ALLOCATABLE      :: stored_field(:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field .NEQV. stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_bool_1d

SUBROUTINE ftg_compare_bool_2d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN)       :: field(:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL(KIND=C_BOOL), ALLOCATABLE      :: stored_field(:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field .NEQV. stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_bool_2d

SUBROUTINE ftg_compare_bool_3d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN)       :: field(:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL(KIND=C_BOOL), ALLOCATABLE      :: stored_field(:,:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field .NEQV. stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_bool_3d

SUBROUTINE ftg_compare_bool_4d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN)       :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL(KIND=C_BOOL), ALLOCATABLE      :: stored_field(:,:,:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field .NEQV. stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_bool_4d

SUBROUTINE ftg_compare_int_0d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER, INTENT(IN)                    :: field
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER, ALLOCATABLE                   :: stored_field
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
  IF (field /= stored_field) THEN
    result = .FALSE.
    WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
    IF (ftg_cmp_max_print_deviations > 0) THEN
      CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
    END IF 
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_int_0d

SUBROUTINE ftg_compare_int_1d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER, INTENT(IN)                    :: field(:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER, ALLOCATABLE                   :: stored_field(:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field /= stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_int_1d

SUBROUTINE ftg_compare_int_2d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER, INTENT(IN)                    :: field(:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER, ALLOCATABLE                   :: stored_field(:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field /= stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_int_2d

SUBROUTINE ftg_compare_int_3d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER, INTENT(IN)                    :: field(:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER, ALLOCATABLE                   :: stored_field(:,:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field /= stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_int_3d

SUBROUTINE ftg_compare_int_4d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER, INTENT(IN)                    :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER, ALLOCATABLE                   :: stored_field(:,:,:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field /= stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_int_4d

SUBROUTINE ftg_compare_long_0d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN)       :: field
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER(KIND=C_LONG), ALLOCATABLE      :: stored_field
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
  IF (field /= stored_field) THEN
    result = .FALSE.
    WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
    IF (ftg_cmp_max_print_deviations > 0) THEN
      CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
    END IF 
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_long_0d

SUBROUTINE ftg_compare_long_1d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN)       :: field(:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER(KIND=C_LONG), ALLOCATABLE      :: stored_field(:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field /= stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_long_1d

SUBROUTINE ftg_compare_long_2d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN)       :: field(:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER(KIND=C_LONG), ALLOCATABLE      :: stored_field(:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field /= stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_long_2d

SUBROUTINE ftg_compare_long_3d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN)       :: field(:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER(KIND=C_LONG), ALLOCATABLE      :: stored_field(:,:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field /= stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_long_3d

SUBROUTINE ftg_compare_long_4d(fieldname, field, result, result_acc, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN)       :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  LOGICAL, INTENT(INOUT), OPTIONAL       :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER(KIND=C_LONG), ALLOCATABLE      :: stored_field(:,:,:,:)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(field /= stored_field)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_long_4d

SUBROUTINE ftg_compare_float_0d(fieldname, field, result, result_acc, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN)           :: field
  LOGICAL, INTENT(OUT)                     :: result
  LOGICAL, INTENT(INOUT), OPTIONAL         :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL   :: fieldname_alias
  CHARACTER(LEN=256)                       :: fieldname_print
  REAL(KIND=C_FLOAT), ALLOCATABLE          :: stored_field
  REAL(KIND=C_FLOAT), INTENT(in), OPTIONAL :: tolerance
  REAL(KIND=C_FLOAT)                       :: t
  
  IF (PRESENT(tolerance)) THEN
    t = tolerance
  ELSE
    t = ftg_cmp_default_tolerance
  END IF
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
  IF (ABS(field - stored_field) > t) THEN
    result = .FALSE.
    WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
    IF (ftg_cmp_max_print_deviations > 0) THEN
      CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
    END IF 
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_float_0d

SUBROUTINE ftg_compare_float_1d(fieldname, field, result, result_acc, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN)           :: field(:)
  LOGICAL, INTENT(OUT)                     :: result
  LOGICAL, INTENT(INOUT), OPTIONAL         :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL   :: fieldname_alias
  CHARACTER(LEN=256)                       :: fieldname_print
  REAL(KIND=C_FLOAT), ALLOCATABLE          :: stored_field(:)
  REAL(KIND=C_FLOAT), INTENT(in), OPTIONAL :: tolerance
  REAL(KIND=C_FLOAT)                       :: t
  
  IF (PRESENT(tolerance)) THEN
    t = tolerance
  ELSE
    t = ftg_cmp_default_tolerance
  END IF
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(ABS(field - stored_field) > t)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_float_1d

SUBROUTINE ftg_compare_float_2d(fieldname, field, result, result_acc, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN)           :: field(:,:)
  LOGICAL, INTENT(OUT)                     :: result
  LOGICAL, INTENT(INOUT), OPTIONAL         :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL   :: fieldname_alias
  CHARACTER(LEN=256)                       :: fieldname_print
  REAL(KIND=C_FLOAT), ALLOCATABLE          :: stored_field(:,:)
  REAL(KIND=C_FLOAT), INTENT(in), OPTIONAL :: tolerance
  REAL(KIND=C_FLOAT)                       :: t
  
  IF (PRESENT(tolerance)) THEN
    t = tolerance
  ELSE
    t = ftg_cmp_default_tolerance
  END IF
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(ABS(field - stored_field) > t)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_float_2d

SUBROUTINE ftg_compare_float_3d(fieldname, field, result, result_acc, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN)           :: field(:,:,:)
  LOGICAL, INTENT(OUT)                     :: result
  LOGICAL, INTENT(INOUT), OPTIONAL         :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL   :: fieldname_alias
  CHARACTER(LEN=256)                       :: fieldname_print
  REAL(KIND=C_FLOAT), ALLOCATABLE          :: stored_field(:,:,:)
  REAL(KIND=C_FLOAT), INTENT(in), OPTIONAL :: tolerance
  REAL(KIND=C_FLOAT)                       :: t
  
  IF (PRESENT(tolerance)) THEN
    t = tolerance
  ELSE
    t = ftg_cmp_default_tolerance
  END IF
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(ABS(field - stored_field) > t)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_float_3d

SUBROUTINE ftg_compare_float_4d(fieldname, field, result, result_acc, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN)           :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                     :: result
  LOGICAL, INTENT(INOUT), OPTIONAL         :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL   :: fieldname_alias
  CHARACTER(LEN=256)                       :: fieldname_print
  REAL(KIND=C_FLOAT), ALLOCATABLE          :: stored_field(:,:,:,:)
  REAL(KIND=C_FLOAT), INTENT(in), OPTIONAL :: tolerance
  REAL(KIND=C_FLOAT)                       :: t
  
  IF (PRESENT(tolerance)) THEN
    t = tolerance
  ELSE
    t = ftg_cmp_default_tolerance
  END IF
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(ABS(field - stored_field) > t)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_float_4d

SUBROUTINE ftg_compare_double_0d(fieldname, field, result, result_acc, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN)           :: field
  LOGICAL, INTENT(OUT)                      :: result
  LOGICAL, INTENT(INOUT), OPTIONAL          :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL    :: fieldname_alias
  CHARACTER(LEN=256)                        :: fieldname_print
  REAL(KIND=C_DOUBLE), ALLOCATABLE          :: stored_field
  REAL(KIND=C_DOUBLE), INTENT(in), OPTIONAL :: tolerance
  REAL(KIND=C_DOUBLE)                       :: t
  
  IF (PRESENT(tolerance)) THEN
    t = tolerance
  ELSE
    t = ftg_cmp_default_tolerance
  END IF
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
  IF (ABS(field - stored_field) > t) THEN
    result = .FALSE.
    WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
    IF (ftg_cmp_max_print_deviations > 0) THEN
      CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
    END IF 
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_double_0d

SUBROUTINE ftg_compare_double_1d(fieldname, field, result, result_acc, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN)           :: field(:)
  LOGICAL, INTENT(OUT)                      :: result
  LOGICAL, INTENT(INOUT), OPTIONAL          :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL    :: fieldname_alias
  CHARACTER(LEN=256)                        :: fieldname_print
  REAL(KIND=C_DOUBLE), ALLOCATABLE          :: stored_field(:)
  REAL(KIND=C_DOUBLE), INTENT(in), OPTIONAL :: tolerance
  REAL(KIND=C_DOUBLE)                       :: t
  
  IF (PRESENT(tolerance)) THEN
    t = tolerance
  ELSE
    t = ftg_cmp_default_tolerance
  END IF
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(ABS(field - stored_field) > t)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_double_1d

SUBROUTINE ftg_compare_double_2d(fieldname, field, result, result_acc, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN)           :: field(:,:)
  LOGICAL, INTENT(OUT)                      :: result
  LOGICAL, INTENT(INOUT), OPTIONAL          :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL    :: fieldname_alias
  CHARACTER(LEN=256)                        :: fieldname_print
  REAL(KIND=C_DOUBLE), ALLOCATABLE          :: stored_field(:,:)
  REAL(KIND=C_DOUBLE), INTENT(in), OPTIONAL :: tolerance
  REAL(KIND=C_DOUBLE)                       :: t
  
  IF (PRESENT(tolerance)) THEN
    t = tolerance
  ELSE
    t = ftg_cmp_default_tolerance
  END IF
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(ABS(field - stored_field) > t)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_double_2d

SUBROUTINE ftg_compare_double_3d(fieldname, field, result, result_acc, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN)           :: field(:,:,:)
  LOGICAL, INTENT(OUT)                      :: result
  LOGICAL, INTENT(INOUT), OPTIONAL          :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL    :: fieldname_alias
  CHARACTER(LEN=256)                        :: fieldname_print
  REAL(KIND=C_DOUBLE), ALLOCATABLE          :: stored_field(:,:,:)
  REAL(KIND=C_DOUBLE), INTENT(in), OPTIONAL :: tolerance
  REAL(KIND=C_DOUBLE)                       :: t
  
  IF (PRESENT(tolerance)) THEN
    t = tolerance
  ELSE
    t = ftg_cmp_default_tolerance
  END IF
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(ABS(field - stored_field) > t)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_double_3d

SUBROUTINE ftg_compare_double_4d(fieldname, field, result, result_acc, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN)           :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                      :: result
  LOGICAL, INTENT(INOUT), OPTIONAL          :: result_acc
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL    :: fieldname_alias
  CHARACTER(LEN=256)                        :: fieldname_print
  REAL(KIND=C_DOUBLE), ALLOCATABLE          :: stored_field(:,:,:,:)
  REAL(KIND=C_DOUBLE), INTENT(in), OPTIONAL :: tolerance
  REAL(KIND=C_DOUBLE)                       :: t
  
  IF (PRESENT(tolerance)) THEN
    t = tolerance
  ELSE
    t = ftg_cmp_default_tolerance
  END IF
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.

  IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
    result = .FALSE.
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (ANY(ABS(field - stored_field) > t)) THEN
      result = .FALSE.
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
      IF (ftg_cmp_max_print_deviations > 0) THEN
        CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
      END IF 
    END IF
  END IF
  
  IF (PRESENT(result_acc)) THEN
    result_acc = result_acc .AND. result
  END IF
    
END SUBROUTINE ftg_compare_double_4d

END MODULE m_ser_ftg_cmp
