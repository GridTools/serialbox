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

PUBLIC :: ftg_cmp_default_tolerance, ftg_cmp_quiet, ftg_cmp_max_print_deviations, ftg_cmp_print_when_equal, &
          ftg_cmp_count_different_bounds_as_failure, ftg_cmp_count_missing_field_as_failure, ftg_cmp_message_prefix, ftg_compare

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
LOGICAL           :: ftg_cmp_quiet = .FALSE.
INTEGER           :: ftg_cmp_max_print_deviations = 10
LOGICAL           :: ftg_cmp_print_when_equal = .FALSE.
LOGICAL           :: ftg_cmp_count_different_bounds_as_failure = .FALSE.
LOGICAL           :: ftg_cmp_count_missing_field_as_failure = .TRUE.
CHARACTER(len=64) :: ftg_cmp_message_prefix = 'FTG Compare ***'

CONTAINS

!=============================================================================
!=============================================================================

FUNCTION ftg_cmp_size(fieldname, actual_shape, fieldname_print)

  CHARACTER(LEN=*), INTENT(IN) :: fieldname, fieldname_print
  INTEGER, INTENT(IN)          :: actual_shape(:)
  INTEGER                      :: rank, expected_shape(4), r
  LOGICAL                      :: ftg_cmp_size
  
  rank = SIZE(actual_shape)
  expected_shape = ftg_get_size(fieldname)
  ftg_cmp_size = ALL(actual_shape == expected_shape(:rank))
  
  IF (.NOT. ftg_cmp_size .AND. .NOT. ftg_cmp_quiet) THEN
    WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Size doesn't match"
    WRITE (*,'(A)',advance="no") "  -> expected: ("
    DO r = 1, rank
      IF (r > 1) THEN
        WRITE(*,'(A)',advance="no") ', '
      END IF
      WRITE (*,'(I0)',advance="no") expected_shape(r)
    END DO
    WRITE (*,'(A)',advance="no") "), actual: ("
    DO r = 1, rank
      IF (r > 1) THEN
        WRITE(*,'(A)',advance="no") ', '
      END IF
      WRITE (*,'(I0)',advance="no") actual_shape(r)
    END DO
    WRITE (*,'(A)') ")"
  END IF

END FUNCTION ftg_cmp_size

FUNCTION ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print)

  CHARACTER(LEN=*), INTENT(IN) :: fieldname, fieldname_print
  INTEGER, INTENT(IN)          :: lbounds(:), ubounds(:)
  INTEGER                      :: rank, expected_bounds(2, 4), r
  LOGICAL                      :: ftg_cmp_bounds
  
  rank = SIZE(lbounds)
  expected_bounds = RESHAPE(ftg_get_bounds(fieldname), (/2, 4/))
  ftg_cmp_bounds = ALL(lbounds == expected_bounds(1,:rank)) .AND. ALL(ubounds == expected_bounds(2,:rank))
  
  IF (.NOT. ftg_cmp_bounds .AND. .NOT. ftg_cmp_quiet) THEN
    WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Bounds don't match"
    WRITE (*,'(A)',advance="no") "  -> expected: ("
    DO r = 1, rank
      IF (r > 1) THEN
        WRITE(*,'(A)',advance="no") ', '
      END IF
      WRITE (*,'(I0)',advance="no") expected_bounds(1,r)
      WRITE (*,'(A)',advance="no") ':'
      WRITE (*,'(I0)',advance="no") expected_bounds(2,r)
    END DO
    WRITE (*,'(A)',advance="no") "), actual: ("
    DO r = 1, rank
      IF (r > 1) THEN
        WRITE(*,'(A)',advance="no") ', '
      END IF
      WRITE (*,'(I0)',advance="no") lbounds(r)
      WRITE (*,'(A)',advance="no") ':'
      WRITE (*,'(I0)',advance="no") ubounds(r)
    END DO
    WRITE (*,'(A)') ")"
  END IF

END FUNCTION ftg_cmp_bounds

!=============================================================================
!=============================================================================

SUBROUTINE ftg_cmp_print_deviations_logical_0d(expected, actual, fieldname_print)
  LOGICAL, INTENT(IN)          :: expected, actual
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  
  IF (actual .NEQV. expected) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(L0)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_logical_0d

SUBROUTINE ftg_cmp_print_deviations_bool_0d(expected, actual, fieldname_print)
  LOGICAL(KIND=C_BOOL), INTENT(IN) :: expected, actual
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  
  IF (actual .NEQV. expected) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(L0)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_bool_0d

SUBROUTINE ftg_cmp_print_deviations_int_0d(expected, actual, fieldname_print)
  INTEGER, INTENT(IN)          :: expected, actual
  CHARACTER(LEN=*), INTENT(IN) :: fieldname_print
  
  IF (actual /= expected) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(I0)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_int_0d

SUBROUTINE ftg_cmp_print_deviations_long_0d(expected, actual, fieldname_print)
  INTEGER(KIND=C_LONG), INTENT(IN) :: expected, actual
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  
  IF (actual /= expected) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(I19)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_long_0d

SUBROUTINE ftg_cmp_print_deviations_float_0d(expected, actual, fieldname_print)
  REAL(KIND=C_FLOAT), INTENT(IN) :: expected, actual
  CHARACTER(LEN=*), INTENT(IN)   :: fieldname_print
  
  IF (.NOT. (actual /= actual .AND. expected /= expected) .AND. actual /= expected) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(F0.14)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_float_0d

SUBROUTINE ftg_cmp_print_deviations_double_0d(expected, actual, fieldname_print)
  REAL(KIND=C_DOUBLE), INTENT(IN) :: expected, actual
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname_print
  
  IF (.NOT. (actual /= actual .AND. expected /= expected) .AND. actual /= expected) THEN
    WRITE (*,'(A)',advance="no") "  -> expected: "
    WRITE (*,'(L0)',advance="no") expected
    WRITE (*,'(A)',advance="no") ", actual: "
    WRITE (*,'(F0.14)') actual
  END IF

END SUBROUTINE ftg_cmp_print_deviations_double_0d

SUBROUTINE ftg_cmp_print_deviations_logical_1d(expected, actual, fieldname_print, lbounds)
  LOGICAL, INTENT(IN)           :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL :: lbounds(1)
  LOGICAL, ALLOCATABLE          :: mask(:)
  INTEGER                       :: indexAdj(1), i, counter
  
  mask = expected .NEQV. actual
  counter = 1
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  outer: DO i = 1, SIZE(mask, 1)
          IF (mask(i)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i + indexAdj(1) - 1
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

SUBROUTINE ftg_cmp_print_deviations_logical_2d(expected, actual, fieldname_print, lbounds)
  LOGICAL, INTENT(IN)           :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL :: lbounds(2)
  LOGICAL, ALLOCATABLE          :: mask(:,:)
  INTEGER                       :: indexAdj(2), i, j, counter
  
  mask = expected .NEQV. actual
  counter = 1
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
          IF (mask(i, j)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i + indexAdj(1) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j + indexAdj(2) - 1
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

SUBROUTINE ftg_cmp_print_deviations_logical_3d(expected, actual, fieldname_print, lbounds)
  LOGICAL, INTENT(IN)           :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL :: lbounds(3)
  LOGICAL, ALLOCATABLE          :: mask(:,:,:)
  INTEGER                       :: indexAdj(3), i, j, k, counter
  
  mask = expected .NEQV. actual
  counter = 1
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
      DO k = 1, SIZE(mask, 3)
          IF (mask(i, j, k)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i + indexAdj(1) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j + indexAdj(2) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") k + indexAdj(3) - 1
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

SUBROUTINE ftg_cmp_print_deviations_logical_4d(expected, actual, fieldname_print, lbounds)
  LOGICAL, INTENT(IN)           :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL :: lbounds(4)
  LOGICAL, ALLOCATABLE          :: mask(:,:,:,:)
  INTEGER                       :: indexAdj(4), i, j, k, l, counter
  
  mask = expected .NEQV. actual
  counter = 1
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
      DO k = 1, SIZE(mask, 3)
        DO l = 1, SIZE(mask, 4)
          IF (mask(i, j, k, l)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i + indexAdj(1) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j + indexAdj(2) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") k + indexAdj(3) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") l + indexAdj(4) - 1
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

SUBROUTINE ftg_cmp_print_deviations_bool_1d(expected, actual, fieldname_print, lbounds)
  LOGICAL(KIND=C_BOOL), INTENT(IN) :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL    :: lbounds(1)
  LOGICAL, ALLOCATABLE             :: mask(:)
  INTEGER                          :: indexAdj(1), i, counter
  
  mask = expected .NEQV. actual
  counter = 1
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  outer: DO i = 1, SIZE(mask, 1)
          IF (mask(i)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i + indexAdj(1) - 1
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

SUBROUTINE ftg_cmp_print_deviations_bool_2d(expected, actual, fieldname_print, lbounds)
  LOGICAL(KIND=C_BOOL), INTENT(IN) :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL    :: lbounds(2)
  LOGICAL, ALLOCATABLE             :: mask(:,:)
  INTEGER                          :: indexAdj(2), i, j, counter
  
  mask = expected .NEQV. actual
  counter = 1
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
          IF (mask(i, j)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i + indexAdj(1) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j + indexAdj(2) - 1
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

SUBROUTINE ftg_cmp_print_deviations_bool_3d(expected, actual, fieldname_print, lbounds)
  LOGICAL(KIND=C_BOOL), INTENT(IN) :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL    :: lbounds(3)
  LOGICAL, ALLOCATABLE             :: mask(:,:,:)
  INTEGER                          :: indexAdj(3), i, j, k, counter
  
  mask = expected .NEQV. actual
  counter = 1
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
      DO k = 1, SIZE(mask, 3)
          IF (mask(i, j, k)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i + indexAdj(1) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j + indexAdj(2) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") k + indexAdj(3) - 1
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

SUBROUTINE ftg_cmp_print_deviations_bool_4d(expected, actual, fieldname_print, lbounds)
  LOGICAL(KIND=C_BOOL), INTENT(IN) :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL    :: lbounds(4)
  LOGICAL, ALLOCATABLE             :: mask(:,:,:,:)
  INTEGER                          :: indexAdj(4), i, j, k, l, counter
  
  mask = expected .NEQV. actual
  counter = 1
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  outer: DO i = 1, SIZE(mask, 1)
    DO j = 1, SIZE(mask, 2)
      DO k = 1, SIZE(mask, 3)
        DO l = 1, SIZE(mask, 4)
          IF (mask(i, j, k, l)) THEN
            WRITE (*,'(A)',advance="no") "  -> ("
            WRITE (*,'(I0)',advance="no") i + indexAdj(1) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") j + indexAdj(2) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") k + indexAdj(3) - 1
            WRITE (*,'(A)',advance="no") ", "
            WRITE (*,'(I0)',advance="no") l + indexAdj(4) - 1
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

SUBROUTINE ftg_cmp_print_deviations_int_1d(expected, actual, fieldname_print, lbounds)
  INTEGER, INTENT(IN)           :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL :: lbounds(1)
  LOGICAL, ALLOCATABLE          :: mask(:)
  INTEGER, ALLOCATABLE          :: deltas(:)
  INTEGER                       :: indices(1), indexAdj(1), i, j
  
  mask = actual /= expected
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
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

SUBROUTINE ftg_cmp_print_deviations_int_2d(expected, actual, fieldname_print, lbounds)
  INTEGER, INTENT(IN)           :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL :: lbounds(2)
  LOGICAL, ALLOCATABLE          :: mask(:,:)
  INTEGER, ALLOCATABLE          :: deltas(:,:)
  INTEGER                       :: indices(2), indexAdj(2), i, j
  
  mask = actual /= expected
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
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

SUBROUTINE ftg_cmp_print_deviations_int_3d(expected, actual, fieldname_print, lbounds)
  INTEGER, INTENT(IN)           :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL :: lbounds(3)
  LOGICAL, ALLOCATABLE          :: mask(:,:,:)
  INTEGER, ALLOCATABLE          :: deltas(:,:,:)
  INTEGER                       :: indices(3), indexAdj(3), i, j
  
  mask = actual /= expected
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3) + indexAdj(3) - 1
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

SUBROUTINE ftg_cmp_print_deviations_int_4d(expected, actual, fieldname_print, lbounds)
  INTEGER, INTENT(IN)           :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL :: lbounds(4)
  LOGICAL, ALLOCATABLE          :: mask(:,:,:,:)
  INTEGER, ALLOCATABLE          :: deltas(:,:,:,:)
  INTEGER                       :: indices(4), indexAdj(4), i, j
  
  mask = actual /= expected
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3) + indexAdj(3) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(4) + indexAdj(4) - 1
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

SUBROUTINE ftg_cmp_print_deviations_long_1d(expected, actual, fieldname_print, lbounds)
  INTEGER(KIND=C_LONG), INTENT(IN)  :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL     :: lbounds(1)
  LOGICAL, ALLOCATABLE              :: mask(:)
  INTEGER(KIND=C_LONG), ALLOCATABLE :: deltas(:)
  INTEGER                           :: indices(1), indexAdj(1), i, j
  
  mask = actual /= expected
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
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

SUBROUTINE ftg_cmp_print_deviations_long_2d(expected, actual, fieldname_print, lbounds)
  INTEGER(KIND=C_LONG), INTENT(IN)  :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL     :: lbounds(2)
  LOGICAL, ALLOCATABLE              :: mask(:,:)
  INTEGER(KIND=C_LONG), ALLOCATABLE :: deltas(:,:)
  INTEGER                           :: indices(2), indexAdj(2), i, j
  
  mask = actual /= expected
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
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

SUBROUTINE ftg_cmp_print_deviations_long_3d(expected, actual, fieldname_print, lbounds)
  INTEGER(KIND=C_LONG), INTENT(IN)  :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL     :: lbounds(3)
  LOGICAL, ALLOCATABLE              :: mask(:,:,:)
  INTEGER(KIND=C_LONG), ALLOCATABLE :: deltas(:,:,:)
  INTEGER                           :: indices(3), indexAdj(3), i, j
  
  mask = actual /= expected
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3) + indexAdj(3) - 1
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

SUBROUTINE ftg_cmp_print_deviations_long_4d(expected, actual, fieldname_print, lbounds)
  INTEGER(KIND=C_LONG), INTENT(IN)  :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL     :: lbounds(4)
  LOGICAL, ALLOCATABLE              :: mask(:,:,:,:)
  INTEGER(KIND=C_LONG), ALLOCATABLE :: deltas(:,:,:,:)
  INTEGER                           :: indices(4), indexAdj(4), i, j
  
  mask = actual /= expected
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3) + indexAdj(3) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(4) + indexAdj(4) - 1
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

SUBROUTINE ftg_cmp_print_deviations_float_1d(expected, actual, fieldname_print, lbounds, t)
  REAL(KIND=C_FLOAT), INTENT(IN)  :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL   :: lbounds(1)
  LOGICAL, ALLOCATABLE            :: mask(:)
  REAL(KIND=C_FLOAT), ALLOCATABLE :: deltas(:)
  INTEGER                         :: indices(1), indexAdj(1), i, j
  REAL, INTENT(in)                :: t
  
  
  mask = .NOT. (actual /= actual .AND. expected /= expected) .AND. ABS(actual - expected) > t
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
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

SUBROUTINE ftg_cmp_print_deviations_float_2d(expected, actual, fieldname_print, lbounds, t)
  REAL(KIND=C_FLOAT), INTENT(IN)  :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL   :: lbounds(2)
  LOGICAL, ALLOCATABLE            :: mask(:,:)
  REAL(KIND=C_FLOAT), ALLOCATABLE :: deltas(:,:)
  INTEGER                         :: indices(2), indexAdj(2), i, j
  REAL, INTENT(in)                :: t
  
  
  mask = .NOT. (actual /= actual .AND. expected /= expected) .AND. ABS(actual - expected) > t
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
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

SUBROUTINE ftg_cmp_print_deviations_float_3d(expected, actual, fieldname_print, lbounds, t)
  REAL(KIND=C_FLOAT), INTENT(IN)  :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL   :: lbounds(3)
  LOGICAL, ALLOCATABLE            :: mask(:,:,:)
  REAL(KIND=C_FLOAT), ALLOCATABLE :: deltas(:,:,:)
  INTEGER                         :: indices(3), indexAdj(3), i, j
  REAL, INTENT(in)                :: t
  
  
  mask = .NOT. (actual /= actual .AND. expected /= expected) .AND. ABS(actual - expected) > t
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3) + indexAdj(3) - 1
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

SUBROUTINE ftg_cmp_print_deviations_float_4d(expected, actual, fieldname_print, lbounds, t)
  REAL(KIND=C_FLOAT), INTENT(IN)  :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL   :: lbounds(4)
  LOGICAL, ALLOCATABLE            :: mask(:,:,:,:)
  REAL(KIND=C_FLOAT), ALLOCATABLE :: deltas(:,:,:,:)
  INTEGER                         :: indices(4), indexAdj(4), i, j
  REAL, INTENT(in)                :: t
  
  
  mask = .NOT. (actual /= actual .AND. expected /= expected) .AND. ABS(actual - expected) > t
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3) + indexAdj(3) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(4) + indexAdj(4) - 1
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

SUBROUTINE ftg_cmp_print_deviations_double_1d(expected, actual, fieldname_print, lbounds, t)
  REAL(KIND=C_DOUBLE), INTENT(IN)  :: expected(:), actual(:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL    :: lbounds(1)
  LOGICAL, ALLOCATABLE             :: mask(:)
  REAL(KIND=C_DOUBLE), ALLOCATABLE :: deltas(:)
  INTEGER                          :: indices(1), indexAdj(1), i, j
  REAL, INTENT(in)                 :: t
  
  
  mask = .NOT. (actual /= actual .AND. expected /= expected) .AND. ABS(actual - expected) > t
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
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

SUBROUTINE ftg_cmp_print_deviations_double_2d(expected, actual, fieldname_print, lbounds, t)
  REAL(KIND=C_DOUBLE), INTENT(IN)  :: expected(:,:), actual(:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL    :: lbounds(2)
  LOGICAL, ALLOCATABLE             :: mask(:,:)
  REAL(KIND=C_DOUBLE), ALLOCATABLE :: deltas(:,:)
  INTEGER                          :: indices(2), indexAdj(2), i, j
  REAL, INTENT(in)                 :: t
  
  
  mask = .NOT. (actual /= actual .AND. expected /= expected) .AND. ABS(actual - expected) > t
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
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

SUBROUTINE ftg_cmp_print_deviations_double_3d(expected, actual, fieldname_print, lbounds, t)
  REAL(KIND=C_DOUBLE), INTENT(IN)  :: expected(:,:,:), actual(:,:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL    :: lbounds(3)
  LOGICAL, ALLOCATABLE             :: mask(:,:,:)
  REAL(KIND=C_DOUBLE), ALLOCATABLE :: deltas(:,:,:)
  INTEGER                          :: indices(3), indexAdj(3), i, j
  REAL, INTENT(in)                 :: t
  
  
  mask = .NOT. (actual /= actual .AND. expected /= expected) .AND. ABS(actual - expected) > t
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3) + indexAdj(3) - 1
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

SUBROUTINE ftg_cmp_print_deviations_double_4d(expected, actual, fieldname_print, lbounds, t)
  REAL(KIND=C_DOUBLE), INTENT(IN)  :: expected(:,:,:,:), actual(:,:,:,:)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname_print
  INTEGER, INTENT(IN), OPTIONAL    :: lbounds(4)
  LOGICAL, ALLOCATABLE             :: mask(:,:,:,:)
  REAL(KIND=C_DOUBLE), ALLOCATABLE :: deltas(:,:,:,:)
  INTEGER                          :: indices(4), indexAdj(4), i, j
  REAL, INTENT(in)                 :: t
  
  
  mask = .NOT. (actual /= actual .AND. expected /= expected) .AND. ABS(actual - expected) > t
  deltas = ABS(expected - actual)
  IF (PRESENT(lbounds)) THEN
    indexAdj = lbounds
  ELSE
    indexAdj(:) = 1
  END IF
  
  DO i = 1, ftg_cmp_max_print_deviations
    IF (ANY(mask)) THEN
      indices = MAXLOC(deltas, mask)
      WRITE (*,'(A)',advance="no") "  -> ("
      WRITE (*,'(I0)',advance="no") indices(1) + indexAdj(1) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(2) + indexAdj(2) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(3) + indexAdj(3) - 1
      WRITE (*,'(A)',advance="no") ", "
      WRITE (*,'(I0)',advance="no") indices(4) + indexAdj(4) - 1
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
!=============================================================================

SUBROUTINE ftg_compare_logical_0d(fieldname, field, result, failure_count, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL, INTENT(IN)                    :: field
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL, ALLOCATABLE                   :: stored_field
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (field .NEQV. stored_field) THEN
      result = .FALSE.
      IF (.NOT. ftg_cmp_quiet) THEN
        WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
        IF (ftg_cmp_max_print_deviations > 0) THEN
          CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
        END IF 
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_logical_0d

SUBROUTINE ftg_compare_logical_1d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL, INTENT(IN)                    :: field(:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL, ALLOCATABLE                   :: stored_field(:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(1), ubounds(1)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field .NEQV. stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_logical_1d

SUBROUTINE ftg_compare_logical_2d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL, INTENT(IN)                    :: field(:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL, ALLOCATABLE                   :: stored_field(:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(2), ubounds(2)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field .NEQV. stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_logical_2d

SUBROUTINE ftg_compare_logical_3d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL, INTENT(IN)                    :: field(:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL, ALLOCATABLE                   :: stored_field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(3), ubounds(3)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field .NEQV. stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_logical_3d

SUBROUTINE ftg_compare_logical_4d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL, INTENT(IN)                    :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL, ALLOCATABLE                   :: stored_field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(4), ubounds(4)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field .NEQV. stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_logical_4d

SUBROUTINE ftg_compare_bool_0d(fieldname, field, result, failure_count, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN)       :: field
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL(KIND=C_BOOL), ALLOCATABLE      :: stored_field
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (field .NEQV. stored_field) THEN
      result = .FALSE.
      IF (.NOT. ftg_cmp_quiet) THEN
        WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
        IF (ftg_cmp_max_print_deviations > 0) THEN
          CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
        END IF 
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_bool_0d

SUBROUTINE ftg_compare_bool_1d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN)       :: field(:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL(KIND=C_BOOL), ALLOCATABLE      :: stored_field(:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(1), ubounds(1)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field .NEQV. stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_bool_1d

SUBROUTINE ftg_compare_bool_2d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN)       :: field(:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL(KIND=C_BOOL), ALLOCATABLE      :: stored_field(:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(2), ubounds(2)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field .NEQV. stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_bool_2d

SUBROUTINE ftg_compare_bool_3d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN)       :: field(:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL(KIND=C_BOOL), ALLOCATABLE      :: stored_field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(3), ubounds(3)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field .NEQV. stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_bool_3d

SUBROUTINE ftg_compare_bool_4d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN)       :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  LOGICAL(KIND=C_BOOL), ALLOCATABLE      :: stored_field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(4), ubounds(4)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field .NEQV. stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_bool_4d

SUBROUTINE ftg_compare_int_0d(fieldname, field, result, failure_count, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER, INTENT(IN)                    :: field
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER, ALLOCATABLE                   :: stored_field
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (field /= stored_field) THEN
      result = .FALSE.
      IF (.NOT. ftg_cmp_quiet) THEN
        WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
        IF (ftg_cmp_max_print_deviations > 0) THEN
          CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
        END IF 
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_int_0d

SUBROUTINE ftg_compare_int_1d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER, INTENT(IN)                    :: field(:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER, ALLOCATABLE                   :: stored_field(:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(1), ubounds(1)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field /= stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_int_1d

SUBROUTINE ftg_compare_int_2d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER, INTENT(IN)                    :: field(:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER, ALLOCATABLE                   :: stored_field(:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(2), ubounds(2)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field /= stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_int_2d

SUBROUTINE ftg_compare_int_3d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER, INTENT(IN)                    :: field(:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER, ALLOCATABLE                   :: stored_field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(3), ubounds(3)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field /= stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_int_3d

SUBROUTINE ftg_compare_int_4d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER, INTENT(IN)                    :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER, ALLOCATABLE                   :: stored_field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(4), ubounds(4)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field /= stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_int_4d

SUBROUTINE ftg_compare_long_0d(fieldname, field, result, failure_count, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN)       :: field
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER(KIND=C_LONG), ALLOCATABLE      :: stored_field
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (field /= stored_field) THEN
      result = .FALSE.
      IF (.NOT. ftg_cmp_quiet) THEN
        WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
        IF (ftg_cmp_max_print_deviations > 0) THEN
          CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
        END IF 
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_long_0d

SUBROUTINE ftg_compare_long_1d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN)       :: field(:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER(KIND=C_LONG), ALLOCATABLE      :: stored_field(:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(1), ubounds(1)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field /= stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_long_1d

SUBROUTINE ftg_compare_long_2d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN)       :: field(:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER(KIND=C_LONG), ALLOCATABLE      :: stored_field(:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(2), ubounds(2)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field /= stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_long_2d

SUBROUTINE ftg_compare_long_3d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN)       :: field(:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER(KIND=C_LONG), ALLOCATABLE      :: stored_field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(3), ubounds(3)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field /= stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_long_3d

SUBROUTINE ftg_compare_long_4d(fieldname, field, result, failure_count, lbounds, ubounds, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN)       :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  INTEGER(KIND=C_LONG), ALLOCATABLE      :: stored_field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(4), ubounds(4)
  
  IF (PRESENT(fieldname_alias)) THEN
    fieldname_print = fieldname_alias
  ELSE
    fieldname_print = fieldname
  END IF
  
  result = .TRUE.
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(field /= stored_field)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_long_4d

SUBROUTINE ftg_compare_float_0d(fieldname, field, result, failure_count, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN)         :: field
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  REAL(KIND=C_FLOAT), ALLOCATABLE        :: stored_field
  REAL, INTENT(in), OPTIONAL             :: tolerance
  REAL                                   :: t
  
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
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (.NOT. (field /= field .AND. stored_field /= stored_field) .AND. ABS(field - stored_field) > t) THEN
      result = .FALSE.
      IF (.NOT. ftg_cmp_quiet) THEN
        WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
        IF (ftg_cmp_max_print_deviations > 0) THEN
          CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
        END IF 
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_float_0d

SUBROUTINE ftg_compare_float_1d(fieldname, field, result, failure_count, lbounds, ubounds, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN)         :: field(:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  REAL(KIND=C_FLOAT), ALLOCATABLE        :: stored_field(:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(1), ubounds(1)
  REAL, INTENT(in), OPTIONAL             :: tolerance
  REAL                                   :: t
  
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
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(.NOT. (field /= field .AND. stored_field /= stored_field) .AND. ABS(field - stored_field) > t)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds, t)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_float_1d

SUBROUTINE ftg_compare_float_2d(fieldname, field, result, failure_count, lbounds, ubounds, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN)         :: field(:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  REAL(KIND=C_FLOAT), ALLOCATABLE        :: stored_field(:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(2), ubounds(2)
  REAL, INTENT(in), OPTIONAL             :: tolerance
  REAL                                   :: t
  
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
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(.NOT. (field /= field .AND. stored_field /= stored_field) .AND. ABS(field - stored_field) > t)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds, t)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_float_2d

SUBROUTINE ftg_compare_float_3d(fieldname, field, result, failure_count, lbounds, ubounds, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN)         :: field(:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  REAL(KIND=C_FLOAT), ALLOCATABLE        :: stored_field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(3), ubounds(3)
  REAL, INTENT(in), OPTIONAL             :: tolerance
  REAL                                   :: t
  
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
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(.NOT. (field /= field .AND. stored_field /= stored_field) .AND. ABS(field - stored_field) > t)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds, t)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_float_3d

SUBROUTINE ftg_compare_float_4d(fieldname, field, result, failure_count, lbounds, ubounds, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN)         :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  REAL(KIND=C_FLOAT), ALLOCATABLE        :: stored_field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(4), ubounds(4)
  REAL, INTENT(in), OPTIONAL             :: tolerance
  REAL                                   :: t
  
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
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(.NOT. (field /= field .AND. stored_field /= stored_field) .AND. ABS(field - stored_field) > t)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds, t)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_float_4d

SUBROUTINE ftg_compare_double_0d(fieldname, field, result, failure_count, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN)        :: field
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  REAL(KIND=C_DOUBLE), ALLOCATABLE       :: stored_field
  REAL, INTENT(in), OPTIONAL             :: tolerance
  REAL                                   :: t
  
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
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
    IF (.NOT. (field /= field .AND. stored_field /= stored_field) .AND. ABS(field - stored_field) > t) THEN
      result = .FALSE.
      IF (.NOT. ftg_cmp_quiet) THEN
        WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
        IF (ftg_cmp_max_print_deviations > 0) THEN
          CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print)
        END IF 
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_double_0d

SUBROUTINE ftg_compare_double_1d(fieldname, field, result, failure_count, lbounds, ubounds, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN)        :: field(:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  REAL(KIND=C_DOUBLE), ALLOCATABLE       :: stored_field(:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(1), ubounds(1)
  REAL, INTENT(in), OPTIONAL             :: tolerance
  REAL                                   :: t
  
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
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(.NOT. (field /= field .AND. stored_field /= stored_field) .AND. ABS(field - stored_field) > t)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds, t)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_double_1d

SUBROUTINE ftg_compare_double_2d(fieldname, field, result, failure_count, lbounds, ubounds, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN)        :: field(:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  REAL(KIND=C_DOUBLE), ALLOCATABLE       :: stored_field(:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(2), ubounds(2)
  REAL, INTENT(in), OPTIONAL             :: tolerance
  REAL                                   :: t
  
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
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(.NOT. (field /= field .AND. stored_field /= stored_field) .AND. ABS(field - stored_field) > t)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds, t)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_double_2d

SUBROUTINE ftg_compare_double_3d(fieldname, field, result, failure_count, lbounds, ubounds, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN)        :: field(:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  REAL(KIND=C_DOUBLE), ALLOCATABLE       :: stored_field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(3), ubounds(3)
  REAL, INTENT(in), OPTIONAL             :: tolerance
  REAL                                   :: t
  
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
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(.NOT. (field /= field .AND. stored_field /= stored_field) .AND. ABS(field - stored_field) > t)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds, t)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_double_3d

SUBROUTINE ftg_compare_double_4d(fieldname, field, result, failure_count, lbounds, ubounds, tolerance, fieldname_alias)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN)        :: field(:,:,:,:)
  LOGICAL, INTENT(OUT)                   :: result
  INTEGER, INTENT(INOUT), OPTIONAL       :: failure_count
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: fieldname_alias
  CHARACTER(LEN=256)                     :: fieldname_print
  REAL(KIND=C_DOUBLE), ALLOCATABLE       :: stored_field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(4), ubounds(4)
  REAL, INTENT(in), OPTIONAL             :: tolerance
  REAL                                   :: t
  
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
  
  IF (.NOT. ftg_field_exists(fieldname)) THEN
    IF (ftg_cmp_count_missing_field_as_failure) THEN
      result = .FALSE.
    END IF
    IF (.NOT. ftg_cmp_quiet) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Don't exist in Serializer"
    END IF
  ELSE
    IF (.NOT. ftg_cmp_size(fieldname, SHAPE(field), fieldname_print)) THEN
      result = .FALSE.
    ELSE
      IF (PRESENT(lbounds) .AND. PRESENT(ubounds)) THEN
        IF (.NOT. ftg_cmp_bounds(fieldname, lbounds, ubounds, fieldname_print) .AND. ftg_cmp_count_different_bounds_as_failure) THEN
          result = .FALSE.
        END IF
      END IF
      CALL ftg_allocate_and_read_allocatable(fieldname, stored_field)
      IF (ANY(.NOT. (field /= field .AND. stored_field /= stored_field) .AND. ABS(field - stored_field) > t)) THEN
        result = .FALSE.
        IF (.NOT. ftg_cmp_quiet) THEN
          WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : Not equal"
          IF (ftg_cmp_max_print_deviations > 0) THEN
            CALL ftg_cmp_print_deviations(stored_field, field, fieldname_print, lbounds, t)
          END IF 
        END IF
      END IF
    END IF
  END IF
  
  IF (result) THEN
    IF (.NOT. ftg_cmp_quiet .AND. ftg_cmp_print_when_equal) THEN
      WRITE (*,'(A,A,A,A)') TRIM(ftg_cmp_message_prefix), " ", TRIM(fieldname_print), " : OK"
    END IF
  ELSE
    IF (PRESENT(failure_count)) THEN
      failure_count = failure_count + 1
    END IF
  END IF
    
END SUBROUTINE ftg_compare_double_4d

END MODULE m_ser_ftg_cmp