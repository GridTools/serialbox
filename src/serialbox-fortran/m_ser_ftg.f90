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

MODULE m_ser_ftg

!------------------------------------------------------------------------------
!
! Description:
!
!   This module contains simplified wrapper subroutines for the Fortran interface
!   of Serialbox2 (m_serialize.f90) to be used by the FortranTestGenerator
!   (https://github.com/fortesg/fortrantestgenerator), plus additional subroutines
!   for allocating array variables based on the stored sizes and bounds.
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
USE m_serialize

IMPLICIT NONE

PUBLIC :: ignore_bullshit, ignore_bullshit_max_dim_size, ignore_bullshit_allow_negative_indices, ignore_not_existing, &
          ftg_set_serializer, ftg_get_serializer, ftg_destroy_serializer, ftg_print_serializer_debuginfo, &
          ftg_set_savepoint, ftg_get_savepoint, ftg_destroy_savepoint, &
          ftg_add_serializer_metainfo, ftg_add_savepoint_metainfo, &
          ftg_field_exists, ftg_get_bounds, ftg_register_only, ftg_write, ftg_read, ftg_allocate

PRIVATE

CHARACTER(LEN=*), PARAMETER :: module_name = 'm_ser_ftg'

INTERFACE ftg_set_serializer
    MODULE PROCEDURE &
      ftg_set_serializer_create, &
      ftg_set_serializer_existing
END INTERFACE

INTERFACE ftg_set_savepoint
    MODULE PROCEDURE &
      ftg_set_savepoint_create, &
      ftg_set_savepoint_existing
END INTERFACE

INTERFACE ftg_add_serializer_metainfo
    MODULE PROCEDURE &
      ftg_add_serializer_metainfo_b, &
      ftg_add_serializer_metainfo_i, &
      ftg_add_serializer_metainfo_f, &
      ftg_add_serializer_metainfo_d, &
      ftg_add_serializer_metainfo_s
END INTERFACE

INTERFACE ftg_add_savepoint_metainfo
    MODULE PROCEDURE &
      ftg_add_savepoint_metainfo_b, &
      ftg_add_savepoint_metainfo_i, &
      ftg_add_savepoint_metainfo_f, &
      ftg_add_savepoint_metainfo_d, &
      ftg_add_savepoint_metainfo_s
END INTERFACE

INTERFACE ftg_write
    MODULE PROCEDURE &
      ftg_write_logical_0d, &
      ftg_write_logical_1d, &
      ftg_write_logical_2d, &
      ftg_write_logical_3d, &
      ftg_write_logical_4d, &
      ftg_write_bool_0d, &
      ftg_write_bool_1d, &
      ftg_write_bool_2d, &
      ftg_write_bool_3d, &
      ftg_write_bool_4d, &
      ftg_write_int_0d, &
      ftg_write_int_1d, &
      ftg_write_int_2d, &
      ftg_write_int_3d, &
      ftg_write_int_4d, &
      ftg_write_long_0d, &
      ftg_write_long_1d, &
      ftg_write_long_2d, &
      ftg_write_long_3d, &
      ftg_write_long_4d, &
      ftg_write_float_0d, &
      ftg_write_float_1d, &
      ftg_write_float_2d, &
      ftg_write_float_3d, &
      ftg_write_float_4d, &
      ftg_write_double_0d, &
      ftg_write_double_1d, &
      ftg_write_double_2d, &
      ftg_write_double_3d, &
      ftg_write_double_4d
END INTERFACE

INTERFACE ftg_read
    MODULE PROCEDURE &
      ftg_read_logical_0d, &
      ftg_read_logical_1d, &
      ftg_read_logical_2d, &
      ftg_read_logical_3d, &
      ftg_read_logical_4d, &
      ftg_read_bool_0d, &
      ftg_read_bool_1d, &
      ftg_read_bool_2d, &
      ftg_read_bool_3d, &
      ftg_read_bool_4d, &
      ftg_read_int_0d, &
      ftg_read_int_1d, &
      ftg_read_int_2d, &
      ftg_read_int_3d, &
      ftg_read_int_4d, &
      ftg_read_long_0d, &
      ftg_read_long_1d, &
      ftg_read_long_2d, &
      ftg_read_long_3d, &
      ftg_read_long_4d, &
      ftg_read_float_0d, &
      ftg_read_float_1d, &
      ftg_read_float_2d, &
      ftg_read_float_3d, &
      ftg_read_float_4d, &
      ftg_read_double_0d, &
      ftg_read_double_1d, &
      ftg_read_double_2d, &
      ftg_read_double_3d, &
      ftg_read_double_4d
END INTERFACE

INTERFACE ftg_allocate
    MODULE PROCEDURE &
      ftg_allocate_pointer_logical_1d, &
      ftg_allocate_pointer_logical_2d, &
      ftg_allocate_pointer_logical_3d, &
      ftg_allocate_pointer_logical_4d, &
      ftg_allocate_pointer_bool_1d, &
      ftg_allocate_pointer_bool_2d, &
      ftg_allocate_pointer_bool_3d, &
      ftg_allocate_pointer_bool_4d, &
      ftg_allocate_pointer_int_1d, &
      ftg_allocate_pointer_int_2d, &
      ftg_allocate_pointer_int_3d, &
      ftg_allocate_pointer_int_4d, &
      ftg_allocate_pointer_long_1d, &
      ftg_allocate_pointer_long_2d, &
      ftg_allocate_pointer_long_3d, &
      ftg_allocate_pointer_long_4d, &
      ftg_allocate_pointer_float_1d, &
      ftg_allocate_pointer_float_2d, &
      ftg_allocate_pointer_float_3d, &
      ftg_allocate_pointer_float_4d, &
      ftg_allocate_pointer_double_1d, &
      ftg_allocate_pointer_double_2d, &
      ftg_allocate_pointer_double_3d, &
      ftg_allocate_pointer_double_4d, &
      ftg_allocate_allocatable_logical_1d, &
      ftg_allocate_allocatable_logical_2d, &
      ftg_allocate_allocatable_logical_3d, &
      ftg_allocate_allocatable_logical_4d, &
      ftg_allocate_allocatable_bool_1d, &
      ftg_allocate_allocatable_bool_2d, &
      ftg_allocate_allocatable_bool_3d, &
      ftg_allocate_allocatable_bool_4d, &
      ftg_allocate_allocatable_int_1d, &
      ftg_allocate_allocatable_int_2d, &
      ftg_allocate_allocatable_int_3d, &
      ftg_allocate_allocatable_int_4d, &
      ftg_allocate_allocatable_long_1d, &
      ftg_allocate_allocatable_long_2d, &
      ftg_allocate_allocatable_long_3d, &
      ftg_allocate_allocatable_long_4d, &
      ftg_allocate_allocatable_float_1d, &
      ftg_allocate_allocatable_float_2d, &
      ftg_allocate_allocatable_float_3d, &
      ftg_allocate_allocatable_float_4d, &
      ftg_allocate_allocatable_double_1d, &
      ftg_allocate_allocatable_double_2d, &
      ftg_allocate_allocatable_double_3d, &
      ftg_allocate_allocatable_double_4d
END INTERFACE ftg_allocate

LOGICAL :: ignore_bullshit = .TRUE.
INTEGER :: ignore_bullshit_max_dim_size = 999999999
LOGICAL :: ignore_bullshit_allow_negative_indices = .TRUE.
LOGICAL :: ignore_not_existing = .TRUE.

TYPE(t_serializer), POINTER :: serializer => NULL()
TYPE(t_savepoint),  POINTER :: savepoint  => NULL()

LOGICAL :: serializer_has_written = .FALSE., serializer_has_registered = .FALSE.

CONTAINS

!=============================================================================
!=============================================================================

SUBROUTINE ftg_set_serializer_create(directory, prefix, mode, opt_archive)

  CHARACTER(LEN=*), INTENT(IN)    :: directory, prefix
  CHARACTER, INTENT(IN)           :: mode
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: opt_archive

  TYPE(t_serializer), POINTER :: new_serializer

  ALLOCATE(new_serializer)
  CALL fs_create_serializer(directory, prefix, mode, new_serializer, opt_archive)
  serializer => new_serializer
  serializer_has_written = .FALSE.
  serializer_has_registered = .FALSE.

END SUBROUTINE ftg_set_serializer_create


SUBROUTINE ftg_set_serializer_existing(new_serializer)

  TYPE(t_serializer), INTENT(IN), TARGET :: new_serializer

  serializer => new_serializer
  serializer_has_written = .FALSE.
  serializer_has_registered = .FALSE.

END SUBROUTINE ftg_set_serializer_existing


SUBROUTINE ftg_destroy_serializer()

  IF (ASSOCIATED(serializer)) THEN
    IF (serializer_has_registered .AND. .NOT. serializer_has_written) THEN
      CALL ftg_write('ftg_dummy_var', .TRUE.)
    END IF

    CALL fs_destroy_serializer(serializer)
    serializer => NULL()
  END IF

END SUBROUTINE ftg_destroy_serializer


FUNCTION ftg_get_serializer()

  TYPE(t_serializer), POINTER :: ftg_get_serializer

  IF (.NOT. ASSOCIATED(serializer)) THEN
    WRITE(*,*) TRIM(module_name)//" - ERROR: No serializer. Call ftg_set_serializer() first!"
    STOP
  ELSE
    ftg_get_serializer => serializer
  END IF

END FUNCTION ftg_get_serializer

SUBROUTINE ftg_print_serializer_debuginfo()
  CALL fs_print_debuginfo(serializer)
END SUBROUTINE ftg_print_serializer_debuginfo

!=============================================================================
!=============================================================================

SUBROUTINE ftg_set_savepoint_create(name)

  CHARACTER(LEN=*), INTENT(IN) :: name

  TYPE(t_savepoint), POINTER :: new_savepoint

  ALLOCATE(new_savepoint)
  CALL fs_create_savepoint(name, new_savepoint)
  savepoint => new_savepoint

END SUBROUTINE ftg_set_savepoint_create


SUBROUTINE ftg_set_savepoint_existing(new_savepoint)

  TYPE(t_savepoint), INTENT(IN), TARGET :: new_savepoint

  savepoint => new_savepoint

END SUBROUTINE ftg_set_savepoint_existing


SUBROUTINE ftg_destroy_savepoint()

  IF (ASSOCIATED(savepoint)) THEN
    CALL fs_destroy_savepoint(savepoint)
    savepoint => NULL()
  END IF

END SUBROUTINE ftg_destroy_savepoint


TYPE(t_savepoint) FUNCTION ftg_get_savepoint()

  IF (.NOT. ASSOCIATED(savepoint)) THEN
    WRITE(*,*) TRIM(module_name)//" - ERROR: No savepoint. Call ftg_set_savepoint() first!"
    STOP
  ELSE
    ftg_get_savepoint = savepoint
  END IF

END FUNCTION ftg_get_savepoint

!=============================================================================
!=============================================================================

LOGICAL FUNCTION ftg_field_exists(fieldname)

  CHARACTER(LEN=*), INTENT(IN)   :: fieldname

  ftg_field_exists = fs_field_exists(serializer,  fieldname)

END FUNCTION ftg_field_exists

!=============================================================================
!=============================================================================

FUNCTION ftg_get_bounds(fieldname)

  CHARACTER(LEN=*), INTENT(IN)   :: fieldname
  INTEGER, DIMENSION(8) :: ftg_get_bounds

  ftg_get_bounds = fs_get_halos(serializer,  fieldname)

END FUNCTION ftg_get_bounds

!=============================================================================
!=============================================================================

SUBROUTINE ftg_add_serializer_metainfo_b(key, val)
  CHARACTER(LEN=*) :: key
  LOGICAL, VALUE   :: val
  CALL fs_add_serializer_metainfo(serializer, key, val)
END SUBROUTINE ftg_add_serializer_metainfo_b


SUBROUTINE ftg_add_serializer_metainfo_i(key, val)
  CHARACTER(LEN=*)               :: key
  INTEGER(C_INT)                 :: val
  CALL fs_add_serializer_metainfo(serializer, key, val)
END SUBROUTINE ftg_add_serializer_metainfo_i


SUBROUTINE ftg_add_serializer_metainfo_f(key, val)
  CHARACTER(LEN=*)               :: key
  REAL(KIND=C_FLOAT)             :: val
  CALL fs_add_serializer_metainfo(serializer, key, val)
END SUBROUTINE ftg_add_serializer_metainfo_f


SUBROUTINE ftg_add_serializer_metainfo_d(key, val)
  CHARACTER(LEN=*)               :: key
  REAL(KIND=C_DOUBLE)            :: val
  CALL fs_add_serializer_metainfo(serializer, key, val)
END SUBROUTINE ftg_add_serializer_metainfo_d


SUBROUTINE ftg_add_serializer_metainfo_s(key, val)
  CHARACTER(LEN=*)               :: key, val
  CALL fs_add_serializer_metainfo(serializer, key, val)
END SUBROUTINE ftg_add_serializer_metainfo_s

!=============================================================================
!=============================================================================

SUBROUTINE ftg_add_savepoint_metainfo_b(key, val)
  CHARACTER(LEN=*) :: key
  LOGICAL, VALUE   :: val
  CALL fs_add_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE ftg_add_savepoint_metainfo_b


SUBROUTINE ftg_add_savepoint_metainfo_i(key, val)
  CHARACTER(LEN=*)               :: key
  INTEGER(C_INT)                 :: val
  CALL fs_add_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE ftg_add_savepoint_metainfo_i


SUBROUTINE ftg_add_savepoint_metainfo_f(key, val)
  CHARACTER(LEN=*)               :: key
  REAL(KIND=C_FLOAT)             :: val
  CALL fs_add_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE ftg_add_savepoint_metainfo_f


SUBROUTINE ftg_add_savepoint_metainfo_d(key, val)
  CHARACTER(LEN=*)               :: key
  REAL(KIND=C_DOUBLE)            :: val
  CALL fs_add_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE ftg_add_savepoint_metainfo_d


SUBROUTINE ftg_add_savepoint_metainfo_s(key, val)
  CHARACTER(LEN=*)               :: key, val
  CALL fs_add_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE ftg_add_savepoint_metainfo_s

!=============================================================================
!=============================================================================

SUBROUTINE ftg_register_only(fieldname, typename, lbounds, ubounds, cptr)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: typename
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(:), ubounds(:)
  TYPE(C_PTR), INTENT(IN), OPTIONAL      :: cptr

  LOGICAL       :: bullshit
  CHARACTER(16) :: loc
  INTEGER       :: sizes(4), bounds(8), i

  sizes  = (/ 1, 0, 0, 0 /)
  bounds = (/ 0, 0, 0, 0, 0, 0, 0, 0 /)

  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = PRESENT(lbounds) .NEQV. PRESENT(ubounds)
    IF (.NOT. bullshit .AND. PRESENT(lbounds)) THEN
      bullshit = SIZE(lbounds) /= SIZE(ubounds)
      IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
        bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
      END IF
      IF (.NOT. bullshit) THEN
        DO i = 1, SIZE(lbounds)
          sizes(i) = ubounds(i) - lbounds(i) + 1
          bounds(i * 2 - 1) = lbounds(i)
          bounds(i * 2) = ubounds(i)
        END DO
        bullshit = ANY(sizes > ignore_bullshit_max_dim_size)
      END IF
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_register_field(serializer, fieldname, 'int', 4, sizes(1), sizes(2), sizes(3), sizes(4), &
                           bounds(1), bounds(2), bounds(3), bounds(4), bounds(5), bounds(6), bounds(7), bounds(8))
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'registered_only', .TRUE.)
    IF (PRESENT(typename)) THEN
      CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'type', TRIM(typename))
    END IF
    IF (PRESENT(cptr)) THEN
      CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ftg_loc_hex(cptr)))
    END IF
    serializer_has_registered = .TRUE.
  END IF

END SUBROUTINE ftg_register_only

!=============================================================================
!=============================================================================

FUNCTION ftg_loc(field)

  TYPE(C_PTR), INTENT(in) :: field
  INTEGER(KIND=C_INTPTR_T) :: ftg_loc

  INTERFACE
     SUBROUTINE ftg_loc_(field, loc) &
          BIND(c, name='serialboxFortranLoc')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE :: field
       INTEGER(C_INTPTR_T), INTENT(OUT)   :: loc
     END SUBROUTINE ftg_loc_
  END INTERFACE

  CALL ftg_loc_(field, ftg_loc)

END FUNCTION ftg_loc

FUNCTION ftg_loc_hex(field)

  TYPE(C_PTR), INTENT(in) :: field
  CHARACTER(16) :: ftg_loc_hex

  WRITE (ftg_loc_hex,'(Z16)') ftg_loc(field)

END FUNCTION ftg_loc_hex

!=============================================================================
!=============================================================================


SUBROUTINE ftg_write_logical_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(IN), TARGET  :: field

  LOGICAL, POINTER :: padd
  LOGICAL          :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_logical_0d

SUBROUTINE ftg_write_logical_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(IN), TARGET  :: field(:)
  INTEGER, INTENT(IN)          :: lbounds(1), ubounds(1)

  LOGICAL, POINTER :: padd(:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_logical_1d

SUBROUTINE ftg_write_logical_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(IN), TARGET  :: field(:,:)
  INTEGER, INTENT(IN)          :: lbounds(2), ubounds(2)

  LOGICAL, POINTER :: padd(:,:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_logical_2d

SUBROUTINE ftg_write_logical_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(IN), TARGET  :: field(:,:,:)
  INTEGER, INTENT(IN)          :: lbounds(3), ubounds(3)

  LOGICAL, POINTER :: padd(:,:,:)
  LOGICAL          :: bullshit
  CHARACTER(16)    :: loc

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_logical_3d

SUBROUTINE ftg_write_logical_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(IN), TARGET  :: field(:,:,:,:)
  INTEGER, INTENT(IN)          :: lbounds(4), ubounds(4)

  LOGICAL, POINTER :: padd(:,:,:,:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_logical_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_write_bool_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field

  LOGICAL(KIND=C_BOOL), POINTER :: padd
  LOGICAL                       :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_bool_0d

SUBROUTINE ftg_write_bool_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN)                      :: lbounds(1), ubounds(1)

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_bool_1d

SUBROUTINE ftg_write_bool_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN)                      :: lbounds(2), ubounds(2)

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_bool_2d

SUBROUTINE ftg_write_bool_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN)                      :: lbounds(3), ubounds(3)

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:,:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_bool_3d

SUBROUTINE ftg_write_bool_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN)                      :: lbounds(4), ubounds(4)

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:,:,:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_bool_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_write_int_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(IN), TARGET  :: field

  INTEGER, POINTER :: padd
  LOGICAL          :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_int_0d

SUBROUTINE ftg_write_int_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(IN), TARGET  :: field(:)
  INTEGER, INTENT(IN)          :: lbounds(1), ubounds(1)

  INTEGER, POINTER :: padd(:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_int_1d

SUBROUTINE ftg_write_int_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(IN), TARGET  :: field(:,:)
  INTEGER, INTENT(IN)          :: lbounds(2), ubounds(2)

  INTEGER, POINTER :: padd(:,:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_int_2d

SUBROUTINE ftg_write_int_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(IN), TARGET  :: field(:,:,:)
  INTEGER, INTENT(IN)          :: lbounds(3), ubounds(3)

  INTEGER, POINTER :: padd(:,:,:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_int_3d

SUBROUTINE ftg_write_int_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(IN), TARGET  :: field(:,:,:,:)
  INTEGER, INTENT(IN)          :: lbounds(4), ubounds(4)

  INTEGER, POINTER :: padd(:,:,:,:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_int_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_write_long_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field

  INTEGER(KIND=C_LONG), POINTER :: padd
  LOGICAL                       :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_long_0d

SUBROUTINE ftg_write_long_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN)                      :: lbounds(1), ubounds(1)

  INTEGER(KIND=C_LONG), POINTER :: padd(:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_long_1d

SUBROUTINE ftg_write_long_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN)                      :: lbounds(2), ubounds(2)

  INTEGER(KIND=C_LONG), POINTER :: padd(:,:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_long_2d

SUBROUTINE ftg_write_long_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN)                      :: lbounds(3), ubounds(3)

  INTEGER(KIND=C_LONG), POINTER :: padd(:,:,:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_long_3d

SUBROUTINE ftg_write_long_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN)                      :: lbounds(4), ubounds(4)

  INTEGER(KIND=C_LONG), POINTER :: padd(:,:,:,:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_long_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_write_float_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field

  REAL(KIND=C_FLOAT), POINTER :: padd
  LOGICAL                     :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_float_0d

SUBROUTINE ftg_write_float_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN)                    :: lbounds(1), ubounds(1)

  REAL(KIND=C_FLOAT), POINTER :: padd(:)
  LOGICAL                     :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_float_1d

SUBROUTINE ftg_write_float_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN)                    :: lbounds(2), ubounds(2)

  REAL(KIND=C_FLOAT), POINTER :: padd(:,:)
  LOGICAL                     :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_float_2d

SUBROUTINE ftg_write_float_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN)                    :: lbounds(3), ubounds(3)

  REAL(KIND=C_FLOAT), POINTER :: padd(:,:,:)
  LOGICAL                     :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_float_3d

SUBROUTINE ftg_write_float_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN)                    :: lbounds(4), ubounds(4)

  REAL(KIND=C_FLOAT), POINTER :: padd(:,:,:,:)
  LOGICAL                     :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_float_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_write_double_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field

  REAL(KIND=C_DOUBLE), POINTER :: padd
  LOGICAL                      :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_double_0d

SUBROUTINE ftg_write_double_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:)
  INTEGER, DIMENSION(1), INTENT(IN)       :: lbounds, ubounds

  REAL(KIND=C_DOUBLE), POINTER :: padd(:)
  LOGICAL                      :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_double_1d

SUBROUTINE ftg_write_double_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:,:)
  INTEGER, DIMENSION(2), INTENT(IN)       :: lbounds, ubounds

  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:)
  LOGICAL                      :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_double_2d

SUBROUTINE ftg_write_double_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, DIMENSION(3), INTENT(IN)       :: lbounds, ubounds

  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:,:)
  LOGICAL                      :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_double_3d

SUBROUTINE ftg_write_double_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, DIMENSION(4), INTENT(IN)       :: lbounds, ubounds

  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:,:,:)
  LOGICAL                      :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
    CALL fs_add_field_metainfo(serializer, TRIM(fieldname), 'loc', TRIM(ADJUSTL(ftg_loc_hex(C_LOC(field)))))
    serializer_has_written = .TRUE.
  END IF

END SUBROUTINE ftg_write_double_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_read_logical_0d(fieldname, field)
  CHARACTER(LEN=*)             :: fieldname
  LOGICAL, INTENT(OUT), TARGET :: field

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_logical_0d

SUBROUTINE ftg_read_logical_1d(fieldname, field)
  CHARACTER(LEN=*)             :: fieldname
  LOGICAL, INTENT(OUT), TARGET :: field(:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_logical_1d

SUBROUTINE ftg_read_logical_2d(fieldname, field)
  CHARACTER(LEN=*)             :: fieldname
  LOGICAL, INTENT(OUT), TARGET :: field(:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_logical_2d

SUBROUTINE ftg_read_logical_3d(fieldname, field)
  CHARACTER(LEN=*)             :: fieldname
  LOGICAL, INTENT(OUT), TARGET :: field(:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_logical_3d

SUBROUTINE ftg_read_logical_4d(fieldname, field)
  CHARACTER(LEN=*)             :: fieldname
  LOGICAL, INTENT(OUT), TARGET :: field(:,:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_logical_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_read_bool_0d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_bool_0d

SUBROUTINE ftg_read_bool_1d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_bool_1d

SUBROUTINE ftg_read_bool_2d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_bool_2d

SUBROUTINE ftg_read_bool_3d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_bool_3d

SUBROUTINE ftg_read_bool_4d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:,:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_bool_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_read_int_0d(fieldname, field)
  CHARACTER(LEN=*)             :: fieldname
  INTEGER, INTENT(OUT), TARGET :: field

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_int_0d

SUBROUTINE ftg_read_int_1d(fieldname, field)
  CHARACTER(LEN=*)             :: fieldname
  INTEGER, INTENT(OUT), TARGET :: field(:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_int_1d

SUBROUTINE ftg_read_int_2d(fieldname, field)
  CHARACTER(LEN=*)             :: fieldname
  INTEGER, INTENT(OUT), TARGET :: field(:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_int_2d

SUBROUTINE ftg_read_int_3d(fieldname, field)
  CHARACTER(LEN=*)             :: fieldname
  INTEGER, INTENT(OUT), TARGET :: field(:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_int_3d

SUBROUTINE ftg_read_int_4d(fieldname, field)
  CHARACTER(LEN=*)             :: fieldname
  INTEGER, INTENT(OUT), TARGET :: field(:,:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_int_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_read_long_0d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_long_0d

SUBROUTINE ftg_read_long_1d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_long_1d

SUBROUTINE ftg_read_long_2d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_long_2d

SUBROUTINE ftg_read_long_3d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_long_3d

SUBROUTINE ftg_read_long_4d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:,:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_long_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_read_float_0d(fieldname, field)
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_float_0d

SUBROUTINE ftg_read_float_1d(fieldname, field)
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_float_1d

SUBROUTINE ftg_read_float_2d(fieldname, field)
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_float_2d

SUBROUTINE ftg_read_float_3d(fieldname, field)
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_float_3d

SUBROUTINE ftg_read_float_4d(fieldname, field)
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:,:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_float_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_read_double_0d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_double_0d

SUBROUTINE ftg_read_double_1d(fieldname, field)
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_double_1d

SUBROUTINE ftg_read_double_2d(fieldname, field)
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_double_2d

SUBROUTINE ftg_read_double_3d(fieldname, field)
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_double_3d

SUBROUTINE ftg_read_double_4d(fieldname, field)
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:,:,:,:)

  IF (.NOT. ignore_not_existing .OR. ftg_field_exists(fieldname)) THEN
    CALL fs_read_field(serializer, savepoint, fieldname, field)
  END IF
END SUBROUTINE ftg_read_double_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_pointer_logical_1d(fieldname, field)
  CHARACTER(LEN=*)              :: fieldname
  LOGICAL, INTENT(OUT), POINTER :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_logical_1d

SUBROUTINE ftg_allocate_pointer_logical_2d(fieldname, field)
  CHARACTER(LEN=*)              :: fieldname
  LOGICAL, INTENT(OUT), POINTER :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_logical_2d

SUBROUTINE ftg_allocate_pointer_logical_3d(fieldname, field)
  CHARACTER(LEN=*)              :: fieldname
  LOGICAL, INTENT(OUT), POINTER :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_logical_3d


SUBROUTINE ftg_allocate_pointer_logical_4d(fieldname, field)
  CHARACTER(LEN=*)              :: fieldname
  LOGICAL, INTENT(OUT), POINTER :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_logical_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_pointer_bool_1d(fieldname, field)
  CHARACTER(LEN=*)                           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), POINTER :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_bool_1d

SUBROUTINE ftg_allocate_pointer_bool_2d(fieldname, field)
  CHARACTER(LEN=*)                           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), POINTER :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_bool_2d

SUBROUTINE ftg_allocate_pointer_bool_3d(fieldname, field)
  CHARACTER(LEN=*)                           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), POINTER :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_bool_3d

SUBROUTINE ftg_allocate_pointer_bool_4d(fieldname, field)
  CHARACTER(LEN=*)                           :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), POINTER :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_bool_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_pointer_int_1d(fieldname, field)
  CHARACTER(LEN=*)              :: fieldname
  INTEGER, INTENT(OUT), POINTER :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_int_1d

SUBROUTINE ftg_allocate_pointer_int_2d(fieldname, field)
  CHARACTER(LEN=*)              :: fieldname
  INTEGER, INTENT(OUT), POINTER :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_int_2d

SUBROUTINE ftg_allocate_pointer_int_3d(fieldname, field)
  CHARACTER(LEN=*)              :: fieldname
  INTEGER, INTENT(OUT), POINTER :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_int_3d


SUBROUTINE ftg_allocate_pointer_int_4d(fieldname, field)
  CHARACTER(LEN=*)              :: fieldname
  INTEGER, INTENT(OUT), POINTER :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_int_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_pointer_long_1d(fieldname, field)
  CHARACTER(LEN=*)                           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), POINTER :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_long_1d

SUBROUTINE ftg_allocate_pointer_long_2d(fieldname, field)
  CHARACTER(LEN=*)                           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), POINTER :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_long_2d

SUBROUTINE ftg_allocate_pointer_long_3d(fieldname, field)
  CHARACTER(LEN=*)                           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), POINTER :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_long_3d

SUBROUTINE ftg_allocate_pointer_long_4d(fieldname, field)
  CHARACTER(LEN=*)                           :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), POINTER :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_long_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_pointer_float_1d(fieldname, field)
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), POINTER :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_float_1d

SUBROUTINE ftg_allocate_pointer_float_2d(fieldname, field)
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), POINTER :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_float_2d

SUBROUTINE ftg_allocate_pointer_float_3d(fieldname, field)
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), POINTER :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_float_3d


SUBROUTINE ftg_allocate_pointer_float_4d(fieldname, field)
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), POINTER :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_float_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_pointer_double_1d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), POINTER :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_double_1d

SUBROUTINE ftg_allocate_pointer_double_2d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), POINTER :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_double_2d

SUBROUTINE ftg_allocate_pointer_double_3d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), POINTER :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_double_3d

SUBROUTINE ftg_allocate_pointer_double_4d(fieldname, field)
  CHARACTER(LEN=*)                          :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), POINTER :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  ELSE
    NULLIFY(field)
  END IF

END SUBROUTINE ftg_allocate_pointer_double_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_allocatable_logical_1d(fieldname, field)
  CHARACTER(LEN=*)                  :: fieldname
  LOGICAL, INTENT(OUT), ALLOCATABLE :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_logical_1d

SUBROUTINE ftg_allocate_allocatable_logical_2d(fieldname, field)
  CHARACTER(LEN=*)                  :: fieldname
  LOGICAL, INTENT(OUT), ALLOCATABLE :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_logical_2d

SUBROUTINE ftg_allocate_allocatable_logical_3d(fieldname, field)
  CHARACTER(LEN=*)                  :: fieldname
  LOGICAL, INTENT(OUT), ALLOCATABLE :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_logical_3d


SUBROUTINE ftg_allocate_allocatable_logical_4d(fieldname, field)
  CHARACTER(LEN=*)                  :: fieldname
  LOGICAL, INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_logical_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_allocatable_bool_1d(fieldname, field)
  CHARACTER(LEN=*)                               :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), ALLOCATABLE :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_bool_1d

SUBROUTINE ftg_allocate_allocatable_bool_2d(fieldname, field)
  CHARACTER(LEN=*)                               :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), ALLOCATABLE :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_bool_2d

SUBROUTINE ftg_allocate_allocatable_bool_3d(fieldname, field)
  CHARACTER(LEN=*)                               :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), ALLOCATABLE :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_bool_3d

SUBROUTINE ftg_allocate_allocatable_bool_4d(fieldname, field)
  CHARACTER(LEN=*)                               :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_bool_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_allocatable_int_1d(fieldname, field)
  CHARACTER(LEN=*)                  :: fieldname
  INTEGER, INTENT(OUT), ALLOCATABLE :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_int_1d

SUBROUTINE ftg_allocate_allocatable_int_2d(fieldname, field)
  CHARACTER(LEN=*)                  :: fieldname
  INTEGER, INTENT(OUT), ALLOCATABLE :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_int_2d

SUBROUTINE ftg_allocate_allocatable_int_3d(fieldname, field)
  CHARACTER(LEN=*)                  :: fieldname
  INTEGER, INTENT(OUT), ALLOCATABLE :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_int_3d


SUBROUTINE ftg_allocate_allocatable_int_4d(fieldname, field)
  CHARACTER(LEN=*)                  :: fieldname
  INTEGER, INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_int_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_allocatable_long_1d(fieldname, field)
  CHARACTER(LEN=*)                               :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), ALLOCATABLE :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_long_1d

SUBROUTINE ftg_allocate_allocatable_long_2d(fieldname, field)
  CHARACTER(LEN=*)                               :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), ALLOCATABLE :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_long_2d

SUBROUTINE ftg_allocate_allocatable_long_3d(fieldname, field)
  CHARACTER(LEN=*)                               :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), ALLOCATABLE :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_long_3d

SUBROUTINE ftg_allocate_allocatable_long_4d(fieldname, field)
  CHARACTER(LEN=*)                               :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_long_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_allocatable_float_1d(fieldname, field)
  CHARACTER(LEN=*)                             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), ALLOCATABLE :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_float_1d

SUBROUTINE ftg_allocate_allocatable_float_2d(fieldname, field)
  CHARACTER(LEN=*)                             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), ALLOCATABLE :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_float_2d

SUBROUTINE ftg_allocate_allocatable_float_3d(fieldname, field)
  CHARACTER(LEN=*)                             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), ALLOCATABLE :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_float_3d


SUBROUTINE ftg_allocate_allocatable_float_4d(fieldname, field)
  CHARACTER(LEN=*)                             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_float_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_allocate_allocatable_double_1d(fieldname, field)
  CHARACTER(LEN=*)                              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), ALLOCATABLE :: field(:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_double_1d

SUBROUTINE ftg_allocate_allocatable_double_2d(fieldname, field)
  CHARACTER(LEN=*)                              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), ALLOCATABLE :: field(:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_double_2d

SUBROUTINE ftg_allocate_allocatable_double_3d(fieldname, field)
  CHARACTER(LEN=*)                              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), ALLOCATABLE :: field(:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_double_3d

SUBROUTINE ftg_allocate_allocatable_double_4d(fieldname, field)
  CHARACTER(LEN=*)                              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)

  INTEGER, DIMENSION(8) :: bounds

  IF (ftg_field_exists(fieldname)) THEN
    bounds = ftg_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2), bounds(3):bounds(4), bounds(5):bounds(6), bounds(7):bounds(8)))
  END IF

END SUBROUTINE ftg_allocate_allocatable_double_4d

!=============================================================================
!=============================================================================

END MODULE m_ser_ftg
