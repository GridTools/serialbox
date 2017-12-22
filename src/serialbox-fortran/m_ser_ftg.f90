MODULE m_ser_ftg

USE iso_c_binding
USE m_serialize

IMPLICIT NONE

PUBLIC :: ftg_write, ignore_bullshit, ignore_bullshit_max_dim_size, ignore_bullshit_allow_negative_indices

PRIVATE

CHARACTER(LEN=*), PARAMETER :: module_name = 'm_ser_ftg'

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
      ftg_write_bool_4d!, &
!      ftg_write_int_0d, &
!      ftg_write_int_1d, &
!      ftg_write_int_2d, &
!      ftg_write_int_3d, &
!      ftg_write_int_4d, &
!      ftg_write_long_0d, &
!      ftg_write_long_1d, &
!      ftg_write_long_2d, &
!      ftg_write_long_3d, &
!      ftg_write_long_4d, &
!      ftg_write_float_0d, &
!      ftg_write_float_1d, &
!      ftg_write_float_2d, &
!      ftg_write_float_3d, &
!      ftg_write_float_4d, &
!      ftg_write_double_0d, &
!      ftg_write_double_1d, &
!      ftg_write_double_2d, &
!      ftg_write_double_3d, &
!      ftg_write_double_4d
END INTERFACE

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

LOGICAL :: ignore_bullshit = .TRUE.
INTEGER :: ignore_bullshit_max_dim_size = 999999999
LOGICAL :: ignore_bullshit_allow_negative_indices = .FALSE.

TYPE(t_serializer), POINTER :: serializer => NULL()
TYPE(t_savepoint),  POINTER :: savepoint  => NULL()

CONTAINS

!=============================================================================
!=============================================================================

SUBROUTINE ftg_set_serializer_create(directory, prefix, mode, opt_archive)

  CHARACTER(LEN=*), INTENT(IN)    :: directory, prefix
  CHARACTER, INTENT(IN)           :: mode
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: opt_archive

  TYPE(t_serializer), TARGET :: new_serializer

  CALL fs_create_serializer(directory, prefix, mode, new_serializer, opt_archive)

  serializer => new_serializer

END SUBROUTINE ftg_set_serializer_create


SUBROUTINE ftg_set_serializer_existing(new_serializer)

  TYPE(t_serializer), INTENT(IN), TARGET :: new_serializer

  serializer => new_serializer

END SUBROUTINE ftg_set_serializer_existing


SUBROUTINE ftg_unsset_serializer()

  CALL fs_destroy_serializer(serializer)
  serializer => NULL()

END SUBROUTINE ftg_unsset_serializer


TYPE(t_serializer) FUNCTION ftg_get_serializer()

  IF (.NOT. ASSOCIATED(serializer)) THEN
    WRITE(*,*) TRIM(module_name)//" - ERROR: No serializer. Call ftg_set_serializer() first!"
    STOP
  ELSE
    ftg_get_serializer = serializer
  END IF

END FUNCTION ftg_get_serializer

!=============================================================================
!=============================================================================

SUBROUTINE ftg_set_savepoint_create(name)

  CHARACTER(LEN=*), INTENT(IN) :: name

  TYPE(t_savepoint), TARGET :: new_savepoint

  CALL fs_create_savepoint(name, new_savepoint)

  savepoint => new_savepoint

END SUBROUTINE ftg_set_savepoint_create


SUBROUTINE ftg_set_savepoint_existing(new_savepoint)

  TYPE(t_savepoint), INTENT(IN), TARGET :: new_savepoint

  savepoint => new_savepoint

END SUBROUTINE ftg_set_savepoint_existing


SUBROUTINE ftg_unsset_savepoint()

  CALL fs_destroy_savepoint(savepoint)
  savepoint => NULL()

END SUBROUTINE ftg_unsset_savepoint


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

SUBROUTINE ftg_write_logical_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)   :: fieldname
  LOGICAL, INTENT(IN), TARGET    :: field

  LOGICAL, POINTER :: padd
  LOGICAL :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = ASSOCIATED(padd, field)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field)
  END IF

END SUBROUTINE ftg_write_logical_0d

SUBROUTINE ftg_write_logical_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL, INTENT(IN), TARGET                 :: field(:)
  INTEGER, DIMENSION(1), INTENT(IN), OPTIONAL :: lbounds, ubounds

  LOGICAL, POINTER :: padd(:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               (PRESENT(lbounds) .NEQV. PRESENT(ubounds))
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices .AND. PRESENT(lbounds)) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_logical_1d

SUBROUTINE ftg_write_logical_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL, INTENT(IN), TARGET                 :: field(:,:)
  INTEGER, DIMENSION(2), INTENT(IN), OPTIONAL :: lbounds, ubounds

  LOGICAL, POINTER :: padd(:,:)
  LOGICAL :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               (PRESENT(lbounds) .NEQV. PRESENT(ubounds))
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices .AND. PRESENT(lbounds)) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_logical_2d

SUBROUTINE ftg_write_logical_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL, INTENT(IN), TARGET                 :: field(:,:,:)
  INTEGER, DIMENSION(3), INTENT(IN), OPTIONAL :: lbounds, ubounds

  LOGICAL, POINTER :: padd(:,:,:)
  LOGICAL :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               (PRESENT(lbounds) .NEQV. PRESENT(ubounds))
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices .AND. PRESENT(lbounds)) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_logical_3d

SUBROUTINE ftg_write_logical_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  LOGICAL, INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, DIMENSION(4), INTENT(IN), OPTIONAL :: lbounds, ubounds

  LOGICAL, POINTER :: padd(:,:,:,:)
  LOGICAL :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size .OR. &
               (PRESENT(lbounds) .NEQV. PRESENT(ubounds))
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices .AND. PRESENT(lbounds)) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_logical_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_write_bool_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field

  LOGICAL(KIND=C_BOOL), POINTER :: padd
  LOGICAL :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = ASSOCIATED(padd, field)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field)
  END IF

END SUBROUTINE ftg_write_bool_0d

SUBROUTINE ftg_write_bool_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET    :: field(:)
  INTEGER, DIMENSION(1), INTENT(IN), OPTIONAL :: lbounds, ubounds

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:)
  LOGICAL :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               (PRESENT(lbounds) .NEQV. PRESENT(ubounds))
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices .AND. PRESENT(lbounds)) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_bool_1d

SUBROUTINE ftg_write_bool_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET    :: field(:,:)
  INTEGER, DIMENSION(2), INTENT(IN), OPTIONAL :: lbounds, ubounds

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:)
  LOGICAL :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               (PRESENT(lbounds) .NEQV. PRESENT(ubounds))
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices .AND. PRESENT(lbounds)) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_bool_2d

SUBROUTINE ftg_write_bool_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET    :: field(:,:,:)
  INTEGER, DIMENSION(3), INTENT(IN), OPTIONAL :: lbounds, ubounds

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:,:)
  LOGICAL :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               (PRESENT(lbounds) .NEQV. PRESENT(ubounds))
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices .AND. PRESENT(lbounds)) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_bool_3d

SUBROUTINE ftg_write_bool_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET    :: field(:,:,:,:)
  INTEGER, DIMENSION(4), INTENT(IN), OPTIONAL :: lbounds, ubounds

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:,:,:)
  LOGICAL :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd, field) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size .OR. &
               (PRESENT(lbounds) .NEQV. PRESENT(ubounds))
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices .AND. PRESENT(lbounds)) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(ftg_get_serializer(), ftg_get_savepoint(), fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_bool_4d

!=============================================================================
!=============================================================================

END MODULE m_ser_ftg
