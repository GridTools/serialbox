MODULE m_ser_ftg

USE iso_c_binding
USE m_serialize

IMPLICIT NONE

PUBLIC :: ftg_write, ignore_bullshit, ignore_bullshit_max_dim_size, ignore_bullshit_allow_negative_indices

PRIVATE

LOGICAL :: ignore_bullshit = .TRUE.
INTEGER :: ignore_bullshit_max_dim_size = 999999999
LOGICAL :: ignore_bullshit_allow_negative_indices = .FALSE.

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

CONTAINS

SUBROUTINE ftg_write_logical_0d(serializer, savepoint, fieldname, field)
  TYPE(t_serializer), INTENT(IN) :: serializer
  TYPE(t_savepoint) , INTENT(IN) :: savepoint
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
    CALL fs_write_field(serializer, savepoint, fieldname, field)
  END IF

END SUBROUTINE ftg_write_logical_0d

SUBROUTINE ftg_write_logical_1d(serializer, savepoint, fieldname, field, lbounds, ubounds)
  TYPE(t_serializer), INTENT(IN)              :: serializer
  TYPE(t_savepoint) , INTENT(IN)              :: savepoint
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
    CALL fs_write_field(serializer, savepoint, fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_logical_1d

SUBROUTINE ftg_write_logical_2d(serializer, savepoint, fieldname, field, lbounds, ubounds)
  TYPE(t_serializer), INTENT(IN)              :: serializer
  TYPE(t_savepoint) , INTENT(IN)              :: savepoint
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
    CALL fs_write_field(serializer, savepoint, fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_logical_2d

SUBROUTINE ftg_write_logical_3d(serializer, savepoint, fieldname, field, lbounds, ubounds)
  TYPE(t_serializer), INTENT(IN)              :: serializer
  TYPE(t_savepoint) , INTENT(IN)              :: savepoint
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
    CALL fs_write_field(serializer, savepoint, fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_logical_3d

SUBROUTINE ftg_write_logical_4d(serializer, savepoint, fieldname, field, lbounds, ubounds)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
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
    CALL fs_write_field(serializer, savepoint, fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_logical_4d

!=============================================================================
!=============================================================================

SUBROUTINE ftg_write_bool_0d(serializer, savepoint, fieldname, field)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
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
    CALL fs_write_field(serializer, savepoint, fieldname, field)
  END IF

END SUBROUTINE ftg_write_bool_0d

SUBROUTINE ftg_write_bool_1d(serializer, savepoint, fieldname, field, lbounds, ubounds)
  TYPE(t_serializer), INTENT(IN)              :: serializer
  TYPE(t_savepoint) , INTENT(IN)              :: savepoint
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
    CALL fs_write_field(serializer, savepoint, fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_bool_1d

SUBROUTINE ftg_write_bool_2d(serializer, savepoint, fieldname, field, lbounds, ubounds)
  TYPE(t_serializer), INTENT(IN)              :: serializer
  TYPE(t_savepoint) , INTENT(IN)              :: savepoint
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
    CALL fs_write_field(serializer, savepoint, fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_bool_2d

SUBROUTINE ftg_write_bool_3d(serializer, savepoint, fieldname, field, lbounds, ubounds)
  TYPE(t_serializer), INTENT(IN)              :: serializer
  TYPE(t_savepoint) , INTENT(IN)              :: savepoint
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
    CALL fs_write_field(serializer, savepoint, fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_bool_3d

SUBROUTINE ftg_write_bool_4d(serializer, savepoint, fieldname, field, lbounds, ubounds)
  TYPE(t_serializer), INTENT(IN)              :: serializer
  TYPE(t_savepoint) , INTENT(IN)              :: savepoint
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
    CALL fs_write_field(serializer, savepoint, fieldname, field, lbounds, ubounds)
  END IF

END SUBROUTINE ftg_write_bool_4d

!=============================================================================
!=============================================================================

END MODULE m_ser_ftg
