MODULE m_ser_ftg

USE m_serialize

IMPLICIT NONE

PUBLIC :: ftg_write

PRIVATE

INTERFACE ftg_write
    MODULE PROCEDURE &
      ftg_write_double_3d
END INTERFACE

CONTAINS

SUBROUTINE ftg_write_double_3d(serializer, savepoint, fieldname, field, lbounds, ubounds)

  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL           :: lbounds(3), ubounds(3)

  CALL fs_write_field(serializer, savepoint, fieldname, field, lbounds, ubounds)

END SUBROUTINE ftg_write_double_3d

END MODULE m_ser_ftg
