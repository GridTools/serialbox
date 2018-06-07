!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license. 
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------
!
!+ This module contains the Fortran interface of Serialbox.
!
!------------------------------------------------------------------------------

MODULE m_serialize

!------------------------------------------------------------------------------
!
! Description:
!
!   This module contains subroutines which allow to store data on external
!   files on disk and reading those files into fields.
!   The data is written to binary files (e.g NetCDF), while the metadata 
!   (name, dimensions, size of halo, ...) are stored in JSON files.
!
!   These routines are implemented in a C++ and exposed in a C API, this is 
!   a wrapper module.
!
! Current Code Owner: ETH, Andrea Arteaga
!  email:  andrea.arteaga@env.ethz.ch
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!============================================================================

USE iso_c_binding

IMPLICIT NONE

PUBLIC :: &
  t_serializer, t_savepoint, &
  fs_create_serializer, fs_destroy_serializer, fs_serializer_openmode, fs_add_serializer_metainfo, fs_get_serializer_metainfo, &
  fs_create_savepoint, fs_destroy_savepoint, fs_add_savepoint_metainfo, fs_get_savepoint_metainfo, &
  fs_field_exists, fs_register_field, fs_add_field_metainfo, fs_get_field_metainfo, fs_write_field, fs_read_field, &
  fs_enable_serialization, fs_disable_serialization, fs_print_debuginfo, &
  fs_get_size, fs_get_halos, fs_get_rank, fs_get_total_size, &
  fs_boolsize, fs_intsize, fs_longsize, fs_floatsize, fs_doublesize
  INTEGER, PARAMETER :: MODE_READ = 0
  INTEGER, PARAMETER :: MODE_WRITE = 1
  INTEGER, PARAMETER :: MODE_APPEND = 2

  INTEGER, PARAMETER :: MAX_LENGTH_ARCHIVE_NAME = 16


PRIVATE

  INTEGER, PARAMETER :: SERIALBOX_FIELD_TYPE_BOOLEAN = 1
  INTEGER, PARAMETER :: SERIALBOX_FIELD_TYPE_INT32 = 2
  INTEGER, PARAMETER :: SERIALBOX_FIELD_TYPE_INT64 = 3
  INTEGER, PARAMETER :: SERIALBOX_FIELD_TYPE_FLOAT32 = 4
  INTEGER, PARAMETER :: SERIALBOX_FIELD_TYPE_FLOAT64 = 5
  INTEGER, PARAMETER :: SERIALBOX_FIELD_TYPE_STRING = 6

  TYPE :: t_serializer
    TYPE(C_PTR) :: serializer_ptr = C_NULL_PTR
  END TYPE t_serializer

  TYPE :: t_savepoint
    TYPE(C_PTR) :: savepoint_ptr = C_NULL_PTR
  END TYPE t_savepoint

  INTERFACE
     FUNCTION fs_field_exists_(serializer, name) &
          BIND(c, name='serialboxSerializerHasField')
       USE, INTRINSIC :: iso_c_binding
       INTEGER(C_INT)                    :: fs_field_exists_
       TYPE(C_PTR), INTENT(IN), VALUE    :: serializer
       CHARACTER(C_CHAR), DIMENSION(*)   :: name
     END FUNCTION fs_field_exists_
  END INTERFACE

  INTERFACE
     SUBROUTINE fs_write_field_(serializer, savepoint, fieldname, &
                               fielddata, istride, jstride, kstride, lstride) &
          BIND(c, name='serialboxFortranSerializerWrite')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer, savepoint, fielddata
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
       INTEGER(C_INT), INTENT(IN), VALUE    :: istride, jstride, kstride, lstride
     END SUBROUTINE fs_write_field_
  END INTERFACE

  INTERFACE
     SUBROUTINE fs_read_field_(serializer, savepoint, fieldname, &
                               fielddata, istride, jstride, kstride, lstride) &
          BIND(c, name='serialboxFortranSerializerRead')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer, savepoint, fielddata
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
       INTEGER(C_INT), INTENT(IN), VALUE    :: istride, jstride, kstride, lstride
     END SUBROUTINE fs_read_field_
  END INTERFACE

  INTERFACE
     SUBROUTINE fs_compute_strides(serializer, fieldname, field, iplus1, jplus1, kplus1, lplus1, &
                           istride, jstride, kstride, lstride) &
          BIND(c, name='serialboxFortranComputeStrides')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE :: serializer, field, iplus1, jplus1, kplus1, lplus1
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
       INTEGER(C_INT), INTENT(OUT)          :: istride, jstride, kstride, lstride
     END SUBROUTINE fs_compute_strides
  END INTERFACE

  INTERFACE
     SUBROUTINE print_debuginfo(serializer) &
          BIND(c, name='serialboxFortranSerializerPrintDebugInfo')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE :: serializer
     END SUBROUTINE print_debuginfo
  END INTERFACE

  INTERFACE
     SUBROUTINE fs_enable_serialization() &
          BIND(c, name='serialboxEnableSerialization')
       USE, INTRINSIC :: iso_c_binding
     END SUBROUTINE fs_enable_serialization
  END INTERFACE

  INTERFACE
     SUBROUTINE fs_disable_serialization() &
          BIND(c, name='serialboxDisableSerialization')
       USE, INTRINSIC :: iso_c_binding
     END SUBROUTINE fs_disable_serialization
  END INTERFACE

  !==============================================================================
  !+ Module interface to attach metainformation to the given serializer
  !------------------------------------------------------------------------------
  INTERFACE fs_add_serializer_metainfo
    MODULE PROCEDURE &
      fs_add_serializer_metainfo_b, &
      fs_add_serializer_metainfo_i, &
      fs_add_serializer_metainfo_l, &
      fs_add_serializer_metainfo_f, &
      fs_add_serializer_metainfo_d, &
      fs_add_serializer_metainfo_s
  END INTERFACE


  !==============================================================================
  !+ Module interface to get metainformation for the given serializer
  !------------------------------------------------------------------------------
  INTERFACE fs_get_serializer_metainfo
    MODULE PROCEDURE &
      fs_get_serializer_metainfo_b, &
      fs_get_serializer_metainfo_i, &
      fs_get_serializer_metainfo_l, &
      fs_get_serializer_metainfo_f, &
      fs_get_serializer_metainfo_d
      ! TODO fs_get_serializer_metainfo_s
  END INTERFACE


  !==============================================================================
  !+ Module interface to attach metainformation to the given field
  !------------------------------------------------------------------------------
  INTERFACE fs_add_field_metainfo
    MODULE PROCEDURE &
      fs_add_field_metainfo_b, &
      fs_add_field_metainfo_i, &
      fs_add_field_metainfo_l, &
      fs_add_field_metainfo_f, &
      fs_add_field_metainfo_d, &
      fs_add_field_metainfo_s
  END INTERFACE


  !==============================================================================
  !+ Module interface to get metainformation for the given field
  !------------------------------------------------------------------------------
  INTERFACE fs_get_field_metainfo
    MODULE PROCEDURE &
      fs_get_field_metainfo_b, &
      fs_get_field_metainfo_i, &
      fs_get_field_metainfo_l, &
      fs_get_field_metainfo_f, &
      fs_get_field_metainfo_d
      ! TODO fs_get_field_metainfo_s
  END INTERFACE


  !==============================================================================
  !+ Module interface to attach metainformation to the given savepoint
  !------------------------------------------------------------------------------
  INTERFACE fs_add_savepoint_metainfo
    MODULE PROCEDURE &
      fs_add_savepoint_metainfo_b, &
      fs_add_savepoint_metainfo_i, &
      fs_add_savepoint_metainfo_l, &
      fs_add_savepoint_metainfo_f, &
      fs_add_savepoint_metainfo_d, &
      fs_add_savepoint_metainfo_s
  END INTERFACE


  !==============================================================================
  !+ Module interface to get metainformation for the given savepoint
  !------------------------------------------------------------------------------
  INTERFACE fs_get_savepoint_metainfo
    MODULE PROCEDURE &
      fs_get_savepoint_metainfo_b, &
      fs_get_savepoint_metainfo_i, &
      fs_get_savepoint_metainfo_l, &
      fs_get_savepoint_metainfo_f, &
      fs_get_savepoint_metainfo_d
      ! TODO fs_get_savepoint_metainfo_s
  END INTERFACE


  !==============================================================================
  !+ Module interface to store the given field at the given savepoint
  !------------------------------------------------------------------------------
  INTERFACE fs_write_field
    MODULE PROCEDURE &
      fs_write_logical_0d, &
      fs_write_logical_1d, &
      fs_write_logical_2d, &
      fs_write_logical_3d, &
      fs_write_logical_4d, &
      fs_write_bool_0d, &
      fs_write_bool_1d, &
      fs_write_bool_2d, &
      fs_write_bool_3d, &
      fs_write_bool_4d, &
      fs_write_int_0d, &
      fs_write_int_1d, &
      fs_write_int_2d, &
      fs_write_int_3d, &
      fs_write_int_4d, &
      fs_write_long_0d, &
      fs_write_long_1d, &
      fs_write_long_2d, &
      fs_write_long_3d, &
      fs_write_long_4d, &
      fs_write_float_0d, &
      fs_write_float_1d, &
      fs_write_float_2d, &
      fs_write_float_3d, &
      fs_write_float_4d, &
      fs_write_double_0d, &
      fs_write_double_1d, &
      fs_write_double_2d, &
      fs_write_double_3d, &
      fs_write_double_4d
  END INTERFACE


  !==============================================================================
  !+ Module interface to read the given field at the given savepoint
  !------------------------------------------------------------------------------
  INTERFACE fs_read_field
    MODULE PROCEDURE &
      fs_read_logical_0d, &
      fs_read_logical_1d, &
      fs_read_logical_2d, &
      fs_read_logical_3d, &
      fs_read_logical_4d, &
      fs_read_bool_0d, &
      fs_read_bool_1d, &
      fs_read_bool_2d, &
      fs_read_bool_3d, &
      fs_read_bool_4d, &
      fs_read_int_0d, &
      fs_read_int_1d, &
      fs_read_int_2d, &
      fs_read_int_3d, &
      fs_read_int_4d, &
      fs_read_long_0d, &
      fs_read_long_1d, &
      fs_read_long_2d, &
      fs_read_long_3d, &
      fs_read_long_4d, &
      fs_read_float_0d, &
      fs_read_float_1d, &
      fs_read_float_2d, &
      fs_read_float_3d, &
      fs_read_float_4d, &
      fs_read_double_0d, &
      fs_read_double_1d, &
      fs_read_double_2d, &
      fs_read_double_3d, &
      fs_read_double_4d
  END INTERFACE

CONTAINS

!============================================================================

FUNCTION fs_boolsize()
  INTEGER(KIND=C_INT) :: fs_boolsize

  CHARACTER(LEN=1), DIMENSION(128) :: buffer
  LOGICAL(KIND=C_BOOL) :: boolvalue

  fs_boolsize = INT(SIZE(TRANSFER(boolvalue, buffer)))
END FUNCTION fs_boolsize

FUNCTION fs_intsize()
  INTEGER(KIND=C_INT) :: fs_intsize

  CHARACTER(LEN=1), DIMENSION(128) :: buffer
  INTEGER(KIND=C_INT) :: intvalue

  fs_intsize = INT(SIZE(TRANSFER(intvalue, buffer)))
END FUNCTION fs_intsize

FUNCTION fs_longsize()
  INTEGER(KIND=C_INT) :: fs_longsize

  CHARACTER(LEN=1), DIMENSION(128) :: buffer
  INTEGER(KIND=C_LONG) :: intvalue

  fs_longsize = INT(SIZE(TRANSFER(intvalue, buffer)))
END FUNCTION fs_longsize

FUNCTION fs_floatsize()
  INTEGER(KIND=C_INT) :: fs_floatsize

  CHARACTER(LEN=1), DIMENSION(128) :: buffer
  REAL(KIND=C_FLOAT) :: floatvalue

  fs_floatsize = INT(SIZE(TRANSFER(floatvalue, buffer)))
END FUNCTION fs_floatsize

FUNCTION fs_doublesize()
  INTEGER(KIND=C_INT) :: fs_doublesize

  CHARACTER(LEN=1), DIMENSION(128) :: buffer
  REAL(KIND=C_DOUBLE) :: doublevalue

  fs_doublesize = INT(SIZE(TRANSFER(doublevalue, buffer)))
END FUNCTION fs_doublesize


!==============================================================================
!+ Module procedure that creates a new serializer object.
!  The returend serializer should be destroyed after usage to free resources.
!  If the serializer object has already been created, it is destroyed first.
!------------------------------------------------------------------------------
SUBROUTINE fs_create_serializer(directory, prefix, mode, serializer, opt_archive)
  USE iso_c_binding, ONLY: C_F_POINTER

  CHARACTER(LEN=*), INTENT(IN)    :: directory, prefix
  CHARACTER, INTENT(IN)           :: mode
  TYPE(t_serializer), INTENT(OUT) :: serializer
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: opt_archive
  CHARACTER(LEN=MAX_LENGTH_ARCHIVE_NAME) :: archive

  ! external functions

  INTERFACE
    FUNCTION fs_create_serializer_(openmode, directory, prefix, archive) &
         BIND(c, name='serialboxSerializerCreate')
      USE, INTRINSIC :: iso_c_binding
      TYPE(C_PTR)                           :: fs_create_serializer_
      INTEGER(KIND=C_INT), VALUE            :: openmode
      CHARACTER(KIND=C_CHAR), DIMENSION(*)  :: directory, prefix, archive
    END FUNCTION fs_create_serializer_
  END INTERFACE


  ! Local variables
  INTEGER(KIND=C_INT) :: c_mode
  TYPE(C_PTR) :: c_serializer

  ! Destroy any pre-existing serializer
  CALL fs_destroy_serializer(serializer)

  SELECT CASE(mode)
  CASE('r')
    c_mode = MODE_READ
  CASE('w')
    c_mode = MODE_WRITE
  CASE('a')
    c_mode = MODE_APPEND
  END SELECT
  
  IF (PRESENT(opt_archive)) THEN
    archive = opt_archive
  ELSE
    archive = C_CHAR_"Binary"
  END IF

  c_serializer = fs_create_serializer_(c_mode,                       &
                                       TRIM(directory)//C_NULL_CHAR, &
                                       TRIM(prefix)//C_NULL_CHAR,    &
                                       TRIM(archive)//C_NULL_CHAR)
  serializer%serializer_ptr = c_serializer

END SUBROUTINE fs_create_serializer


!==============================================================================
!+ Module procedure that destroys a serializer object.
!  If the serializer has never been created, the function does nothing.
!------------------------------------------------------------------------------
SUBROUTINE fs_destroy_serializer(serializer)

  TYPE(t_serializer), INTENT(INOUT) :: serializer

  ! External function
  INTERFACE
     SUBROUTINE fs_destroy_serializer_(serializer) &
          BIND(c, name='serialboxSerializerDestroy')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE :: serializer
     END SUBROUTINE fs_destroy_serializer_
  END INTERFACE

  ! Distroy only if associated
  IF ( C_ASSOCIATED(serializer%serializer_ptr) ) THEN
    CALL fs_destroy_serializer_(serializer%serializer_ptr)
  ENDIF

  serializer%serializer_ptr = C_NULL_PTR

END SUBROUTINE fs_destroy_serializer


!==============================================================================
!+ Module procedure that returns the open mode of the given serializer.
!------------------------------------------------------------------------------
FUNCTION fs_serializer_openmode(serializer)

  CHARACTER(KIND=C_CHAR)         :: fs_serializer_openmode
  TYPE(t_serializer), INTENT(IN) :: serializer

  ! Old: char fs_serializer_openmode(void* serializer)
  ! New: enum serialboxOpenModeKind serialboxSerializerGetMode(const serialboxSerializer_t* serializer);
  ! External function
  INTERFACE
     FUNCTION fs_serializer_openmode_(serializer) &
          BIND(c, name='serialboxSerializerGetMode')
       USE, INTRINSIC :: iso_c_binding
       INTEGER(KIND=C_INT) :: fs_serializer_openmode_
       TYPE(C_PTR), VALUE  :: serializer
     END FUNCTION fs_serializer_openmode_
  END INTERFACE

  ! Local variables
  INTEGER :: c_mode

  c_mode = fs_serializer_openmode_(serializer%serializer_ptr)

  fs_serializer_openmode = 'r'
  SELECT CASE(c_mode)
  CASE(MODE_READ)
    fs_serializer_openmode = 'r'
  CASE(MODE_WRITE)
    fs_serializer_openmode = 'w'
  CASE(MODE_APPEND)
    fs_serializer_openmode = 'a'
  END SELECT

END FUNCTION fs_serializer_openmode


SUBROUTINE fs_add_serializer_metainfo_b(serializer, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: key
  LOGICAL, VALUE    :: val

  INTERFACE
     SUBROUTINE fs_add_serializer_metainfo_b_(serializer, key, val) &
          BIND(c, name='serialboxFortranSerializerAddMetainfoBoolean')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       LOGICAL(KIND=C_BOOL), VALUE          :: val
     END SUBROUTINE fs_add_serializer_metainfo_b_
  END INTERFACE

  LOGICAL(KIND=C_BOOL) :: c_val
  c_val = val

  CALL fs_add_serializer_metainfo_b_(serializer%serializer_ptr, TRIM(key)//C_NULL_CHAR, c_val)
END SUBROUTINE fs_add_serializer_metainfo_b


SUBROUTINE fs_add_serializer_metainfo_i(serializer, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: key
  INTEGER(C_INT)                 :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_serializer_metainfo_i_(serializer, key, val) &
          BIND(c, name='serialboxFortranSerializerAddMetainfoInt32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       INTEGER(KIND=C_INT), VALUE           :: val
     END SUBROUTINE fs_add_serializer_metainfo_i_
  END INTERFACE

  CALL fs_add_serializer_metainfo_i_(serializer%serializer_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_serializer_metainfo_i


SUBROUTINE fs_add_serializer_metainfo_l(serializer, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: key
  INTEGER(C_LONG)                :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_serializer_metainfo_l_(serializer, key, val) &
          BIND(c, name='serialboxFortranSerializerAddMetainfoInt64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       INTEGER(KIND=C_LONG), VALUE          :: val
     END SUBROUTINE fs_add_serializer_metainfo_l_
  END INTERFACE

  CALL fs_add_serializer_metainfo_l_(serializer%serializer_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_serializer_metainfo_l


SUBROUTINE fs_add_serializer_metainfo_f(serializer, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: key
  REAL(KIND=C_FLOAT)             :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_serializer_metainfo_f_(serializer, key, val) &
          BIND(c, name='serialboxFortranSerializerAddMetainfoFloat32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       REAL(KIND=C_FLOAT), VALUE            :: val
     END SUBROUTINE fs_add_serializer_metainfo_f_
  END INTERFACE

  CALL fs_add_serializer_metainfo_f_(serializer%serializer_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_serializer_metainfo_f


SUBROUTINE fs_add_serializer_metainfo_d(serializer, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: key
  REAL(KIND=C_DOUBLE)            :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_serializer_metainfo_d_(serializer, key, val) &
          BIND(c, name='serialboxFortranSerializerAddMetainfoFloat64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       REAL(KIND=C_DOUBLE), VALUE           :: val
     END SUBROUTINE fs_add_serializer_metainfo_d_
  END INTERFACE

  CALL fs_add_serializer_metainfo_d_(serializer%serializer_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_serializer_metainfo_d


SUBROUTINE fs_add_serializer_metainfo_s(serializer, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: key, val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_serializer_metainfo_s_(serializer, key, val) &
          BIND(c, name='serialboxFortranSerializerAddMetainfoString')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key, val
     END SUBROUTINE fs_add_serializer_metainfo_s_
  END INTERFACE

  CALL fs_add_serializer_metainfo_s_(serializer%serializer_ptr, &
                                     TRIM(key)//C_NULL_CHAR,    &
                                     TRIM(val)//C_NULL_CHAR)
END SUBROUTINE fs_add_serializer_metainfo_s


! Getter have to be subroutines instead of functions for having unique argument lists

SUBROUTINE fs_get_serializer_metainfo_b(serializer, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*), INTENT(IN)   :: key
  LOGICAL, INTENT(OUT)           :: val

  INTERFACE
     SUBROUTINE fs_get_serializer_metainfo_b_(serializer, key, val) &
          BIND(c, name='serialboxFortranSerializerGetMetainfoBoolean')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key
       LOGICAL, INTENT(OUT)                             :: val
     END SUBROUTINE fs_get_serializer_metainfo_b_
  END INTERFACE

  CALL fs_get_serializer_metainfo_b_(serializer%serializer_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_serializer_metainfo_b

SUBROUTINE fs_get_serializer_metainfo_i(serializer, key, val)
  TYPE(t_serializer), INTENT(IN)   :: serializer
  CHARACTER(LEN=*), INTENT(IN)     :: key
  INTEGER(KIND=C_INT), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_serializer_metainfo_i_(serializer, key, val) &
          BIND(c, name='serialboxFortranSerializerGetMetainfoInt32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key
       INTEGER(KIND=C_INT), INTENT(OUT)                 :: val
     END SUBROUTINE fs_get_serializer_metainfo_i_
  END INTERFACE

  CALL fs_get_serializer_metainfo_i_(serializer%serializer_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_serializer_metainfo_i

SUBROUTINE fs_get_serializer_metainfo_l(serializer, key, val)
  TYPE(t_serializer), INTENT(IN)    :: serializer
  CHARACTER(LEN=*), INTENT(IN)      :: key
  INTEGER(KIND=C_LONG), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_serializer_metainfo_l_(serializer, key, val) &
          BIND(c, name='serialboxFortranSerializerGetMetainfoInt64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key
       INTEGER(KIND=C_LONG), INTENT(OUT)                :: val
     END SUBROUTINE fs_get_serializer_metainfo_l_
  END INTERFACE

  CALL fs_get_serializer_metainfo_l_(serializer%serializer_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_serializer_metainfo_l

SUBROUTINE fs_get_serializer_metainfo_f(serializer, key, val)
  TYPE(t_serializer), INTENT(IN)  :: serializer
  CHARACTER(LEN=*), INTENT(IN)    :: key
  REAL(KIND=C_FLOAT), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_serializer_metainfo_f_(serializer, key, val) &
          BIND(c, name='serialboxFortranSerializerGetMetainfoFloat32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key
       REAL(KIND=C_FLOAT), INTENT(OUT)                  :: val
     END SUBROUTINE fs_get_serializer_metainfo_f_
  END INTERFACE

  CALL fs_get_serializer_metainfo_f_(serializer%serializer_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_serializer_metainfo_f

SUBROUTINE fs_get_serializer_metainfo_d(serializer, key, val)
  TYPE(t_serializer), INTENT(IN)   :: serializer
  CHARACTER(LEN=*), INTENT(IN)     :: key
  REAL(KIND=C_DOUBLE), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_serializer_metainfo_d_(serializer, key, val) &
          BIND(c, name='serialboxFortranSerializerGetMetainfoFloat64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key
       REAL(KIND=C_DOUBLE), INTENT(OUT)                 :: val
     END SUBROUTINE fs_get_serializer_metainfo_d_
  END INTERFACE

  CALL fs_get_serializer_metainfo_d_(serializer%serializer_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_serializer_metainfo_d

!=============================================================================
!=============================================================================

SUBROUTINE fs_print_debuginfo(serializer)
  TYPE(t_serializer), INTENT(IN) :: serializer

  ! External function
  INTERFACE
     SUBROUTINE print_debuginfo(serializer) &
          BIND(c, name='serialboxFortranSerializerPrintDebugInfo')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
     END SUBROUTINE print_debuginfo
  END INTERFACE

  CALL print_debuginfo(serializer%serializer_ptr)
END SUBROUTINE fs_print_debuginfo

!=============================================================================
!=============================================================================

!==============================================================================
!+ Module procedure to register a field
!  If the field exists already in the serializer, the function does nothing
!  if the information about the type, the size of the field and of the halo
!  matches with the corresponding information in the serializer. Otherwise,
!  the execution is stopped with an error message.
!------------------------------------------------------------------------------
SUBROUTINE fs_register_field(serializer, fieldname, data_type, bytes_per_element, &
                             isize, jsize, ksize, lsize,                          &
                             iminushalo, iplushalo, jminushalo, jplushalo,        &
                             kminushalo, kplushalo, lminushalo, lplushalo)

  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: fieldname, data_type
  INTEGER, INTENT(IN)            :: bytes_per_element, isize, jsize, ksize, lsize, &
                                    iminushalo, iplushalo, jminushalo, jplushalo,  &
                                    kminushalo, kplushalo, lminushalo, lplushalo

  ! External function
  INTERFACE
     SUBROUTINE fs_register_field_(serializer, fieldname, datatype, bytes_per_element,           &
                                  isize, jsize, ksize, lsize, iminushalo, iplushalo, jminushalo, &
                                  jplushalo, kminushalo, kplushalo, lminushalo, lplushalo) &
          BIND(c, name='serialboxFortranSerializerRegisterField')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE                    :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*)  :: fieldname
       INTEGER(C_INT), VALUE                 :: datatype, bytes_per_element,                  &
                                                isize, jsize, ksize, lsize,                   &
                                                iminushalo, iplushalo, jminushalo, jplushalo, &
                                                kminushalo, kplushalo, lminushalo, lplushalo
     END SUBROUTINE fs_register_field_
  END INTERFACE

  INTEGER :: c_type

  c_type = SERIALBOX_FIELD_TYPE_INT32
  SELECT CASE(data_type)
  CASE('bool')
    c_type = SERIALBOX_FIELD_TYPE_BOOLEAN
  CASE('int')
    c_type = SERIALBOX_FIELD_TYPE_INT32
  CASE('long')
    c_type = SERIALBOX_FIELD_TYPE_INT64
  CASE('float')
    c_type = SERIALBOX_FIELD_TYPE_FLOAT32
  CASE('double')
    c_type = SERIALBOX_FIELD_TYPE_FLOAT64
  END SELECT

  CALL fs_register_field_(serializer%serializer_ptr, TRIM(fieldname)//C_NULL_CHAR, &
                          c_type, bytes_per_element, isize, jsize, ksize, lsize,   &
                          iminushalo, iplushalo, jminushalo, jplushalo,            &
                          kminushalo, kplushalo, lminushalo, lplushalo)

END SUBROUTINE fs_register_field


SUBROUTINE fs_add_field_metainfo_b(serializer, fieldname, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: fieldname, key
  LOGICAL, VALUE    :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_field_metainfo_b_(serializer, fieldname, key, val) &
          BIND(c, name='serialboxFortranSerializerAddFieldMetainfoBoolean')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       INTEGER(KIND=C_INT), VALUE           :: val
     END SUBROUTINE fs_add_field_metainfo_b_
  END INTERFACE

  INTEGER(KIND=C_INT) :: c_val
  IF (val) THEN
    c_val = 1
  ELSE
    c_val = 0
  ENDIF

  CALL fs_add_field_metainfo_b_(serializer%serializer_ptr,    &
                                TRIM(fieldname)//C_NULL_CHAR, &
                                TRIM(key)//C_NULL_CHAR, c_val)
END SUBROUTINE fs_add_field_metainfo_b


SUBROUTINE fs_add_field_metainfo_i(serializer, fieldname, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: fieldname, key
  INTEGER(C_INT), VALUE          :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_field_metainfo_i_(serializer, fieldname, key, val) &
          BIND(c, name='serialboxFortranSerializerAddFieldMetainfoInt32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       INTEGER(KIND=C_INT), VALUE           :: val
     END SUBROUTINE fs_add_field_metainfo_i_
  END INTERFACE

  CALL fs_add_field_metainfo_i_(serializer%serializer_ptr,    &
                                TRIM(fieldname)//C_NULL_CHAR, &
                                TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_field_metainfo_i


SUBROUTINE fs_add_field_metainfo_l(serializer, fieldname, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: fieldname, key
  INTEGER(C_LONG), VALUE          :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_field_metainfo_l_(serializer, fieldname, key, val) &
          BIND(c, name='serialboxFortranSerializerAddFieldMetainfoInt64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       INTEGER(KIND=C_LONG), VALUE           :: val
     END SUBROUTINE fs_add_field_metainfo_l_
  END INTERFACE

  CALL fs_add_field_metainfo_l_(serializer%serializer_ptr,    &
                                TRIM(fieldname)//C_NULL_CHAR, &
                                TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_field_metainfo_l


SUBROUTINE fs_add_field_metainfo_f(serializer, fieldname, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: fieldname, key
  REAL(KIND=C_FLOAT), VALUE      :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_field_metainfo_f_(serializer, fieldname, key, val) &
          BIND(c, name='serialboxFortranSerializerAddFieldMetainfoFloat32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       REAL(KIND=C_FLOAT), VALUE            :: val
     END SUBROUTINE fs_add_field_metainfo_f_
  END INTERFACE

  CALL fs_add_field_metainfo_f_(serializer%serializer_ptr,    &
                                TRIM(fieldname)//C_NULL_CHAR, &
                                TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_field_metainfo_f


SUBROUTINE fs_add_field_metainfo_d(serializer, fieldname, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: fieldname, key
  REAL(KIND=C_DOUBLE), VALUE      :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_field_metainfo_d_(serializer, fieldname, key, val) &
          BIND(c, name='serialboxFortranSerializerAddFieldMetainfoFloat64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       REAL(KIND=C_DOUBLE), VALUE           :: val
     END SUBROUTINE fs_add_field_metainfo_d_
  END INTERFACE

  CALL fs_add_field_metainfo_d_(serializer%serializer_ptr,    &
                                TRIM(fieldname)//C_NULL_CHAR, &
                                TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_field_metainfo_d


SUBROUTINE fs_add_field_metainfo_s(serializer, fieldname, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*)               :: fieldname, key, val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_field_metainfo_s_(serializer, fieldname, key, val) &
          BIND(c, name='serialboxFortranSerializerAddFieldMetainfoString')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: fieldname
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: val
     END SUBROUTINE fs_add_field_metainfo_s_
  END INTERFACE

  CALL fs_add_field_metainfo_s_(serializer%serializer_ptr,    &
                                TRIM(fieldname)//C_NULL_CHAR, &
                                TRIM(key)//C_NULL_CHAR,       &
                                TRIM(val)//C_NULL_CHAR)
END SUBROUTINE fs_add_field_metainfo_s


! Getter have to be subroutines instead of functions for having unique argument lists

SUBROUTINE fs_get_field_metainfo_b(serializer, fieldname, key, val)
  TYPE(t_serializer), INTENT(IN) :: serializer
  CHARACTER(LEN=*), INTENT(IN)   :: fieldname, key
  LOGICAL, INTENT(OUT)           :: val

  INTERFACE
     SUBROUTINE fs_get_field_metainfo_b_(serializer, fieldname, key, val) &
          BIND(c, name='serialboxFortranSerializerGetFieldMetainfoBoolean')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: fieldname, key
       LOGICAL, INTENT(OUT)                             :: val
     END SUBROUTINE fs_get_field_metainfo_b_
  END INTERFACE

  CALL fs_get_field_metainfo_b_(serializer%serializer_ptr, TRIM(fieldname)//C_NULL_CHAR, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_field_metainfo_b

SUBROUTINE fs_get_field_metainfo_i(serializer, fieldname, key, val)
  TYPE(t_serializer), INTENT(IN)   :: serializer
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname, key
  INTEGER(KIND=C_INT), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_field_metainfo_i_(serializer, fieldname, key, val) &
          BIND(c, name='serialboxFortranSerializerGetFieldMetainfoInt32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: fieldname, key
       INTEGER(KIND=C_INT), INTENT(OUT)                 :: val
     END SUBROUTINE fs_get_field_metainfo_i_
  END INTERFACE

  CALL fs_get_field_metainfo_i_(serializer%serializer_ptr, TRIM(fieldname)//C_NULL_CHAR, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_field_metainfo_i

SUBROUTINE fs_get_field_metainfo_l(serializer, fieldname, key, val)
  TYPE(t_serializer), INTENT(IN)    :: serializer
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname, key
  INTEGER(KIND=C_LONG), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_field_metainfo_l_(serializer, fieldname, key, val) &
          BIND(c, name='serialboxFortranSerializerGetFieldMetainfoInt64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: fieldname, key
       INTEGER(KIND=C_LONG), INTENT(OUT)                :: val
     END SUBROUTINE fs_get_field_metainfo_l_
  END INTERFACE

  CALL fs_get_field_metainfo_l_(serializer%serializer_ptr, TRIM(fieldname)//C_NULL_CHAR, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_field_metainfo_l

SUBROUTINE fs_get_field_metainfo_f(serializer, fieldname, key, val)
  TYPE(t_serializer), INTENT(IN)  :: serializer
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname, key
  REAL(KIND=C_FLOAT), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_field_metainfo_f_(serializer, fieldname, key, val) &
          BIND(c, name='serialboxFortranSerializerGetFieldMetainfoFloat32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: fieldname, key
       REAL(KIND=C_FLOAT), INTENT(OUT)                  :: val
     END SUBROUTINE fs_get_field_metainfo_f_
  END INTERFACE

  CALL fs_get_field_metainfo_f_(serializer%serializer_ptr, TRIM(fieldname)//C_NULL_CHAR, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_field_metainfo_f

SUBROUTINE fs_get_field_metainfo_d(serializer, fieldname, key, val)
  TYPE(t_serializer), INTENT(IN)   :: serializer
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname, key
  REAL(KIND=C_DOUBLE), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_field_metainfo_d_(serializer, fieldname, key, val) &
          BIND(c, name='serialboxFortranSerializerGetFieldMetainfoFloat64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: serializer
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: fieldname, key
       REAL(KIND=C_DOUBLE), INTENT(OUT)                 :: val
     END SUBROUTINE fs_get_field_metainfo_d_
  END INTERFACE

  CALL fs_get_field_metainfo_d_(serializer%serializer_ptr, TRIM(fieldname)//C_NULL_CHAR, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_field_metainfo_d

!=============================================================================
!=============================================================================


!==============================================================================
!+ Module procedure the checks if the field is registered in the serializer.
!------------------------------------------------------------------------------
FUNCTION fs_field_exists(serializer, fieldname)
  TYPE(t_serializer) :: serializer
  CHARACTER(LEN=*)   :: fieldname
  LOGICAL            :: fs_field_exists


  fs_field_exists = fs_field_exists_(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR) > 0

END FUNCTION fs_field_exists

!==============================================================================
!+ Module procedure that checks that the size of the requested field is
!  consistent with what the serializer has.
!  If the sizes are not consistent, as error message is printed and the
!  execution is stopped.
!  If the field is not registered and the serializer is open in write or
!  append mode, the field is automatically registered with the given sizes.
!------------------------------------------------------------------------------
SUBROUTINE fs_check_size(serializer, fieldname, data_type, bytes_per_element, isize, jsize, ksize, lsize, minushalos, plushalos)
  TYPE(t_serializer) :: serializer
  CHARACTER(LEN=*)   :: fieldname, data_type
  INTEGER            :: bytes_per_element, isize, jsize, ksize, lsize
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(:), plushalos(:)
  INTEGER            :: iminushalo, iplushalo, jminushalo, jplushalo, kminushalo, kplushalo, lminushalo, lplushalo

  ! External functions
  INTERFACE
    SUBROUTINE fs_check_field(serializer, name, fieldtype, isize, jsize, ksize, lsize) &
         BIND(c, name='serialboxFortranSerializerCheckField')
      USE, INTRINSIC :: iso_c_binding
      TYPE(C_PTR), INTENT(IN), VALUE        :: serializer
      CHARACTER(KIND=C_CHAR), DIMENSION(*)  :: name
      INTEGER(C_INT), INTENT(IN)            :: fieldtype, isize, jsize, ksize, lsize
    END SUBROUTINE fs_check_field

  END INTERFACE
  
  ! Local variables
  INTEGER(KIND=C_INT) :: c_type

  c_type = SERIALBOX_FIELD_TYPE_INT32
  SELECT CASE(data_type)
  CASE('bool')
    c_type = SERIALBOX_FIELD_TYPE_BOOLEAN
  CASE('int')
    c_type = SERIALBOX_FIELD_TYPE_INT32
  CASE('long')
    c_type = SERIALBOX_FIELD_TYPE_INT64
  CASE('float')
    c_type = SERIALBOX_FIELD_TYPE_FLOAT32
  CASE('double')
    c_type = SERIALBOX_FIELD_TYPE_FLOAT64
  END SELECT
  
  ! If it does, do checks
  IF (fs_field_exists(serializer, fieldname)) THEN
    CALL fs_check_field(serializer%serializer_ptr, TRIM(fieldname)//C_NULL_CHAR, c_type, &
                        isize, jsize, ksize, lsize)

  ! Else register field
  ELSE IF(fs_serializer_openmode(serializer) /= 'r') THEN
    iminushalo = 0
    iplushalo = 0
    jminushalo = 0
    jplushalo = 0
    kminushalo = 0
    kplushalo = 0
    lminushalo = 0
    lplushalo = 0

    IF (PRESENT(minushalos)) THEN
      IF (SIZE(minushalos) > 0) THEN
        iminushalo = minushalos(1)
        IF (SIZE(minushalos) > 1) THEN
          jminushalo = minushalos(2)
          IF (SIZE(minushalos) > 2) THEN
            kminushalo = minushalos(3)
            IF (SIZE(minushalos) > 3) THEN
              lminushalo = minushalos(4)
            END IF
          END IF
        END IF
      END IF
    END IF

    IF (PRESENT(plushalos)) THEN
      IF (SIZE(plushalos) > 0) THEN
        iplushalo = plushalos(1)
        IF (SIZE(plushalos) > 1) THEN
          jplushalo = plushalos(2)
          IF (SIZE(plushalos) > 2) THEN
            kplushalo = plushalos(3)
            IF (SIZE(plushalos) > 3) THEN
              lplushalo = plushalos(4)
            END IF
          END IF
        END IF
      END IF
    END IF

    CALL fs_register_field(serializer, fieldname, data_type, bytes_per_element, &
                           isize, jsize, ksize, lsize, &
                           iminushalo, iplushalo, jminushalo, jplushalo, kminushalo, kplushalo, lminushalo, lplushalo)
  ELSE
    WRITE(*,*) "Serialbox: ERROR: field ", fieldname, " does not exist in the serializer"
    STOP
  END IF

END SUBROUTINE fs_check_size

!==============================================================================
!+ Module function that returns the rank of the requested field
!  For both, scalars and 1-dimensional arrays, the result is 1.
!------------------------------------------------------------------------------
FUNCTION fs_get_rank(serializer, fieldname)
  TYPE(t_serializer) :: serializer
  CHARACTER(LEN=*)   :: fieldname
  INTEGER            :: fs_get_rank

  INTERFACE
    SUBROUTINE fs_get_rank_(serializer, name, rank) &
        BIND(c, name='serialboxFortranSerializerGetFieldRank')
     USE, INTRINSIC :: iso_c_binding
     TYPE(C_PTR), VALUE                    :: serializer
     CHARACTER(KIND=C_CHAR), DIMENSION(*)  :: name
     INTEGER(C_INT), INTENT(OUT)           :: rank
    END SUBROUTINE fs_get_rank_
  END INTERFACE

  IF (fs_field_exists(serializer, fieldname)) THEN
    CALL fs_get_rank_(serializer%serializer_ptr, TRIM(fieldname)//C_NULL_CHAR, fs_get_rank)
  ELSE
    WRITE(*,*) "Serialbox: ERROR: field ", fieldname, " does not exist in the serializer"
    STOP
  END IF

END FUNCTION fs_get_rank

!==============================================================================
!+ Module function that returns the size of the requested field
!  Always returns an array with 4 elements.
!  For fields with a rank less than 4, the upper dimensions are given with size 0.
!  For scalars the result is {1,0,0,0}.
!------------------------------------------------------------------------------
FUNCTION fs_get_size(serializer, fieldname)
  TYPE(t_serializer)    :: serializer
  CHARACTER(LEN=*)      :: fieldname
  INTEGER, DIMENSION(4) :: fs_get_size

  INTERFACE
    SUBROUTINE fs_get_field_dimensions_(serializer, name, isize, jsize, ksize, lsize) &
        BIND(c, name='serialboxFortranSerializerGetFieldDimensions')
     USE, INTRINSIC :: iso_c_binding
     TYPE(C_PTR), VALUE                    :: serializer
     CHARACTER(KIND=C_CHAR), DIMENSION(*)  :: name
     INTEGER(C_INT), INTENT(OUT)           :: isize, jsize, ksize, lsize
    END SUBROUTINE fs_get_field_dimensions_
  END INTERFACE

  INTEGER(KIND=C_INT) :: isize, jsize, ksize, lsize

  IF (fs_field_exists(serializer, fieldname)) THEN
    CALL fs_get_field_dimensions_(serializer%serializer_ptr, TRIM(fieldname)//C_NULL_CHAR, isize, jsize, ksize, lsize)
    fs_get_size = (/ isize, jsize, ksize, lsize /)
  ELSE
    WRITE(*,*) "Serialbox: ERROR: field ", fieldname, " does not exist in the serializer"
    STOP
  END IF

END FUNCTION fs_get_size

!==============================================================================
!+ Module function that returns the total size of the requested field,
!  that is the product of the sizes of the individual dimensions
!  For scalars the result is 1.
!------------------------------------------------------------------------------
FUNCTION fs_get_total_size(serializer, fieldname)
  TYPE(t_serializer)    :: serializer
  CHARACTER(LEN=*)      :: fieldname
  INTEGER :: fs_get_total_size

  INTEGER :: d, sizes(4)

  sizes = fs_get_size(serializer, fieldname)
  fs_get_total_size = 1
  DO d =  1, fs_get_rank(serializer, fieldname)
      fs_get_total_size = fs_get_total_size * sizes(d)
  END DO

END FUNCTION fs_get_total_size


!==============================================================================
!+ Module function that returns the halos of the requested field
!  Always returns an array with 8 elements, containing the values
!  iminushalo, iplushalo, jminushalo, jplushalo, kminushalo, kplushalo, lminushalo, lplushalo
!  All non-applicable dimensions are given with 0.
!------------------------------------------------------------------------------
FUNCTION fs_get_halos(serializer, fieldname)
  TYPE(t_serializer)    :: serializer
  CHARACTER(LEN=*)      :: fieldname
  INTEGER, DIMENSION(8) :: fs_get_halos

  INTERFACE
    SUBROUTINE fs_get_halos_(serializer, name, &
                             iminushalo, iplushalo, jminushalo, jplushalo, kminushalo, kplushalo, lminushalo, lplushalo) &
        BIND(c, name='serialboxFortranSerializerGetFieldHalos')
     USE, INTRINSIC :: iso_c_binding
     TYPE(C_PTR), VALUE                    :: serializer
     CHARACTER(KIND=C_CHAR), DIMENSION(*)  :: name
     INTEGER(C_INT), INTENT(OUT)           :: iminushalo, iplushalo, jminushalo, jplushalo, &
                                              kminushalo, kplushalo, lminushalo, lplushalo
    END SUBROUTINE fs_get_halos_
  END INTERFACE

  INTEGER(KIND=C_INT) :: iminushalo, iplushalo, jminushalo, jplushalo, kminushalo, kplushalo, lminushalo, lplushalo

  IF (fs_field_exists(serializer, fieldname)) THEN
    CALL fs_get_halos_(serializer%serializer_ptr, TRIM(fieldname)//C_NULL_CHAR, &
                       iminushalo, iplushalo, jminushalo, jplushalo, kminushalo, kplushalo, lminushalo, lplushalo)
    fs_get_halos = (/ iminushalo, iplushalo, jminushalo, jplushalo, kminushalo, kplushalo, lminushalo, lplushalo /)
  ELSE
    WRITE(*,*) "Serialbox: ERROR: field ", fieldname, " does not exist in the serializer"
    STOP
  END IF

END FUNCTION fs_get_halos

!=============================================================================
!=============================================================================

!==============================================================================
!+ Module procedure that creates a new savepoint object
!  The returend savepoint should be destroyed after usage to free resources.
!  If the savepoint object has already been created, it is destroyed first.
!------------------------------------------------------------------------------
SUBROUTINE fs_create_savepoint(savepointname, savepoint)

  CHARACTER(LEN=*), INTENT(IN)   :: savepointname
  TYPE(t_savepoint), INTENT(OUT) :: savepoint

  ! External function
  INTERFACE
     FUNCTION fs_create_savepoint_(name) &
          BIND(c, name='serialboxSavepointCreate')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR)                           :: fs_create_savepoint_
       CHARACTER(KIND=C_CHAR), DIMENSION(*)  :: name
     END FUNCTION fs_create_savepoint_
  END INTERFACE

  ! Destroy pre-existing savepoint object
  CALL fs_destroy_savepoint(savepoint)

  savepoint%savepoint_ptr = fs_create_savepoint_(TRIM(savepointname)//C_NULL_CHAR)

END SUBROUTINE fs_create_savepoint


!==============================================================================
!+ Module procedure that destroys a savepoint object.
!  If the savepoint has never been created, the function does nothing.
!------------------------------------------------------------------------------
SUBROUTINE fs_destroy_savepoint(savepoint)

  TYPE(t_savepoint), INTENT(INOUT) :: savepoint

  ! External function
  INTERFACE
     SUBROUTINE fs_destroy_savepoint_(savepoint) &
          BIND(c, name='serialboxSavepointDestroy')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), VALUE :: savepoint
     END SUBROUTINE fs_destroy_savepoint_
  END INTERFACE

  ! Destroy savepoint object only if associated
  IF (C_ASSOCIATED(savepoint%savepoint_ptr)) THEN
    CALL fs_destroy_savepoint_(savepoint%savepoint_ptr)
  ENDIF

  savepoint%savepoint_ptr = C_NULL_PTR

END SUBROUTINE fs_destroy_savepoint


SUBROUTINE fs_add_savepoint_metainfo_b(savepoint, key, val)
  TYPE(t_savepoint), INTENT(IN) :: savepoint
  CHARACTER(LEN=*)               :: key
  LOGICAL, VALUE    :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_savepoint_metainfo_b_(savepoint, key, val) &
          BIND(c, name='serialboxFortranSavepointAddMetainfoBoolean')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: savepoint
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       INTEGER(KIND=C_INT), VALUE           :: val
     END SUBROUTINE fs_add_savepoint_metainfo_b_
  END INTERFACE

  INTEGER(KIND=C_INT) :: c_val
  IF (val) THEN
    c_val = 1
  ELSE
    c_val = 0
  ENDIF

  CALL fs_add_savepoint_metainfo_b_(savepoint%savepoint_ptr,      &
                                    TRIM(key)//C_NULL_CHAR, c_val)
END SUBROUTINE fs_add_savepoint_metainfo_b


SUBROUTINE fs_add_savepoint_metainfo_i(savepoint, key, val)
  TYPE(t_savepoint), INTENT(IN) :: savepoint
  CHARACTER(LEN=*)              :: key
  INTEGER(KIND=C_INT)           :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_savepoint_metainfo_i_(savepoint, key, val) &
          BIND(c, name='serialboxFortranSavepointAddMetainfoInt32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: savepoint
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       INTEGER(KIND=C_INT), VALUE           :: val
     END SUBROUTINE fs_add_savepoint_metainfo_i_
  END INTERFACE

  CALL fs_add_savepoint_metainfo_i_(savepoint%savepoint_ptr,      &
                                    TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_savepoint_metainfo_i


SUBROUTINE fs_add_savepoint_metainfo_l(savepoint, key, val)
  TYPE(t_savepoint), INTENT(IN) :: savepoint
  CHARACTER(LEN=*)              :: key
  INTEGER(KIND=C_LONG)           :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_savepoint_metainfo_l_(savepoint, key, val) &
          BIND(c, name='serialboxFortranSavepointAddMetainfoInt64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: savepoint
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       INTEGER(KIND=C_LONG), VALUE           :: val
     END SUBROUTINE fs_add_savepoint_metainfo_l_
  END INTERFACE

  CALL fs_add_savepoint_metainfo_l_(savepoint%savepoint_ptr,      &
                                    TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_savepoint_metainfo_l


SUBROUTINE fs_add_savepoint_metainfo_f(savepoint, key, val)
  TYPE(t_savepoint), INTENT(IN) :: savepoint
  CHARACTER(LEN=*)               :: key
  REAL(KIND=C_FLOAT)             :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_savepoint_metainfo_f_(savepoint, key, val) &
          BIND(c, name='serialboxFortranSavepointAddMetainfoFloat32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: savepoint
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       REAL(KIND=C_FLOAT), VALUE            :: val
     END SUBROUTINE fs_add_savepoint_metainfo_f_
  END INTERFACE

  CALL fs_add_savepoint_metainfo_f_(savepoint%savepoint_ptr,      &
                                    TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_savepoint_metainfo_f


SUBROUTINE fs_add_savepoint_metainfo_d(savepoint, key, val)
  TYPE(t_savepoint), INTENT(IN) :: savepoint
  CHARACTER(LEN=*)               :: key
  REAL(KIND=C_DOUBLE)            :: val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_savepoint_metainfo_d_(savepoint, key, val) &
          BIND(c, name='serialboxFortranSavepointAddMetainfoFloat64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: savepoint
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key
       REAL(KIND=C_DOUBLE), VALUE           :: val
     END SUBROUTINE fs_add_savepoint_metainfo_d_
  END INTERFACE

  CALL fs_add_savepoint_metainfo_d_(savepoint%savepoint_ptr,      &
                                    TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_add_savepoint_metainfo_d


SUBROUTINE fs_add_savepoint_metainfo_s(savepoint, key, val)
  TYPE(t_savepoint), INTENT(IN) :: savepoint
  CHARACTER(LEN=*)               :: key, val

  ! External function
  INTERFACE
     SUBROUTINE fs_add_savepoint_metainfo_s_(savepoint, key, val) &
          BIND(c, name='serialboxFortranSavepointAddMetainfoString')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE       :: savepoint
       CHARACTER(KIND=C_CHAR), DIMENSION(*) :: key, val
     END SUBROUTINE fs_add_savepoint_metainfo_s_
  END INTERFACE

  CALL fs_add_savepoint_metainfo_s_(savepoint%savepoint_ptr,      &
                                    TRIM(key)//C_NULL_CHAR,       &
                                    TRIM(val)//C_NULL_CHAR)
END SUBROUTINE fs_add_savepoint_metainfo_s

! Getter have to be subroutines instead of functions for having unique argument lists

SUBROUTINE fs_get_savepoint_metainfo_b(savepoint, key, val)
  TYPE(t_savepoint), INTENT(IN) :: savepoint
  CHARACTER(LEN=*), INTENT(IN)   :: key
  LOGICAL, INTENT(OUT)           :: val

  INTERFACE
     SUBROUTINE fs_get_savepoint_metainfo_b_(savepoint, key, val) &
          BIND(c, name='serialboxFortranSavepointGetMetainfoBoolean')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: savepoint
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key
       LOGICAL, INTENT(OUT)                             :: val
     END SUBROUTINE fs_get_savepoint_metainfo_b_
  END INTERFACE

  CALL fs_get_savepoint_metainfo_b_(savepoint%savepoint_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_savepoint_metainfo_b

SUBROUTINE fs_get_savepoint_metainfo_i(savepoint, key, val)
  TYPE(t_savepoint), INTENT(IN)    :: savepoint
  CHARACTER(LEN=*), INTENT(IN)     :: key
  INTEGER(KIND=C_INT), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_savepoint_metainfo_i_(savepoint, key, val) &
          BIND(c, name='serialboxFortranSavepointGetMetainfoInt32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: savepoint
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key
       INTEGER(KIND=C_INT), INTENT(OUT)                 :: val
     END SUBROUTINE fs_get_savepoint_metainfo_i_
  END INTERFACE

  CALL fs_get_savepoint_metainfo_i_(savepoint%savepoint_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_savepoint_metainfo_i

SUBROUTINE fs_get_savepoint_metainfo_l(savepoint, key, val)
  TYPE(t_savepoint), INTENT(IN)    :: savepoint
  CHARACTER(LEN=*), INTENT(IN)     :: key
  INTEGER(KIND=C_LONG), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_savepoint_metainfo_l_(savepoint, key, val) &
          BIND(c, name='serialboxFortranSavepointGetMetainfoInt64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: savepoint
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key
       INTEGER(KIND=C_LONG), INTENT(OUT)                 :: val
     END SUBROUTINE fs_get_savepoint_metainfo_l_
  END INTERFACE

  CALL fs_get_savepoint_metainfo_l_(savepoint%savepoint_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_savepoint_metainfo_l

SUBROUTINE fs_get_savepoint_metainfo_f(savepoint, key, val)
  TYPE(t_savepoint), INTENT(IN)  :: savepoint
  CHARACTER(LEN=*), INTENT(IN)    :: key
  REAL(KIND=C_FLOAT), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_savepoint_metainfo_f_(savepoint, key, val) &
          BIND(c, name='serialboxFortranSavepointGetMetainfoFloat32')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: savepoint
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key
       REAL(KIND=C_FLOAT), INTENT(OUT)                  :: val
     END SUBROUTINE fs_get_savepoint_metainfo_f_
  END INTERFACE

  CALL fs_get_savepoint_metainfo_f_(savepoint%savepoint_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_savepoint_metainfo_f

SUBROUTINE fs_get_savepoint_metainfo_d(savepoint, key, val)
  TYPE(t_savepoint), INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)     :: key
  REAL(KIND=C_DOUBLE), INTENT(OUT) :: val

  INTERFACE
     SUBROUTINE fs_get_savepoint_metainfo_d_(savepoint, key, val) &
          BIND(c, name='serialboxFortranSavepointGetMetainfoFloat64')
       USE, INTRINSIC :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE                   :: savepoint
       CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: key
       REAL(KIND=C_DOUBLE), INTENT(OUT)                 :: val
     END SUBROUTINE fs_get_savepoint_metainfo_d_
  END INTERFACE

  CALL fs_get_savepoint_metainfo_d_(savepoint%savepoint_ptr, TRIM(key)//C_NULL_CHAR, val)
END SUBROUTINE fs_get_savepoint_metainfo_d

!=============================================================================
!=============================================================================

SUBROUTINE fs_write_logical_0d(serializer, savepoint, fieldname, field)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  LOGICAL, INTENT(IN), TARGET :: field

  ! Local variables
  LOGICAL(KIND=C_BOOL) :: bool

  bool = field
  CALL fs_write_field(serializer, savepoint, fieldname, bool)

END SUBROUTINE fs_write_logical_0d


SUBROUTINE fs_write_logical_1d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  LOGICAL, INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(1), plushalos(1)

  ! Local variables
  LOGICAL(KIND=C_BOOL), ALLOCATABLE :: bool(:)

  ALLOCATE(bool(SIZE(field, 1)))
  bool = field
  CALL fs_write_field(serializer, savepoint, fieldname, bool, minushalos, plushalos)

END SUBROUTINE fs_write_logical_1d


SUBROUTINE fs_write_logical_2d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  LOGICAL, INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(2), plushalos(2)

  ! Local variables
  LOGICAL(KIND=C_BOOL), ALLOCATABLE :: bool(:,:)

  ALLOCATE(bool(SIZE(field, 1), SIZE(field, 2)))
  bool = field
  CALL fs_write_field(serializer, savepoint, fieldname, bool, minushalos, plushalos)

END SUBROUTINE fs_write_logical_2d


SUBROUTINE fs_write_logical_3d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  LOGICAL, INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(3), plushalos(3)

  ! Local variables
  LOGICAL(KIND=C_BOOL), ALLOCATABLE :: bool(:,:,:)

  ALLOCATE(bool(SIZE(field, 1), SIZE(field, 2), SIZE(field, 3)))
  bool = field
  CALL fs_write_field(serializer, savepoint, fieldname, bool, minushalos, plushalos)

END SUBROUTINE fs_write_logical_3d


SUBROUTINE fs_write_logical_4d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  LOGICAL, INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(4), plushalos(4)

  ! Local variables
  LOGICAL(KIND=C_BOOL), ALLOCATABLE :: bool(:,:,:,:)

  ALLOCATE(bool(SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), SIZE(field, 4)))
  bool = field
  CALL fs_write_field(serializer, savepoint, fieldname, bool, minushalos, plushalos)

END SUBROUTINE fs_write_logical_4d

!=============================================================================
!=============================================================================

SUBROUTINE fs_write_bool_0d(serializer, savepoint, fieldname, field)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  LOGICAL(KIND=C_BOOL), POINTER :: padd

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "bool", fs_boolsize(), 1, 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), istride, -1, -1, -1)
END SUBROUTINE fs_write_bool_0d


SUBROUTINE fs_write_bool_1d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(1), plushalos(1)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  LOGICAL(KIND=C_BOOL), POINTER :: padd(:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "bool", fs_boolsize(), SIZE(field, 1), 0, 0, 0, minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)))), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1)), istride, -1, -1, -1)
END SUBROUTINE fs_write_bool_1d


SUBROUTINE fs_write_bool_2d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(2), plushalos(2)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "bool", fs_boolsize(), SIZE(field, 1), SIZE(field, 2), 0, 0, minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)))), &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1)), istride, jstride, -1, -1)
END SUBROUTINE fs_write_bool_2d


SUBROUTINE fs_write_bool_3d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(3), plushalos(3)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "bool", fs_boolsize(), SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), 0, &
                                                                   minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)))), &
                       C_LOC(padd(1, 1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1)), istride, jstride, kstride, -1)
END SUBROUTINE fs_write_bool_3d


SUBROUTINE fs_write_bool_4d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(4), plushalos(4)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "bool", fs_boolsize(), SIZE(field, 1), SIZE(field, 2), &
                                                                   SIZE(field, 3), SIZE(field, 4), minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1, 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)), 1)), &
                       C_LOC(padd(1, 1, 1, MIN(2, SIZE(field, 4)))), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1,1)), istride, jstride, kstride, lstride)
END SUBROUTINE fs_write_bool_4d

!=============================================================================
!=============================================================================

SUBROUTINE fs_write_int_0d(serializer, savepoint, fieldname, field)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  INTEGER(KIND=C_INT), INTENT(IN), TARGET :: field

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_INT), POINTER :: padd

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "int", fs_intsize(), 1, 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), istride, -1, -1, -1)
END SUBROUTINE fs_write_int_0d


SUBROUTINE fs_write_int_1d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  INTEGER(KIND=C_INT), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(1), plushalos(1)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_INT), POINTER :: padd(:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "int", fs_intsize(), SIZE(field, 1), 0, 0, 0, minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)))), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1)), istride, -1, -1, -1)
END SUBROUTINE fs_write_int_1d


SUBROUTINE fs_write_int_2d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  INTEGER(KIND=C_INT), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(2), plushalos(2)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_INT), POINTER :: padd(:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "int", fs_intsize(), SIZE(field, 1), SIZE(field, 2), 0, 0, minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)))), &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1)), istride, jstride, -1, -1)
END SUBROUTINE fs_write_int_2d


SUBROUTINE fs_write_int_3d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  INTEGER(KIND=C_INT), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(3), plushalos(3)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_INT), POINTER :: padd(:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "int", fs_intsize(), SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), 0, &
                                                                   minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)))), &
                       C_LOC(padd(1, 1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1)), istride, jstride, kstride, -1)
END SUBROUTINE fs_write_int_3d


SUBROUTINE fs_write_int_4d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  INTEGER(KIND=C_INT), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(4), plushalos(4)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_INT), POINTER :: padd(:,:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "int", fs_intsize(), SIZE(field, 1), SIZE(field, 2), &
                                                                 SIZE(field, 3), SIZE(field, 4), minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1, 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)), 1)), &
                       C_LOC(padd(1, 1, 1, MIN(2, SIZE(field, 4)))), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1,1)), istride, jstride, kstride, lstride)
END SUBROUTINE fs_write_int_4d


!=============================================================================
!=============================================================================

SUBROUTINE fs_write_long_0d(serializer, savepoint, fieldname, field)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_LONG), POINTER :: padd

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "long", fs_longsize(), 1, 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), istride, -1, -1, -1)
END SUBROUTINE fs_write_long_0d


SUBROUTINE fs_write_long_1d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(1), plushalos(1)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_LONG), POINTER :: padd(:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "long", fs_longsize(), SIZE(field, 1), 0, 0, 0, minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)))), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1)), istride, -1, -1, -1)
END SUBROUTINE fs_write_long_1d


SUBROUTINE fs_write_long_2d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(2), plushalos(2)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_LONG), POINTER :: padd(:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "long", fs_longsize(), SIZE(field, 1), SIZE(field, 2), 0, 0, minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)))), &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1)), istride, jstride, -1, -1)
END SUBROUTINE fs_write_long_2d


SUBROUTINE fs_write_long_3d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(3), plushalos(3)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_LONG), POINTER :: padd(:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "long", fs_longsize(), SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), 0, &
                                                                   minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)))), &
                       C_LOC(padd(1, 1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1)), istride, jstride, kstride, -1)
END SUBROUTINE fs_write_long_3d


SUBROUTINE fs_write_long_4d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(4), plushalos(4)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_LONG), POINTER :: padd(:,:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "long", fs_longsize(), SIZE(field, 1), SIZE(field, 2), &
                                                                   SIZE(field, 3), SIZE(field, 4), minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1, 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)), 1)), &
                       C_LOC(padd(1, 1, 1, MIN(2, SIZE(field, 4)))), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1,1)), istride, jstride, kstride, lstride)
END SUBROUTINE fs_write_long_4d

!=============================================================================
!=============================================================================

SUBROUTINE fs_write_float_0d(serializer, savepoint, fieldname, field)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_FLOAT), POINTER :: padd

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "float", fs_floatsize(), 1, 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd), istride, -1, -1, -1)
END SUBROUTINE fs_write_float_0d


SUBROUTINE fs_write_float_1d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(1), plushalos(1)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_FLOAT), POINTER :: padd(:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "float", fs_floatsize(), SIZE(field, 1), 0, 0, 0, minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)))), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1)), istride, -1, -1, -1)
END SUBROUTINE fs_write_float_1d


SUBROUTINE fs_write_float_2d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(2), plushalos(2)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_FLOAT), POINTER :: padd(:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "float", fs_floatsize(), SIZE(field, 1), SIZE(field, 2), 0, 0, minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)))), &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1)), istride, jstride, -1, -1)
END SUBROUTINE fs_write_float_2d


SUBROUTINE fs_write_float_3d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(3), plushalos(3)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_FLOAT), POINTER :: padd(:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "float", fs_floatsize(), SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), 0, &
                                                                   minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)))), &
                       C_LOC(padd(1, 1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1)), istride, jstride, kstride, -1)
END SUBROUTINE fs_write_float_3d


SUBROUTINE fs_write_float_4d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(4), plushalos(4)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_FLOAT), POINTER :: padd(:,:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "float", fs_floatsize(), SIZE(field, 1), SIZE(field, 2), &
                                                                     SIZE(field, 3), SIZE(field, 4), minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1, 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)), 1)), &
                       C_LOC(padd(1, 1, 1, MIN(2, SIZE(field, 4)))), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1,1)), istride, jstride, kstride, lstride)
END SUBROUTINE fs_write_float_4d

!=============================================================================
!=============================================================================

SUBROUTINE fs_write_double_0d(serializer, savepoint, fieldname, field)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_DOUBLE), POINTER :: padd

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "double", fs_doublesize(), 1, 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd), istride, -1, -1, -1)
END SUBROUTINE fs_write_double_0d


SUBROUTINE fs_write_double_1d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(1), plushalos(1)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_DOUBLE), POINTER :: padd(:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "double", fs_doublesize(), SIZE(field, 1), 0, 0, 0, minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)))), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1)), istride, -1, -1, -1)
END SUBROUTINE fs_write_double_1d


SUBROUTINE fs_write_double_2d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(2), plushalos(2)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "double", fs_doublesize(), SIZE(field, 1), SIZE(field, 2), 0, 0, &
                     minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)))), &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1)), istride, jstride, -1, -1)
END SUBROUTINE fs_write_double_2d


SUBROUTINE fs_write_double_3d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(3), plushalos(3)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "double", fs_doublesize(), SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), 0, &
                                                                   minushalos, plushalos)

  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)))), &
                       C_LOC(padd(1, 1, 1)), &
                       istride, jstride, kstride, lstride)
  
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1)), istride, jstride, kstride, -1)
END SUBROUTINE fs_write_double_3d


SUBROUTINE fs_write_double_4d(serializer, savepoint, fieldname, field, minushalos, plushalos)
  TYPE(t_serializer), INTENT(IN)          :: serializer
  TYPE(t_savepoint) , INTENT(IN)          :: savepoint
  CHARACTER(LEN=*)                        :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN), OPTIONAL :: minushalos(4), plushalos(4)

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "double", fs_doublesize(), SIZE(field, 1), SIZE(field, 2), &
                                                                       SIZE(field, 3), SIZE(field, 4), minushalos, plushalos)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1, 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)), 1)), &
                       C_LOC(padd(1, 1, 1, MIN(2, SIZE(field, 4)))), &
                       istride, jstride, kstride, lstride)
  CALL fs_write_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                        TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1,1)), istride, jstride, kstride, lstride)
END SUBROUTINE fs_write_double_4d

!=============================================================================
!=============================================================================

SUBROUTINE fs_read_logical_0d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  LOGICAL, INTENT(OUT), TARGET             :: field
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  LOGICAL(KIND=C_BOOL) :: bool

  CALL fs_read_field(serializer, savepoint, fieldname, bool)
  field = bool

END SUBROUTINE fs_read_logical_0d


SUBROUTINE fs_read_logical_1d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  LOGICAL, INTENT(OUT), TARGET             :: field(:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  LOGICAL(KIND=C_BOOL), ALLOCATABLE :: bool(:)

  ALLOCATE(bool(SIZE(field, 1)))
  CALL fs_read_field(serializer, savepoint, fieldname, bool)
  field = bool

END SUBROUTINE fs_read_logical_1d


SUBROUTINE fs_read_logical_2d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  LOGICAL, INTENT(OUT), TARGET             :: field(:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  LOGICAL(KIND=C_BOOL), ALLOCATABLE :: bool(:,:)

  ALLOCATE(bool(SIZE(field, 1), SIZE(field, 2)))
  CALL fs_read_field(serializer, savepoint, fieldname, bool)
  field = bool

END SUBROUTINE fs_read_logical_2d


SUBROUTINE fs_read_logical_3d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  LOGICAL, INTENT(OUT), TARGET             :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  LOGICAL(KIND=C_BOOL), ALLOCATABLE :: bool(:,:,:)

  ALLOCATE(bool(SIZE(field, 1), SIZE(field, 2), SIZE(field, 3)))
  CALL fs_read_field(serializer, savepoint, fieldname, bool)
  field = bool

END SUBROUTINE fs_read_logical_3d

SUBROUTINE fs_read_logical_4d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  LOGICAL, INTENT(OUT), TARGET             :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  LOGICAL(KIND=C_BOOL), ALLOCATABLE :: bool(:,:,:,:)

  ALLOCATE(bool(SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), SIZE(field, 4)))
  CALL fs_read_field(serializer, savepoint, fieldname, bool)
  field = bool

END SUBROUTINE fs_read_logical_4d

!=============================================================================
!=============================================================================

SUBROUTINE fs_read_bool_0d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  LOGICAL(KIND=C_BOOL), POINTER :: padd

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "bool", fs_boolsize(), 1, 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                      TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd), istride, -1, -1, -1)
END SUBROUTINE fs_read_bool_0d


SUBROUTINE fs_read_bool_1d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  LOGICAL(KIND=C_BOOL), POINTER :: padd(:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "bool", fs_boolsize(), SIZE(field, 1), 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)))), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1)), istride, -1, -1, -1)
END SUBROUTINE fs_read_bool_1d


SUBROUTINE fs_read_bool_2d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "bool", fs_boolsize(), SIZE(field, 1), SIZE(field, 2), 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)))), &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1)), istride, jstride, -1, -1)
END SUBROUTINE fs_read_bool_2d


SUBROUTINE fs_read_bool_3d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "bool", fs_boolsize(), SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)))), &
                       C_LOC(padd(1, 1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1)), istride, jstride, kstride, -1)
END SUBROUTINE fs_read_bool_3d

SUBROUTINE fs_read_bool_4d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "bool", fs_boolsize(), SIZE(field, 1), SIZE(field, 2), &
                                                      SIZE(field, 3), SIZE(field, 4))
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1, 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)), 1)), &
                       C_LOC(padd(1, 1, 1, MIN(2, SIZE(field, 4)))), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1,1)), istride, jstride, kstride, lstride)
END SUBROUTINE fs_read_bool_4d

!=============================================================================
!=============================================================================

SUBROUTINE fs_read_int_0d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  INTEGER(KIND=C_INT), INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_INT), POINTER :: padd

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "int", fs_intsize(), 1, 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                      TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd), istride, -1, -1, -1)
END SUBROUTINE fs_read_int_0d


SUBROUTINE fs_read_int_1d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  INTEGER(KIND=C_INT), INTENT(OUT), TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_INT), POINTER :: padd(:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "int", fs_intsize(), SIZE(field, 1), 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)))), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1)), istride, -1, -1, -1)
END SUBROUTINE fs_read_int_1d


SUBROUTINE fs_read_int_2d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  INTEGER(KIND=C_INT), INTENT(OUT), TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_INT), POINTER :: padd(:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "int", fs_intsize(), SIZE(field, 1), SIZE(field, 2), 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)))), &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1)), istride, jstride, -1, -1)
END SUBROUTINE fs_read_int_2d


SUBROUTINE fs_read_int_3d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  INTEGER(KIND=C_INT), INTENT(OUT), TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_INT), POINTER :: padd(:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "int", fs_intsize(), SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)))), &
                       C_LOC(padd(1, 1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1)), istride, jstride, kstride, -1)
END SUBROUTINE fs_read_int_3d

SUBROUTINE fs_read_int_4d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  INTEGER(KIND=C_INT), INTENT(OUT), TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_INT), POINTER :: padd(:,:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "int", fs_intsize(), SIZE(field, 1), SIZE(field, 2), &
                                                      SIZE(field, 3), SIZE(field, 4))
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1, 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)), 1)), &
                       C_LOC(padd(1, 1, 1, MIN(2, SIZE(field, 4)))), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1,1)), istride, jstride, kstride, lstride)
END SUBROUTINE fs_read_int_4d


!=============================================================================
!=============================================================================

SUBROUTINE fs_read_long_0d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_LONG), POINTER :: padd

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "long", fs_longsize(), 1, 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                      TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd), istride, -1, -1, -1)
END SUBROUTINE fs_read_long_0d


SUBROUTINE fs_read_long_1d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_LONG), POINTER :: padd(:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "long", fs_longsize(), SIZE(field, 1), 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)))), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1)), istride, -1, -1, -1)
END SUBROUTINE fs_read_long_1d


SUBROUTINE fs_read_long_2d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_LONG), POINTER :: padd(:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "long", fs_longsize(), SIZE(field, 1), SIZE(field, 2), 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)))), &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1)), istride, jstride, -1, -1)
END SUBROUTINE fs_read_long_2d


SUBROUTINE fs_read_long_3d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_LONG), POINTER :: padd(:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "long", fs_longsize(), SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)))), &
                       C_LOC(padd(1, 1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1)), istride, jstride, kstride, -1)
END SUBROUTINE fs_read_long_3d

SUBROUTINE fs_read_long_4d(serializer, savepoint, fieldname, field, rperturb)
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  INTEGER(KIND=C_LONG), POINTER :: padd(:,:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "long", fs_longsize(), SIZE(field, 1), SIZE(field, 2), &
                                                      SIZE(field, 3), SIZE(field, 4))
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1, 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)), 1)), &
                       C_LOC(padd(1, 1, 1, MIN(2, SIZE(field, 4)))), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1,1)), istride, jstride, kstride, lstride)
END SUBROUTINE fs_read_long_4d

!=============================================================================
!=============================================================================

SUBROUTINE fs_read_float_0d(serializer, savepoint, fieldname, field, rperturb)
  use m_ser_perturb, ONLY: ser_fld_perturb
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_FLOAT), POINTER :: padd

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "float", fs_floatsize(), 1, 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd), istride, -1, -1, -1)

  ! Perturb field
  IF (rperturb .NE. 0.0) THEN
    CALL ser_fld_perturb(field, rperturb)
  END IF
END SUBROUTINE fs_read_float_0d

SUBROUTINE fs_read_float_1d(serializer, savepoint, fieldname, field, rperturb)
  use m_ser_perturb, ONLY: ser_fld_perturb
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_FLOAT), POINTER :: padd(:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "float", fs_floatsize(), SIZE(field, 1), 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)))), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1)), istride, -1, -1, -1)

  ! Perturb field
  IF (rperturb .NE. 0.0) THEN
    CALL ser_fld_perturb(field, rperturb)
  END IF
END SUBROUTINE fs_read_float_1d


SUBROUTINE fs_read_float_2d(serializer, savepoint, fieldname, field, rperturb)
  use m_ser_perturb, ONLY: ser_fld_perturb
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_FLOAT), POINTER :: padd(:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "float", fs_floatsize(), SIZE(field, 1), SIZE(field, 2), 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)))), &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1)), istride, jstride, -1, -1)

  ! Perturb field
  IF (rperturb .NE. 0.0) THEN
    CALL ser_fld_perturb(field, rperturb)
  END IF
END SUBROUTINE fs_read_float_2d


SUBROUTINE fs_read_float_3d(serializer, savepoint, fieldname, field, rperturb)
  use m_ser_perturb, ONLY: ser_fld_perturb
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_FLOAT), POINTER :: padd(:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "float", fs_floatsize(), SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)))), &
                       C_LOC(padd(1, 1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1)), istride, jstride, kstride, -1)

  ! Perturb field
  IF (rperturb .NE. 0.0) THEN
    CALL ser_fld_perturb(field, rperturb)
  END IF
END SUBROUTINE fs_read_float_3d

SUBROUTINE fs_read_float_4d(serializer, savepoint, fieldname, field, rperturb)
  use m_ser_perturb, ONLY: ser_fld_perturb
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_FLOAT), POINTER :: padd(:,:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "float", fs_floatsize(), SIZE(field, 1), SIZE(field, 2), &
                                                      SIZE(field, 3), SIZE(field, 4))
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1, 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)), 1)), &
                       C_LOC(padd(1, 1, 1, MIN(2, SIZE(field, 4)))), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1,1)), istride, jstride, kstride, lstride)

  ! Perturb field
  IF (rperturb .NE. 0.0) THEN
    CALL ser_fld_perturb(field, rperturb)
  END IF
END SUBROUTINE fs_read_float_4d

!=============================================================================
!=============================================================================

SUBROUTINE fs_read_double_0d(serializer, savepoint, fieldname, field, rperturb)
  use m_ser_perturb, ONLY: ser_fld_perturb
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_DOUBLE), POINTER :: padd

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "double", fs_doublesize(), 1, 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), C_LOC(padd), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd), istride, -1, -1, -1)

  ! Perturb field
  IF (rperturb .NE. 0.0) THEN
    CALL ser_fld_perturb(field, rperturb)
  END IF
END SUBROUTINE fs_read_double_0d

SUBROUTINE fs_read_double_1d(serializer, savepoint, fieldname, field, rperturb)
  use m_ser_perturb, ONLY: ser_fld_perturb
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_DOUBLE), POINTER :: padd(:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "double", fs_doublesize(), SIZE(field, 1), 0, 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)))), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       C_LOC(padd(1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1)), istride, -1, -1, -1)

  ! Perturb field
  IF (rperturb .NE. 0.0) THEN
    CALL ser_fld_perturb(field, rperturb)
  END IF
END SUBROUTINE fs_read_double_1d


SUBROUTINE fs_read_double_2d(serializer, savepoint, fieldname, field, rperturb)
  use m_ser_perturb, ONLY: ser_fld_perturb
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "double", fs_doublesize(), SIZE(field, 1), SIZE(field, 2), 0, 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)))), &
                       C_LOC(padd(1, 1)), &
                       C_LOC(padd(1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1)), istride, jstride, -1, -1)

  ! Perturb field
  IF (rperturb .NE. 0.0) THEN
    CALL ser_fld_perturb(field, rperturb)
  END IF
END SUBROUTINE fs_read_double_2d


SUBROUTINE fs_read_double_3d(serializer, savepoint, fieldname, field, rperturb)
  use m_ser_perturb, ONLY: ser_fld_perturb
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "double", fs_doublesize(), SIZE(field, 1), SIZE(field, 2), SIZE(field, 3), 0)
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)))), &
                       C_LOC(padd(1, 1, 1)), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1)), istride, jstride, kstride, -1)

  ! Perturb field
  IF (rperturb .NE. 0.0) THEN
    CALL ser_fld_perturb(field, rperturb)
  END IF
END SUBROUTINE fs_read_double_3d

SUBROUTINE fs_read_double_4d(serializer, savepoint, fieldname, field, rperturb)
  use m_ser_perturb, ONLY: ser_fld_perturb
  TYPE(t_serializer), INTENT(IN)           :: serializer
  TYPE(t_savepoint) , INTENT(IN)           :: savepoint
  CHARACTER(LEN=*)                         :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb

  ! Local variables
  INTEGER(C_INT) :: istride, jstride, kstride, lstride
  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:,:,:)

  ! This workaround is needed for gcc < 4.9
  padd=>field

  CALL fs_check_size(serializer, fieldname, "double", fs_doublesize(), SIZE(field, 1), SIZE(field, 2), &
                                                      SIZE(field, 3), SIZE(field, 4))
  CALL fs_compute_strides(serializer%serializer_ptr,  TRIM(fieldname)//C_NULL_CHAR, &
                       C_LOC(padd(1, 1, 1, 1)), &
                       C_LOC(padd(MIN(2, SIZE(field, 1)), 1, 1, 1)), &
                       C_LOC(padd(1, MIN(2, SIZE(field, 2)), 1, 1)), &
                       C_LOC(padd(1, 1, MIN(2, SIZE(field, 3)), 1)), &
                       C_LOC(padd(1, 1, 1, MIN(2, SIZE(field, 4)))), &
                       istride, jstride, kstride, lstride)
  CALL fs_read_field_(serializer%serializer_ptr, savepoint%savepoint_ptr, &
                       TRIM(fieldname)//C_NULL_CHAR, &
                      C_LOC(padd(1,1,1,1)), istride, jstride, kstride, lstride)

  ! Perturb field
  IF (rperturb .NE. 0.0) THEN
    CALL ser_fld_perturb(field, rperturb)
  END IF
END SUBROUTINE fs_read_double_4d

END MODULE m_serialize

