!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license.
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------
!
!+ This module contains the interface for serializing k-blocked fields
!
!------------------------------------------------------------------------------

MODULE m_ser_kbuffer

!------------------------------------------------------------------------------
!
! Description:
!
!   This module contains subroutines which allow to serialize k-blocked
!   fields using internal buffering of the data before flushing them
!   off to serialbox. It uses buffering of the fields and fields are
!   automatically flushed once all data has been written.
!
! Current Code Owner: Vulcan Inc, Oliver Fuhrer
!  email:  oliverf@vulcan.com
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!============================================================================

USE iso_c_binding
USE m_serialize

IMPLICIT NONE

PUBLIC :: &
  fs_write_kbuff, finalize_kbuff

PRIVATE

  TYPE kbuff_type
    LOGICAL :: in_use = .FALSE.                   ! is this buffer in use?
    TYPE(C_PTR) :: serializer                     ! serializer object associated with buffer
    TYPE(C_PTR) :: savepoint                      ! savepoint object associated with buffer
    CHARACTER(LEN=256) :: savepointname
    CHARACTER(LEN=256) :: fieldname
    INTEGER :: dim_i = 0, dim_j = 0, dim_k = 0    ! dimensions of 3d-field to be serialized
    LOGICAL :: has_minushalos, has_plushalos
    INTEGER :: minushalos(3), plushalos(3)
    INTEGER :: field_type = 0                     ! 0 = not used, 1 = int, 2 = r4, 3 = r8
    INTEGER, ALLOCATABLE :: buff_i4(:,:,:)
    REAL(KIND=C_FLOAT), ALLOCATABLE :: buff_r4(:,:,:)
    REAL(KIND=C_DOUBLE), ALLOCATABLE :: buff_r8(:,:,:)
    LOGICAL, ALLOCATABLE :: ok(:)                 ! has this k-level been written?
  END TYPE kbuff_type

  INTEGER, PARAMETER :: max_kbuff = 999           ! increase in case you get errors
  TYPE(kbuff_type) :: kbuff(max_kbuff)            ! array containing buffers

  ! overload interface for different types and dimensions
  INTERFACE fs_write_kbuff
      MODULE PROCEDURE fs_write_kbuff_integer_3d_i4
      MODULE PROCEDURE fs_write_kbuff_float_3d_r4
      MODULE PROCEDURE fs_write_kbuff_float_3d_r8
  END INTERFACE

  LOGICAL :: first_call = .false.                 ! used for initialization

  LOGICAL, PARAMETER :: debug = .false.           ! get verbose messaging

CONTAINS

!============================================================================

! initialize buffering: makes sure all buffers are set to not in use
SUBROUTINE init_kbuff()
  IMPLICIT NONE

  INTEGER :: idx

  IF (debug) THEN
    WRITE(0,*) 'DEBUG init_kbuff'
  END IF

  first_call = .FALSE.

  DO idx = 1, max_kbuff
    kbuff(idx)%in_use = .FALSE.
    kbuff(idx)%fieldname = ""
    kbuff(idx)%savepointname = ""
  END DO

END SUBROUTINE init_kbuff

!============================================================================

! finalize buffering: should be called once all buffers have been flushed
SUBROUTINE finalize_kbuff()
  IMPLICIT NONE

  INTEGER :: idx

  IF (debug) THEN
    WRITE(0,*) 'DEBUG init_kbuff'
  END IF

  IF (first_call) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: finalize called before init'
    STOP
  END IF

  DO idx = 1, max_kbuff
    IF (kbuff(idx)%in_use) THEN
      WRITE(0,*) 'ERROR in m_ser_kbuffer: finalize called before all buffers have been flushed'
      STOP
    END IF
  END DO

  first_call = .TRUE.

END SUBROUTINE finalize_kbuff

!============================================================================

! overloads fs_write_kbuff: version for r8 floats and 3d fields
SUBROUTINE fs_write_kbuff_float_3d_r8(serializer, savepoint, fieldname, field, &
                                      k, k_size, minushalos, plushalos)
  IMPLICIT NONE

  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET  :: field(:,:)
  INTEGER, INTENT(IN)                     :: k, k_size
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! local vars
  INTEGER :: kbuff_id = 0
  INTEGER :: field_type = 3

  ! find kbuff_id and check if a buffer slot was found
  call setup_buffer(kbuff_id, serializer, savepoint, fieldname, field_type, &
                    SIZE(field,1), SIZE(field,2), k_size, k, minushalos, plushalos)


  ! store data
  IF (debug) THEN
    WRITE(0,*) 'DEBUG fs_write_kbuff_float_3d_r8: store data'
  END IF
  kbuff(kbuff_id)%buff_r8(:,:,k) = field(:,:)
  kbuff(kbuff_id)%ok(k) = .TRUE.

  ! write if we are complete
  IF (ALL(kbuff(kbuff_id)%ok(:))) THEN
    IF (debug) THEN
      WRITE(0,*) 'DEBUG fs_write_kbuff_float_3d_r8: flush data'
    END IF
    IF (kbuff(kbuff_id)%has_minushalos) THEN
      IF (kbuff(kbuff_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_r8, &
          minushalos=kbuff(kbuff_id)%minushalos, plushalos=kbuff(kbuff_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_r8, &
          minushalos=kbuff(kbuff_id)%minushalos)
      END IF
    ELSE
      IF (kbuff(kbuff_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_r8, &
          plushalos=kbuff(kbuff_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_r8)
      END IF
    END IF
    CALL destroy_kbuff(kbuff_id)
  END IF

END SUBROUTINE fs_write_kbuff_float_3d_r8

!============================================================================

! overloads fs_write_kbuff: version for r4 floats and 3d fields
SUBROUTINE fs_write_kbuff_float_3d_r4(serializer, savepoint, fieldname, field, &
                                      k, k_size, minushalos, plushalos)
  IMPLICIT NONE

  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET  :: field(:,:)
  INTEGER, INTENT(IN)                     :: k, k_size
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! local vars
  INTEGER :: kbuff_id = 0
  INTEGER :: field_type = 2

  ! find kbuff_id and check if a buffer slot was found
  call setup_buffer(kbuff_id, serializer, savepoint, fieldname, field_type, &
                    SIZE(field,1), SIZE(field,2), k_size, k, minushalos, plushalos)


  ! store data
  IF (debug) THEN
    WRITE(0,*) 'DEBUG fs_write_kbuff_float_3d_r4: store data'
  END IF
  kbuff(kbuff_id)%buff_r4(:,:,k) = field(:,:)
  kbuff(kbuff_id)%ok(k) = .TRUE.

  ! write if we are complete
  IF (ALL(kbuff(kbuff_id)%ok(:))) THEN
    IF (debug) THEN
      WRITE(0,*) 'DEBUG fs_write_kbuff_float_3d_r4: flush data'
    END IF
    IF (kbuff(kbuff_id)%has_minushalos) THEN
      IF (kbuff(kbuff_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_r4, &
          minushalos=kbuff(kbuff_id)%minushalos, plushalos=kbuff(kbuff_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_r4, &
          minushalos=kbuff(kbuff_id)%minushalos)
      END IF
    ELSE
      IF (kbuff(kbuff_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_r4, &
          plushalos=kbuff(kbuff_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_r4)
      END IF
    END IF
    CALL destroy_kbuff(kbuff_id)
  END IF

END SUBROUTINE fs_write_kbuff_float_3d_r4

!============================================================================

! overloads fs_write_kbuff: version for i4 integers and 3d fields
SUBROUTINE fs_write_kbuff_integer_3d_i4(serializer, savepoint, fieldname, field, &
                                        k, k_size, minushalos, plushalos)
  IMPLICIT NONE

  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN), TARGET             :: field(:,:)
  INTEGER, INTENT(IN)                     :: k, k_size
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! local vars
  INTEGER :: kbuff_id = 0
  INTEGER :: field_type = 1

  ! find kbuff_id and check if a buffer slot was found
  call setup_buffer(kbuff_id, serializer, savepoint, fieldname, field_type, &
                    SIZE(field,1), SIZE(field,2), k_size, k, minushalos, plushalos)

  ! store data
  IF (debug) THEN
    WRITE(0,*) 'DEBUG fs_write_kbuff_integer_3d_i4: store data'
  END IF
  kbuff(kbuff_id)%buff_i4(:,:,k) = field(:,:)
  kbuff(kbuff_id)%ok(k) = .TRUE.

  ! write if we are complete
  IF (ALL(kbuff(kbuff_id)%ok(:))) THEN
    IF (debug) THEN
      WRITE(0,*) 'DEBUG fs_write_kbuff_integer_3d_i4: flush data'
    END IF
    IF (kbuff(kbuff_id)%has_minushalos) THEN
      IF (kbuff(kbuff_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_i4, &
          minushalos=kbuff(kbuff_id)%minushalos, plushalos=kbuff(kbuff_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_i4, &
          minushalos=kbuff(kbuff_id)%minushalos)
      END IF
    ELSE
      IF (kbuff(kbuff_id)%has_plushalos) THEN
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_i4, &
          plushalos=kbuff(kbuff_id)%plushalos)
      ELSE
        CALL fs_write_field(serializer, savepoint, fieldname, kbuff(kbuff_id)%buff_i4)
      END IF
    END IF
    CALL destroy_kbuff(kbuff_id)
  END IF

END SUBROUTINE fs_write_kbuff_integer_3d_i4

!============================================================================

! checks if a buffer exists for this fields and if yes, checks consistency with
! current request. if not, it creates a new buffer.
SUBROUTINE setup_buffer(kbuff_id, serializer, savepoint, fieldname, field_type, &
                        field_nx, field_ny, k_size, k, minushalos, plushalos)
  IMPLICIT NONE
  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN)                     :: k, k_size, field_nx, field_ny, field_type
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)
  INTEGER, INTENT(OUT)                    :: kbuff_id

  IF (debug) THEN
    WRITE(0,*) 'DEBUG setup_buffer: savepoint=', TRIM(savepoint%savepoint_name)
    WRITE(0,*) 'DEBUG setup_buffer: fieldname=', TRIM(fieldname)
    WRITE(0,*) 'DEBUG setup_buffer: k=', k
    WRITE(0,*) 'DEBUG setup_buffer: k_size=', k_size
  END IF

  ! initialize if this is the first call
  IF (first_call) THEN
    CALL init_kbuff()
  END IF

  ! find ID if it already exists
  CALL find_kbuff_id(fieldname, savepoint%savepoint_name, kbuff_id)

  ! check if a buffer slot was found
  IF ( kbuff_id == 0 ) THEN
    ! no, so create a new buffer
    CALL get_free_kbuff_id(kbuff_id)
    CALL create_kbuff(kbuff_id, serializer, savepoint, fieldname, field_type, &
                      field_nx, field_ny, k_size, minushalos, plushalos)
  ELSE
    ! yes, so check for consistency of current request with stored metadata
    CALL check_kbuff(kbuff_id, serializer, savepoint, fieldname, field_type, &
                     field_nx, field_ny, k_size, k, minushalos, plushalos)
  END IF

 END SUBROUTINE setup_buffer

!============================================================================

! create a new buffer (allocate memory, store metadata)
SUBROUTINE create_kbuff(kbuff_id, serializer, savepoint, fieldname, field_type, &
                        dim_i, dim_j, dim_k, minushalos, plushalos)
  IMPLICIT NONE

  INTEGER, INTENT(IN)                     :: kbuff_id
  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN)                     :: field_type
  INTEGER, INTENT(IN)                     :: dim_i, dim_j, dim_k
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! debug information
  IF (debug) THEN
    WRITE(0,*) 'DEBUG create_kbuff: kbuff_id=', kbuff_id
    WRITE(0,*) 'DEBUG create_kbuff: savepoint=', TRIM(savepoint%savepoint_name)
    WRITE(0,*) 'DEBUG create_kbuff: fieldname=', TRIM(fieldname)
    WRITE(0,*) 'DEBUG create_kbuff: field_type=', field_type
    WRITE(0,*) 'DEBUG create_kbuff: dim_i,dim_j,dim_k=', dim_i, dim_j, dim_k
  END IF

  ! security check
  IF (kbuff_id < 1 .OR. kbuff_id > max_kbuff) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: illegal kbuff_id encountered'
    STOP
  END IF
  IF (kbuff(kbuff_id)%in_use) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: create called for buffer already in use'
    STOP
  END IF

  ! store metadata
  kbuff(kbuff_id)%in_use = .TRUE.
  kbuff(kbuff_id)%serializer = serializer%serializer_ptr
  kbuff(kbuff_id)%savepoint = savepoint%savepoint_ptr
  kbuff(kbuff_id)%savepointname = TRIM(savepoint%savepoint_name)
  kbuff(kbuff_id)%fieldname = TRIM(fieldname)
  kbuff(kbuff_id)%dim_i = dim_i
  kbuff(kbuff_id)%dim_j = dim_j
  kbuff(kbuff_id)%dim_k = dim_k
  IF (PRESENT(minushalos)) THEN
    kbuff(kbuff_id)%minushalos = minushalos
    kbuff(kbuff_id)%has_minushalos = .TRUE.
  ELSE
    kbuff(kbuff_id)%has_minushalos = .FALSE.
  ENDIF
  IF (PRESENT(plushalos)) THEN
    kbuff(kbuff_id)%plushalos = plushalos
    kbuff(kbuff_id)%has_plushalos = .TRUE.
  ELSE
    kbuff(kbuff_id)%has_plushalos = .FALSE.
  ENDIF
  kbuff(kbuff_id)%field_type = field_type

  ! allocate memory
  SELECT CASE (field_type)
    CASE(1)
      ALLOCATE(kbuff(kbuff_id)%buff_i4(dim_i, dim_j, dim_k))
    CASE(2)
      ALLOCATE(kbuff(kbuff_id)%buff_r4(dim_i, dim_j, dim_k))
    CASE(3)
      ALLOCATE(kbuff(kbuff_id)%buff_r8(dim_i, dim_j, dim_k))
    CASE DEFAULT
      WRITE(0,*) 'ERROR in m_ser_kbuffer: unsupported field_type encountered'
  END SELECT
  ALLOCATE(kbuff(kbuff_id)%ok(dim_k))

  ! make sure all k-levels are marked as unwritten
  kbuff(kbuff_id)%ok(:) = .FALSE.

END SUBROUTINE create_kbuff

!============================================================================

! release a buffer (release memory, reset metadata)
SUBROUTINE destroy_kbuff(kbuff_id)
  IMPLICIT NONE

  INTEGER, INTENT(IN)                     :: kbuff_id

  ! debug information
  IF (debug) THEN
    WRITE(0,*) 'DEBUG destroy_kbuff: kbuff_id=', kbuff_id
    WRITE(0,*) 'DEBUG destroy_kbuff: savepoint=', TRIM(kbuff(kbuff_id)%savepointname)
    WRITE(0,*) 'DEBUG destroy_kbuff: fieldname=', TRIM(kbuff(kbuff_id)%fieldname)
  END IF

  ! security check
  IF (kbuff_id < 1 .OR. kbuff_id > max_kbuff) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: illegal kbuff_id encountered'
    STOP
  END IF
  IF (.NOT. kbuff(kbuff_id)%in_use) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: destroy called for buffer not in use'
    STOP
  END IF

  ! release memory
  SELECT CASE (kbuff(kbuff_id)%field_type)
    CASE(1)
      DEALLOCATE(kbuff(kbuff_id)%buff_i4)
    CASE(2)
      DEALLOCATE(kbuff(kbuff_id)%buff_r4)
    CASE(3)
      DEALLOCATE(kbuff(kbuff_id)%buff_r8)
    CASE DEFAULT
      WRITE(0,*) 'ERROR in m_ser_kbuffer: unsupported field_type encountered in destroy'
  END SELECT
  DEALLOCATE(kbuff(kbuff_id)%ok)

  ! reset metadata
  kbuff(kbuff_id)%in_use = .FALSE.
  kbuff(kbuff_id)%serializer = C_NULL_PTR
  kbuff(kbuff_id)%savepoint = C_NULL_PTR
  kbuff(kbuff_id)%savepointname = ""
  kbuff(kbuff_id)%fieldname = ""
  kbuff(kbuff_id)%field_type = 0
  kbuff(kbuff_id)%dim_i = 0
  kbuff(kbuff_id)%dim_j = 0
  kbuff(kbuff_id)%dim_k = 0
  IF (kbuff(kbuff_id)%has_minushalos) THEN
    kbuff(kbuff_id)%minushalos = (/0, 0, 0/)
  ENDIF
  kbuff(kbuff_id)%has_minushalos = .FALSE.
  IF (kbuff(kbuff_id)%has_plushalos) THEN
    kbuff(kbuff_id)%plushalos = (/0, 0, 0/)
  ENDIF
  kbuff(kbuff_id)%has_plushalos = .FALSE.

END SUBROUTINE destroy_kbuff

!============================================================================

! check consistency of current request with metadata stored in buffer
SUBROUTINE check_kbuff(kbuff_id, serializer, savepoint, fieldname, field_type, &
                       dim_i, dim_j, dim_k, k, minushalos, plushalos)
  IMPLICIT NONE

  INTEGER, INTENT(IN)                     :: kbuff_id
  TYPE(t_serializer), TARGET, INTENT(IN)  :: serializer
  TYPE(t_savepoint), TARGET, INTENT(IN)   :: savepoint
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  INTEGER, INTENT(IN)                     :: field_type
  INTEGER, INTENT(IN)                     :: dim_i, dim_j, dim_k, k
  INTEGER, INTENT(IN), OPTIONAL           :: minushalos(3), plushalos(3)

  ! debug information
  IF (debug) THEN
    WRITE(0,*) 'DEBUG check_kbuff: kbuff_id=', kbuff_id
    WRITE(0,*) 'DEBUG check_kbuff: savepoint=', TRIM(savepoint%savepoint_name)
    WRITE(0,*) 'DEBUG check_kbuff: fieldname=', TRIM(fieldname)
    WRITE(0,*) 'DEBUG check_kbuff: field_type=', field_type
    WRITE(0,*) 'DEBUG check_kbuff: dim_i,dim_j,dim_k=', dim_i, dim_j, dim_k
    WRITE(0,*) 'DEBUG check_kbuff: k=', k
  END IF

  ! security check
  IF (kbuff_id < 1 .OR. kbuff_id > max_kbuff) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: illegal kbuff_id encountered'
    STOP
  END IF
  IF (.NOT. kbuff(kbuff_id)%in_use) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: check called for buffer not in use'
    STOP
  END IF

  ! check consistency
  IF (.NOT. (TRIM(kbuff(kbuff_id)%fieldname) == TRIM(fieldname))) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: inconsistent name encountered'
    STOP
  END IF
  IF (.NOT. (C_ASSOCIATED(kbuff(kbuff_id)%serializer, serializer%serializer_ptr))) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: write called for same field but different serializer'
    STOP
  END IF
  ! NOTE: we cannot check savepoint consistency since we are probably creating a new
  ! savepoint for every k-loop iteration.
!  IF (.NOT. (C_ASSOCIATED(kbuff(kbuff_id)%savepoint, savepoint%savepoint_ptr))) THEN
!    WRITE(0,*) 'ERROR in m_ser_kbuffer: write called for same field but different savepoint'
!    STOP
!  END IF
  IF (.NOT. (TRIM(kbuff(kbuff_id)%savepointname) == TRIM(savepoint%savepoint_name))) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: write called for same field but different savepoint'
    STOP
  END IF
  IF (ANY( (/kbuff(kbuff_id)%dim_i, kbuff(kbuff_id)%dim_j, kbuff(kbuff_id)%dim_k/) /= (/dim_i, dim_j, dim_k/) )) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: write called with inconsistent dimensions'
    STOP
  END IF
  IF ((k < 1) .OR. (k > dim_k)) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: out of bound k-index encountered'
    STOP
  END IF
  IF (kbuff(kbuff_id)%has_minushalos .AND. PRESENT(minushalos)) THEN
    IF (ANY(kbuff(kbuff_id)%minushalos /= minushalos)) THEN
      WRITE(0,*) 'ERROR in m_ser_kbuffer: inconsistent minushalos encountered'
      STOP
    END IF
  END IF
  IF (kbuff(kbuff_id)%has_plushalos .AND. PRESENT(plushalos)) THEN
    IF (ANY(kbuff(kbuff_id)%plushalos /= plushalos)) THEN
      WRITE(0,*) 'ERROR in m_ser_kbuffer: inconsistent plushalos encountered'
      STOP
    END IF
  END IF
  IF (kbuff(kbuff_id)%field_type /= field_type) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: write with inconsistent field_type encountered'
    STOP
  END IF
  IF (kbuff(kbuff_id)%ok(k)) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: k-index already written'
    STOP
  END IF

END SUBROUTINE check_kbuff

!============================================================================

! find the ID of a buffer given name of field and savepoint
SUBROUTINE find_kbuff_id(fieldname, savepointname, kbuff_id)
  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  CHARACTER(LEN=*), INTENT(IN) :: savepointname
  INTEGER, INTENT(OUT)         :: kbuff_id

  ! local vars
  INTEGER :: idx

  kbuff_id = 0

  IF (debug) THEN
    WRITE(0,*) 'DEBUG find_kbuff_id: fieldname=', TRIM(fieldname), ' savepoint=', TRIM(savepointname)
  END IF

  DO idx = 1, max_kbuff
    IF (TRIM(fieldname) == TRIM(kbuff(idx)%fieldname)) THEN
      IF (TRIM(savepointname) == TRIM(kbuff(idx)%savepointname)) THEN
        kbuff_id = idx
        EXIT
      END IF
    END IF
  END DO

  IF (debug) THEN
    IF (kbuff_id == 0) THEN
      WRITE(0,*) 'DEBUG find_kbuff_id: no kbuff found'
    ELSE
      WRITE(0,*) 'DEBUG find_kbuff_id: found kbuff_id=', kbuff_id
    END IF
  END IF

END SUBROUTINE find_kbuff_id

!============================================================================

! find a free buffer ID
SUBROUTINE get_free_kbuff_id(kbuff_id)
  IMPLICIT NONE

  INTEGER, INTENT(OUT)         :: kbuff_id

  ! local vars
  INTEGER :: idx

  kbuff_id = 0

  ! find a free index
  DO idx = 1, max_kbuff
    IF (.NOT. kbuff(idx)%in_use) THEN
      kbuff_id = idx
      EXIT
    END IF
  END DO

  ! abort if no free index has been found
  IF (idx > max_kbuff) THEN
    WRITE(0,*) 'ERROR in m_ser_kbuffer: no more free buffers (increase max_kbuff)'
    STOP
  END IF

END SUBROUTINE get_free_kbuff_id

!============================================================================

END MODULE m_ser_kbuffer
