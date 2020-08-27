!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license. 
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------
!
!+ Module containing procedures for input/output to/from disk.
!
!------------------------------------------------------------------------------

MODULE utils_ppser

!------------------------------------------------------------------------------
!
! Description:
!   This module contains utilities for the pp_ser serialization helper.
!   The functions and subroutines defined in this module are not intended to
!   be used directly: they will be used by the preprocessor at compilation
!   time.
!
! Current Code Owner: ETH, Andrea Arteaga
!  email:  andrea.arteaga@env.ethz.ch
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!============================================================================

USE iso_fortran_env
USE iso_c_binding
USE m_serialize

  IMPLICIT NONE

  TYPE(t_serializer) :: ppser_serializer
  TYPE(t_serializer) :: ppser_serializer_ref
  TYPE(t_savepoint)  :: ppser_savepoint
  LOGICAL            :: ppser_initialized = .false.
  LOGICAL            :: ppser_hasref = .false.

  INTEGER            :: ppser_intlength, ppser_reallength
  CHARACTER (LEN=6)  :: ppser_realtype
  REAL               :: ppser_zrperturb = 0.0

  ! 0 corresponds to "read"
  ! 1 corresponds to "write"
  INTEGER            :: ppser_mode = 0

  ! This is defined in serialbox-python/pp_ser/pp_ser.py,
  ! Please update there if you change it here and vice versa
  INTEGER, PARAMETER, PUBLIC :: PPSER_MODE_WRITE = 0
      
PUBLIC :: &
  ppser_serializer, ppser_savepoint, ppser_initialize,  &
  ppser_intlength, ppser_reallength, ppser_realtype,    &
  ppser_set_mode, ppser_get_mode, ppser_serializer_ref

CONTAINS

!============================================================================

SUBROUTINE ppser_initialize(directory, prefix, mode, directory_ref, prefix_ref, &
                mpi_rank, rprecision, rperturb, realtype, archive, unique_id)
  CHARACTER(LEN=*), INTENT(IN)           :: directory, prefix
  INTEGER, OPTIONAL, INTENT(IN)          :: mode
  CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: directory_ref, prefix_ref ! use a different serializer for reading (optional)
  INTEGER, OPTIONAL, INTENT(IN)          :: mpi_rank
  REAL(KIND=8), OPTIONAL, INTENT(IN)     :: rprecision, rperturb
  INTEGER, OPTIONAL, INTENT(IN)          :: realtype
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: archive
  LOGICAL, INTENT(IN), OPTIONAL          :: unique_id

  CHARACTER(LEN=256)                     :: dir_ref
  CHARACTER(LEN=1), DIMENSION(128)       :: buffer
  CHARACTER(LEN=15)                      :: suffix
  INTEGER                                :: intvalue
  
  IF (PRESENT(directory_ref)) THEN
    dir_ref = directory_ref
  ELSE
    dir_ref = directory
  ENDIF

  ! Initialize serializer and savepoint
  IF ( .NOT. ppser_initialized ) THEN
    CALL fs_init(enable_unique_savepoint_id=unique_id)
    IF ( PRESENT(mpi_rank) ) THEN
      WRITE(suffix, '(A5,I0)') "_rank", mpi_rank
      IF ( PRESENT(archive) ) THEN
        CALL fs_create_serializer(directory, TRIM(prefix)//TRIM(suffix), 'w', ppser_serializer, archive)
      ELSE
        CALL fs_create_serializer(directory, TRIM(prefix)//TRIM(suffix), 'w', ppser_serializer)
      END IF
    ELSE
      IF ( PRESENT(archive) ) THEN
        CALL fs_create_serializer(directory, TRIM(prefix), 'w', ppser_serializer, archive)
      ELSE
        CALL fs_create_serializer(directory, TRIM(prefix), 'w', ppser_serializer)
      END IF
    END IF
    CALL fs_create_savepoint('', ppser_savepoint)
    ppser_mode = 0
    IF ( PRESENT(mode) ) ppser_mode = mode
    IF ( PRESENT(prefix_ref) ) THEN
      ppser_hasref = .true.
      IF ( PRESENT(mpi_rank) ) THEN
        IF ( PRESENT(archive) ) THEN
          CALL fs_create_serializer(dir_ref, TRIM(prefix_ref)//TRIM(suffix), 'r', ppser_serializer_ref, archive)
        ELSE
          CALL fs_create_serializer(dir_ref, TRIM(prefix_ref)//TRIM(suffix), 'r', ppser_serializer_ref)
        END IF
      ELSE
        IF ( PRESENT(archive) ) THEN
          CALL fs_create_serializer(dir_ref, TRIM(prefix_ref), 'r', ppser_serializer_ref, archive)
        ELSE
          CALL fs_create_serializer(dir_ref, TRIM(prefix_ref), 'r', ppser_serializer_ref)
        END IF
      END IF
    END IF
  END IF
  ppser_initialized = .TRUE.

  ! Get data size
  intvalue = 0
  IF ( PRESENT(realtype) ) THEN
    ppser_reallength = realtype
  ELSE
    ppser_reallength = 4 ! Default real length
  END IF

  ppser_intlength = INT(SIZE(TRANSFER(intvalue, buffer)))

  ! Get name of real
  IF ( ppser_reallength == 4 ) THEN
    ppser_realtype = 'float'
  ELSE
    ppser_realtype = 'double'
  END IF

  ppser_zrperturb = 0.0
  IF ( PRESENT(rprecision) .AND. PRESENT(rperturb) ) THEN
    ! generate epsilon
    IF (rperturb > 0.0) THEN
      ! relative perturbation with a specified magnitude
      ! Example: if rperturb = 1.0e-5 then perturbation will be ...
      !    field = field * (1 + 1.0e-5 * R) in double precision
      !    field = field * (1 + 1.0e-5 * R) in single precision
      !  ... where R is a random number from -1.0 to 1.0
      ppser_zrperturb = rperturb
    ELSE
      ! relative perturbation with a magnitude relative to precision
      ! Example: if rperturb = -10.0 then perturbation will be ...
      !    field = field * (1 + 10.0 * 1e-16 * R) in double precision
      !    field = field * (1 + 10.0 * 1e-7 * R) in single precision
      !  ... where R is a random number from -1.0 to 1.0
      ppser_zrperturb = - rperturb * rprecision
    ENDIF
  ELSE IF ( PRESENT(rprecision) ) THEN
    PRINT*,'Perturbation initialization not complete. rperturb is missing' 
    CALL EXIT(1)
  ELSE IF ( PRESENT(rperturb) ) THEN
    PRINT*,'Perturbation initialization not complete. rprecision is missing' 
    CALL EXIT(1)
  ENDIF

END SUBROUTINE ppser_initialize


SUBROUTINE ppser_finalize()

  IF ( ppser_initialized ) THEN
    CALL fs_destroy_savepoint(ppser_savepoint)
    CALL fs_destroy_serializer(ppser_serializer)
    IF ( ppser_hasref ) THEN
      CALL fs_destroy_serializer(ppser_serializer_ref)
    ENDIF
  ENDIF
  ppser_initialized = .FALSE.

END SUBROUTINE ppser_finalize


SUBROUTINE ppser_set_mode(mode)
  INTEGER, INTENT(IN) :: mode

  ppser_mode = mode

END SUBROUTINE ppser_set_mode

FUNCTION ppser_get_mode()
  INTEGER :: ppser_get_mode

  ppser_get_mode = ppser_mode

END FUNCTION ppser_get_mode

END MODULE utils_ppser
