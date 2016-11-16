!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license. 
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------
!
!+ This module contains the perturbation module of Serialbox.
!
!------------------------------------------------------------------------------

MODULE m_ser_perturb

!-------------------------------------------------------------------------------
!
! Description:
!   This module provides tools to add random perturbation to the prognostic
!   fields. This is usefull to define threashold values for rounding error
!
! Current Code Owner: MeteoSwiss, Oliver Fuhrer
!  phone:  +41  44  256 9359
!  fax:    +XX  XX  XXX XXXX
!  email:  oliver.fuhrer@meteoswiss.ch
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! VX_XX        2013/10/07 Xavier Lapillonne
!  First version, adapted from Oliver's ensemble module
!
! Description
!   These routines perturb a field with a perturbation of the following style
!
!     field = (1 + epsilon) * field
!
!   where epsilon can either be chose relative to the least significant bit 
!   (rperturb<0.0) or as an absolute perturbation value (rperturb>0.0).
!
!   Note: this type of perturbation does not alter zero's and conserves the 
!         positive definitiveness of a field.
!
!==============================================================================


USE iso_c_binding

IMPLICIT NONE

! Global module variables
LOGICAL, SAVE :: lseedinit=.FALSE.

!==============================================================================
! Module procedures
!==============================================================================

INTERFACE ser_fld_perturb
   MODULE PROCEDURE         &
     ser_fld_perturb_float_0d,  &
     ser_fld_perturb_float_1d,  &
     ser_fld_perturb_float_2d,  &
     ser_fld_perturb_float_3d,  &
     ser_fld_perturb_float_4d,  &
     ser_fld_perturb_double_0d, &
     ser_fld_perturb_double_1d, &
     ser_fld_perturb_double_2d, &
     ser_fld_perturb_double_3d, &
     ser_fld_perturb_double_4d
END INTERFACE

!==============================================================================

CONTAINS

  !==============================================================================
  !+ Subroutines that perturb a field randomly
  !------------------------------------------------------------------------------

  SUBROUTINE ser_fld_perturb_float_0d( fld_data, rperturb )

    IMPLICIT NONE

    REAL(KIND=C_FLOAT), INTENT(INOUT) :: fld_data  ! field data
    REAL, INTENT(IN)    :: rperturb
    REAL(KIND=C_FLOAT)                :: repsmat
    INTEGER             :: ierr      ! error code

    IF (.NOT. lseedinit) CALL init_seed

    CALL RANDOM_NUMBER(repsmat) ! repsmat between 0 and 1

    ! apply perturbation
    repsmat = rperturb * (2.0 * repsmat - 1.0)   ! repsmat between -rperturb and rperturb
    fld_data = fld_data * (1.0 + repsmat)

  END SUBROUTINE ser_fld_perturb_float_0d

  !------------------------------------------------------------------------------

  SUBROUTINE ser_fld_perturb_float_1d( fld_data, rperturb )

    IMPLICIT NONE

    REAL(KIND=C_FLOAT), INTENT(INOUT) :: fld_data(:)  ! field data
    REAL, INTENT(IN) :: rperturb
    REAL(KIND=C_FLOAT), ALLOCATABLE :: repsmat(:)
    INTEGER :: ni,  & ! field dimensions
               ierr   ! error code

    ni = SIZE(fld_data, DIM=1)

    IF (.NOT. lseedinit) CALL init_seed

    ALLOCATE(repsmat(ni))

    CALL RANDOM_NUMBER(repsmat) ! repsmat between 0 and 1

    ! apply perturbation
    repsmat = rperturb * (2.0 * repsmat - 1.0)   ! repsmat between -rperturb and rperturb
    fld_data = fld_data * (1.0 + repsmat)

    DEALLOCATE(repsmat)

  END SUBROUTINE ser_fld_perturb_float_1d

  !------------------------------------------------------------------------------

  SUBROUTINE ser_fld_perturb_float_2d( fld_data, rperturb )

    IMPLICIT NONE

    REAL(KIND=C_FLOAT), INTENT(INOUT) :: fld_data(:,:)  ! field data
    REAL, INTENT(IN) :: rperturb
    REAL(KIND=C_FLOAT), ALLOCATABLE :: repsmat(:,:)     ! field data
    INTEGER :: ni, nj, & ! field dimensions
               ierr      ! error code

    ni = SIZE(fld_data, DIM=1)
    nj = SIZE(fld_data, DIM=2)

    ALLOCATE(repsmat(ni, nj))

    IF (.NOT. lseedinit) CALL init_seed

    CALL RANDOM_NUMBER(repsmat) ! repsmat between 0 and 1

    ! apply perturbation
    repsmat = rperturb * (2.0 * repsmat - 1.0)   ! repsmat between -rperturb and rperturb
    fld_data = fld_data * (1.0 + repsmat)

    DEALLOCATE(repsmat)

  END SUBROUTINE ser_fld_perturb_float_2d

  !------------------------------------------------------------------------------

  SUBROUTINE ser_fld_perturb_float_3d( fld_data, rperturb )

    IMPLICIT NONE

    REAL(KIND=C_FLOAT), INTENT(INOUT) :: fld_data(:,:,:)  ! field data
    REAL, INTENT(IN) :: rperturb
    REAL :: reps
    REAL(KIND=C_FLOAT), ALLOCATABLE :: repsmat(:,:,:)     ! field data
    INTEGER :: ni, nj, nk,  & ! field dimensions
               ierr           ! error code

    ni = SIZE(fld_data, DIM=1)
    nj = SIZE(fld_data, DIM=2)
    nk = SIZE(fld_data, DIM=3)

    ALLOCATE(repsmat(ni, nj, nk))

    IF (.NOT. lseedinit) CALL init_seed

    CALL RANDOM_NUMBER(repsmat) !repsmat between 0 and 1

    ! apply perturbation
    repsmat = rperturb * (2.0 * repsmat - 1.0)   ! repsmat between -rperturb and rperturb
    fld_data = fld_data * (1.0 + repsmat)

    DEALLOCATE(repsmat)

  END SUBROUTINE ser_fld_perturb_float_3d

  !------------------------------------------------------------------------------

  SUBROUTINE ser_fld_perturb_float_4d( fld_data, rperturb )

    IMPLICIT NONE

    REAL(KIND=C_FLOAT), INTENT(INOUT) :: fld_data(:,:,:,:)  ! field data
    REAL, INTENT(IN) :: rperturb
    REAL :: reps
    REAL(KIND=C_FLOAT), ALLOCATABLE :: repsmat(:,:,:,:)     ! field data

    INTEGER :: ni, nj, nk, nz, & ! field dimensions
               ierr              ! error code

    ni = SIZE(fld_data, DIM=1)
    nj = SIZE(fld_data, DIM=2)
    nk = SIZE(fld_data, DIM=3)
    nz = SIZE(fld_data, DIM=4)

    ALLOCATE(repsmat(ni, nj, nk, nz))

    IF (.NOT. lseedinit) CALL init_seed

    CALL RANDOM_NUMBER(repsmat) !repsmat between 0 and 1

    ! apply perturbation
    repsmat = rperturb * (2.0 * repsmat - 1.0)   ! repsmat between -rperturb and rperturb
    fld_data = fld_data * (1.0 + repsmat)

    DEALLOCATE(repsmat)

  END SUBROUTINE ser_fld_perturb_float_4d

  !------------------------------------------------------------------------------

  SUBROUTINE ser_fld_perturb_double_0d( fld_data, rperturb )

    IMPLICIT NONE

    REAL(KIND=C_DOUBLE), INTENT(INOUT) :: fld_data  ! field data
    REAL, INTENT(IN)    :: rperturb
    REAL(KIND=C_DOUBLE)                :: repsmat
    INTEGER             :: ierr      ! error code

    IF (.NOT. lseedinit) CALL init_seed

    CALL RANDOM_NUMBER(repsmat) ! repsmat between 0 and 1

    ! apply perturbation
    repsmat = rperturb * (2.0 * repsmat - 1.0)   ! repsmat between -rperturb and rperturb
    fld_data = fld_data * (1.0 + repsmat)

  END SUBROUTINE ser_fld_perturb_double_0d

  SUBROUTINE ser_fld_perturb_double_1d( fld_data, rperturb )

    IMPLICIT NONE

    REAL(KIND=C_DOUBLE), INTENT(INOUT) :: fld_data(:)  ! field data
    REAL, INTENT(IN) :: rperturb
    REAL(KIND=C_DOUBLE), ALLOCATABLE :: repsmat(:)
    INTEGER :: ni,  & ! field dimensions
               ierr   ! error code

    ni = SIZE(fld_data, DIM=1)

    IF (.NOT. lseedinit) CALL init_seed

    ALLOCATE(repsmat(ni))

    CALL RANDOM_NUMBER(repsmat) ! repsmat between 0 and 1

    ! apply perturbation
    repsmat = rperturb * (2.0 * repsmat - 1.0)   ! repsmat between -rperturb and rperturb
    fld_data = fld_data * (1.0 + repsmat)

    DEALLOCATE(repsmat)

  END SUBROUTINE ser_fld_perturb_double_1d

  !------------------------------------------------------------------------------

  SUBROUTINE ser_fld_perturb_double_2d( fld_data, rperturb )

    IMPLICIT NONE

    REAL(KIND=C_DOUBLE), INTENT(INOUT) :: fld_data(:,:)  ! field data
    REAL, INTENT(IN) :: rperturb
    REAL(KIND=C_DOUBLE), ALLOCATABLE :: repsmat(:,:)     ! field data
    INTEGER :: ni, nj, & ! field dimensions
               ierr      ! error code

    ni = SIZE(fld_data, DIM=1)
    nj = SIZE(fld_data, DIM=2)

    ALLOCATE(repsmat(ni, nj))

    IF (.NOT. lseedinit) CALL init_seed

    CALL RANDOM_NUMBER(repsmat) ! repsmat between 0 and 1

    ! apply perturbation
    repsmat = rperturb * (2.0 * repsmat - 1.0)   ! repsmat between -rperturb and rperturb
    fld_data = fld_data * (1.0 + repsmat)

    DEALLOCATE(repsmat)

  END SUBROUTINE ser_fld_perturb_double_2d

  !------------------------------------------------------------------------------

  SUBROUTINE ser_fld_perturb_double_3d( fld_data, rperturb )

    IMPLICIT NONE

    REAL(KIND=C_DOUBLE), INTENT(INOUT) :: fld_data(:,:,:)  ! field data
    REAL, INTENT(IN) :: rperturb
    REAL :: reps
    REAL(KIND=C_DOUBLE), ALLOCATABLE :: repsmat(:,:,:)     ! field data
    INTEGER :: ni, nj, nk,  & ! field dimensions
               ierr           ! error code

    ni = SIZE(fld_data, DIM=1)
    nj = SIZE(fld_data, DIM=2)
    nk = SIZE(fld_data, DIM=3)

    ALLOCATE(repsmat(ni, nj, nk))

    IF (.NOT. lseedinit) CALL init_seed

    CALL RANDOM_NUMBER(repsmat) !repsmat between 0 and 1

    ! apply perturbation
    repsmat = rperturb * (2.0 * repsmat - 1.0)   ! repsmat between -rperturb and rperturb
    fld_data = fld_data * (1.0 + repsmat)

    DEALLOCATE(repsmat)

  END SUBROUTINE ser_fld_perturb_double_3d

  !------------------------------------------------------------------------------

  SUBROUTINE ser_fld_perturb_double_4d( fld_data, rperturb )

    IMPLICIT NONE

    REAL(KIND=C_DOUBLE), INTENT(INOUT) :: fld_data(:,:,:,:)  ! field data
    REAL, INTENT(IN) :: rperturb
    REAL :: reps
    REAL(KIND=C_DOUBLE), ALLOCATABLE :: repsmat(:,:,:,:)     ! field data

    INTEGER :: ni, nj, nk, nz, & ! field dimensions
               ierr              ! error code

    ni = SIZE(fld_data, DIM=1)
    nj = SIZE(fld_data, DIM=2)
    nk = SIZE(fld_data, DIM=3)
    nz = SIZE(fld_data, DIM=4)

    ALLOCATE(repsmat(ni, nj, nk, nz))

    IF (.NOT. lseedinit) CALL init_seed

    CALL RANDOM_NUMBER(repsmat) !repsmat between 0 and 1

    ! apply perturbation
    repsmat = rperturb * (2.0 * repsmat - 1.0)   ! repsmat between -rperturb and rperturb
    fld_data = fld_data * (1.0 + repsmat)

    DEALLOCATE(repsmat)

  END SUBROUTINE ser_fld_perturb_double_4d

  !------------------------------------------------------------------------------

  !==============================================================================
  ! init seed for random generator.
  ! Two subsequent runs should give use
  ! different sequence
  !------------------------------------------------------------------------------
  SUBROUTINE init_seed
    INTEGER, ALLOCATABLE :: seed(:)
    REAL, ALLOCATABLE :: Rseed(:)
    INTEGER :: dt(8), n, dsum, i

    ! Use the default seed to generate a first random list
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n), Rseed(n))
    CALL RANDOM_NUMBER(Rseed)


    ! Generate a number for the seed which should differ for every run.
    ! Concatenate hours (H), minutes (M), second (S), milliseconds (m)
    ! as follow : dsum = mmmmSSMMHH
    ! Every run on a given day are garantee to get a different dsum.
    ! Over several days it is very unlikely to get two times the same number
    ! as we would need to run at the exact same time of the day in millisecond.
    ! Note : adding days, month, year to dsum seems to make the number to
    ! large for RANDOM_SEED(PUT...) and should be avoided.
    CALL DATE_AND_TIME(values=dt)
    dsum= dt(5)+dt(6)*1e2+dt(7)*1e4+dt(8)*1e6

    ! Combine dsum with the default random list
    seed(:)=INT(Rseed*dsum, 4)
    CALL RANDOM_SEED(PUT = seed)

    ! set lseedinit
    lseedinit=.TRUE.

    DEALLOCATE(seed, rseed)

  END SUBROUTINE init_seed

END MODULE m_ser_perturb
