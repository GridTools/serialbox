!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license. 
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------


#define ACC_PREFIX !$acc
MODULE m_ser

USE m_serialize, ONLY: &
  fs_add_savepoint_metainfo, &
  fs_read_field, &
  fs_create_savepoint, &
  fs_write_field, &
  fs_read_and_perturb_field
USE utils_ppser, ONLY:  &
  ppser_set_mode, &
  ppser_initialize, &
  ppser_get_mode, &
  ppser_savepoint, &
  ppser_serializer, &
  ppser_serializer_ref, &
  ppser_intlength, &
  ppser_reallength, &
  ppser_realtype, &
  ppser_zrperturb

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE serialize(a)
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(:,:,:) :: a

    ! setup serialization environment
    call ppser_initialize(directory='.',prefix='SerialboxTest')
    call fs_create_savepoint('sp1', ppser_savepoint)
    call ppser_set_mode(0)
    
    SELECT CASE ( ppser_get_mode() )
      CASE(0)
        call fs_write_field(ppser_serializer, ppser_savepoint, 'ser_a', a)
      CASE(1)
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'ser_a', a)
      CASE(2)
        call fs_read_and_perturb_field(ppser_serializer_ref, ppser_savepoint,&
                                                 'ser_a', a, ppser_zrperturb)
    END SELECT

  END SUBROUTINE serialize

  SUBROUTINE deserialize(a)
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(:,:,:) :: a
    ! setup serialization environment
    call ppser_initialize(directory='.',prefix='SerialboxTest-output', &
                                                prefix_ref='SerialboxTest')
    call fs_create_savepoint('sp1', ppser_savepoint)
    call ppser_set_mode(1)

    SELECT CASE ( ppser_get_mode() )
      CASE(0)
        call fs_write_field(ppser_serializer, ppser_savepoint, 'ser_a', a)
      CASE(1)
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'ser_a', a)
      CASE(2)
        call fs_read_and_perturb_field(ppser_serializer_ref, ppser_savepoint,& 
                                                'ser_a', a, ppser_zrperturb)
    END SELECT
    call ppser_set_mode(0)

    SELECT CASE ( ppser_get_mode() )
      CASE(0)
        call fs_write_field(ppser_serializer, ppser_savepoint, 'ser_a', a)
      CASE(1)
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'ser_a', a)
      CASE(2)
        call fs_read_and_perturb_field(ppser_serializer_ref, ppser_savepoint,&
                                                 'ser_a', a, ppser_zrperturb)
    END SELECT

  END SUBROUTINE deserialize

  SUBROUTINE deserialize_with_perturb(a)
    IMPLICIT NONE
    REAL(KIND=8), DIMENSION(:,:,:) :: a
    REAL(KIND=8) :: rprecision
    rprecision = 10.0**(-PRECISION(1.0))

    ! setup serialization environment
    call ppser_initialize(directory='.',prefix='SerialboxTest-output', &
          prefix_ref='SerialboxTest',rprecision=rprecision,rperturb=1.0e-5_8)
    call fs_create_savepoint('sp1', ppser_savepoint)
    call ppser_set_mode(2)
    ! file: /Volumes/MeteoSwissCode/serialbox2/examples/Fortran/with_pp_ser/m_ser.f90 lineno: #40
    SELECT CASE ( ppser_get_mode() )
      CASE(0)
        call fs_write_field(ppser_serializer, ppser_savepoint, 'ser_a', a)
      CASE(1)
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, 'ser_a', a)
      CASE(2)
        call fs_read_and_perturb_field(ppser_serializer_ref, ppser_savepoint,&
                                                 'ser_a', a, ppser_zrperturb)
    END SELECT

  END SUBROUTINE deserialize_with_perturb

END MODULE m_ser
