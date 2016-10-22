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


END MODULE m_serialize

