!----------------------------------------------------------------------------
! This file is part of parIO
!
! Version 1.3
!
! Copyright (C) 2012 Jens Henrik Goebbert <jens.henrik.goebbert()rwth-aachen.de>
! All rights reserved.
!
!    parIO is free software; you can redistribute it and/or modify
!    it under the terms of the GNU Lesser General Public License as
!    published by the Free Software Foundation and appearing in the
!    file LICENSE.LGPL included in the packaging of this file;
!    either version 3 of the License, or (at your option) any later version.
!
!    Please review the following information to ensure the GNU Lesser
!    General Public License version 3 requirements will be met:
!    http://www.gnu.org/licenses/lgpl-3.0.txt
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU Lesser General Public License for more details.
!----------------------------------------------------------------------------

!========================================
!> @addtogroup main
!! @{
!!
!> @file pario_gdat.F90
!! @brief main public parIO functions
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "pario_defines.inc"

module pario_gdat
    use hdf5
    use pario_consts
    use pario_funcs
    use pario_dset2dset
    implicit none
    save

    ! read global data to file (using r_dset2dset_h5(..))
    !  - independent read + broadcast to all mpi procs
    !  - read with mpi proc 0
    !  - mpi broadcast to all others
    !  - optional dump_test
    !  - must be called by all mpi_procs simultaniously, but with h5-file opened only from mpi proc 0
    !  - file io_mpi0_template.F90
    interface r_gdat
      module procedure &
      r_gdat_integer4, &
      r_gdat_real4, &
      r_gdat_real8, &
      r_gdat_integer4s, &
      r_gdat_real4s, &
      r_gdat_real8s, &
      r_gdat_1d_integer4, &
      r_gdat_2d_integer4, &
      r_gdat_3d_integer4, &
      r_gdat_4d_integer4, &
      r_gdat_1d_real4, &
      r_gdat_2d_real4, &
      r_gdat_3d_real4, &
      r_gdat_4d_real4, &
      r_gdat_1d_real8, &
      r_gdat_2d_real8, &
      r_gdat_3d_real8, &
      r_gdat_4d_real8
    end interface r_gdat

    ! write global data to file (using w_dset2dset(..))
    !  - independent write from mpi proc mpi proc 0
    !  - init dataset
    !  - optional dump test
    !  - must be called by all mpi_procs simultaniously, but with h5-file opened only from mpi proc 0
    !  - file io_mpi0_template.F90
    interface w_gdat
      module procedure &
      w_gdat_integer4, &
      w_gdat_real4, &
      w_gdat_real8, &
      w_gdat_integer4s, &
      w_gdat_real4s, &
      w_gdat_real8s, &
      w_gdat_1d_integer4, &
      w_gdat_2d_integer4, &
      w_gdat_3d_integer4, &
      w_gdat_4d_integer4, &
      w_gdat_1d_real4, &
      w_gdat_2d_real4, &
      w_gdat_3d_real4, &
      w_gdat_4d_real4, &
      w_gdat_1d_real8, &
      w_gdat_2d_real8, &
      w_gdat_3d_real8, &
      w_gdat_4d_real8
    end interface w_gdat

    contains

!---------
! include interf_gdat.templ files
!---------
#include "interf/gdat/gdat_1d_real4.F90"
#include "interf/gdat/gdat_2d_real4.F90"
#include "interf/gdat/gdat_3d_real4.F90"
#include "interf/gdat/gdat_4d_real4.F90"

#include "interf/gdat/gdat_1d_real8.F90"
#include "interf/gdat/gdat_2d_real8.F90"
#include "interf/gdat/gdat_3d_real8.F90"
#include "interf/gdat/gdat_4d_real8.F90"

#include "interf/gdat/gdat_1d_integer4.F90"
#include "interf/gdat/gdat_2d_integer4.F90"
#include "interf/gdat/gdat_3d_integer4.F90"
#include "interf/gdat/gdat_4d_integer4.F90"

end module
