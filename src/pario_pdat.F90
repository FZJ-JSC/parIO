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
!> @file pario_pdat.F90
!! @brief main public parIO functions
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "pario_defines.inc"

module pario_pdat
    use hdf5
    use pario_consts
    use pario_funcs
    use pario_hysl2dset
    use pario_hysl2hysl
    implicit none
    save

    ! read processor specific data to file (using r_hysl2dset(..))
    interface r_pdat
      module procedure &
      r_pdat_1d_integer4_v1, &
      r_pdat_2d_integer4_v1, &
      r_pdat_3d_integer4_v1, &
      r_pdat_4d_integer4_v1, &
      r_pdat_1d_real4_v1, &
      r_pdat_2d_real4_v1, &
      r_pdat_3d_real4_v1, &
      r_pdat_4d_real4_v1, &
      r_pdat_1d_real8_v1, &
      r_pdat_2d_real8_v1, &
      r_pdat_3d_real8_v1, &
      r_pdat_4d_real8_v1, &
      r_pdat_1d_integer4_v2, &
      r_pdat_2d_integer4_v2, &
      r_pdat_3d_integer4_v2, &
      r_pdat_4d_integer4_v2, &
      r_pdat_1d_real4_v2, &
      r_pdat_2d_real4_v2, &
      r_pdat_3d_real4_v2, &
      r_pdat_4d_real4_v2, &
      r_pdat_1d_real8_v2, &
      r_pdat_2d_real8_v2, &
      r_pdat_3d_real8_v2, &
      r_pdat_4d_real8_v2
    end interface r_pdat

    ! write processor specific data to file (using w_dset2hysl(..))
    interface w_pdat
      module procedure &
      w_pdat_1d_integer4_v1, &
      w_pdat_2d_integer4_v1, &
      w_pdat_3d_integer4_v1, &
      w_pdat_4d_integer4_v1, &
      w_pdat_1d_real4_v1, &
      w_pdat_2d_real4_v1, &
      w_pdat_3d_real4_v1, &
      w_pdat_4d_real4_v1, &
      w_pdat_1d_real8_v1, &
      w_pdat_2d_real8_v1, &
      w_pdat_3d_real8_v1, &
      w_pdat_4d_real8_v1, &
      w_pdat_1d_integer4_v2, &
      w_pdat_2d_integer4_v2, &
      w_pdat_3d_integer4_v2, &
      w_pdat_4d_integer4_v2, &
      w_pdat_1d_real4_v2, &
      w_pdat_2d_real4_v2, &
      w_pdat_3d_real4_v2, &
      w_pdat_4d_real4_v2, &
      w_pdat_1d_real8_v2, &
      w_pdat_2d_real8_v2, &
      w_pdat_3d_real8_v2, &
      w_pdat_4d_real8_v2
    end interface w_pdat

    contains

!---------
! include interf_pdat.templ files
!---------
#include "interf/pdat/pdat_1d_real4.F90"
#include "interf/pdat/pdat_2d_real4.F90"
#include "interf/pdat/pdat_3d_real4.F90"
#include "interf/pdat/pdat_4d_real4.F90"

#include "interf/pdat/pdat_1d_real8.F90"
#include "interf/pdat/pdat_2d_real8.F90"
#include "interf/pdat/pdat_3d_real8.F90"
#include "interf/pdat/pdat_4d_real8.F90"

#include "interf/pdat/pdat_1d_integer4.F90"
#include "interf/pdat/pdat_2d_integer4.F90"
#include "interf/pdat/pdat_3d_integer4.F90"
#include "interf/pdat/pdat_4d_integer4.F90"

end module
