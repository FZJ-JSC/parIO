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
!> @file pario_pval.F90
!! @brief main public parIO functions
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "pario_defines.inc"

module pario_pval
    use hdf5
    use pario_consts
    use pario_funcs
    use pario_pdat
    implicit none
    save

    ! read processor specific data to file (using r_hysl2dset(..))
    interface r_pval
      module procedure  &
      r_pval_1d_integer4_v1, &
      r_pval_2d_integer4_v1, &
      r_pval_3d_integer4_v1, &
      r_pval_4d_integer4_v1, &
      r_pval_1d_real4_v1, &
      r_pval_2d_real4_v1, &
      r_pval_3d_real4_v1, &
      r_pval_4d_real4_v1, &
      r_pval_1d_real8_v1, &
      r_pval_2d_real8_v1, &
      r_pval_3d_real8_v1, &
      r_pval_3d_complex4_v1, &
      r_pval_3d_complex8_v1, &
      r_pval_4d_real8_v1, &
      r_pval_4d_complex4_v1, &
      r_pval_4d_complex8_v1, &
      r_pval_1d_integer4_v2, &
      r_pval_2d_integer4_v2, &
      r_pval_3d_integer4_v2, &
      r_pval_4d_integer4_v2, &
      r_pval_1d_real4_v2, &
      r_pval_2d_real4_v2, &
      r_pval_3d_real4_v2, &
      r_pval_4d_real4_v2, &
      r_pval_1d_real8_v2, &
      r_pval_2d_real8_v2, &
      r_pval_3d_real8_v2, &
      r_pval_3d_complex4_v2,&
      r_pval_3d_complex8_v2, &
      r_pval_4d_real8_v2, &
      r_pval_4d_complex4_v2,&
      r_pval_4d_complex8_v2
    end interface r_pval

    ! write processor specific data to file (using w_dset2hysl(..))
    interface w_pval
      module procedure  &
      w_pval_1d_integer4_v1, &
      w_pval_2d_integer4_v1, &
      w_pval_3d_integer4_v1, &
      w_pval_4d_integer4_v1, &
      w_pval_1d_real4_v1, &
      w_pval_2d_real4_v1, &
      w_pval_3d_real4_v1, &
      w_pval_4d_real4_v1, &
      w_pval_1d_real8_v1, &
      w_pval_2d_real8_v1, &
      w_pval_3d_real8_v1, &
      w_pval_3d_complex4_v1,&
      w_pval_3d_complex8_v1,&
      w_pval_4d_real8_v1, &
      w_pval_4d_complex4_v1,&
      w_pval_4d_complex8_v1, &
      w_pval_1d_integer4_v2, &
      w_pval_2d_integer4_v2, &
      w_pval_3d_integer4_v2, &
      w_pval_4d_integer4_v2, &
      w_pval_1d_real4_v2, &
      w_pval_2d_real4_v2, &
      w_pval_3d_real4_v2, &
      w_pval_4d_real4_v2, &
      w_pval_1d_real8_v2, &
      w_pval_2d_real8_v2, &
      w_pval_3d_real8_v2, &
      w_pval_3d_complex4_v2,&
      w_pval_3d_complex8_v2,&
      w_pval_4d_real8_v2, &
      w_pval_4d_complex4_v2,&
      w_pval_4d_complex8_v2
    end interface w_pval

    contains

!---------
! include interf_pval.templ files
!---------
#include "interf/pval/pval_1d_real4.F90"
#include "interf/pval/pval_2d_real4.F90"
#include "interf/pval/pval_3d_real4.F90"
#include "interf/pval/pval_4d_real4.F90"

#include "interf/pval/pval_1d_real8.F90"
#include "interf/pval/pval_2d_real8.F90"
#include "interf/pval/pval_3d_real8.F90"
#include "interf/pval/pval_4d_real8.F90"

#include "interf/pval/pval_1d_complex4.F03"
#include "interf/pval/pval_2d_complex4.F03"
#include "interf/pval/pval_3d_complex4.F03"
#include "interf/pval/pval_4d_complex4.F03"

#include "interf/pval/pval_1d_complex8.F03"
#include "interf/pval/pval_2d_complex8.F03"
#include "interf/pval/pval_3d_complex8.F03"
#include "interf/pval/pval_4d_complex8.F03"

#include "interf/pval/pval_1d_integer4.F90"
#include "interf/pval/pval_2d_integer4.F90"
#include "interf/pval/pval_3d_integer4.F90"
#include "interf/pval/pval_4d_integer4.F90"

end module
