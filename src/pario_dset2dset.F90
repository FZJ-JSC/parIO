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
!> @file pario_dset2dset.F90
!! @brief main public parIO functions
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "pario_defines.inc"

module pario_dset2dset
    use hdf5
    use pario_consts
    use pario_funcs
    implicit none
    save

    ! collective/independent read dataset to dataset
    !  - low level hdf5 read
    !  - file io_dset2dset_template.F90
    interface r_dset2dset
      module procedure &
      r_dset2dset_1d_integer4, &
      r_dset2dset_2d_integer4, &
      r_dset2dset_3d_integer4, &
      r_dset2dset_4d_integer4, &
      r_dset2dset_1d_real4, &
      r_dset2dset_2d_real4, &
      r_dset2dset_3d_real4, &
      r_dset2dset_4d_real4, &
      r_dset2dset_1d_real8, &
      r_dset2dset_2d_real8, &
      r_dset2dset_3d_real8, &
      r_dset2dset_4d_real8
    end interface r_dset2dset

    ! collective/independent write dataset to dataset
    !  - low level hdf5 write
    !  - file io_dset2dset_template.F90
    interface w_dset2dset
      module procedure &
      w_dset2dset_1d_integer4, &
      w_dset2dset_2d_integer4, &
      w_dset2dset_3d_integer4, &
      w_dset2dset_4d_integer4, &
      w_dset2dset_1d_real4, &
      w_dset2dset_2d_real4, &
      w_dset2dset_3d_real4, &
      w_dset2dset_4d_real4, &
      w_dset2dset_1d_real8, &
      w_dset2dset_2d_real8, &
      w_dset2dset_3d_real8, &
      w_dset2dset_4d_real8
    end interface w_dset2dset

    contains

!---------
! include interf_dset2dset.templ files
!---------
#include "interf/dset2dset/dset2dset_1d_real4.F90"
#include "interf/dset2dset/dset2dset_2d_real4.F90"
#include "interf/dset2dset/dset2dset_3d_real4.F90"
#include "interf/dset2dset/dset2dset_4d_real4.F90"

#include "interf/dset2dset/dset2dset_1d_real8.F90"
#include "interf/dset2dset/dset2dset_2d_real8.F90"
#include "interf/dset2dset/dset2dset_3d_real8.F90"
#include "interf/dset2dset/dset2dset_4d_real8.F90"

#include "interf/dset2dset/dset2dset_1d_integer4.F90"
#include "interf/dset2dset/dset2dset_2d_integer4.F90"
#include "interf/dset2dset/dset2dset_3d_integer4.F90"
#include "interf/dset2dset/dset2dset_4d_integer4.F90"

end module
