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
!> @file pario_hysl2hysl.F90
!! @brief main public parIO functions
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "pario_defines.inc"

module pario_hysl2hysl
    use hdf5
    use pario_consts
    use pario_funcs
    implicit none
    save

    ! collective/independent read hyperslab to hyperslab
    !  - low level hdf5 read
    !  - file io_hysl2hysl_template.F90
    interface r_hysl2hysl
      module procedure &
      r_hysl2hysl_1d_integer4, &
      r_hysl2hysl_2d_integer4, &
      r_hysl2hysl_3d_integer4, &
      r_hysl2hysl_4d_integer4, &
      r_hysl2hysl_1d_real4, &
      r_hysl2hysl_2d_real4, &
      r_hysl2hysl_3d_real4, &
      r_hysl2hysl_4d_real4, &
      r_hysl2hysl_1d_real8, &
      r_hysl2hysl_2d_real8, &
      r_hysl2hysl_3d_real8, &
      r_hysl2hysl_4d_real8
    end interface r_hysl2hysl

    ! collective/independent write hyperslab to hyperslab
    !  - low level hdf5 write
    !  - file io_hysl2hysl_template.F90
    interface w_hysl2hysl
      module procedure &
      w_hysl2hysl_1d_integer4, &
      w_hysl2hysl_2d_integer4, &
      w_hysl2hysl_3d_integer4, &
      w_hysl2hysl_4d_integer4, &
      w_hysl2hysl_1d_real4, &
      w_hysl2hysl_2d_real4, &
      w_hysl2hysl_3d_real4, &
      w_hysl2hysl_4d_real4, &
      w_hysl2hysl_1d_real8, &
      w_hysl2hysl_2d_real8, &
      w_hysl2hysl_3d_real8, &
      w_hysl2hysl_4d_real8
    end interface w_hysl2hysl

    contains

!---------
! include interf_hysl2hysl.templ files
!---------
#include "interf/hysl2hysl/hysl2hysl_1d_real4.F90"
#include "interf/hysl2hysl/hysl2hysl_2d_real4.F90"
#include "interf/hysl2hysl/hysl2hysl_3d_real4.F90"
#include "interf/hysl2hysl/hysl2hysl_4d_real4.F90"

#include "interf/hysl2hysl/hysl2hysl_1d_real8.F90"
#include "interf/hysl2hysl/hysl2hysl_2d_real8.F90"
#include "interf/hysl2hysl/hysl2hysl_3d_real8.F90"
#include "interf/hysl2hysl/hysl2hysl_4d_real8.F90"

#include "interf/hysl2hysl/hysl2hysl_1d_integer4.F90"
#include "interf/hysl2hysl/hysl2hysl_2d_integer4.F90"
#include "interf/hysl2hysl/hysl2hysl_3d_integer4.F90"
#include "interf/hysl2hysl/hysl2hysl_4d_integer4.F90"

end module
