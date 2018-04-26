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
!> @file pario_gval.F90
!! @brief main public parIO functions
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "pario_defines.inc"

module pario_gval
    use hdf5
    use pario_consts
    use pario_funcs
    use pario_gdat
    implicit none
    save

    ! read global values from file (using r_gdat(..))
    !  - convert precision if nessesary
    !  - must be called by all mpi_procs simultaniously, but with h5-file opened only from mpi proc 0
    !  - file io_glob_template.F90
    interface r_gval
      module procedure  &
      r_gval_logi,        &
      r_gval_integer4,    &
      r_gval_real4,       &
      r_gval_real8,       &
      r_gval_complex4,    &
      r_gval_complex8,    &
      r_gval_integer4s,   &
      r_gval_real4s,      &
      r_gval_real8s,      &
      r_gval_complex4s,   &
      r_gval_complex8s,   &
      r_gval_1d_integer4, &
      r_gval_1d_real4,    &
      r_gval_1d_real8,    &
      r_gval_1d_complex4, &
      r_gval_1d_complex8, &
      r_gval_2d_integer4, &
      r_gval_2d_real4,    &
      r_gval_2d_real8,    &
      r_gval_2d_complex4, &
      r_gval_2d_complex8, &
      r_gval_3d_integer4, &
      r_gval_3d_real4,    &
      r_gval_3d_real8,    &
      r_gval_3d_complex4, &
      r_gval_3d_complex8, &
      r_gval_4d_integer4, &
      r_gval_4d_real4,    &
      r_gval_4d_real8,    &
      r_gval_4d_complex4, &
      r_gval_4d_complex8
    end interface r_gval

    ! write global values to file (using w_gdat(..))
    !  - optional convertion of precision (not implemented yet)
    !  - must be called by all mpi_procs simultaniously, but with h5-file opened only from mpi proc 0
    !  - file io_glob_template.F90
    interface w_gval
      module procedure  &
      w_gval_logi,        &
      w_gval_integer4,    &
      w_gval_real4,       &
      w_gval_real8,       &
      w_gval_complex4,    &
      w_gval_complex8,    &
      w_gval_integer4s,   &
      w_gval_real4s,      &
      w_gval_real8s,      &
      w_gval_complex4s,   &
      w_gval_complex8s,   &
      w_gval_1d_integer4, &
      w_gval_1d_real4,    &
      w_gval_1d_real8,    &
      w_gval_1d_complex4, &
      w_gval_1d_complex8, &
      w_gval_2d_integer4, &
      w_gval_2d_real4,    &
      w_gval_2d_real8,    &
      w_gval_2d_complex4, &
      w_gval_2d_complex8, &
      w_gval_3d_integer4, &
      w_gval_3d_real4,    &
      w_gval_3d_real8,    &
      w_gval_3d_complex4, &
      w_gval_3d_complex8, &
      w_gval_4d_integer4, &
      w_gval_4d_real4,    &
      w_gval_4d_real8,    &
      w_gval_4d_complex4, &
      w_gval_4d_complex8
    end interface w_gval

    contains

    !---------------------------------------------------------------------------
    !> @brief   version of read_1d_logi() with logical,non-array input
    !> @details [parallel] collectivly read same 1d dataset from HDF5 file to all mpi procs (incl. optional dump-test)
    !>          Attention: http://www.hdfgroup.org/HDF5/doc/RM/CollectiveCalls.html
    !>                     http://www.hdfgroup.org/HDF5/faq/parallel-apis.html
    !>           'loc_id' must be from an HDF5 file opened by all mpi procs collectivly !!!
    !>           e.g. call H5Pset_fapl_mpio_f(pario_coll_facc_id, mpi_mycomm, mpi_info, ierr)
    !>                call H5Fopen_f(filepath, H5F_ACC_RDONLY_F, file_id, ierr, access_prp = pario_coll_facc_id)
    !>           -read data with read_dset[_IO_MEMVAR_DIMS_]d_[_IO_MEMVAR_TYPE_][_IO_MEMVAR_KIND_]()
    !>           -if dump-test check data
    !>
    !> @param   loc_id    hdf5 location id representing path to dataset group in hdf file
    !> @param   dname     dataset name in hdf file
    !> @param   dvalue    start address of dataset in memory
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine r_gval_logi(loc_id, dname, dvalue, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T), intent(in):: loc_id
      character(*), intent(in) :: dname
      logical, intent(out) :: dvalue
      integer, intent(out) :: ierr

      ! other vars
      integer :: dvalue_tmp

      PRTFNC(r_gval_logi)
      ierr = 0

      call r_gval_integer4(loc_id, dname, dvalue_tmp, ierr)
      if( ierr == 0) then
        if(dvalue_tmp <= 0) then
          dvalue = .false.
        else
          dvalue = .true.
        endif
      endif

    end subroutine r_gval_logi

    !---------------------------------------------------------------------------
    !> @brief   version of write_1d_logi() with logical,non-array output
    !> @details [serial] init dataset and non-collectivly write dataset from memory to dataset in HDF file (incl. optional dump-test)
    !>           Attention: http://www.hdfgroup.org/HDF5/doc/RM/CollectiveCalls.html
    !>                     http://www.hdfgroup.org/HDF5/faq/parallel-apis.html
    !>           'loc_id' must be from an HDF5 file opened by a single mpi proc non-collectivly !!!
    !>           eg. call H5Fcreate_f(filepath, H5F_ACC_EXCL_F, file_id, ierr, access_prp = H5P_DEFAULT)
    !>           function must only be called by this single mpi proc !!!
    !>           -initialise dataset
    !>           -if dump-test overwrite data
    !>           -dump data with w_dset2dset_[_IO_MEMVAR_DIMS_]d_[_IO_MEMVAR_TYPE_][_IO_MEMVAR_KIND_]
    !>
    !> @param   loc_id    hdf5 location id representing path to dataset group in hdf file
    !> @param   dname     dataset name in hdf file
    !> @param   dvalue    start address of dataset in memory
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine w_gval_logi(loc_id, dname, dvalue, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T), intent(in):: loc_id
      character(*), intent(in) :: dname
      logical, intent(in) :: dvalue
      integer, intent(out) :: ierr

      ! other vars
      integer :: dvalue_tmp

      PRTFNC(w_gval_logi)
      ierr = 0

      if(dvalue) then
        dvalue_tmp = 1
      else
        dvalue_tmp = 0
      endif
      call w_gval_integer4(loc_id, dname, dvalue_tmp, ierr)

    end subroutine w_gval_logi

!---------
! include interf_gval.templ files
!---------
#include "interf/gval/gval_1d_real4.F90"
#include "interf/gval/gval_2d_real4.F90"
#include "interf/gval/gval_3d_real4.F90"
#include "interf/gval/gval_4d_real4.F90"

#include "interf/gval/gval_1d_real8.F90"
#include "interf/gval/gval_2d_real8.F90"
#include "interf/gval/gval_3d_real8.F90"
#include "interf/gval/gval_4d_real8.F90"

#include "interf/gval/gval_1d_integer4.F90"
#include "interf/gval/gval_2d_integer4.F90"
#include "interf/gval/gval_3d_integer4.F90"
#include "interf/gval/gval_4d_integer4.F90"

#include "interf/gval/gval_1d_complex4.F03"
#include "interf/gval/gval_2d_complex4.F03"
#include "interf/gval/gval_3d_complex4.F03"
#include "interf/gval/gval_4d_complex4.F03"

#include "interf/gval/gval_1d_complex8.F03"
#include "interf/gval/gval_2d_complex8.F03"
#include "interf/gval/gval_3d_complex8.F03"
#include "interf/gval/gval_4d_complex8.F03"

end module
