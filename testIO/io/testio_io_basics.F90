!----------------------------------------------------------------------------
! This file is part of testIO
!
! Version 1.3
!
! Copyright (C) 2012 Jens Henrik Goebbert <jens.henrik.goebbert()rwth-aachen.de>
! All rights reserved.
!
!    testIO is free software; Permission is hereby granted, free of charge, to
!    any person obtaining a copy of this software and associated documentation
!    files (the "Software"), to deal in the Software without restriction,
!    including without limitation the rights to use, copy, modify, merge,
!    publish, distribute, sublicense, and/or sell copies of the Software, and
!    to permit persons to whom the Software is furnished to do so, subject to
!    the following conditions:
!
!    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
!    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
!    DEALINGS IN THE SOFTWARE.
!----------------------------------------------------------------------------

!========================================
!> @addtogroup _io_
!! @{
!!
!> @file testio_io_basics.F90
!! @brief file of the io_basics module
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "testio_defines.inc"

      module testio_io_basics
      use hdf5
      use pario
      use testio_data

      implicit none
      save
      private

      public :: read_globals_basics,	    &
                 write_globals_basics,      &
                 read_proc_data_basics,		&
                 write_proc_data_basics

      contains

     !
     !========================================
     !
     !	read global settings
     !
     !========================================
     !
      subroutine read_globals_basics(file_id, ierr)
        implicit none

     !	function args
        integer(HID_T), intent(in) ::	file_id
        integer, intent(out) :: ierr

     !  hd5 file vars
        integer(HID_T)  ::  group_id, sub_group_id
        character(len=256) :: sub_gname
        integer :: int_id, sub_idx
        real(kind=4) :: real4_id
        real(kind=8) :: real8_id
        character(len=32) :: chars32

        PRTFNC(read_globals_basics)
        ierr = 0

     !          read "/globals_0d"
     !      ---------------------------
            if(open_group(file_id, '/globals_0d', group_id) ) then

                ! loop over all subgroups and try to open them
                sub_idx = -1
                do while(get_subgroup(file_id, '/globals_0d', sub_idx, sub_gname))
                   if(open_group(group_id, sub_gname, sub_group_id) ) then

                      call close_group(sub_group_id, ierr); CHKWARN0(ierr)
                   end if
                end do

                call r_gval(group_id, 'real4_0d', greal4_0d, ierr)
                call r_gval(group_id, 'real8_0d', greal8_0d, ierr)
                call r_gval(group_id, 'int4_0d',  gint4_0d,  ierr)
                !call r_gval(group_id, 'int8_0d',  gint8_0d,  ierr)
                call r_gval(group_id, 'cmplx4_0d', gcmplx4_0d, ierr)
                call r_gval(group_id, 'cmplx8_0d', gcmplx8_0d, ierr)

                ! test precision conversion
                call r_gval(group_id, 'real8_0d', greal4_0d, ierr)
                call r_gval(group_id, 'real4_0d', greal8_0d, ierr)
                call r_gval(group_id, 'cmplx8_0d', gcmplx4_0d, ierr)
                call r_gval(group_id, 'cmplx4_0d', gcmplx8_0d, ierr)

                ! read integer attributes
                call r_gprop_attr(group_id, 'real4_0d',  'int_id', int_id, ierr)
                call r_gprop_attr(group_id, 'real8_0d',  'int_id', int_id, ierr)
                call r_gprop_attr(group_id, 'int4_0d',   'int_id', int_id,  ierr)
                !call r_gprop_attr(group_id, 'int8_0d',   'int_id', int_id,  ierr)
                call r_gprop_attr(group_id, 'cmplx4_0d', 'int_id', int_id, ierr)
                call r_gprop_attr(group_id, 'cmplx8_0d', 'int_id', int_id, ierr)

                ! read real4 attributes
                call r_gprop_attr(group_id, 'real4_0d', 'real4_id', real4_id, ierr)
                call r_gprop_attr(group_id, 'real8_0d', 'real4_id', real4_id, ierr)
                call r_gprop_attr(group_id, 'int4_0d',  'real4_id', real4_id, ierr)
                !call r_gprop_attr(group_id, 'int8_0d',  'real4_id', real4_id, ierr)
                call r_gprop_attr(group_id, 'cmplx4_0d', 'real4_id', real4_id, ierr)
                call r_gprop_attr(group_id, 'cmplx8_0d', 'real4_id', real4_id, ierr)

                ! read real8 attributes
                call r_gprop_attr(group_id, 'real4_0d', 'real8_id', real8_id, ierr)
                call r_gprop_attr(group_id, 'real8_0d', 'real8_id', real8_id, ierr)
                call r_gprop_attr(group_id, 'int4_0d',  'real8_id', real8_id, ierr)
                !call r_gprop_attr(group_id, 'int8_0d',  'real8_id', real8_id, ierr)
                call r_gprop_attr(group_id, 'cmplx4_0d', 'real8_id', real8_id, ierr)
                call r_gprop_attr(group_id, 'cmplx8_0d', 'real8_id', real8_id, ierr)

                ! read character attributes
                call r_gprop_attr(group_id, 'real4_0d', 'chars_id', chars32, ierr)
                call r_gprop_attr(group_id, 'real8_0d', 'chars_id', chars32, ierr)
                call r_gprop_attr(group_id, 'int4_0d',  'chars_id', chars32, ierr)
                !call r_gprop_attr(group_id, 'int8_0d',  'chars_id', chars32, ierr)
                call r_gprop_attr(group_id, 'cmplx4_0d', 'chars_id', chars32, ierr)
                call r_gprop_attr(group_id, 'cmplx8_0d', 'chars_id', chars32, ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            end if

     !          read "/globals_1d"
     !      ---------------------------
            if(open_group(file_id, '/globals_1d', group_id) ) then
                call r_gval(group_id, 'real4_1d', gdims_1d, greal4_1d, ierr)
                call r_gval(group_id, 'real8_1d', gdims_1d, greal8_1d, ierr)
                call r_gval(group_id, 'int4_1d',  gdims_1d, gint4_1d,  ierr)
                !call r_gval(group_id, 'int8_1d',  gdims_1d, gint8_1d,  ierr)
                call r_gval(group_id, 'cmplx4_1d',gdims_1d, gcmplx4_1d,ierr)
                call r_gval(group_id, 'cmplx8_1d',gdims_1d, gcmplx8_1d,ierr)

                ! test precision conversion
                PRTINFO1('precision conversion: 8 <-> 4')
                call r_gval(group_id, 'real8_1d', gdims_1d, greal4_1d, ierr)
                call r_gval(group_id, 'real4_1d', gdims_1d, greal8_1d, ierr)
                call r_gval(group_id, 'cmplx8_1d',gdims_1d, gcmplx4_1d,ierr)
                call r_gval(group_id, 'cmplx4_1d',gdims_1d, gcmplx8_1d,ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            end if

     !          read "/globals_2d"
     !      ---------------------------
            if(open_group(file_id, '/globals_2d', group_id) ) then
                call r_gval(group_id, 'real4_2d', gdims_2d, greal4_2d, ierr)
                call r_gval(group_id, 'real8_2d', gdims_2d, greal8_2d, ierr)
                call r_gval(group_id, 'int4_2d',  gdims_2d, gint4_2d,  ierr)
                !call r_gval(group_id, 'int8_2d',  gdims_2d, gint8_2d,  ierr)
                call r_gval(group_id, 'cmplx4_2d',gdims_2d, gcmplx4_2d,ierr)
                call r_gval(group_id, 'cmplx8_2d',gdims_2d, gcmplx8_2d,ierr)

                ! test precision conversion
                PRTINFO1('precision conversion: 8 <-> 4')
                call r_gval(group_id, 'real8_2d', gdims_2d, greal4_2d, ierr)
                call r_gval(group_id, 'real4_2d', gdims_2d, greal8_2d, ierr)
                call r_gval(group_id, 'cmplx8_2d',gdims_2d, gcmplx4_2d,ierr)
                call r_gval(group_id, 'cmplx4_2d',gdims_2d, gcmplx8_2d,ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            end if

     !          read "/globals_3d"
     !      ---------------------------
            if(open_group(file_id, '/globals_3d', group_id) ) then
                call r_gval(group_id, 'real4_3d', gdims_3d, greal4_3d, ierr)
                call r_gval(group_id, 'real8_3d', gdims_3d, greal8_3d, ierr)
                call r_gval(group_id, 'int4_3d',  gdims_3d, gint4_3d,  ierr)
                !call r_gval(group_id, 'int8_3d',  gdims_3d, gint8_3d,  ierr)
                call r_gval(group_id, 'cmplx4_3d',gdims_3d, gcmplx4_3d,ierr)
                call r_gval(group_id, 'cmplx8_3d',gdims_3d, gcmplx8_3d,ierr)

                ! test precision conversion
                PRTINFO1('precision conversion: 8 <-> 4')
                call r_gval(group_id, 'real8_3d', gdims_3d, greal4_3d, ierr)
                call r_gval(group_id, 'real4_3d', gdims_3d, greal8_3d, ierr)
                call r_gval(group_id, 'cmplx8_3d',gdims_3d, gcmplx4_3d,ierr)
                call r_gval(group_id, 'cmplx4_3d',gdims_3d, gcmplx8_3d,ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            end if

      end subroutine read_globals_basics

     !
     !========================================
     !
     !  write global settings
     !
     !========================================
     !
      subroutine write_globals_basics(file_id, ierr)
        implicit none

     !  function args
        integer(HID_T), intent(in)  ::  file_id
        integer, intent(out) :: ierr

     !  hd5 file vars
        integer(HID_T)   :: group_id, sub_group_id
        integer :: int_id
        real(kind=4) :: real4_id
        real(kind=8) :: real8_id
        character(len=32) :: chars32

        PRTFNC(write_globals_basics)
        ierr=0

     !          write "/globals_0d"
     !      ---------------------------
            if(create_group(file_id, '/globals_0d', group_id)) then

                if(create_group(file_id, '/sub1_globals_0d', sub_group_id)) then
                   call close_group(sub_group_id, ierr); CHKWARN0(ierr)
                end if
                if(create_group(file_id, '/sub2_globals_0d', sub_group_id)) then
                   call close_group(sub_group_id, ierr); CHKWARN0(ierr)
                end if

                ! write data
                call w_gval(group_id, 'real4_0d',  greal4_0d, ierr)
                call w_gval(group_id, 'real8_0d',  greal8_0d, ierr)
                call w_gval(group_id, 'int4_0d',   gint4_0d,  ierr)
                !call w_gval(group_id, 'int8_0d',   gint8_0d,  ierr)
                call w_gval(group_id, 'cmplx4_0d', gcmplx4_0d, ierr)
                call w_gval(group_id, 'cmplx8_0d', gcmplx8_0d, ierr)

                ! write integer attributes
                int_id = 1
                call w_gprop_attr(group_id, 'real4_0d',  'int_id', int_id, ierr)
                call w_gprop_attr(group_id, 'real8_0d',  'int_id', int_id, ierr)
                call w_gprop_attr(group_id, 'int4_0d',   'int_id', int_id,  ierr)
                !call w_gprop_attr(group_id, 'int8_0d',   'int_id', int_id,  ierr)
                call w_gprop_attr(group_id, 'cmplx4_0d', 'int_id', int_id, ierr)
                call w_gprop_attr(group_id, 'cmplx8_0d', 'int_id', int_id, ierr)

                ! write real4 attributes
                real4_id = 1.0
                call w_gprop_attr(group_id, 'real4_0d', 'real4_id', real4_id, ierr)
                call w_gprop_attr(group_id, 'real8_0d', 'real4_id', real4_id, ierr)
                call w_gprop_attr(group_id, 'int4_0d',  'real4_id', real4_id, ierr)
                !call w_gprop_attr(group_id, 'int8_0d',  'real4_id', real4_id, ierr)
                call w_gprop_attr(group_id, 'cmplx4_0d', 'real4_id', real4_id, ierr)
                call w_gprop_attr(group_id, 'cmplx8_0d', 'real4_id', real4_id, ierr)

                ! write real8 attributes
                real8_id = 1.d0
                call w_gprop_attr(group_id, 'real4_0d', 'real8_id', real8_id, ierr)
                call w_gprop_attr(group_id, 'real8_0d', 'real8_id', real8_id, ierr)
                call w_gprop_attr(group_id, 'int4_0d',  'real8_id', real8_id, ierr)
                !call w_gprop_attr(group_id, 'int8_0d',  'real8_id', real8_id, ierr)
                call w_gprop_attr(group_id, 'cmplx4_0d', 'real8_id', real8_id, ierr)
                call w_gprop_attr(group_id, 'cmplx8_0d', 'real8_id', real8_id, ierr)

                ! write character attributes
                chars32 = 'test it ...'
                call w_gprop_attr(group_id, 'real4_0d', 'chars_id', chars32, ierr)
                call w_gprop_attr(group_id, 'real8_0d', 'chars_id', chars32, ierr)
                call w_gprop_attr(group_id, 'int4_0d',  'chars_id', chars32, ierr)
                !call w_gprop_attr(group_id, 'int8_0d',  'chars_id', chars32, ierr)
                call w_gprop_attr(group_id, 'cmplx4_0d', 'chars_id', chars32, ierr)
                call w_gprop_attr(group_id, 'cmplx8_0d', 'chars_id', chars32, ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(creating /globals_0d FAILED)
            end if

     !          read "/globals_1d"
     !      ---------------------------
            if(create_group(file_id, '/globals_1d', group_id)) then
                call w_gval(group_id, 'real4_1d', gdims_1d, greal4_1d, ierr)
                call w_gval(group_id, 'real8_1d', gdims_1d, greal8_1d, ierr)
                call w_gval(group_id, 'int4_1d',  gdims_1d, gint4_1d,  ierr)
                !call w_gval(group_id, 'int8_1d',  gdims_1d, gint8_1d,  ierr)
                call w_gval(group_id, 'cmplx4_1d',gdims_1d, gcmplx4_1d,ierr)
                call w_gval(group_id, 'cmplx8_1d',gdims_1d, gcmplx8_1d,ierr)
                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(creating /globals_1d FAILED)
            end if

     !          read "/globals_2d"
     !      ---------------------------
            if(create_group(file_id, '/globals_2d', group_id)) then
                call w_gval(group_id, 'real4_2d',  gdims_2d, greal4_2d,  ierr)
                call w_gval(group_id, 'real8_2d',  gdims_2d, greal8_2d,  ierr)
                call w_gval(group_id, 'int4_2d',   gdims_2d, gint4_2d,   ierr)
                !call w_gval(group_id, 'int8_2d',   gdims_2d, gint8_2d,   ierr)
                call w_gval(group_id, 'cmplx4_2d', gdims_2d, gcmplx4_2d, ierr)
                call w_gval(group_id, 'cmplx8_2d', gdims_2d, gcmplx8_2d, ierr)
                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(creating /globals_2d FAILED)
            end if

     !          read "/globals_3d"
     !      ---------------------------
            if(create_group(file_id, '/globals_3d', group_id)) then
                call w_gval(group_id, 'real4_3d',  gdims_3d, greal4_3d, ierr)
                call w_gval(group_id, 'real8_3d',  gdims_3d, greal8_3d, ierr)
                call w_gval(group_id, 'int4_3d',   gdims_3d, gint4_3d,  ierr)
                !call w_gval(group_id, 'int8_3d',   gdims_3d, gint8_3d,  ierr)
                call w_gval(group_id, 'cmplx4_3d', gdims_3d, gcmplx4_3d, ierr)
                call w_gval(group_id, 'cmplx8_3d', gdims_3d, gcmplx8_3d, ierr)
                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(creating /globals_3d FAILED)
            end if

     if(test_hysl2dset) then
     !          init "/procs_3d_hysl2dset"
     !      ---------------------------
        if(on_3d) then
            if(create_group(file_id, '/procs_3d_hysl2dset',group_id)) then
                call init_dataset(group_id, 'real4_3d',  3, domain_dims_3d, H5T_NATIVE_REAL,    ierr, chunk_3d)
                call init_dataset(group_id, 'real8_3d',  3, domain_dims_3d, H5T_NATIVE_DOUBLE,  ierr, chunk_3d)
                call init_dataset(group_id, 'int4_3d',   3, domain_dims_3d, H5T_NATIVE_INTEGER, ierr, chunk_3d)
                !call init_dataset(group_id, 'int8_3d',  3, domain_dims_3d, H5T_NATIVE_??, ierr)
                call init_dataset(group_id, 'cmplx4_3d', 3, cdomain_dims_3d, H5T_NATIVE_REAL,   ierr, chunk_3d)
                call init_dataset(group_id, 'cmplx8_3d', 3, cdomain_dims_3d, H5T_NATIVE_DOUBLE, ierr, chunk_3d)
                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(creating /procs_3d_hysl2dset FAILED)
            end if
        end if

     !          init "/procs_4d_hysl2dset"
     !      ---------------------------
        if(on_4d) then
            if(create_group(file_id, '/procs_4d_hysl2dset',group_id)) then
                call init_dataset(group_id, 'real4_4d',  4, domain_dims_4d, H5T_NATIVE_REAL,    ierr, chunk_4d)
                call init_dataset(group_id, 'real8_4d',  4, domain_dims_4d, H5T_NATIVE_DOUBLE,  ierr, chunk_4d)
                call init_dataset(group_id, 'int4_4d',   4, domain_dims_4d, H5T_NATIVE_INTEGER, ierr, chunk_4d)
                !call init_dataset(group_id, 'int8_4d',  4, domain_dims_4d, H5T_NATIVE_??, ierr)
                call init_dataset(group_id, 'cmplx4_4d', 4, cdomain_dims_4d, H5T_NATIVE_REAL,   ierr, chunk_4d)
                call init_dataset(group_id, 'cmplx8_4d', 4, cdomain_dims_4d, H5T_NATIVE_DOUBLE, ierr, chunk_4d)
                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(creating /procs_4d_hysl2dset FAILED)
            end if
        end if
     end if

     if(test_hysl2hysl) then
     !          init "/procs_3d_hysl2hysl"
     !      ---------------------------
        if(on_3d) then
            if(create_group(file_id, '/procs_3d_hysl2hysl',group_id)) then
                call init_dataset(group_id, 'real4_3d',  3, domain_dims_3d, H5T_NATIVE_REAL,    ierr, chunk_3d)
                call init_dataset(group_id, 'real8_3d',  3, domain_dims_3d, H5T_NATIVE_DOUBLE,  ierr, chunk_3d)
                call init_dataset(group_id, 'int4_3d',   3, domain_dims_3d, H5T_NATIVE_INTEGER, ierr, chunk_3d)
                !call init_dataset(group_id, 'int8_3d',  3, domain_dims_3d, H5T_NATIVE_??, ierr)
                call init_dataset(group_id, 'cmplx4_3d', 3, cdomain_dims_3d, H5T_NATIVE_REAL,   ierr, chunk_3d)
                call init_dataset(group_id, 'cmplx8_3d', 3, cdomain_dims_3d, H5T_NATIVE_DOUBLE, ierr, chunk_3d)
                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(creating /procs_3d_hysl2hysl FAILED)
            end if
        end if

     !          init "/procs_4d_hysl2dset"
     !      ---------------------------
        if(on_4d) then
            if(create_group(file_id, '/procs_4d_hysl2hysl',group_id)) then
                call init_dataset(group_id, 'real4_4d',  4, domain_dims_4d, H5T_NATIVE_REAL,    ierr, chunk_4d)
                call init_dataset(group_id, 'real8_4d',  4, domain_dims_4d, H5T_NATIVE_DOUBLE,  ierr, chunk_4d)
                call init_dataset(group_id, 'int4_4d',   4, domain_dims_4d, H5T_NATIVE_INTEGER, ierr, chunk_4d)
                !call init_dataset(group_id, 'int8_4d',  4, domain_dims_4d, H5T_NATIVE_??, ierr)
                call init_dataset(group_id, 'cmplx4_4d', 4, cdomain_dims_4d, H5T_NATIVE_REAL,   ierr, chunk_4d)
                call init_dataset(group_id, 'cmplx8_4d', 4, cdomain_dims_4d, H5T_NATIVE_DOUBLE, ierr, chunk_4d)
                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(creating /procs_4d_hysl2hysl FAILED)
            end if
        end if
      end if

      end subroutine write_globals_basics

     !
     !========================================
     !
     !	read in hdf5 data for processor
     !
     !========================================
     !
      subroutine read_proc_data_basics(file_id, ierr)
        implicit none

     !	function args
        integer(HID_T), intent(in) ::	file_id
        integer, intent(out) :: ierr

     !	other args
        integer(HID_T)	::	group_id

        PRTFNC(read_proc_data_basics)
        ierr = 0

     if(test_hysl2dset) then
     !          read "/procs_3d_hysl2dset"
     !      ---------------------------
        if(on_3d) then
            if(open_group(file_id, '/procs_3d_hysl2dset', group_id)) then
                call r_pval(group_id, 'real4_3d', poffs_3d, pdims_3d, preal4_3d_ptr, ierr)
                call r_pval(group_id, 'real8_3d', poffs_3d, pdims_3d, preal8_3d_ptr, ierr)
                call r_pval(group_id, 'int4_3d',  poffs_3d, pdims_3d, pint4_3d_ptr,  ierr)
                !call r_pval(group_id, 'int8_3d',  poffs_3d, pdims_3d, pint8_3d_ptr,  ierr)
                call r_pval(group_id, 'cmplx4_3d',poffs_3d, pdims_3d, pcmplx4_3d_ptr,ierr)
                call r_pval(group_id, 'cmplx8_3d',poffs_3d, pdims_3d, pcmplx8_3d_ptr,ierr)

                ! test precision conversion
                PRTINFO1('precision conversion: 8 <-> 4')
                call r_pval(group_id, 'real8_3d', poffs_3d, pdims_3d, preal4_3d_ptr, ierr)
                call r_pval(group_id, 'real4_3d', poffs_3d, pdims_3d, preal8_3d_ptr, ierr)
                call r_pval(group_id, 'cmplx8_3d',poffs_3d, pdims_3d, pcmplx4_3d_ptr,ierr)
                call r_pval(group_id, 'cmplx4_3d',poffs_3d, pdims_3d, pcmplx8_3d_ptr,ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(opening /procs_3d_hysl2dset FAILED)
            end if
        end if

     !          read "/procs_4d_hysl2dset"
     !      ---------------------------
        if(on_4d) then
            if(open_group(file_id, '/procs_4d_hysl2dset', group_id)) then
                call r_pval(group_id, 'real4_4d', poffs_4d, pdims_4d, preal4_4d_ptr, ierr)
                call r_pval(group_id, 'real8_4d', poffs_4d, pdims_4d, preal8_4d_ptr, ierr)
                call r_pval(group_id, 'int4_4d',  poffs_4d, pdims_4d, pint4_4d_ptr,  ierr)
                !call r_pval(group_id, 'int8_4d',  poffs_4d, pdims_4d, pint8_4d_ptr,  ierr)
                call r_pval(group_id, 'cmplx4_4d',poffs_4d, pdims_4d, pcmplx4_4d_ptr,ierr)
                call r_pval(group_id, 'cmplx8_4d',poffs_4d, pdims_4d, pcmplx8_4d_ptr,ierr)

                ! test precision conversion
                PRTINFO1('precision conversion: 8 <-> 4')
                call r_pval(group_id, 'real8_4d', poffs_4d, pdims_4d, preal4_4d_ptr, ierr)
                call r_pval(group_id, 'real4_4d', poffs_4d, pdims_4d, preal8_4d_ptr, ierr)
                call r_pval(group_id, 'cmplx8_4d',poffs_4d, pdims_4d, pcmplx4_4d_ptr,ierr)
                call r_pval(group_id, 'cmplx4_4d',poffs_4d, pdims_4d, pcmplx8_4d_ptr,ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(opening /procs_4d_hysl2dset FAILED)
            end if
        end if
      end if

     if(test_hysl2hysl) then
     !          read "/procs_3d_hysl2hysl"
     !      ---------------------------
        if(on_3d) then
            if(open_group(file_id, '/procs_3d_hysl2hysl', group_id)) then
                call r_pval(group_id, 'real4_3d', poffs_3d, pdims_3d, preal4_3d_ptr,  msize_3d, moffs_3d, ierr)
                call r_pval(group_id, 'real8_3d', poffs_3d, pdims_3d, preal8_3d_ptr,  msize_3d, moffs_3d, ierr)
                call r_pval(group_id, 'int4_3d',  poffs_3d, pdims_3d, pint4_3d_ptr,   msize_3d, moffs_3d, ierr)
                !call r_pval(group_id, 'int8_3d',  poffs_3d, pdims_3d, pint8_3d_ptr,   msize_3d, moffs_3d, ierr)
                call r_pval(group_id, 'cmplx4_3d',poffs_3d, pdims_3d, pcmplx4_3d_ptr, msize_3d, moffs_3d, ierr)
                call r_pval(group_id, 'cmplx8_3d',poffs_3d, pdims_3d, pcmplx8_3d_ptr, msize_3d, moffs_3d, ierr)

                ! test precision conversion
                PRTINFO1('precision conversion: 8 <-> 4')
                call r_pval(group_id, 'real8_3d', poffs_3d, pdims_3d, preal4_3d_ptr,  msize_3d, moffs_3d, ierr)
                call r_pval(group_id, 'real4_3d', poffs_3d, pdims_3d, preal8_3d_ptr,  msize_3d, moffs_3d, ierr)
                call r_pval(group_id, 'cmplx8_3d',poffs_3d, pdims_3d, pcmplx4_3d_ptr, msize_3d, moffs_3d, ierr)
                call r_pval(group_id, 'cmplx4_3d',poffs_3d, pdims_3d, pcmplx8_3d_ptr, msize_3d, moffs_3d, ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(opening /procs_3d_hysl2hysl FAILED)
            end if
        end if

     !          read "/procs_4d_hysl2hysl"
     !      ---------------------------
        if(on_4d) then
            if(open_group(file_id, '/procs_4d_hysl2hysl', group_id)) then
                call r_pval(group_id, 'real4_4d', poffs_4d, pdims_4d, preal4_4d_ptr,  msize_4d, moffs_4d, ierr)
                call r_pval(group_id, 'real8_4d', poffs_4d, pdims_4d, preal8_4d_ptr,  msize_4d, moffs_4d, ierr)
                call r_pval(group_id, 'int4_4d',  poffs_4d, pdims_4d, pint4_4d_ptr,   msize_4d, moffs_4d, ierr)
                !call r_pval(group_id, 'int8_4d',  poffs_4d, pdims_4d, pint8_4d_ptr,   msize_4d, moffs_4d, ierr)
                call r_pval(group_id, 'cmplx4_4d',poffs_4d, pdims_4d, pcmplx4_4d_ptr, msize_4d, moffs_4d, ierr)
                call r_pval(group_id, 'cmplx8_4d',poffs_4d, pdims_4d, pcmplx8_4d_ptr, msize_4d, moffs_4d, ierr)

                ! test precision conversion
                PRTINFO1('precision conversion: 8 <-> 4')
                call r_pval(group_id, 'real8_4d', poffs_4d, pdims_4d, preal4_4d_ptr,  msize_4d, moffs_4d, ierr)
                call r_pval(group_id, 'real4_4d', poffs_4d, pdims_4d, preal8_4d_ptr,  msize_4d, moffs_4d, ierr)
                call r_pval(group_id, 'cmplx8_4d',poffs_4d, pdims_4d, pcmplx4_4d_ptr, msize_4d, moffs_4d, ierr)
                call r_pval(group_id, 'cmplx4_4d',poffs_4d, pdims_4d, pcmplx8_4d_ptr, msize_4d, moffs_4d, ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(opening /procs_4d_hysl2hysl FAILED)
            end if
        end if
      end if

      end subroutine read_proc_data_basics

     !
     !========================================
     !
     !	write in hdf5 data for processor
     !
     !========================================
     !
      subroutine write_proc_data_basics(file_id, ierr)
        implicit none

     !	function args
        integer(HID_T), intent(in) ::	file_id
        integer, intent(out) :: ierr

     !	other args
        integer(HID_T)	::	group_id


        PRTFNC(write_proc_data_basics)
        ierr = 0

     if(test_hysl2dset) then
     !          write "/procs_3d_hysl2dset"
     !      ---------------------------
        if(on_3d) then
            if(open_group(file_id, '/procs_3d_hysl2dset',group_id)) then
                call w_pval(group_id, 'real4_3d', poffs_3d, pdims_3d, preal4_3d_ptr, ierr)
                call w_pval(group_id, 'real8_3d', poffs_3d, pdims_3d, preal8_3d_ptr, ierr)
                call w_pval(group_id, 'int4_3d',  poffs_3d, pdims_3d, pint4_3d_ptr,  ierr)
                !call w_pval(group_id, 'int8_3d',  poffs_3d, pdims_3d, pint8_3d_ptr,  ierr)
                call w_pval(group_id, 'cmplx4_3d',poffs_3d, pdims_3d,pcmplx4_3d_ptr, ierr)
                call w_pval(group_id, 'cmplx8_3d',poffs_3d, pdims_3d,pcmplx8_3d_ptr, ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(opening /procs_3d_hysl2dset FAILED)
            end if
        end if

     !          write "/procs_4d_hysl2dset"
     !      ---------------------------
        if(on_4d) then
            if(open_group(file_id, '/procs_4d_hysl2dset',group_id)) then
                call w_pval(group_id, 'real4_4d', poffs_4d, pdims_4d, preal4_4d_ptr, ierr)
                call w_pval(group_id, 'real8_4d', poffs_4d, pdims_4d, preal8_4d_ptr, ierr)
                call w_pval(group_id, 'int4_4d',  poffs_4d, pdims_4d, pint4_4d_ptr,  ierr)
                !call w_pval(group_id, 'int8_4d',  poffs_4d, pdims_4d, pint8_4d_ptr,  ierr)
                call w_pval(group_id, 'cmplx4_4d',poffs_4d, pdims_4d,pcmplx4_4d_ptr, ierr)
                call w_pval(group_id, 'cmplx8_4d',poffs_4d, pdims_4d,pcmplx8_4d_ptr, ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(opening /procs_4d_hysl2dset FAILED)
            end if
        end if
     end if

     if(test_hysl2hysl) then
     !          write "/procs_3d_hysl2hysl"
     !      ---------------------------
        if(on_3d) then
            if(open_group(file_id, '/procs_3d_hysl2hysl',group_id)) then
                call w_pval(group_id, 'real4_3d', poffs_3d, pdims_3d, preal4_3d_ptr, msize_3d, moffs_3d, ierr)
                call w_pval(group_id, 'real8_3d', poffs_3d, pdims_3d, preal8_3d_ptr, msize_3d, moffs_3d, ierr)
                call w_pval(group_id, 'int4_3d',  poffs_3d, pdims_3d, pint4_3d_ptr,  msize_3d, moffs_3d,  ierr)
                !call w_pval(group_id, 'int8_3d',  poffs_3d, pdims_3d, pint8_3d_ptr, msize_3d, moffs_3d,  ierr)
                call w_pval(group_id, 'cmplx4_3d',poffs_3d, pdims_3d,pcmplx4_3d_ptr, msize_3d, moffs_3d, ierr)
                call w_pval(group_id, 'cmplx8_3d',poffs_3d, pdims_3d,pcmplx8_3d_ptr, msize_3d, moffs_3d, ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(opening /procs_3d_hysl2hysl FAILED)
            end if
        end if

     !          write "/procs_4d_hysl2hysl"
     !      ---------------------------
        if(on_4d) then
            if(open_group(file_id, '/procs_4d_hysl2hysl',group_id)) then
                call w_pval(group_id, 'real4_4d', poffs_4d, pdims_4d, preal4_4d_ptr, msize_4d, moffs_4d, ierr)
                call w_pval(group_id, 'real8_4d', poffs_4d, pdims_4d, preal8_4d_ptr, msize_4d, moffs_4d, ierr)
                call w_pval(group_id, 'int4_4d',  poffs_4d, pdims_4d, pint4_4d_ptr, msize_4d, moffs_4d,  ierr)
                !call w_pval(group_id, 'int8_4d',  poffs_4d, pdims_4d, pint8_4d_ptr, msize_4d, moffs_4d,  ierr)
                call w_pval(group_id, 'cmplx4_4d',poffs_4d, pdims_4d,pcmplx4_4d_ptr, msize_4d, moffs_4d, ierr)
                call w_pval(group_id, 'cmplx8_4d',poffs_4d, pdims_4d,pcmplx8_4d_ptr, msize_4d, moffs_4d, ierr)

                call close_group(group_id, ierr); CHKWARN0(ierr)
            else
                PRTWARN1(opening /procs_4d_hysl2hysl FAILED)
            end if
        end if
      end if

      end subroutine write_proc_data_basics

      end module
