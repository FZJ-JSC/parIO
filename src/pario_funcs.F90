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
!> @file pario_funcs.F90
!! @brief constant values of parIO
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "pario_defines.inc"

module pario_funcs
    implicit none
    save

    contains

    !---------------------------------------------------------------------------
    !> @brief   open file for global read
    !> @details ...
    !>
    !> @param   file_path   file path
    !> @param   file_id     HDF5 file id
    !> @param   ierr        return error code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine open_file_read4gval(file_path_in, file_id, open_err)
      use hdf5
      use pario_data
      implicit none

      ! function args
      character(len=*), intent(in) :: file_path_in
      integer(HID_T), intent(out) :: file_id
      integer, intent(out) :: open_err

      ! other vars
      integer(HID_T) :: driver
      character(len=1024) :: file_path, file_name
      integer :: ierr, pos
      open_err = 0
      PRTFNC(open_file_read4gval)

      file_path = file_path_in

      ! h5 driver depended settings
      call H5Pget_driver_f(pario_noncoll_facc_id, driver, ierr); CHKERRLW (ierr, 0)
      if(driver == H5FD_FAMILY_F) then

        ! get file name from file path
        pos = index(file_path,'/',BACK=.true.)
        !if(pos == 0) pos = index(file_path,'\',BACK=.true.) ! MS Windows
        if(pos == 0) pos = 1
        file_name = file_path(pos:len_trim(file_path))

        ! extend file path
        file_path = trim(file_path)//'/'//file_name

        pos = index(file_path,'.',BACK=.true.)
        file_path = trim(file_path(1:pos-1))//'_%d.h5'
      end if

      file_id = 0 ! important for parIO functions
      PRTVERBOSE2('open file :',trim(file_path),1)
      if(mpi_proc_id == mpi_myroot) then ! important for parIO functions
        call H5Fopen_f(file_path, H5F_ACC_RDONLY_F, file_id, open_err,  &
                       access_prp = pario_noncoll_facc_id)
      end if
      call MPI_bcast(open_err, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      PRTVERBOSE2('file opened :',trim(file_path),1)

    end subroutine open_file_read4gval

    !---------------------------------------------------------------------------
    !> @brief   open file for processor read
    !> @details ...
    !>
    !> @param   file_path   file path
    !> @param   file_id     HDF5 file id
    !> @param   ierr        return error code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine open_file_read4pval(file_path_in, file_id, open_err)
      use hdf5
      use pario_data
      implicit none

      ! function args
      character(len=*), intent(in) :: file_path_in
      integer(HID_T), intent(out) :: file_id
      integer :: open_err

      ! other vars
      integer(HID_T) :: driver
      character(len=1024) :: file_path, file_name
      integer :: ierr, pos
      open_err = 0
      PRTFNC(open_file_read4pval)

      file_path = file_path_in

      ! h5 driver depended settings
      call H5Pget_driver_f(pario_coll_facc_id, driver, ierr); CHKERRLW (ierr, 0)
      if(driver == H5FD_FAMILY_F) then

        ! get file name from file path
        pos = index(file_path,'/',BACK=.true.)
        !if(pos == 0) pos = index(file_path,'\',BACK=.true.) ! MS Windows
        if(pos == 0) pos = 1
        file_name = file_path(pos:len_trim(file_path))

        ! extend file path
        file_path = trim(file_path)//'/'//file_name

        ! modify file name for family-io
        pos = index(file_path,'.',BACK=.true.)
        file_path = trim(file_path(1:pos-1))//'_%d.h5'
      end if

      ! open file
      PRTVERBOSE2('open file :',trim(file_path),1)
      call H5Fopen_f(file_path, H5F_ACC_RDONLY_F, file_id, open_err, &
                     access_prp = pario_coll_facc_id)

      ! wait for everyone to open (if not MPI-IO)
      if(driver /= H5FD_MPIO_F) then
        call MPI_Barrier(mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      end if

      if(open_err == 0) then
        PRTVERBOSE2('file opened :',trim(file_path),1)
      end if

    end subroutine open_file_read4pval

    !---------------------------------------------------------------------------
    !> @brief   open file for global write
    !> @details ...
    !>
    !> @param   file_path   file path
    !> @param   file_id     HDF5 file id
    !> @param   ierr        return error code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine open_file_write4gval(file_path_in, overwrite, file_id, open_err)
      use hdf5
      use pario_data
      implicit none

      ! function args
      character(len=*), intent(in) :: file_path_in
      logical, intent(in) :: overwrite
      integer(HID_T), intent(out) :: file_id
      integer, intent(out) :: open_err

      ! other vars
      integer(HID_T) :: driver
      character(len=1024) :: file_path, file_name
      integer(HID_T)  :: tmp_facc_id
      integer :: ierr

      ! family vars
      integer :: pos, istat
      integer(c_int) :: stat
      integer(c_int16_t) :: mkmode = o'0750'
      character(len=1024, kind=c_char) path

      open_err = 0
      PRTFNC(open_file_write4gval)

      file_path = file_path_in

      ! delete/overwrite old files, if present
      ! sadly we cannot delete directories for family-io
      if(mpi_proc_id == mpi_myroot) then
        if(overwrite) then
          open(unit=10, file=file_path, iostat=open_err)
          if(open_err == 0) then
            PRTVERBOSE2('delete file :', trim(file_path),1)
            close (unit=10, status='delete')
          end if
        end if
      end if

      ! h5 driver depended settings
      call H5Pget_driver_f(pario_noncoll_facc_id, driver, ierr); CHKERRLW (ierr, 0)
      if(driver == H5FD_FAMILY_F) then
        ! /my/path/to/data/file.h5 -> /my/path/to/data/file.h5/file_%d.h5

        istat = 0
        if(overwrite) then ! create new directory from mpi proc 0
          if(mpi_proc_id == mpi_myroot) then
            path = trim(file_path)//c_null_char
            stat = mkdir(trim(path), mkmode)
            istat = stat
            if(istat /= 0 ) then
              PRTINFO2('Could not create directory: ', trim(file_path))
              PRTINFO2('Failed with mkdir-status =', istat)
            end if
          end if
          call MPI_bcast(istat, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
        end if

        if(istat == 0) then ! use directory for family files

          ! get file name from file path
          pos = index(file_path,'/',BACK=.true.)
          !if(pos == 0) pos = index(file_path,'\',BACK=.true.) ! MS Windows
          if(pos == 0) pos = 1
          file_name = file_path(pos:len_trim(file_path))

          ! extend file path
          file_path = trim(file_path)//'/'//file_name
        end if

        ! modify file name for family-io
        pos = index(file_path,'.',BACK=.true.)
        file_path = trim(file_path(1:pos-1))//'_%d.h5'
      end if

      file_id = 0
      if(mpi_proc_id == mpi_myroot) then ! important for parIO functions !

        ! create new file if overwrite enabled
        if(overwrite) then
          PRTVERBOSE2('create file :', trim(file_path),1)
          call H5Fcreate_f(file_path, H5F_ACC_TRUNC_F, file_id, open_err,  &
                                             creation_prp= pario_fcpl_id,   &
                                             access_prp  = pario_noncoll_facc_id)
        ! extend old file, if present
        else
          PRTVERBOSE2('open file :', trim(file_path),1)
          call H5Fopen_f(file_path, H5F_ACC_RDWR_F, file_id, open_err, access_prp = pario_noncoll_facc_id)

        end if

      end if

      call MPI_bcast(open_err, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      if(open_err == 0) then
        PRTVERBOSE2('file opened/created :',trim(file_path),1)
      end if
      ierr = open_err

    end subroutine open_file_write4gval

    !---------------------------------------------------------------------------
    !> @brief   open file for processor write
    !> @details ...
    !>
    !> @param   file_path   file path
    !> @param   file_id     HDF5 file id
    !> @param   ierr        return error code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine open_file_write4pval(file_path_in, overwrite, file_id, open_err)
      use iso_c_binding
      use hdf5
      use pario_data
      implicit none

      ! function args
      character(len=*), intent(in) :: file_path_in
      logical, intent(in) :: overwrite
      integer(HID_T), intent(out) :: file_id
      integer(HID_T)  :: tmp_facc_id
      integer :: open_err

      ! other vars
      integer(HID_T) :: driver
      character(len=1024) :: file_path, file_name
      integer :: ierr

      ! family vars
      integer :: pos, istat
      integer(c_int) :: stat
      integer(c_int16_t) :: mkmode = o'0750'
      character(len=1024, kind=c_char) path

      open_err = 0
      PRTFNC(open_file_write4pval)

      file_path = file_path_in

      ! delete/overwrite old files, if present
      ! sadly we cannot delete directories for family-io
      if(mpi_proc_id == mpi_myroot) then
        if(overwrite) then
          open(unit=10, file=file_path, iostat=open_err)
          if(open_err == 0) then
            PRTVERBOSE2('delete file :', trim(file_path),1)
            close (unit=10, status='delete')
          end if
        end if
      end if

      ! h5 driver depended settings
      call H5Pget_driver_f(pario_coll_facc_id, driver, ierr); CHKERRLW (ierr, 0)
      if(driver == H5FD_FAMILY_F) then
        ! /my/path/to/data/file.h5 -> /my/path/to/data/file.h5/file_%d.h5

        istat = 0
        if(overwrite) then ! create new directory from mpi proc 0
          if(mpi_proc_id == mpi_myroot) then
            path = trim(file_path)//c_null_char
            stat = mkdir(trim(path), mkmode)
            istat = stat
            if(istat /= 0) then
              PRTINFO2('Could not create directory: ', trim(file_path))
              PRTINFO2('Failed with mkdir-status =', istat)
            end if
          end if
          call MPI_bcast(istat, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
        end if

        if(istat == 0) then ! use directory for family files

          ! get file name from file path
          pos = index(file_path,'/',BACK=.true.)
          !if(pos == 0) pos = index(file_path,'\',BACK=.true.) ! MS Windows
          if(pos == 0) pos = 1
          file_name = file_path(pos:len_trim(file_path))

          ! extend file path
          file_path = trim(file_path)//'/'//file_name
        end if

        ! modify file name for family-io
        pos = index(file_path,'.',BACK=.true.)
        file_path = trim(file_path(1:pos-1))//'_%d.h5'
      end if

      file_id = 0
      ! overwrite old files, if present
      if(overwrite) then

        ! create the new file
        PRTVERBOSE2('create file :', trim(file_path),1)
        call H5Fcreate_f(file_path, H5F_ACC_TRUNC_F, file_id, open_err, &
                                          creation_prp= pario_fcpl_id,   &
                                           access_prp = pario_coll_facc_id)

      ! extend old file, if present
      else
        PRTVERBOSE2('open file :', trim(file_path),1)
          call H5Fopen_f(file_path, H5F_ACC_RDWR_F, file_id, open_err, access_prp = pario_coll_facc_id)

      end if

      ! wait for everyone to open (if not MPI-IO)
      if(driver /= H5FD_MPIO_F) then
        call MPI_Barrier(mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      end if
      PRTVERBOSE2('file opened/created :',trim(file_path),1)

    end subroutine open_file_write4pval

    !---------------------------------------------------------------------------
    !> @brief   create HDF5 group
    !> @details ...
    !>
    !> @param   loc_id    hdf5 location id
    !> @param   gname     group name
    !> @param   group_id  group_id
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    logical function create_group(loc_id, gname, group_id)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T), intent(in):: loc_id
      character(*), intent(in) :: gname
      integer(HID_T), intent(out):: group_id

      ! other vars
      integer :: ierr
      integer :: create_err, open_err, major_err

      PRTFNC(create_group)
      create_group = .true.

      ! check location
      if(loc_id /= 0) then

          PRTVERBOSE2('create group ',trim(gname),1)
          call H5Eset_auto_f(0, ierr); CHKERRLW (ierr, 0)
            ! H5Gcreate_f:http://www.hdfgroup.org/HDF5/doc/RM/CollectiveCalls.html
            ! "All processes must use the same creation properties."
            ! "All processes must use the same access properties."
            call H5Gcreate_f(loc_id, trim(gname), group_id, create_err)
            ! H5Gopen_f:http://www.hdfgroup.org/HDF5/doc/RM/CollectiveCalls.html
            ! "All processes must use the same access properties."
            if(create_err /= 0) call H5Gopen_f(loc_id, trim(gname), group_id, open_err)
          call H5Eset_auto_f(1, ierr); CHKERRLW (ierr, 0)

          if(create_err /= 0 .and. open_err /= 0) then
            PRTINFO2('FAILED create/open group ',trim(gname))
            major_err = -1
          else
            major_err = 0
          end if

      else
          group_id = 0
      end if

      call MPI_bcast(major_err, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      if(major_err /= 0) create_group = .false.

    end function create_group

    !---------------------------------------------------------------------------
    !> @brief   open HDF5 group
    !> @details ...
    !>
    !> @param[in]   loc_id    hdf5 location id
    !> @param[in]   gname     group name
    !> @param[out]  group_id  group_id
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    logical function open_group(loc_id, gname, group_id)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T), intent(in):: loc_id
      character(*), intent(in) :: gname
      integer(HID_T), intent(out):: group_id

      ! other vars
      integer :: open_err, ierr

      PRTFNC(open_group)
      open_group = .true.

      ! check location
      if(loc_id /= 0) then

          PRTVERBOSE2('open group ',trim(gname),1)
          call H5Eset_auto_f(0, ierr); CHKERRLW (ierr, 0)
            ! H5Gopen_f:http://www.hdfgroup.org/HDF5/doc/RM/CollectiveCalls.html
            ! "All processes must use the same access properties."
            call H5Gopen_f(loc_id, trim(gname), group_id, open_err)
          call H5Eset_auto_f(1, ierr); CHKERRLW (ierr, 0)

          if(open_err /= 0) then
            PRTINFO2('FAILED open group ',trim(gname))
          endif

      else
          group_id = 0
      end if

      call MPI_bcast(open_err, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      if(open_err /= 0) open_group = .false.

    end function open_group

    !---------------------------------------------------------------------------
    !> @brief   get HDF5 subgroup
    !> @details ...
    !>
    !> @param[in]   loc_id    hdf5 location id (parent of gname)
    !> @param[in]   gname     group name inside loc_id
    !> @param[inout] sidx     index of subgroup to check for
    !> @param[out]  sgname    sub group name
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    logical function get_subgroup(loc_id, gname, sidx, sgname)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T), intent(in):: loc_id
      character(len=*), intent(in) :: gname
      integer, intent(inout) :: sidx
      character(len=*), intent(out) :: sgname

      ! other vars
      integer :: found_subgrp, sgrp_idx, i, ierr
      character(len=256) :: name_buffer ! buffer to hold object's name
      integer :: type ! Type of the object (H5G_LINK_F, H5G_GROUP_F, H5G_DATASET_F, H5G_TYPE_F)
      integer :: nmembers ! Number of group members

      PRTFNC(get_subgroup)
      get_subgroup = .true.
      if(sidx < 1) sidx = 1

      ! check location
      sgname = ''
      found_subgrp = 0
      if(loc_id /= 0) then

        ! get all members in group
        call H5Gn_members_f(loc_id, gname, nmembers, ierr); CHKERRLW(ierr, 0)
        if(nmembers > 0) then

          ! search for subgroup with index sidx
          sgrp_idx = 0
          do i=0, nmembers-1

            ! get object info and check for group
            call H5Gget_obj_info_idx_f(loc_id, gname, i, name_buffer, type, ierr); CHKERRLW(ierr, 0)
            if(type == H5G_GROUP_F) sgrp_idx = sgrp_idx +1

            ! found the subgroup
            if(sgrp_idx == sidx) then
              found_subgrp = 1
              exit
            end if

          end do
        end if
      end if

      ! send data to all procs
      call MPI_bcast(found_subgrp, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      if(found_subgrp == 0) then
        get_subgroup = .false.
      else
        call MPI_bcast(name_buffer, 256, MPI_CHARACTER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
        sgname = name_buffer
        sidx = sidx +1
      end if

    end function get_subgroup

    !---------------------------------------------------------------------------
    !> @brief   close HDF5 group
    !> @details
    !>
    !> @param   group_id  group_id
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine close_group(group_id, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T), intent(in):: group_id
      integer, intent(out) :: ierr

      PRTFNC(close_group)
      ierr = 0

      ! check location
      if(group_id /= 0) then

          PRTVERBOSE1('close group',1)
          ! H5GClose_f: http://www.hdfgroup.org/HDF5/doc/RM/CollectiveCalls.html
          ! "All processes must participate only if all file identifiers for a file have been closed and this is the last outstanding object identifier."
          call H5Gclose_f(group_id, ierr)
          if(ierr /= 0) then
            PRTINFO1('FAILED close group')
          endif

      end if

    end subroutine close_group

    !---------------------------------------------------------------------------
    !> @brief   [serial] init a hdf5 dataset (no MPI-IO)
    !> @details - skipping call if mpi_proc_id not equal mpi_myroot
    !>           eg. call H5Fcreate_f(filepath, H5F_ACC_EXCL_F, file_id, ierr, access_prp = H5P_DEFAULT)
    !>           function must only be called by this single mpi proc !!!
    !>
    !> @param   loc_id    hdf5 location id
    !> @param   dset_name dataset name
    !> @param   dset_dims dataset dimensions
    !> @param   dset_size dataset sizes
    !> @param   dset_type_id HDF5 dataset type
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine init_dataset(loc_id, dset_name, dset_dims, dset_size, dset_type_id, ierr, chunk_size)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T),  intent(in) :: loc_id
      character(len=*),intent(in) :: dset_name
      integer,         intent(in) :: dset_dims
      integer,         intent(in) :: dset_size(dset_dims)
      integer(HID_T),  intent(in) :: dset_type_id
      integer,         intent(out):: ierr
      integer,optional,intent(in) :: chunk_size(dset_dims)

      ! other vars
      integer(HID_T)  :: dset_id
      integer(HID_T)  :: dspace_id
      integer(HSIZE_T):: dset_size_(dset_dims)
      integer :: create_err

      integer(HID_T)  :: tmp_dcreate_id
      integer(HSIZE_T):: chunk_size_(dset_dims)
      double precision :: itimer
      PRTFNC(init_dataset)
      ierr=0

      ! skip call if mpi_proc_id not equal mpi_myroot
      if(mpi_proc_id /= mpi_myroot) return

      itimer = -MPI_wtime()
      dset_size_ = dset_size

      ! TODO: check if dataset exists (change type/size if existant)
      !call H5Eset_auto_f(0, ierr); CHKERRLW (ierr, 0)

      ! create dataspace for the whole dataset in the file
      call H5Screate_simple_f(dset_dims, dset_size_, dspace_id, ierr, dset_size_); CHKERRLW(ierr, 0)

      ! get chunk size (default or specific)
      chunk_size_ = 0
      if(dset_dims == 2 .and. product(pario_chunk_size_2d) > 0) chunk_size_ = pario_chunk_size_2d
      if(dset_dims == 3 .and. product(pario_chunk_size_3d) > 0) chunk_size_ = pario_chunk_size_3d
      if(dset_dims == 4 .and. product(pario_chunk_size_4d) > 0) chunk_size_ = pario_chunk_size_4d
      if(present(chunk_size)) then
        if(chunk_size(1) > 0) chunk_size_ = chunk_size
      end if

      ! create dataset
      dset_id = 0
      call H5Eset_auto_f(0, ierr); CHKERRLW (ierr, 0)
      if(product(chunk_size_) > 0) then ! try to use chunking (if enable) instead of continuous in file
        PRTVERBOSE2('  set chunk size = ', chunk_size_, 3)

        ! check number of chunks
        if(product(int(dset_size,8)) / product(int(chunk_size_,8)) > 2**16-1) then
          PRTERR1('HDF5 <=1.8.11 does not support more than 65535 chunks per dataset')
        end if

        call H5Pcopy_f(pario_noncoll_dcreate_id, tmp_dcreate_id, ierr); CHKERRLW(ierr, 0)
        call H5Pset_chunk_f(tmp_dcreate_id, dset_dims, chunk_size_, ierr); CHKERRLW(ierr, 0)
        call H5Dcreate_f(loc_id, dset_name, dset_type_id, dspace_id, dset_id, create_err, tmp_dcreate_id)
        call H5Pclose_f(tmp_dcreate_id, ierr); CHKERRLW(ierr, 0)
      end if
      if(dset_id == 0) then
        call H5Dcreate_f(loc_id, dset_name, dset_type_id, dspace_id, dset_id, create_err, pario_noncoll_dcreate_id)
      end if
      if(create_err == 0) then
        call H5Dclose_f(dset_id, ierr); CHKERRLW(ierr, 0)
      end if
      call H5Eset_auto_f(1, ierr); CHKERRLW (ierr, 0)

      ! close dataspace for the whole dataset in the file
      call H5Sclose_f(dspace_id, ierr); CHKERRLW(ierr, 0)

      ierr = create_err

      itimer = itimer +MPI_wtime()
      if(dump_speed) write(*,'(a,a,a,es10.2,a)') '         init dataset  "',trim(dset_name), '" in ', itimer, '[s]'

    end subroutine init_dataset

end module pario_funcs
