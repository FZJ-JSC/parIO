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
!> @file pario_gprop.F90
!! @brief main public parIO functions
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "pario_defines.inc"

module pario_gprop
    use hdf5
    use pario_consts
    implicit none
    save

    ! read attributes of dataset from file (using r_gprop_attr(..))
    !  - must be called by all mpi_procs simultaniously, but with h5-file opened only from mpi proc 0
    interface r_gprop_attr
      module procedure      &
      r_gprop_attr_integer, &
      r_gprop_attr_real4,   &
      r_gprop_attr_real8,   &
      r_gprop_attr_chars
    end interface r_gprop_attr

    ! read attributes of dataset from file (using r_gprop_attr(..))
    !  - must be called by all mpi_procs simultaniously, but with h5-file opened only from mpi proc 0
    interface w_gprop_attr
      module procedure      &
      w_gprop_attr_integer, &
      w_gprop_attr_real4,   &
      w_gprop_attr_real8,   &
      w_gprop_attr_chars
    end interface w_gprop_attr

    contains

    !---------------------------------------------------------------------------
    !> @brief    read number of dimensions of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[in,out]   ndims         number of dataset dimensions
    !> @param[in,out]   ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine r_gprop_ndims(loc_id, dset_name, ndims, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      integer,         intent(out)   :: ndims
      integer         ,intent(out)   :: ierr

      ! other vars
      integer(HID_T) :: did, dspace_id
      integer :: open_err

      PRTFNC(r_gprop_ndims)
      ierr = 0

      ! only with mpi proc mpi_myroot
      if(mpi_proc_id == mpi_myroot) then

        ! open dataset
        call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
          call H5Dopen_f (loc_id, dset_name, did, open_err)
        call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
        if (open_err /= 0) then
          ierr = open_err
          return
        end if

        ! get number of dataset dimensions
        call H5Dget_space_f(did, dspace_id, ierr); CHKERRLW(ierr,0)
        call H5Sget_simple_extent_ndims_f(dspace_id, ndims, ierr); CHKERRLW(ierr,0)

        ! close
        call H5Sclose_f(dspace_id, ierr); CHKERRLW(ierr,0)
        call H5Dclose_f(did, ierr); CHKERRLW(ierr,0)
      end if

      ! broadcast to all mpi procs
      call MPI_Bcast(ndims, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)

    end subroutine r_gprop_ndims

    !---------------------------------------------------------------------------
    !> @brief    read size of each dimention of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[in,out]   ndims         number of dataset dimensions
    !> @param[in,out]   ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine r_gprop_dims(loc_id, dset_name, dims, maxdims, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      integer(HSIZE_T), dimension(:), intent(out) :: dims, maxdims
      integer, intent(out) :: ierr

      ! other vars
      integer(HID_T) :: did, dspace_id, dtype_id
      integer :: ndims
      integer :: open_err

      integer, dimension(:), allocatable :: dims_int, maxdims_int

      PRTFNC(r_gprop_ndims)
      ierr = 0

      ! check input
      if(size(dims) /= size(maxdims)) then
        PRTVERBOSE1('dimensions of dims not equal dimensions of maxdims',verbose_mode)
        ierr = -1
        return
      end if

      ! only with mpi proc mpi_myroot
      if(mpi_proc_id == mpi_myroot) then

        ! open dataset
        call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
          call H5Dopen_f (loc_id, dset_name, did, open_err)
        call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
        if (open_err /= 0) then
          ierr = open_err
          return
        end if

        ! get number of dataset dimensions
        call H5Dget_space_f(did, dspace_id, ierr); CHKERRLW(ierr,0)
        call H5Sget_simple_extent_ndims_f(dspace_id, ndims, ierr); CHKERRLW(ierr,0)

        ! get size of dataset dimensions
        if(ndims == size(dims)) then
          call H5Sget_simple_extent_dims_f(dspace_id, dims, maxdims, ierr); CHKERRQ1(ierr,ndims)
        else
          PRTVERBOSE1('dimensions of dims not equal dimensions of dataset',verbose_mode)
          ierr = -1
        end if

        ! close
        call H5Sclose_f(dspace_id, ierr); CHKERRLW(ierr,0)
        call H5Dclose_f(did, ierr); CHKERRLW(ierr,0)
      end if

      ! broadcast to all mpi procs
      call MPI_Bcast(ndims, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
    
      if(ndims > 0) then
          allocate(dims_int(ndims), stat=ierr); CHKERRQ0(ierr)
          if(mpi_proc_id == mpi_myroot) dims_int(1:ndims) = dims(1:ndims)
          call MPI_Bcast(dims_int,ndims, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
          dims(1:ndims) = dims_int(1:ndims)
          deallocate(dims_int, stat=ierr); CHKERRQ0(ierr)
      
          allocate(maxdims_int(ndims), stat=ierr); CHKERRQ0(ierr)
          if(mpi_proc_id == mpi_myroot) maxdims_int(1:ndims) = maxdims(1:ndims)
          call MPI_Bcast(maxdims_int, ndims, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
          maxdims(1:ndims) = maxdims_int(1:ndims)
          deallocate(maxdims_int, stat=ierr); CHKERRQ0(ierr)
     else
          dims(:) = 0
          maxdims(:) = 0
     endif

    end subroutine r_gprop_dims

    !---------------------------------------------------------------------------
    !> @brief    read attribute of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[in]       attr_name     attribute name in dataset
    !> @param[out]      mem           attribute value
    !> @param[in,out]   ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine r_gprop_attr_integer(loc_id, dset_name, attr_name, mem, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      character(len=*),intent(in)    :: attr_name
      integer, intent(out) :: mem
      integer, intent(out) :: ierr

      ! other vars
      integer(HID_T) :: dset_id, attr_id
      integer(HSIZE_T) :: mem_dims(1)
      integer :: open_err

      PRTFNC(r_gprop_ndims)
      ierr = 0

      ! only with mpi proc mpi_myroot
      if(mpi_proc_id == mpi_myroot) then

        ! open dataset
        call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
          call H5Dopen_f (loc_id, dset_name, dset_id, open_err)
        call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
        if (open_err == 0) then

          ! read attribute
          call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
            call H5Aopen_f(dset_id, attr_name, attr_id, open_err)
          call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
          if (open_err == 0) then

            ! read the data.
            mem_dims(1) = 1
            call H5Aread_f(attr_id, H5T_NATIVE_INTEGER, mem, mem_dims, ierr); CHKERRLW(ierr,0)

            ! close attribute
            call H5Aclose_f(attr_id, ierr); CHKERRLW(ierr,0)
          end if

          ! close dataset
          call H5Dclose_f(dset_id, ierr); CHKERRLW(ierr,0)
        end if
      end if

      ! broadcast to all mpi procs
      call MPI_Bcast(open_err, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      if(open_err == 0) then
        call MPI_Bcast(mem, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      else
        ierr = -1
      end if

    end subroutine r_gprop_attr_integer

    !---------------------------------------------------------------------------
    !> @brief    read attribute of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[in]       attr_name     attribute name in dataset
    !> @param[out]      mem           attribute value
    !> @param[in,out]   ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine r_gprop_attr_real4(loc_id, dset_name, attr_name, mem, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      character(len=*),intent(in)    :: attr_name
      real(kind=4), intent(out) :: mem
      integer, intent(out) :: ierr

      ! other vars
      integer(HID_T) :: dset_id, attr_id
      integer(HSIZE_T) :: mem_dims(1)
      integer :: open_err

      PRTFNC(r_gprop_ndims)
      ierr = 0

      ! only with mpi proc mpi_myroot
      if(mpi_proc_id == mpi_myroot) then

        ! open dataset
        call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
          call H5Dopen_f (loc_id, dset_name, dset_id, open_err)
        call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
        if (open_err == 0) then

          ! read attribute
          call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
            call H5Aopen_f(dset_id, attr_name, attr_id, open_err)
          call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
          if (open_err == 0) then

            ! read the data.
            mem_dims(1) = 1
            call H5Aread_f(attr_id, H5T_NATIVE_REAL, mem, mem_dims, ierr); CHKERRLW(ierr,0)

            ! close attribute
            call H5Aclose_f(attr_id, ierr); CHKERRLW(ierr,0)
          end if

          ! close dataset
          call H5Dclose_f(dset_id, ierr); CHKERRLW(ierr,0)
        end if
      end if

      ! broadcast to all mpi procs
      call MPI_Bcast(open_err, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      if(open_err == 0) then
        call MPI_Bcast(mem, 1, MPI_REAL, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      else
        ierr = -1
      end if

    end subroutine r_gprop_attr_real4

    !---------------------------------------------------------------------------
    !> @brief    read attribute of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[in]       attr_name     attribute name in dataset
    !> @param[out]      mem           attribute value
    !> @param[in,out]   ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine r_gprop_attr_real8(loc_id, dset_name, attr_name, mem, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      character(len=*),intent(in)    :: attr_name
      real(kind=8), intent(out) :: mem
      integer, intent(out) :: ierr

      ! other vars
      integer(HID_T) :: dset_id, attr_id
      integer(HSIZE_T) :: mem_dims(1)
      integer :: open_err

      PRTFNC(r_gprop_ndims)
      ierr = 0

      ! only with mpi proc mpi_myroot
      if(mpi_proc_id == mpi_myroot) then

        ! open dataset
        call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
          call H5Dopen_f (loc_id, dset_name, dset_id, open_err)
        call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
        if (open_err == 0) then

          ! read attribute
          call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
            call H5Aopen_f(dset_id, attr_name, attr_id, open_err)
          call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
          if (open_err == 0) then

            ! read the data.
            mem_dims(1) = 1
            call H5Aread_f(attr_id, H5T_NATIVE_DOUBLE, mem, mem_dims, ierr); CHKERRLW(ierr,0)

            ! close attribute
            call H5Aclose_f(attr_id, ierr); CHKERRLW(ierr,0)
          end if

          ! close dataset
          call H5Dclose_f(dset_id, ierr); CHKERRLW(ierr,0)
        end if
      end if

      ! broadcast to all mpi procs
      call MPI_Bcast(open_err, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      if(open_err == 0) then
        call MPI_Bcast(mem, 1, MPI_DOUBLE_PRECISION, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      else
        ierr = -1
      end if

    end subroutine r_gprop_attr_real8

    !---------------------------------------------------------------------------
    !> @brief    read attribute of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[in]       attr_name     attribute name in dataset
    !> @param[out]      mem           attribute value
    !> @param[in,out]   ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine r_gprop_attr_chars(loc_id, dset_name, attr_name, chars, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      character(len=*),intent(in)    :: attr_name
      character(len=*), intent(out) :: chars
      integer, intent(out) :: ierr

      ! other vars
      integer(HID_T) :: dset_id, attr_id, chars_type_id
      integer(HSIZE_T) :: chars_len
      integer(HSIZE_T) :: chars_dims(1)
      integer :: open_err

      PRTFNC(r_gprop_ndims)
      ierr = 0

      ! only with mpi proc mpi_myroot
      if(mpi_proc_id == mpi_myroot) then

        ! open dataset
        call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
          call H5Dopen_f (loc_id, dset_name, dset_id, open_err)
        call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
        if (open_err == 0) then

          ! read attribute
          call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
            call H5Aopen_f(dset_id, attr_name, attr_id, open_err)
          call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
          if (open_err == 0) then

            ! define memory type
            call H5Tcopy_f(H5T_C_S1, chars_type_id, ierr); CHKERRLW(ierr,0)
            chars_len = len(chars)
            call H5Tset_size_f(chars_type_id, chars_len, ierr); CHKERRLW(ierr,0)

            ! read the data.
            chars_dims(1) = 1
            call H5Aread_f(attr_id, chars_type_id, chars, chars_dims, ierr); CHKERRLW(ierr,0)

            ! close attribute
            call H5Tclose_f(chars_type_id, ierr); CHKERRLW(ierr,0)
            call H5Aclose_f(attr_id, ierr); CHKERRLW(ierr,0)
          end if

          ! close dataset
          call H5Dclose_f(dset_id, ierr); CHKERRLW(ierr,0)
        end if
      end if

      ! broadcast to all mpi procs
      call MPI_Bcast(open_err, 1, MPI_INTEGER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      if(open_err == 0) then
        call MPI_Bcast(chars, len(chars), MPI_CHARACTER, mpi_myroot, mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      else
        ierr = -1
      end if

    end subroutine r_gprop_attr_chars

    !---------------------------------------------------------------------------
    !> @brief    write attribute of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[in]       attr_name     attribute name in dataset
    !> @param[in]       mem           attribute value
    !> @param[in,out]   ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine w_gprop_attr_integer(loc_id, dset_name, attr_name, mem, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      character(len=*),intent(in)    :: attr_name
      integer, intent(in) :: mem
      integer, intent(out) :: ierr

      ! other vars
      integer(HID_T) :: dset_id, attr_id,  attr_space
      integer(HSIZE_T) :: attr_dims(1), mem_dims(1)
      integer :: open_err

      PRTFNC(r_gprop_ndims)
      ierr = 0

      ! only with mpi proc mpi_myroot
      if(mpi_proc_id == mpi_myroot) then

        ! open dataset
        call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
          call H5Dopen_f (loc_id, dset_name, dset_id, open_err)
        call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
        if (open_err /= 0) then
          ierr = open_err
          return
        end if

        ! create attribute
        attr_dims(1) = 1
        call H5Screate_simple_f(1, attr_dims, attr_space, ierr); CHKERRLW(ierr,0)
        call H5Acreate_f(dset_id, attr_name, H5T_NATIVE_INTEGER, attr_space, attr_id, ierr); CHKERRLW(ierr,0)

        ! write the data
        mem_dims(1) = 1
        call H5Awrite_f(attr_id, H5T_NATIVE_INTEGER, mem, mem_dims, ierr); CHKERRLW(ierr,0)

        ! close
        call H5Sclose_f(attr_space, ierr); CHKERRLW(ierr,0)
        call H5Aclose_f(attr_id, ierr); CHKERRLW(ierr,0)
        call H5Dclose_f(dset_id, ierr); CHKERRLW(ierr,0)
      end if

    end subroutine w_gprop_attr_integer

    !---------------------------------------------------------------------------
    !> @brief    write attribute of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[in]       attr_name     attribute name in dataset
    !> @param[in]       mem           attribute value
    !> @param[in,out]   ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine w_gprop_attr_real4(loc_id, dset_name, attr_name, mem, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      character(len=*),intent(in)    :: attr_name
      real(kind=4), intent(in) :: mem
      integer, intent(out) :: ierr

      ! other vars
      integer(HID_T) :: dset_id, attr_id,  attr_space
      integer(HSIZE_T) :: attr_dims(1), mem_dims(1)
      integer :: open_err

      PRTFNC(r_gprop_ndims)
      ierr = 0

      ! only with mpi proc mpi_myroot
      if(mpi_proc_id == mpi_myroot) then

        ! open dataset
        call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
          call H5Dopen_f (loc_id, dset_name, dset_id, open_err)
        call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
        if (open_err /= 0) then
          ierr = open_err
          return
        end if

        ! create attribute
        attr_dims(1) = 1
        call H5Screate_simple_f(1, attr_dims, attr_space, ierr); CHKERRLW(ierr,0)
        call H5Acreate_f(dset_id, attr_name, H5T_NATIVE_REAL, attr_space, attr_id, ierr); CHKERRLW(ierr,0)

        ! write the data
        mem_dims(1) = 1
        call H5Awrite_f(attr_id, H5T_NATIVE_REAL, mem, mem_dims, ierr); CHKERRLW(ierr,0)

        ! close
        call H5Sclose_f(attr_space, ierr); CHKERRLW(ierr,0)
        call H5Aclose_f(attr_id, ierr); CHKERRLW(ierr,0)
        call H5Dclose_f(dset_id, ierr); CHKERRLW(ierr,0)
      end if

    end subroutine w_gprop_attr_real4

    !---------------------------------------------------------------------------
    !> @brief    write attribute of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[in]       attr_name     attribute name in dataset
    !> @param[in]       mem           attribute value
    !> @param[in,out]   ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine w_gprop_attr_real8(loc_id, dset_name, attr_name, mem, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      character(len=*),intent(in)    :: attr_name
      real(kind=8), intent(in) :: mem
      integer, intent(out) :: ierr

      ! other vars
      integer(HID_T) :: dset_id, attr_id,  attr_space
      integer(HSIZE_T) :: attr_dims(1), mem_dims(1)
      integer :: open_err

      PRTFNC(r_gprop_ndims)
      ierr = 0

      ! only with mpi proc mpi_myroot
      if(mpi_proc_id == mpi_myroot) then

        ! open dataset
        call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
          call H5Dopen_f (loc_id, dset_name, dset_id, open_err)
        call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
        if (open_err /= 0) then
          ierr = open_err
          return
        end if

        ! create attribute
        attr_dims(1) = 1
        call H5Screate_simple_f(1, attr_dims, attr_space, ierr); CHKERRLW(ierr,0)
        call H5Acreate_f(dset_id, attr_name, H5T_NATIVE_DOUBLE, attr_space, attr_id, ierr); CHKERRLW(ierr,0)

        ! write the data
        mem_dims(1) = 1
        call H5Awrite_f(attr_id, H5T_NATIVE_DOUBLE, mem, mem_dims, ierr); CHKERRLW(ierr,0)

        ! close
        call H5Sclose_f(attr_space, ierr); CHKERRLW(ierr,0)
        call H5Aclose_f(attr_id, ierr); CHKERRLW(ierr,0)
        call H5Dclose_f(dset_id, ierr); CHKERRLW(ierr,0)
      end if

    end subroutine w_gprop_attr_real8

    !---------------------------------------------------------------------------
    !> @brief    write attribute of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[in]       attr_name     attribute name in dataset
    !> @param[in]       mem           attribute value
    !> @param[in,out]   ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine w_gprop_attr_chars(loc_id, dset_name, attr_name, chars, ierr)
      use hdf5
      use pario_data
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      character(len=*),intent(in)    :: attr_name
      character(len=*), intent(in) :: chars
      integer, intent(out) :: ierr

      ! other vars
      integer(HID_T) :: dset_id, attr_id,  attr_space, chars_type_id
      integer(HSIZE_T) :: attr_dims(1), chars_dims(1)
      integer(HSIZE_T) :: chars_len
      integer :: open_err

      PRTFNC(r_gprop_ndims)
      ierr = 0

      ! only with mpi proc mpi_myroot
      if(mpi_proc_id == mpi_myroot) then

        ! open dataset
        call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
          call H5Dopen_f (loc_id, dset_name, dset_id, open_err)
        call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
        if (open_err /= 0) then
          ierr = open_err
          return
        end if

        ! define memory type
        call H5Tcopy_f(H5T_C_S1, chars_type_id, ierr); CHKERRLW(ierr,0)
        chars_len = len(chars)
        call H5Tset_size_f(chars_type_id, chars_len, ierr); CHKERRLW(ierr,0)

        ! create attribute
        attr_dims(1) = 1
        call H5Screate_simple_f(1, attr_dims, attr_space, ierr); CHKERRLW(ierr,0)
        call H5Acreate_f(dset_id, attr_name, chars_type_id, attr_space, attr_id, ierr); CHKERRLW(ierr,0)

        ! write the data
        chars_dims(1) = 1
        call H5Awrite_f(attr_id, chars_type_id, chars, chars_dims, ierr); CHKERRLW(ierr,0)

        ! close
        call H5Tclose_f(chars_type_id, ierr); CHKERRLW(ierr,0)
        call H5Sclose_f(attr_space, ierr); CHKERRLW(ierr,0)
        call H5Aclose_f(attr_id, ierr); CHKERRLW(ierr,0)
        call H5Dclose_f(dset_id, ierr); CHKERRLW(ierr,0)
      end if

    end subroutine w_gprop_attr_chars
end module
