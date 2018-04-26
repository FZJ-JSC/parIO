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
!> @addtogroup data
!! @{
!!
!> @file pario_data.F90
!! @brief stores parIO private global data
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "pario_defines.inc"

module pario_data
    use iso_c_binding
    use hdf5
#ifdef USE_MPI_MODULE
    use mpi
    implicit none
#else
    implicit none
    include 'mpif.h'
#endif
    save

    ! set precision of integer datatype
    integer, parameter :: INT_SP  = 4
    integer, parameter :: INT_DP  = 8
    integer (kind=INT_SP) :: myINTEGER

    ! set precision of real datatype
    real :: mySINGLE
    double precision   :: myDOUBLE
    integer, parameter :: REAL_SP = Selected_Real_Kind (PRECISION(mySINGLE), RANGE(mySINGLE))
    integer, parameter :: REAL_DP = Selected_Real_Kind (PRECISION(myDOUBLE), RANGE(myDOUBLE))

    ! MPI settings from init_pario()
    integer             :: mpi_proc_id
    integer             :: mpi_nproc
    integer             :: mpi_mycomm
    integer             :: mpi_myroot

    ! verbose mode 0-3 for more and more level of informations
    integer :: verbose_mode = 0

    logical :: dump_test       = .false.
    logical :: dump_minmaxmean = .false.
    logical :: dump_hist       = .false.
    logical :: dump_speed      = .false.

    logical :: initialised     = .false.

    ! HDF5 property list identifier
    integer(HID_T)  ::  pario_fcpl_id
    integer(HID_T)  ::  pario_noncoll_facc_id
    integer(HID_T)  ::  pario_noncoll_dcreate_id
    integer(HID_T)  ::  pario_coll_facc_id
    integer(HID_T)  ::  pario_coll_dxfer_id

    ! HDF5 MPIIO settings
    integer(HSIZE_T) :: pario_std_sievebuffer  = -1 ! 4194304 = 4*1024*1024 bytes = 4 mbyte
    integer(HSIZE_T) :: pario_std_threshold    = -1 ! 524288  =    512*1024 bytes = 512 kbyte
    integer(HSIZE_T) :: pario_std_alignment    = -1 ! 4194304 = 4*1024*1024 bytes = 4 mbyte
    integer(HSIZE_T) :: pario_std_transbuffer  = -1 !         = 1*1024*1024 bytes = 1 mbyte
    integer(HSIZE_T) :: pario_std_metablock_bytes = -1 !
    integer          :: pario_std_metablock_flushatclose = -1
    integer          :: pario_std_romio_collective_buffering = -1 ! automatic collective buffering [0=disable, 1=enable]
    integer          :: pario_std_romio_data_sieving = -1 ! automatic data sieving [0=disable, 1=enable]

    ! HDF5 FAMILY settings
    integer(HSIZE_T) :: pario_family_ffsize      = 2**28 ! = 256 MByte

    ! HDF5 chunk settings
    integer(HSIZE_T) :: pario_chunk_size_2d(2)        = (/0,0/)
    integer(HSIZE_T) :: pario_chunk_size_3d(3)        = (/0,0,0/)
    integer(HSIZE_T) :: pario_chunk_size_4d(4)        = (/0,0,0,0/)
    integer(HSIZE_T) :: pario_chunk_cache_nslots      = 521
    integer(HSIZE_T) :: pario_chunk_cache_bytes       = 1024*1024 ! 1 MByte
    real             :: pario_chunk_cache_preemption  = 0.75
    integer          :: pario_chunk_btree_size        = 32

    ! HDF5 MPI-POSIX settings
    !logical :: pario_mpiposix_gpfshints = .false. ! support removed in hdf5 1.8.13

    ! access to c-function 'mkdir'
    !   integer(c_int) stat
    !   integer(c_int16_t) :: mode = o'0777'
    !   character(len=8, kind=c_char) path
    !   path = 'mypath'
    !   stat = mkdir(trim(path), mode)
    interface
      function mkdir(path, mode) bind(c)
        use iso_c_binding
        integer(c_int)            :: mkdir
        character(kind=c_char)    :: path
        integer(c_int16_t), value :: mode
      end function mkdir
    end interface

    contains

    ! access to c-function 'pario_H5Pset_mdc_config'
    subroutine pario_disable_evictions(facc_prp, hdferr)
      implicit none
      integer(HID_T), intent(inout) :: facc_prp ! file access properties
      integer, intent(out) :: hdferr        ! error code

      INTERFACE
        INTEGER(c_int) FUNCTION pario_disable_evictions_c(facc_prp) bind(c)
          USE iso_c_binding, only: c_int
          USE H5GLOBAL
          !DEC$IF DEFINED(HDF5F90_WINDOWS)
          !DEC$ATTRIBUTES C,reference,decorate,alias:'PARIO_DISABLE_EVICTIONS_C':: pario_disable_evictions_c
          !DEC$ENDIF
          !DEC$ATTRIBUTES reference :: name
          INTEGER(HID_T), INTENT(INOUT) :: facc_prp
        END FUNCTION pario_disable_evictions_c
      END INTERFACE

      hdferr = pario_disable_evictions_c(facc_prp)

    end subroutine pario_disable_evictions

    !---------------------------------------------------------------------------
    !> @brief   initialise HDF5
    !> @details CHECK this: http://www.hdfgroup.org/HDF5/doc/TechNotes/VFL.html
    !>
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine init_h5(io_meth, ierr)
      use hdf5
      use pario_consts
      implicit none

      ! function args
      integer, intent(in) :: io_meth
      integer, intent(out) :: ierr

      PRTFNC(init_h5)
      ierr=0

      ! initialize HDF5 interface (initialize predefined datatypes)
      call H5open_f(ierr); CHKERRLW (ierr, 0)

      ! create default property lists
      !---------------------------
      call H5Pcreate_f(H5P_FILE_CREATE_F,    pario_fcpl_id, ierr); CHKERRLW (ierr, 0)
      call H5Pcreate_f(H5P_FILE_ACCESS_F,    pario_noncoll_facc_id,  ierr); CHKERRLW (ierr, 0)
      call H5Pcreate_f(H5P_FILE_ACCESS_F,    pario_coll_facc_id,  ierr); CHKERRLW (ierr, 0)
      call H5Pcreate_f(H5P_DATASET_CREATE_F, pario_noncoll_dcreate_id, ierr); CHKERRLW (ierr, 0)
      call H5Pcreate_f(H5P_DATASET_XFER_F,   pario_coll_dxfer_id, ierr); CHKERRLW (ierr, 0)

      ! TODO: sets number of I/O vectors to be read/written in hyperslab I/O (default = 1024)
      !call H5Pset_hyper_vector_size(hid_t dxpl_id, size_t vector_size )

      if(io_meth == PARIO_MPIIO) then
        call init_h5_mpiio(ierr); CHKERRQ0(ierr)

      else if(io_meth == PARIO_POSIX) then
        call init_h5_posix(ierr); CHKERRQ0(ierr)

!      else if(io_meth == PARIO_MPIPOSIX) then    ! support removed in hdf5 1.8.13
!        call init_h5_mpiposix(ierr); CHKERRQ0(ierr)

      else if(io_meth == PARIO_FAMILY) then
        call init_h5_family(ierr); CHKERRQ0(ierr)

      else if(io_meth == PARIO_STDIO) then
        call init_h5_stdio(ierr); CHKERRQ0(ierr)

      else if(io_meth == PARIO_DIRECT) then
        call init_h5_direct(ierr); CHKERRQ0(ierr)

      else
        PRTERR1("FATAL ERROR: unknown IO method for parIO choosen")
      end if

      !!! property lists for serial dataset creation
      !---------------------------------------------
      call H5Pset_alloc_time_f(pario_noncoll_dcreate_id, H5D_ALLOC_TIME_EARLY_F, ierr); CHKERRLW (ierr, 0)

!     call H5Pset_fill_time_f (pario_noncoll_dcreate_id, H5D_FILL_TIME_ALLOC_F,  ierr); CHKERRLW (ierr, 0)
      call H5Pset_fill_time_f (pario_noncoll_dcreate_id, H5D_FILL_TIME_NEVER_F,  ierr); CHKERRLW (ierr, 0)

      !!! property lists for file access
      !---------------------------------------------
      ! all objects of 'threshold' or more bytes starts at the boundary of 'alignment' bytes (on client side)
      ! good for GPFS
      if(pario_std_alignment > 0) then
        call H5Pset_alignment_f(pario_coll_facc_id,    pario_std_threshold, pario_std_alignment, ierr); CHKERRLW (ierr, 0)
        call H5Pset_alignment_f(pario_noncoll_facc_id, pario_std_threshold, pario_std_alignment, ierr); CHKERRLW (ierr, 0)
      end if

      if(pario_std_metablock_bytes > 0) then
        call H5Pset_meta_block_size_f(pario_coll_facc_id,    pario_std_metablock_bytes, ierr); CHKERRLW (ierr, 0)
        call H5Pset_meta_block_size_f(pario_noncoll_facc_id, pario_std_metablock_bytes, ierr); CHKERRLW (ierr, 0)
      end if

!      ! To reduce the frequency of performing small I/O operations, it is possible to put the eviction of items from the HDF5 library's metadata cache
!      ! entirely under the application's control with the following.
!      ! This sequence of calls disables evictions from the metadata cache, unless H5Fflush is called or the file is closed.
!      ! The evictions_enabled field may not be set to FALSE unless all adaptive cache resizing code is disabled via the incr_mode, flash_incr_mode, and decr_mode fields.
      if(pario_std_metablock_flushatclose > 0) then
        call pario_disable_evictions(pario_coll_facc_id, ierr); CHKERRLW (ierr, 0)
      end if

      ! cache chunked data -- chunked data provide better access for parallel IO (but only in in readonly mode)
      ! H5Pset_cache is used to adjust the chunk cache parameters for all datasets via a GLOBAL setting for the file
      call H5Pset_cache_f(pario_coll_facc_id, 1,        &
                          pario_chunk_cache_nslots,     &
                          pario_chunk_cache_bytes,      &
                          pario_chunk_cache_preemption, &
                          ierr); CHKERRLW (ierr, 0)
!      ! H5Pset_chunk_cache is used to adjust the chunk cache parameters for individual datasets.
!      call H5Pset_chunk_cache_f(pario_coll_dacc_id,     &  ! The parameters will come from the file access property list used to open the file.
!                          pario_chunk_cache_nslots,     &  ! H5D_CHUNK_CACHE_NSLOTS_DEFAULT,   &
!                          pario_chunk_cache_bytes,      &  ! H5D_CHUNK_CACHE_NBYTES_DEFAULT,   &
!                          pario_chunk_cache_preemption, &  ! H5D_CHUNK_CACHE_W0_DEFAULT
!                          ierr); CHKERRLW (ierr, 0)

      ! Sets the size of the parameter used to control the B-trees for indexing chunked datasets.
      ! Only for file-creation-property-list.
      if(pario_chunk_btree_size > 0) then
        call H5Pset_istore_k_f(pario_fcpl_id, pario_chunk_btree_size, ierr); CHKERRLW (ierr, 0)
      end if

      ! dataset transfer tuning (buffer on server)
      ! (keep in mind: server side optimisation might stop your code from working)
      ! nbytes = 4*(1024*1024)    ! (in bytes) 4 MByte
      if(pario_std_sievebuffer > 0) then
        call H5Pset_sieve_buf_size_f(pario_coll_facc_id, pario_std_sievebuffer,ierr); CHKERRLW (ierr, 0)
      end if

      !!! property lists for data transfer
      !---------------------------------------------
      ! dataset transfer tuning (buffer on client), (in bytes, 1 MByte is default of hdf5)
      ! FIXME on BlueGene/Q: this function throws errors on
      ! check docu for minimum size: http://www.hdfgroup.org/HDF5/doc/RM/H5P/H5Pset_buffer.htm
      if(pario_std_transbuffer > 0) then
        call H5Pset_buffer_f(pario_coll_dxfer_id, pario_std_transbuffer, ierr); CHKERRLW (ierr, 0)
      end if

      ! print all error messages from hdf5-lib
      call H5Eset_auto_f(1, ierr); CHKERRLW (ierr, 0)

    end subroutine init_h5

    !---------------------------------------------------------------------------
    !> @brief   initialise HDF5 for MPI-IO
    !> @details CHECK this: http://www.hdfgroup.org/HDF5/doc/TechNotes/VFL.html
    !>
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine init_h5_mpiio(ierr)
      use hdf5
      implicit none

      ! function args
      integer, intent(out) :: ierr

      ! other vars
      logical :: gpfs_hints = .false.
      integer :: mpiio_info

      PRTFNC(init_h5_mpiio)
      ierr=0

      !!! property lists for non-collective file access
      !----------------------------------------

      ! non-collective MPI-posix incl. GPFS hints
      !! gpfs_hints = .true.
      !!call H5Pset_fapl_mpiposix_f(pario_noncoll_facc_id, mpi_mycomm, gpfs_hints, ierr); CHKERRLW (ierr, 0)

      !!! property lists for collective file access
      !------------------------------------------

      ! hints for MPIO layer in (eg. IBM_largeblock_io=true from GPFS)
      ! on some platforms it is useful to pass some information onto the underlaying MPI_File_open call
      call MPI_Info_create(mpiio_info, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      call MPI_Info_set(mpiio_info, "IBM_largeblock_io","true", ierr); CHKERRQ1(ierr, MPI_SUCCESS)
!      call MPI_Info_set(mpiio_info, "IBM_io_buffer_size",??,    ierr); CHKERRQ1(ierr, MPI_SUCCESS)
!      call MPI_Info_set(mpiio_info, "IBM_sparse_access","true", ierr); CHKERRQ1(ierr, MPI_SUCCESS)

!      http://www.prace-ri.eu/IMG/pdf/Parallel_IO_performance_and_scalability_study_on_the_PRACE_CURIE_supercomputer-2.pdf
!        romio_ds_[read/write] --- data sieving is an optimization technique used for efficiently accessing noncontiguous regions of data in files
!                                  when noncontiguous accesses are not provided as a file system primitive.
!        romio_cb_[read/write] --- collective buffering is another optimization technique which consists in using a two-stage approach; for example,
!                                  in the case of a reading operation, data buffers are first split up amongst a set of aggregator processes
!                                  that will then actually perform I/O operations through filesystem calls.
!      example: export MPICH_MPIIO_HINTS="*:romio_cb_write=disable:romio_ds_write=enable:romio_cb_read=disable:romio_ds_read=enable"

      ! collective buffering
      if(pario_std_romio_collective_buffering==0) then
        call MPI_Info_set(mpiio_info, "romio_cb_write", "disable", ierr); CHKERRQ1(ierr, MPI_SUCCESS)
        call MPI_Info_set(mpiio_info, "romio_cb_read",  "disable", ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      else if(pario_std_romio_collective_buffering==1) then
        call MPI_Info_set(mpiio_info, "romio_cb_write", "enable", ierr); CHKERRQ1(ierr, MPI_SUCCESS)
        call MPI_Info_set(mpiio_info, "romio_cb_read",  "enable", ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      else
        ! take default = automatic
      end if

      ! data sieving
      if(pario_std_romio_data_sieving==0) then
        call MPI_Info_set(mpiio_info, "romio_ds_write", "disable", ierr); CHKERRQ1(ierr, MPI_SUCCESS)
        call MPI_Info_set(mpiio_info, "romio_ds_read",  "disable", ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      else if(pario_std_romio_data_sieving==1) then
        call MPI_Info_set(mpiio_info, "romio_ds_write", "enable", ierr); CHKERRQ1(ierr, MPI_SUCCESS)
        call MPI_Info_set(mpiio_info, "romio_ds_read",  "enable", ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      else
        ! take default = automatic
      end if

      ! collective MPI-IO
      call H5Pset_fapl_mpio_f(pario_coll_facc_id, mpi_mycomm, mpiio_info, ierr); CHKERRLW (ierr, 0)

      !!! property lists for parallel data transfer
      !--------

      ! collective MPI-IO
      call H5Pset_dxpl_mpio_f(pario_coll_dxfer_id, H5FD_MPIO_COLLECTIVE_F, ierr); CHKERRLW (ierr, 0)

      ! optimize chunking
      ! http://www.hdfgroup.uiuc.edu/papers/papers/ParallelIO/HDF5-CollectiveChunkIO.pdf
      ! call H5Pset_dxpl_mpio_chunk_opt(...) ! Sets a flag specifying linked-chunk I/O or multi-chunk I/O.
      ! call H5Pset_dxpl_mpio_chunk_opt_num   
      ! call H5Pset_dxpl_mpio_chunk_opt_ratio   
      ! call H5Pset_dxpl_mpio_collective_opt 

    end subroutine init_h5_mpiio

! support removed in hdf5 1.8.13
!    !---------------------------------------------------------------------------
!    !> @brief   initialise HDF5 for MPI-IO
!    !> @details CHECK this: http://www.hdfgroup.org/HDF5/doc/TechNotes/VFL.html
!    !>
!    !> @param   ierr      return code
!    !>
!    !> @ingroup io
!    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
!    !---------------------------------------------------------------------------
!    subroutine init_h5_mpiposix(ierr)
!      use hdf5
!      implicit none
!
!      ! function args
!      integer, intent(out) :: ierr
!
!      ! other vars
!      integer :: mpiio_info
!
!      PRTFNC(init_h5_mpiposix)
!      ierr=0
!
!      !!! property lists for non-collective file access
!      !----------------------------------------
!
!      !!! property lists for collective file access
!      !------------------------------------------
!
!      ! collective MPI-posix incl. GPFS hints
!      call H5Pset_fapl_mpiposix_f(pario_coll_facc_id, mpi_mycomm, pario_mpiposix_gpfshints, ierr); CHKERRLW (ierr, 0)
!
!      !!! property lists for parallel data transfer
!      !--------
!
!    end subroutine init_h5_mpiposix

    !---------------------------------------------------------------------------
    !> @brief   initialise HDF5 for POSIX IO
    !> @details CHECK this: http://www.hdfgroup.org/HDF5/doc/TechNotes/VFL.html
    !>
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine init_h5_posix(ierr)
      use hdf5
      implicit none

      ! function args
      integer, intent(out) :: ierr

      PRTFNC(init_h5_posix)
      ierr=0

      !!! property lists for non-collective file access
      !----------------------------------------

      ! SEC2 I/O driver (std. POSIX)
      call H5Pset_fapl_sec2_f(pario_noncoll_facc_id, ierr); CHKERRLW (ierr, 0)

      !!! property lists for collective file access
      !------------------------------------------

      ! SEC2 I/O driver (std. POSIX)
      call H5Pset_fapl_sec2_f(pario_coll_facc_id, ierr); CHKERRLW (ierr, 0)

    end subroutine init_h5_posix

    !---------------------------------------------------------------------------
    !> @brief   initialise HDF5 for FAMILY-IO
    !> @details CHECK this: http://www.hdfgroup.org/HDF5/doc/TechNotes/VFL.html
    !>
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine init_h5_family(ierr)
      use hdf5
      implicit none

      ! function args
      integer, intent(out) :: ierr

      PRTFNC(init_h5_family)
      ierr=0

      !!! property lists for non-collective file access
      !----------------------------------------

      ! split file into a family of files with no more than 256 MByte each using POSIX
      call H5Pset_fapl_family_f(pario_noncoll_facc_id, pario_family_ffsize, H5P_DEFAULT_F, ierr); CHKERRLW (ierr, 0)

      !!! property lists for collective file access
      !------------------------------------------

      ! split file into a family of files with no more than 256 MByte each using POSIX
      call H5Pset_fapl_family_f(pario_coll_facc_id,    pario_family_ffsize, H5P_DEFAULT_F, ierr); CHKERRLW (ierr, 0)

    end subroutine init_h5_family

    !---------------------------------------------------------------------------
    !> @brief   initialise HDF5 for STD-IO
    !> @details CHECK this: http://www.hdfgroup.org/HDF5/doc/TechNotes/VFL.html
    !>
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine init_h5_stdio(ierr)
      use hdf5
      implicit none

      ! function args
      integer, intent(out) :: ierr

      PRTFNC(init_h5_stdio)
      ierr=0

      !!! property lists for non-collective file access
      !----------------------------------------

      ! standard I/O driver of system
      call H5Pset_fapl_stdio_f(pario_noncoll_facc_id, ierr); CHKERRLW (ierr, 0)

      !!! property lists for collective file access
      !------------------------------------------

      ! standard I/O driver of system
      call H5Pset_fapl_stdio_f(pario_coll_facc_id, ierr); CHKERRLW (ierr, 0)

    end subroutine init_h5_stdio

    !---------------------------------------------------------------------------
    !> @brief   initialise HDF5 for DIRECT-IO
    !> @details CHECK this: http://www.hdfgroup.org/HDF5/doc/TechNotes/VFL.html
    !>
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine init_h5_direct(ierr)
      use hdf5
      implicit none

      ! function args
      integer, intent(out) :: ierr

      ! other vars
      integer(HSIZE_T) :: alignment, bsize, cbuffsize

      PRTFNC(init_h5_direct)
      ierr=0

      !!! property lists for non-collective file access
      !----------------------------------------

      ! direct access (alignment=4bk, block_size=default=4kb, copy buffer=16mb)
      ! copy buffer size must be a multiple of block size
      alignment = 4096    ! Required memory alignment boundary (in bytes)
      bsize     = 4096    ! File system block size (in bytes)
      cbuffsize = 4*1024*bsize ! Copy buffer size (in bytes)
      call H5Pset_fapl_direct_f(pario_noncoll_facc_id, alignment, bsize, cbuffsize, ierr); CHKERRLW (ierr, 0)

      !!! property lists for collective file access
      !------------------------------------------

      ! direct access (alignment=4kb, block_size=default=4kb, copy buffer=16mb)
      ! copy buffer size must be a multiple of block size"
      alignment = 4096    ! Required memory alignment boundary (in bytes)
      bsize     = 4096    ! File system block size (in bytes)
      cbuffsize = 4*1024*bsize ! Copy buffer size (in bytes)
      call H5Pset_fapl_direct_f(pario_coll_facc_id, alignment, bsize, cbuffsize, ierr); CHKERRLW (ierr, 0)

    end subroutine init_h5_direct

    !---------------------------------------------------------------------------
    !> @brief   close HDF5
    !> @details CHECK this: http://www.hdfgroup.org/HDF5/doc/TechNotes/VFL.html
    !>
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine close_h5(ierr)
      use hdf5
      implicit none
      !!! no access to data-vars allowed

      ! function args
      integer, intent(out) :: ierr

      PRTFNC(close_h5)
      ierr=0

      ! close property lists
      call H5Pclose_f(pario_coll_dxfer_id, ierr); CHKERRLW (ierr, 0)
      call H5Pclose_f(pario_coll_facc_id, ierr); CHKERRLW (ierr, 0)
      call H5Pclose_f(pario_noncoll_dcreate_id, ierr); CHKERRLW (ierr, 0)
      call H5Pclose_f(pario_noncoll_facc_id, ierr); CHKERRLW (ierr, 0)
      call H5Pclose_f(pario_fcpl_id, ierr); CHKERRLW (ierr, 0)

      ! close HDF5 interface
      call H5close_f(ierr)

    end subroutine close_h5

    !---------------------------------------------------------------------------
    !> @brief    read infos of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[out]      dset_ntype_id dataset type
    !> @param[out]      dset_prec     dataset precision
    !> @param[out]      ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine read_dset_type(loc_id, dset_name, dset_ntype_id, dset_prec, ierr)
      use hdf5
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      integer(HID_T),  intent(inout) :: dset_ntype_id
      integer(SIZE_T), intent(inout) :: dset_prec
      integer         ,intent(out)   :: ierr

      ! other vars
      integer(HID_T) :: did, dspace_id, dtype_id
      integer :: spacedim
      integer(HSIZE_T), dimension(3) :: maxdims3
      integer :: open_err

      PRTFNC(read_dset_type)
      ierr = 0

      ! open dataset
      call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
        call H5Dopen_f (loc_id, dset_name, did, open_err)
      call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
      if (open_err /= 0) then
        ierr = open_err
        return
      end if

      ! get dataset type (dtype_id is a pointer address to some HDF5 internal struct - use HDF5 functions to examine)
      call H5Dget_type_f(did, dtype_id, ierr); CHKERRLW(ierr,0)

      ! get native datatype
      call H5Tget_native_type_f(dtype_id, 1, dset_ntype_id, ierr); CHKERRLW(ierr,0)
      if(dset_ntype_id < 0) then
        write(*,*) 'ERROR: datatype of ', trim(dset_name),' is not of any native datatype'
        stop
      end if

      ! get precision of dataset in bits and return as bytes
      call H5Tget_precision_f(dtype_id, dset_prec, ierr); CHKERRLW(ierr,0)
      dset_prec = dset_prec / 8

      call H5Dclose_f(did, ierr); CHKERRLW(ierr,0)

    end subroutine read_dset_type

    !---------------------------------------------------------------------------
    !> @brief    read infos of 3d hdf5 dataset
    !> @details  ...
    !>
    !> @param[in]       loc_id        hdf5 location id representing path to dataset group in hdf file
    !> @param[in]       dset_name     dataset name in hdf file
    !> @param[in,out]   dset_dims     dataset dimensions
    !> @param[in,out]   ierr          return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine read_dset_dims(loc_id, dset_name, dset_dims, ierr)
      use hdf5
      implicit none

      ! function args
      integer(HID_T)  ,intent(in)    :: loc_id
      character(len=*),intent(in)    :: dset_name
      integer(HSIZE_T), dimension(3), intent(inout) :: dset_dims
      integer         ,intent(inout) :: ierr

      ! other vars
      integer(HID_T) :: did, dspace_id, dtype_id
      integer :: spacedim
      integer(HSIZE_T), dimension(3) :: maxdims
      integer :: open_err

      PRTFNC(read_dset_dims)
      ierr = 0

      ! open dataset
      call H5Eset_auto_f (0, ierr); CHKERRLW (ierr, 0)
        call H5Dopen_f (loc_id, dset_name, did, open_err)
      call H5Eset_auto_f (1, ierr); CHKERRLW (ierr, 0)
      if (open_err /= 0) then
        ierr = open_err
        return
      end if

      ! get dataset dimensions
      call H5Dget_space_f(did, dspace_id, ierr); CHKERRLW(ierr,0)
      call H5Sget_simple_extent_ndims_f(dspace_id, spacedim, ierr); CHKERRLW(ierr,0)
      if(spacedim .ne. 3) then
        write(*,*) 'ERROR: input dataset must be 3D (not ',spacedim,')'
        stop
      endif
      call H5Sget_simple_extent_dims_f(dspace_id, dset_dims, maxdims, ierr); CHKERRQ1(ierr,3)

      call H5Dclose_f(did, ierr); CHKERRLW(ierr,0)

    end subroutine read_dset_dims

end module pario_data

