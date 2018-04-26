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
!> @file pario.F90
!! @brief main public parIO functions
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "pario_defines.inc"

module pario
    use hdf5
    use pario_consts

    use pario_funcs
    use pario_gprop
    use pario_dset2dset
    use pario_gdat
    use pario_gval
    use pario_hysl2dset
    use pario_hysl2hysl
    use pario_pdat
    use pario_pval

    implicit none
    save

    integer(HID_T)  ::  pario_create_prp
    integer(HID_T)  ::  pario_noncoll_access_prp
    integer(HID_T)  ::  pario_coll_access_prp

    contains

    !---------------------------------------------------------------------------
    !> @brief   initialise parIO
    !> @details ...
    !>
    !> @param[in]   mpi_mycomm_in MPI communicator
    !> @param[out]  ierr        return code
    !> @param[in]   io_meth     [optional] IO method: PARIO_MPIIO (default), PARIO_POSIX, PARIO_STDIO, PARIO_DIRECT, PARIO_FAMILY

    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine init_pario(mpi_mycomm_in, ierr, io_meth, mpi_rootid)
      use pario_data
      implicit none

      ! function args
      integer, intent(in) :: mpi_mycomm_in
      integer, intent(out) :: ierr
      integer, optional, intent(in) :: io_meth
      integer, optional, intent(in) :: mpi_rootid

      ! other vars
      integer :: io_method
      integer (kind=MPI_ADDRESS_KIND) :: attr_val
      logical :: attr_flag

      PRTFNC(init_pario)
      ierr=0

      ! set IO method (default MPIIO)
      io_method = PARIO_MPIIO
      if(present(io_meth)) io_method = io_meth

      if(initialised) then
        PRTWARN1('parIO already initialised - close parIO first')
        return
      end if

      mpi_myroot = 0
      if(present(mpi_rootid)) mpi_myroot = mpi_rootid
      mpi_mycomm = mpi_mycomm_in

      ! just make sure all threads participate
      call MPI_Barrier (mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)

      ! get mpi stuff
      call MPI_COMM_SIZE (mpi_mycomm, mpi_nproc, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      call MPI_COMM_RANK (mpi_mycomm, mpi_proc_id, ierr); CHKERRQ1(ierr, MPI_SUCCESS)

      ! check if MPI IO available
      if(io_method == PARIO_MPIIO) then
        call MPI_COMM_GET_ATTR (mpi_mycomm, MPI_IO, attr_val, attr_flag, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
        if (.not. attr_flag) then
          PRTERR1('FATAL ERROR: _NO_ MPI I/O support.')
          ierr = -1
          return
        end if
      end if

      ! print machine settings
      if (mpi_proc_id == mpi_myroot) then
        write (*,*)
        write (*,*) 'parIO Info:'
        write (*,*) '  ROOT_ID       : ', mpi_myroot
        write (*,*) '  HID_T         : ', Int (HID_T, 4)
        write (*,*) '  HSIZE_T       : ', Int (HSIZE_T, 4)
        write (*,*) '  NATIVE_FLOAT  : ', Int (sizeof(mySINGLE), 4)
        write (*,*) '  NATIVE_DOUBLE : ', Int (sizeof(myDOUBLE), 4)
        write (*,*) '  NATIVE_INTEGER: ', Int (sizeof(myINTEGER), 4)
        write (*,*)
        if(pario_std_alignment > 0) then
          write (*,*) '  alignthreshold (mbytes) = ', pario_std_threshold/(1024*1024)
          write (*,*) '  alignment      (mbytes) = ', pario_std_alignment/(1024*1024)
        end if
        if(pario_std_transbuffer > 0)            write (*,*) '  transbuffer    (mbytes) = ', pario_std_transbuffer/(1024*1024)
        if(pario_std_sievebuffer > 0)            write (*,*) '  sievebuffer    (mbytes) = ', pario_std_sievebuffer/(1024*1024)
        if(pario_std_metablock_bytes > 0)        write (*,*) '  metablock      (kbytes) = ', pario_std_metablock_bytes/1024
        if(pario_std_metablock_flushatclose > 0) then
          write (*,*) '  only meta-flush at close= yes'
        else
          write (*,*) '  only meta-flush at close= no'
        end if
        write (*,*)
        if(pario_chunk_size_2d(1) > 0 .or. pario_chunk_size_3d(1) > 0) then
          write (*,fmt='(a,2(i6))') '   2d chunk size           = ', pario_chunk_size_2d
          write (*,fmt='(a,3(i6))') '   3d chunk size           = ', pario_chunk_size_3d
          write (*,*) '  chunk_cache_nslots      = ', pario_chunk_cache_nslots
          write (*,*) '  chunk_cache (mbytes)    = ', pario_chunk_cache_bytes /(1024*1024)
          write (*,*) '  chunk_cache_preemptiont = ', pario_chunk_cache_preemption
          write (*,*)
        end if
        if(io_meth == PARIO_MPIIO ) then
          write (*,*) '  HDF5 IO-API   : PARIO_MPIIO'
        end if
        if(io_meth == PARIO_POSIX ) then
          write (*,*) '  HDF5 IO-API   : PARIO_POSIX'
        end if
        if(io_meth == PARIO_FAMILY) then
          write (*,*) '  HDF5 IO-API   : PARIO_FAMILY'
          write (*,*) '    family file size (mbytes) = ', pario_family_ffsize/(1024*1024)
        end if
! support removed in hdf5 1.8.13
!        if(io_meth == PARIO_MPIPOSIX ) then
!          write (*,*) '  HDF5 IO-API   : PARIO_MPIPOSIX'
!          write (*,*) '    GPFS hint                 = ', pario_mpiposix_gpfshints
!        end if
      end if

      ! create and set hdf5 property lists
      call init_h5(io_method, ierr); CHKERRQ0(ierr)

      ! copy property lists from parIO private data to public data
      !   h5pcopy_f(pario_noncoll_facc_id, pario_noncoll_access_prp, hdferr) for real copy, which has to be closed after use
      pario_create_prp         = pario_fcpl_id
      pario_noncoll_access_prp = pario_noncoll_facc_id
      pario_coll_access_prp    = pario_coll_facc_id

      initialised = .true.

    end subroutine init_pario

    !---------------------------------------------------------------------------
    !> @brief   initialise parIO
    !> @details
    !>
    !> @param   ierr      return code
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine close_pario(ierr)
      use pario_data
      implicit none

      ! function args
      integer, intent(out) :: ierr

      PRTFNC(close_pario)
      ierr=0

      if(.not.initialised) return

      ! just make sure all threads participate
      call MPI_Barrier (mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)

      call close_h5(ierr); CHKERRQ0(ierr)
      pario_create_prp         = H5P_DEFAULT_F
      pario_noncoll_access_prp = H5P_DEFAULT_F
      pario_coll_access_prp    = H5P_DEFAULT_F

      initialised = .false.

    end subroutine close_pario

    !---------------------------------------------------------------------------
    !> @brief   set extras of parIO
    !> @details on >0, off <=0
    !>          Must be called before init_pario.
    !>
    !> @param[in] test_onoff       [optional] write datasets with test-values, check values on read
    !> @param[in] minmaxmean_onoff [optional] write min,max,mean for all collective datasets
    !> @param[in] hist_onoff       [optional] write min,max,mean + histogram for all collective datasets
    !> @param[in] prtspeed_onoff   [optional] print io speed to stdout after each read/write of collective dataset
    !> @param[in] verbose_level    [optional] set level of information written to stdout
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine settings_pario(test_onoff, minmaxmean_onoff, hist_onoff, prtspeed_onoff, verbose_level)
      use pario_data
      implicit none

      ! function args
      integer, optional, intent(in) :: test_onoff
      integer, optional, intent(in) :: minmaxmean_onoff
      integer, optional, intent(in) :: hist_onoff
      integer, optional, intent(in) :: prtspeed_onoff
      integer, optional, intent(in) :: verbose_level

      PRTFNC(settings_pario)

      if(present(test_onoff)) then
        if(test_onoff > 0) then
          dump_test = .true.
        else
          dump_test = .false.
        end if
      end if

      if(present(minmaxmean_onoff)) then
        if(minmaxmean_onoff > 0) then
          dump_minmaxmean = .true.
        else
          dump_minmaxmean = .false.
        end if
      end if

      if(present(hist_onoff)) then
        if(hist_onoff > 0) then
          dump_hist = .true.
        else
          dump_hist = .false.
        end if
      end if

      if(present(prtspeed_onoff)) then
        if(prtspeed_onoff > 0) then
          dump_speed = .true.
        else
          dump_speed = .false.
        end if
      end if

      if(present(verbose_level)) then
        verbose_mode = verbose_level
      end if

    end subroutine settings_pario

    !---------------------------------------------------------------------------
    !> @brief   set extras of HDF's standard stuff
    !> @details Must be called before init_pario.
    !>
    !> @param[in] alignthreshold    [optional] if a dataset reaches 'alignthreshold' (in bytes), dataset is aligned in the hdf5 file by 'alignment'
    !> @param[in] alignment         [optional] if a dataset reaches 'alignthreshold' (in bytes), dataset is aligned in the hdf5 file by 'alignment'
    !> @param[in] transbuffer_bytes [optional]
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine settings_pario_std(alignthreshold_bytes, alignment_bytes, &
                                    transbuffer_bytes, sievebuffer_bytes, metablock_bytes, &
                                    metablock_flushatclose, &
                                    romio_collective_buffering, romio_data_sieving)
      use pario_data
      implicit none

      ! function args
      integer(kind=8), optional, intent(in) :: alignthreshold_bytes
      integer(kind=8), optional, intent(in) :: alignment_bytes
      integer(kind=8), optional, intent(in) :: transbuffer_bytes
      integer(kind=8), optional, intent(in) :: sievebuffer_bytes
      integer(kind=8), optional, intent(in) :: metablock_bytes
      integer        , optional, intent(in) :: metablock_flushatclose
      integer        , optional, intent(in) :: romio_collective_buffering
      integer        , optional, intent(in) :: romio_data_sieving

      PRTFNC(settings_pario_std)

      if(present(alignthreshold_bytes)) then
        if(alignthreshold_bytes > 0) pario_std_threshold = alignthreshold_bytes
      end if

      if(present(alignment_bytes)) then
        if(alignment_bytes > 0) pario_std_alignment = alignment_bytes
      end if

      if(present(transbuffer_bytes)) then
        if(transbuffer_bytes > 0) pario_std_transbuffer = transbuffer_bytes
      end if

      if(present(sievebuffer_bytes)) then
        if(sievebuffer_bytes > 0) pario_std_sievebuffer = sievebuffer_bytes
      end if

      if(present(metablock_bytes)) then
        if(metablock_bytes > 0) pario_std_metablock_bytes = metablock_bytes
      end if

      if(present(metablock_flushatclose)) then
        pario_std_metablock_flushatclose = metablock_flushatclose
      end if

      if(present(romio_collective_buffering)) then
        pario_std_romio_collective_buffering = romio_collective_buffering
      end if

      if(present(romio_data_sieving)) then
        pario_std_romio_data_sieving = romio_data_sieving
      end if

    end subroutine settings_pario_std

!    !---------------------------------------------------------------------------
!    !> @brief   set extras of HDF's MPI-IO API
!    !> @details Must be called before init_pario.
!    !>
!    !> @ingroup io
!    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
!    !---------------------------------------------------------------------------
!    subroutine settings_pario_mpiio()
!      use pario_data
!      implicit none
!
!      ! function args
!
!      PRTFNC(settings_pario_mpiio)
!
!    end subroutine settings_pario_mpiio

    !---------------------------------------------------------------------------
    !> @brief   set extras of HDF's FAMILY-IO API
    !> @details Must be called before init_pario.
    !>
    !> @param[in] ffsize_bytes         [optional] maximum size of a single family file (default 256 MByte)
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine settings_pario_family(ffsize_bytes)
      use pario_data
      implicit none

      ! function args
      integer(kind=8), optional, intent(in) :: ffsize_bytes

      PRTFNC(settings_pario_family)

      if(present(ffsize_bytes)) then
        if(ffsize_bytes > 0) pario_family_ffsize = ffsize_bytes
      end if

    end subroutine settings_pario_family

! support removed in hdf5 1.8.13
!    !---------------------------------------------------------------------------
!    !> @brief   set extras of HDF's MPIPOSIX-IO API
!    !> @details Must be called before init_pario.
!    !>
!    !> @param[in] ffsize_bytes         [optional] maximum size of a single family file (default 256 MByte)
!    !>
!    !> @ingroup io
!    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
!    !---------------------------------------------------------------------------
!    subroutine settings_pario_mpiposix(gpfs_hint)
!      use pario_data
!      implicit none
!
!      ! function args
!      logical, optional, intent(in) :: gpfs_hint
!
!      PRTFNC(settings_pario_mpiposix)
!
!      if(present(gpfs_hint)) then
!        pario_mpiposix_gpfshints = gpfs_hint
!      end if
!
!    end subroutine settings_pario_mpiposix

    !---------------------------------------------------------------------------
    !> @brief   set default chunk sizes for a certain chunk dimension
    !> @details It can be called multiple times, once for each chunk_dims and is used in init_dataset()
    !>          http://www.hdfgroup.uiuc.edu/papers/papers/ParallelIO/HDF5-CollectiveChunkIO.pdf
    !>
    !> @param[in] chunk_dims       dimensions of chunk to set the size for
    !> @param[in] chunk_size       size array with chunk_dims dimensions
    !> @param[in] chunk_btree_size size of btree for coordiante mapping of chunk-coords to file-coords
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine settings_pario_chunksize(chunk_dims, chunk_size, chunk_btree_size)
      use pario_data
      implicit none

      ! function args
      integer, intent(in) :: chunk_dims
      integer, intent(in) :: chunk_size(chunk_dims)
      integer, optional, intent(in) :: chunk_btree_size

      PRTFNC(settings_pario_chunksize)

      if(product(chunk_size) > 0) then
        if(chunk_dims == 2) then
          pario_chunk_size_2d = chunk_size
        else if (chunk_dims == 3) then
          pario_chunk_size_3d = chunk_size
        else if (chunk_dims == 4) then
          pario_chunk_size_4d = chunk_size
        else
          PRTERR1('only set default size of 2D,3D,4D chunks')
        end if
      end if

      if(present(chunk_btree_size)) then
        if(chunk_btree_size > 0) then
          pario_chunk_btree_size = chunk_btree_size
        end if
      end if

    end subroutine settings_pario_chunksize

    !---------------------------------------------------------------------------
    !> @brief   set chunk caching properties
    !> @details Must be called before init_pario.
    !>
    !> @param[in] chunk_chache_nslots      [optional] should be at least 10 (for best performance 100) times no. chunks that can fit in chunk_cache_bytes. Default 521
    !> @param[in] chunk_cache_bytes        [optional] default size is 1 MB.
    !> @param[in] chunk_chache_preemption  [optional] (0.0 - 1.0) If your application only reads or writes data once, this can be safely set to 1.0. Default 0.75
    !>
    !> @ingroup io
    ! additional doxygen-tags: @ingroup @param @author @version @bug, @warning, @todo, @see, @test
    !---------------------------------------------------------------------------
    subroutine settings_pario_chunkcache(chunk_cache_bytes, chunk_cache_nslots, chunk_cache_preemption)
      use pario_data
      implicit none

      ! function args
      integer(kind=8), optional, intent(in) :: chunk_cache_bytes
      integer, optional, intent(in) :: chunk_cache_nslots
      real,    optional, intent(in) :: chunk_cache_preemption

      PRTFNC(settings_pario_chunkcache)

      ! Raw dataset chunk caching is not currently supported when using the MPI I/O and MPI POSIX file drivers in read/write mode (see H5Pset_fapl_mpio and H5Pset_fapl_mpiposix, respectively).
      ! When using one of these file drivers, all calls to H5Dread and H5Dwrite will access the disk directly, and H5Pset_cache will have no effect on performance.
      ! Raw dataset chunk caching IS supported when these drivers are used in read-only mode.

      ! The default value is 521. Due to the hashing strategy, this value should ideally be a prime number.
      ! As a rule of thumb, this value should be at least 10 times the number of chunks that can fit in rdcc_nbytes bytes.
      ! For maximum performance, this value should be set approximately
      ! 100 times that number of chunks.
      if(present(chunk_cache_nslots)) then
        if(chunk_cache_nslots > 0) then
          pario_chunk_cache_nslots = chunk_cache_nslots
          ! we do not want to search for the next prime number here, but we can do at least two simple tests
          if(mod(chunk_cache_nslots,5) == 0) pario_chunk_cache_nslots = pario_chunk_cache_nslots +1 ! avoid 5 dividables
          if(mod(chunk_cache_nslots,2) == 0) pario_chunk_cache_nslots = pario_chunk_cache_nslots +1 ! avoid even numbers
        end if
      end if

      ! The default size is 1 MB.  In most cases increasing this number will improve performance, as long as you have enough free memory.
      if(present(chunk_cache_bytes)) then
        if(chunk_cache_bytes > 0) pario_chunk_cache_bytes = chunk_cache_bytes
      end if

      ! If your application only reads or writes data once, this can be safely set to 1.0
      ! Otherwise, this should be set lower (eg. 0.75), depending on how often you re-read or re-write the same data.
      if(present(chunk_cache_preemption)) then
        if(chunk_cache_preemption >= 0.0 .and. chunk_cache_preemption <= 1.0) pario_chunk_cache_preemption = chunk_cache_preemption
      end if

    end subroutine settings_pario_chunkcache

end module
