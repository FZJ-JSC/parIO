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
!> @file testio_io.F90
!! @brief file of the io module
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "testio_defines.inc"

      module testio_io
      use pario
      use testio_data

      implicit none
      save
      private

      public :: init_io, close_io
      public :: read_file, write_file

      logical :: initialised = .false.

      contains

     !
     !========================================
     !
     !  init io
     !
     !========================================
     !
      subroutine init_io(ierr)
        implicit none

     !  function args
        integer, intent(out) :: ierr

        integer(kind=8) :: chunk_cache_bytes
        integer :: chunk_cache_nslots, chunk_btree_size
        PRTFNC(init_io)
        ierr = 0

        if(initialised) return

        call settings_pario(PARIO_ON,  & ! use read/write with test data
                            PARIO_OFF, & ! dump min, max, mean of processor data
                            PARIO_OFF, & ! dump histogram of processor data
                            PARIO_ON,  & ! print read/write timing
                            verbose_mode)! verbose level (0-4)

        if(chunking > 0) then

          ! setting cache for chunking (if parallel HDF5 it has only impact on read operations)
          ! setting to 2.5 times the default 3d chunk and 100 times more slots than chunks
          chunk_cache_bytes  = (int(chunk_3d(1),8) *int(chunk_3d(2),8) *int(chunk_3d(3),8) *8) *2.5
          chunk_cache_bytes  = max(chunk_cache_bytes, int(1024*1024,8))

          chunk_btree_size   = (int(domain_dims_3d(1),8) *int(domain_dims_3d(2),8) *int(domain_dims_3d(2),8) *8) / &
                               (int(chunk_3d(1)      ,8) *int(chunk_3d(2)      ,8) *int(chunk_3d(3)      ,8) *8) +1 ! == total number of chunks +1
          if(chunk_btree_size < 32) chunk_btree_size = 32

          chunk_cache_nslots = (int(pdims_3d(1),8) *int(pdims_3d(2),8) *int(pdims_3d(3),8) *8) / (chunk_cache_bytes/2.5) * 100
          chunk_cache_nslots = max(chunk_cache_nslots, 521)

          ! setting chunk stuff
          call settings_pario_chunksize(2, chunk_2d, chunk_btree_size)
          call settings_pario_chunksize(3, chunk_3d, chunk_btree_size)
          call settings_pario_chunksize(4, chunk_4d, chunk_btree_size)
          call settings_pario_chunkcache(chunk_cache_bytes, chunk_cache_nslots, 1.0)
        end if

        call settings_pario_std(int(128*(1024*1024),8),  & ! threshold for dataset size (in bytes) from which alignment is used
                                int(  4*(1024*1024),8),  & ! alignment of dataset in HDF5 file in bytes
                                int(  4*(1024*1024),8),  & ! transfer buffer (-1 == HDF5 default)
                                int(  4*(1024*1024),8),  & ! sieve buffer (-1 == HDF5 default)
                                int(     256*(1024),8),  & ! metablock size - default is 2048 bytes, meaning that the library aggregates metadata in at least 2K blocks in the file
                                1       )                  ! disables evictions from the metadata cache, unless H5Fflush is called or the file is closed

        ! settings for MPI-IO
        if(ioAPI == USE_MPIIO) then
          call init_pario(mpi_mycomm, ierr, PARIO_MPIIO, mpi_rootid)

        ! settings for HDF5-POSIX-IO using a single file
        else if(ioAPI == USE_POSIX) then
          call init_pario(mpi_mycomm, ierr, PARIO_POSIX, mpi_rootid)

        ! settings for HDF5-Family-IO using multiple files over POSIX
        else if(ioAPI == USE_FAMILY) then
          call settings_pario_family( int(2**24,8) ) ! size of single family file in bytes ( 2**24 == 16 MByte, 2**28 == 256 MByte)
          call init_pario(mpi_mycomm, ierr, PARIO_FAMILY, mpi_rootid)

        ! settings for HDF5-Family-IO using multiple files over POSIX
        !else if(ioAPI == USE_MPIPOSIX) then
        !  call settings_pario_mpiposix(.true.) ! set GPFS hints
        !  call init_pario(mpi_mycomm, ierr, PARIO_MPIPOSIX, mpi_rootid)

        else
          PRTERR1('set unknown IO-API')
        end if

        initialised = .true.

      end subroutine init_io

     !
     !========================================
     !
     !  close io
     !
     !========================================
     !
      subroutine close_io(ierr)
        implicit none

     !  function args
        integer, intent(out) :: ierr

        PRTFNC(close_io)
        ierr = 0

        if(.not.initialised) return

        call close_pario(ierr)

        initialised = .false.

      end subroutine close_io

     !
     !========================================
     !
     !  read file
     !
     !========================================
     !
      subroutine read_file(file_path, ierr)
        implicit none

     !  function args
        character(len=*), intent(in)   :: file_path
        integer,          intent(out)   :: ierr

        PRTFNC(read_file)
        ierr = 0
        call init_io(ierr); CHKERRQ0(ierr)

        PRTVERBOSE2('file path: ',trim(file_path),1)

        PRTVERBOSE1('read globals ...',1)
        call read_globals(file_path, ierr)

        PRTVERBOSE1('read proc data ...',1)
        call read_proc_data(file_path, ierr)

      end subroutine read_file

     !
     !========================================
     !
     !	write file
     !
     !========================================
     !
      subroutine write_file(file_path, ierr)
        implicit none

     !	function args
        character(len=*), intent(in)	 :: file_path
        integer,          intent(out)	 :: ierr

        PRTFNC(write_file)
        ierr = 0
        call init_io(ierr); CHKERRQ0(ierr)

        PRTVERBOSE2('file path: ',trim(file_path),1)

        PRTVERBOSE1('write globals ...',1)
      call write_globals(file_path, ierr)

        PRTVERBOSE1('write proc data ...',1)
        call write_proc_data(file_path, ierr)

      end subroutine write_file

     !
     !========================================
     !
     !	read global settings
     !
     !========================================
     !
      subroutine read_globals(file_path, ierr)
        use hdf5
        use testio_data
        use testio_io_basics
        !use testio_io_extras
        implicit none

     !	function args
        character(len=*), intent(in)	:: file_path
        integer,          intent(out)	:: ierr

     !	hd5 file vars
        integer(HID_T)	::	file_id

        PRTFNC(read_globals)
        ierr=0
        call init_io(ierr); CHKERRQ0(ierr)

     !	open hd5-file with mpi proc == 0
     !	---------------------------
        call open_file_read4gval(file_path, file_id, ierr); CHKERRLW (ierr, 0)

     !  read hd5-file with mpi proc == 0, but call function with all procs to enable broadcast
     !  ---------------------------
        call read_globals_basics  (file_id, ierr)
        !call read_globals_extras  (file_id, ierr)

     !	close hd5-file with mpi proc == 0
     !	---------------------------
        PRTVERBOSE2('close file ',trim(file_path),1)
        if(mpi_proc_id == mpi_rootid) then
          call H5Fclose_f(file_id, ierr); CHKERRLW (ierr, 0)
        end if

     ! make sure no process leaves the readin function before everyone has finished
        call MPI_Barrier(mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)

      end subroutine read_globals

     !
     !========================================
     !
     !  write global settings
     !
     !========================================
     !
      subroutine write_globals(file_path, ierr)
        use hdf5
        use testio_data
        use testio_io_basics
        !use testio_io_extras
        implicit none

     !  function args
        character(len=*), intent(in)    :: file_path
        integer,          intent(out)   :: ierr

     !  hd5 file vars
        integer(HID_T)  ::  file_id
        integer         :: open_err

        PRTFNC(write_globals)
        ierr=0
        call init_io(ierr); CHKERRQ0(ierr)

     !  create hd5-file with mpi proc == mpi_rootid
     !  ---------------------------
        call open_file_write4gval(file_path, .true., file_id, ierr); CHKERRLW (ierr, 0)

     ! write hd5-file with mpi proc == mpi_rootid, but call function with all procs
     ! ---------------------------
        call write_globals_basics(file_id, ierr)
        !call write_globals_extras(file_id, ierr)

     ! close hd5-file
     ! ---------------------------
        if(mpi_proc_id == mpi_rootid) then
          call H5Fclose_f(file_id, ierr); CHKERRLW (ierr, 0)
        end if

     ! make sure no process will try to access the file before it is written and closed by process 0
        call MPI_Barrier(mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)

      end subroutine write_globals

     !
     !========================================
     !
     !	read in hdf5 data for processor
     !
     !========================================
     !
      subroutine read_proc_data(file_path,ierr)
        use hdf5
        use testio_data
        use testio_io_basics
        !use testio_io_extras
        implicit none

     !	function args
        character(len=*), intent(in)	:: file_path
        integer,          intent(out)	:: ierr

     !	hd5 vars
        integer(HID_T)	::	file_id

        PRTFNC(read_proc_data)
        ierr=0
        call init_io(ierr); CHKERRQ0(ierr)

     ! open hd5-file collectively
     ! ---------------------------
        PRTVERBOSE2('open file ',trim(file_path),1)
        call open_file_read4pval(file_path, file_id, ierr); CHKERRLW (ierr, 0)

     ! read hd5-file collectively
     ! ---------------------------
        call read_proc_data_basics(file_id,ierr)
        !call read_proc_data_extras(file_id, ierr)

     ! close hd5-file collectively
     ! ---------------------------
     ! make sure no process is writing any more
        call MPI_Barrier(mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)

        PRTVERBOSE2('close file ',trim(file_path),1)
        call H5Fclose_f(file_id, ierr); CHKERRLW (ierr, 0)

     ! make sure no process leaves the readin function before everyone has finished
        call MPI_Barrier(mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)

      end subroutine read_proc_data

     !
     !========================================
     !
     !	write in hdf5 data for processor
     !
     !========================================
     !
      subroutine write_proc_data(file_path, ierr)
        use hdf5
        use testio_data
        use testio_io_basics
        !use testio_io_extras
       implicit none

     !	function args
        character(len=*), intent(in)  :: file_path
        integer,          intent(out) :: ierr

     !	hd5 vars
        integer(HID_T)   :: file_id
        integer(HID_T)	 ::	group_id
        character(len=64):: group_name, dset_name

        PRTFNC(write_proc_data)
        ierr=0
        call init_io(ierr); CHKERRQ0(ierr)

     !	open the file collectively
     !	---------------------------
        call open_file_write4pval(file_path, .false., file_id, ierr); CHKERRLW (ierr, 0)

     ! read hd5-file collectively
     ! ---------------------------

        call write_proc_data_basics(file_id, ierr)
        !call write_proc_data_extras(file_id, ierr)

     ! close hd5-file collectively
     ! ---------------------------
     ! make sure no process is writing any more
        call MPI_Barrier(mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)

        PRTVERBOSE2('close file ',trim(file_path),1)
        call H5Fclose_f(file_id, ierr); CHKERRLW (ierr, 0)

     ! make sure no process will try to access the file before it is written
        call MPI_Barrier(mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)

      end subroutine write_proc_data

      end module

