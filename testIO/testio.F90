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
!> @addtogroup main
!! @{
!!
!> @file testio.F90
!! @brief the main program
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "testio_defines.inc"

!---------------------------------------------------------------------------
!
!> @ingroup main
!! @brief   testIO main function
!! @details
!
!---------------------------------------------------------------------------
program testIO
    use testio_io
    use testio_data
    implicit none

    ! usual stuff
    integer :: l, ierr
    double precision :: wtimer

    ! command line arguments
    integer :: i
    integer(kind=4) :: iargc
    character(len=1024) :: arg_string

    ! ---------------------------
    ! init MPI
    ! ---------------------------
    mpi_mycomm = MPI_COMM_WORLD
    call MPI_INIT (ierr); CHKERRQ1(ierr, MPI_SUCCESS)
    call MPI_COMM_SIZE (mpi_mycomm, mpi_nproc, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
    call MPI_COMM_RANK (mpi_mycomm, mpi_proc_id, ierr); CHKERRQ1(ierr, MPI_SUCCESS)

    ! ---------------------------
    ! read command line arguments
    ! ---------------------------
    PRTINFO1("")
    PRTINFO1("args: filename p1 p2 p3 chunking ioAPI verbose_mode loop")
    PRTINFO1("chunking     - 0=off, 1=on")
    PRTINFO1("ioAPI        - 1=PARIO_MPIIO 2=PARIO_POSIX 3=PARIO_FAMILY")
    PRTINFO1("verbose_mode - 0-4")
    PRTINFO1("loop         - >1")
    PRTINFO1("-------")
    PRTINFO1("")
    do i=0, iargc()
      call getarg(i, arg_string)
      arg_string = adjustl(trim(arg_string))
      if(i==1) filename = trim(arg_string)
      if(i==2) read(arg_string,*) p1
      if(i==3) read(arg_string,*) p2
      if(i==4) read(arg_string,*) p3
      if(i==5) read(arg_string,*) chunking
      if(i==6) read(arg_string,*) ioAPI
      if(i==7) read(arg_string,*) verbose_mode
      if(i==8) read(arg_string,*) loop
    end do

    ! ---------------------------
    ! allocate dynamic memory
    ! ---------------------------
    PRTINFO1("Allocating Memory")
    call init_domain()

    ! 3d
    if(on_3d) then
      allocate (preal4_3d_mem(p1, p2, p3), stat=ierr); CHKERRQ0(ierr)
      allocate (preal8_3d_mem(p1, p2, p3), stat=ierr); CHKERRQ0(ierr)
      allocate (pint4_3d_mem(p1, p2, p3),  stat=ierr); CHKERRQ0(ierr)
      !allocate (pint8_3d_mem(p1, p2, p3),  stat=ierr); CHKERRQ0(ierr)
      allocate (pcmplx4_3d_mem(p1, p2, p3), stat=ierr); CHKERRQ0(ierr)
      allocate (pcmplx8_3d_mem(p1, p2, p3), stat=ierr); CHKERRQ0(ierr)
      preal4_3d_ptr  => preal4_3d_mem
      preal8_3d_ptr  => preal8_3d_mem
      pint4_3d_ptr   => pint4_3d_mem
      !pint8_3d_ptr   => pint8_3d_mem
      pcmplx4_3d_ptr => pcmplx4_3d_mem
      pcmplx8_3d_ptr => pcmplx8_3d_mem
    end if

    ! 4d
    if(on_4d) then
      allocate (preal4_4d_mem(p1, p2, p3, v4), stat=ierr); CHKERRQ0(ierr)
      allocate (preal8_4d_mem(p1, p2, p3, v4), stat=ierr); CHKERRQ0(ierr)
      allocate (pint4_4d_mem(p1, p2, p3, v4),  stat=ierr); CHKERRQ0(ierr)
      !allocate (pint8_4d_mem(p1, p2, p3, v4),  stat=ierr); CHKERRQ0(ierr)
      allocate (pcmplx4_4d_mem(p1, p2, p3, v4), stat=ierr); CHKERRQ0(ierr)
      allocate (pcmplx8_4d_mem(p1, p2, p3, v4), stat=ierr); CHKERRQ0(ierr)
      preal4_4d_ptr  => preal4_4d_mem
      preal8_4d_ptr  => preal8_4d_mem
      pint4_4d_ptr   => pint4_4d_mem
      !pint8_4d_ptr   => pint8_4d_mem
      pcmplx4_4d_ptr => pcmplx4_4d_mem
      pcmplx8_4d_ptr => pcmplx8_4d_mem
    end if

    ! ---------------------------
    ! loop incl. init/close (test for memory leaks)
    ! ---------------------------
    do l=1, loop
      PRTINFO1("=======")
      PRTINFO2("Loop ", loop)

      ! ---------------------------
      ! init IO
      ! ---------------------------
      PRTINFO1("")
      PRTINFO1("Init IO")
      PRTINFO1("-------")
      call init_io(ierr); CHKERRQ0 (ierr)

      ! ---------------------------
      ! write test file
      ! ---------------------------
      PRTINFO1("")
      PRTINFO1("Write Testfile")
      PRTINFO1("--------------")
      wtimer = -MPI_wtime()
      call write_file(trim(filename), ierr); CHKERRQ0(ierr)
      call MPI_Barrier(mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      wtimer = wtimer +MPI_wtime()

      ! ---------------------------
      ! read test file
      ! ---------------------------
      PRTINFO1("")
      PRTINFO1("Read Testfile")
      PRTINFO1("-------------")
      wtimer = -MPI_wtime()
      call read_file(trim(filename), ierr); CHKERRQ0(ierr)
      call MPI_Barrier(mpi_mycomm, ierr); CHKERRQ1(ierr, MPI_SUCCESS)
      wtimer = wtimer +MPI_wtime()

      ! ---------------------------
      ! close IO
      ! ---------------------------
      PRTINFO1("")
      PRTINFO1("Close IO")
      PRTINFO1("-------")
      call close_io(ierr); CHKERRQ0 (ierr)

    end do

    ! ---------------------------
    ! clean-up
    ! ---------------------------
    if(on_3d) then
      deallocate(preal4_3d_mem)
      deallocate(preal8_3d_mem)
      deallocate(pint4_3d_mem)
      !deallocate(pint8_3d_mem)
      deallocate(pcmplx4_3d_mem)
      deallocate(pcmplx8_3d_mem)
    end if
    if(on_4d) then
      deallocate(preal4_4d_mem)
      deallocate(preal8_4d_mem)
      deallocate(pint4_4d_mem)
      !deallocate(pint8_4d_mem)
      deallocate(pcmplx4_4d_mem)
      deallocate(pcmplx8_4d_mem)
    end if

    call MPI_FINALIZE (ierr); CHKERRQ1 (ierr, MPI_SUCCESS)

end
