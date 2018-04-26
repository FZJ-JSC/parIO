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
!> @file testio_data.F90
!! @brief stores testIO-specific global data
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#include "testio_defines.inc"

module testio_data
#ifdef USE_MPI_MODULE
    use mpi
    implicit none
#else
    implicit none
    include 'mpif.h'
#endif

    save

    integer :: loop = 1
    logical :: on_3d = .true.
    logical :: on_4d = .true.
    logical :: test_hysl2dset = .true.
    logical :: test_hysl2hysl = .true.

    ! info stuff
    integer :: verbose_mode = 1
    character(len=1024) :: filename = 'testIO.h5'

    ! MPI stuff
    integer :: mpi_mycomm
    integer :: mpi_proc_id
    integer :: mpi_nproc
    integer :: mpi_rootid = 0

    ! I/O data - test values
    !!!!!!!!!!!!!!!!!!!!!!!!

    ! parIO settings
    integer, parameter :: USE_MPIIO    = 1
    integer, parameter :: USE_POSIX    = 2
    integer, parameter :: USE_FAMILY   = 3
    integer, parameter :: USE_MPIPOSIX = 4
    integer :: ioAPI = USE_MPIIO

    integer :: chunking = 0
    integer :: chunk_2d(2) = (/0,0/)
    integer :: chunk_3d(3) = (/0,0,0/)
    integer :: chunk_4d(4) = (/0,0,0,0/)

    ! global values
    integer, parameter :: g1=3, g2=6, g3=9
    integer :: gdims_1d(1)=(/g1/)
    integer :: gdims_2d(2)=(/g1,g2/)
    integer :: gdims_3d(3)=(/g1,g2,g3/)

    real(kind=4) :: greal4_0d, greal4_1d(g1), greal4_2d(g1,g2), greal4_3d(g1,g2,g3)
    real(kind=8) :: greal8_0d, greal8_1d(g1), greal8_2d(g1,g2), greal8_3d(g1,g2,g3)
    integer(kind=4) :: gint4_0d, gint4_1d(g1), gint4_2d(g1,g2), gint4_3d(g1,g2,g3)
    !integer(kind=8) :: gint8_0d, gint8_1d(g1), gint8_2d(g1,g2), gint8_3d(g1,g2,g3)
    complex(kind=4) :: gcmplx4_0d, gcmplx4_1d(g1), gcmplx4_2d(g1,g2), gcmplx4_3d(g1,g2,g3)
    complex(kind=8) :: gcmplx8_0d, gcmplx8_1d(g1), gcmplx8_2d(g1,g2), gcmplx8_3d(g1,g2,g3)

    ! processor spezific values
    integer :: domain_dims_1d(1)
    integer :: domain_dims_2d(2)
    integer :: domain_dims_3d(3)
    integer :: domain_dims_4d(4)

    integer :: cdomain_dims_1d(1)
    integer :: cdomain_dims_2d(2)
    integer :: cdomain_dims_3d(3)
    integer :: cdomain_dims_4d(4)

    integer :: poffs_1d(1)
    integer :: poffs_2d(2)
    integer :: poffs_3d(3)
    integer :: poffs_4d(4)

    integer :: p1=30, p2=60, p3=90, v4=4
    integer :: pdims_1d(1)
    integer :: pdims_2d(2)
    integer :: pdims_3d(3)
    integer :: pdims_4d(4)

    integer :: msize_1d(1)
    integer :: msize_2d(2)
    integer :: msize_3d(3)
    integer :: msize_4d(4)

    integer :: moffs_1d(1)
    integer :: moffs_2d(2)
    integer :: moffs_3d(3)
    integer :: moffs_4d(4)

    ! 3d
    real(kind=4), allocatable, target :: preal4_3d_mem(:,:,:)
    real(kind=8), allocatable, target :: preal8_3d_mem(:,:,:)
    integer(kind=4), allocatable, target :: pint4_3d_mem(:,:,:)
    !integer(kind=8), allocatable, target :: pint8_3d_mem(:,:,:)
    complex(kind=4), allocatable, target :: pcmplx4_3d_mem(:,:,:)
    complex(kind=8), allocatable, target :: pcmplx8_3d_mem(:,:,:)

    real(kind=4), pointer :: preal4_3d_ptr(:,:,:)
    real(kind=8), pointer :: preal8_3d_ptr(:,:,:)
    integer(kind=4), pointer :: pint4_3d_ptr(:,:,:)
    !integer(kind=8), pointer :: pint8_3d_ptr(:,:,:)
    complex(kind=4), pointer :: pcmplx4_3d_ptr(:,:,:)
    complex(kind=8), pointer :: pcmplx8_3d_ptr(:,:,:)

    ! 4d
    real(kind=4), allocatable, target :: preal4_4d_mem(:,:,:,:)
    real(kind=8), allocatable, target :: preal8_4d_mem(:,:,:,:)
    integer(kind=4), allocatable, target :: pint4_4d_mem(:,:,:,:)
    !integer(kind=8), allocatable, target :: pint8_4d_mem(:,:,:,:)
    complex(kind=4), allocatable, target :: pcmplx4_4d_mem(:,:,:,:)
    complex(kind=8), allocatable, target :: pcmplx8_4d_mem(:,:,:,:)

    real(kind=4), pointer :: preal4_4d_ptr(:,:,:,:)
    real(kind=8), pointer :: preal8_4d_ptr(:,:,:,:)
    integer(kind=4), pointer :: pint4_4d_ptr(:,:,:,:)
    !integer(kind=8), pointer :: pint8_4d_ptr(:,:,:,:)
    complex(kind=4), pointer :: pcmplx4_4d_ptr(:,:,:,:)
    complex(kind=8), pointer :: pcmplx8_4d_ptr(:,:,:,:)

    contains

     !
     !========================================
     !
     !  init domain
     !
     !========================================
     !
    subroutine init_domain()
      implicit none

      domain_dims_1d = (/mpi_nproc*p1/)
      domain_dims_2d = (/mpi_nproc*p1,p2/)
      domain_dims_3d = (/mpi_nproc*p1,p2,p3/)
      domain_dims_4d = (/mpi_nproc*p1,p2,p3,v4/)

      ! domain size for complex values written as real
      cdomain_dims_1d = domain_dims_1d; cdomain_dims_1d(1) = cdomain_dims_1d(1)*2
      cdomain_dims_2d = domain_dims_2d; cdomain_dims_2d(1) = cdomain_dims_2d(1)*2
      cdomain_dims_3d = domain_dims_3d; cdomain_dims_3d(1) = cdomain_dims_3d(1)*2
      cdomain_dims_4d = domain_dims_4d; cdomain_dims_4d(1) = cdomain_dims_4d(1)*2

      poffs_1d = (/mpi_proc_id * p1/)
      poffs_2d = (/mpi_proc_id * p1, 0/)
      poffs_3d = (/mpi_proc_id * p1, 0, 0/)
      poffs_4d = (/mpi_proc_id * p1, 0, 0, 0/)

      pdims_1d = (/p1/)
      pdims_2d = (/p1,p2/)
      pdims_3d = (/p1,p2,p3/)
      pdims_4d = (/p1,p2,p3,v4/)

      msize_1d = (/p1*0.5/)
      msize_2d = (/p1*0.5,p2*0.5/)
      msize_3d = (/p1*0.5,p2*0.5,p3*0.5/)
      msize_4d = (/p1*0.5,p2*0.5,p3*0.5,v4*0.5/)

      moffs_1d = msize_1d*0.5
      moffs_2d = msize_2d*0.5
      moffs_3d = msize_3d*0.5
      moffs_4d = msize_4d*0.5

      if(chunking > 0) then
        chunk_2d = (/p1,p2/)
        chunk_3d = (/p1,p2,p3/)
        chunk_4d = (/p1,p2,p3,v4/)
        ! for forcing a HDF5-chunking bug with more than 65535 chunks,
        ! set mpiprocs=32 p1=16 p2=512 p3=512 and chunk_3d = (/2,2,p3/)
      end if

      if(mpi_proc_id == mpi_rootid) then
        write(*,*) '   p1,p2,p3,v1 : ', p1,p2,p3,v4
        write(*,*)
        write(*,*) '   domain 1d   : ', domain_dims_1d
        write(*,*) '   domain 2d   : ', domain_dims_2d
        write(*,*) '   domain 3d   : ', domain_dims_3d
        write(*,*) '   domain 4d   : ', domain_dims_4d
        write(*,*)
        write(*,*) '   proc-dims 1d: ', pdims_1d
        write(*,*) '   proc-dims 2d: ', pdims_2d
        write(*,*) '   proc-dims 3d: ', pdims_3d
        write(*,*) '   proc-dims 4d: ', pdims_4d
        if(chunking > 0) then
          write(*,*)
          write(*,*) '   chunk 2d    : ', chunk_2d
          write(*,*) '   chunk 3d    : ', chunk_3d
          write(*,*) '   chunk 4d    : ', chunk_4d
        end if
        if(test_hysl2hysl) then
          write(*,*)
          write(*,*) '   memory hysl 1d : ', msize_1d
          write(*,*) '   memory hysl 2d : ', msize_2d
          write(*,*) '   memory hysl 3d : ', msize_3d
          write(*,*) '   memory hysl 4d : ', msize_4d
          write(*,*)
          write(*,*) '   memory offs 1d : ', moffs_1d
          write(*,*) '   memory offs 2d : ', moffs_2d
          write(*,*) '   memory offs 3d : ', moffs_3d
          write(*,*) '   memory offs 4d : ', moffs_4d
        end if
      end if

    end subroutine init_domain

end module
