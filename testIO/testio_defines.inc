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
!> @file testio_defines.inc
!! @brief file for defines
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

! info
#define PRTINFO1(infotxt1)                   if(mpi_proc_id == 0) then; write(*,*) "  ",infotxt1; end if
#define PRTINFO2(infotxt1,infotxt2)          if(mpi_proc_id == 0) then; write(*,*) "  ",infotxt1,infotxt2; end if
#define PRTINFO3(infotxt1,infotxt2,infotxt3) if(mpi_proc_id == 0) then; write(*,*) "  ",infotxt1,infotxt2,infotxt3; end if

! verbose info
#define PRTVERBOSE1(infotxt1,verb)                   if(mpi_proc_id == 0 .and. verb <= verbose_mode) then; write(*,*) "  ",infotxt1; end if
#define PRTVERBOSE2(infotxt1,infotxt2,verb)          if(mpi_proc_id == 0 .and. verb <= verbose_mode) then; write(*,*) "  ",infotxt1,infotxt2; end if
#define PRTVERBOSE3(infotxt1,infotxt2,infotxt3,verb) if(mpi_proc_id == 0 .and. verb <= verbose_mode) then; write(*,*) "  ",infotxt1,infotxt2,infotxt3; end if

! warnings
#define PRTWARN1(warntxt)                if(.true.)           then; write(*,fmt='(a,a,a,i5,a,a)')       "WARNING: file="    , __FILE__, ", line=", __LINE__, ", txt=",#warntxt; end if
#define CHKWARN0(warnval)                if(warnval /= 0)     then; write(*,fmt='(a,a,a,i5)')           "WARNING: file="    , __FILE__, ", line=", __LINE__; end if
#define CHKWARN1(warnval,okval)          if(warnval /= okval) then; write(*,fmt='(a,a,a,i5,a,i5)')      "WARNING: file="    , __FILE__, ", line=", __LINE__, ", warn=", warnval; end if
#define CHKWARN2(warnval,okval,warnflag) if(warnval /= okval) then; write(*,fmt='(a,a,a,i5,a,i5,a,i5)') "WARNING: file="    , __FILE__, ", line=", __LINE__, ", warn=", warnval, ",flag=", warnflag; end if
#define CHKWARNLW(warnval,lwval)         if(warnval <  lwval) then; write(*,fmt='(a,a,a,i5,a,i5)')      "WARNING: file="    , __FILE__, ", line=", __LINE__, ", warn=", warnval; end if

! errors
#define PRTERR1(errtxt)                  if(.true.)           then; write(*,fmt='(a,a,a,i5,a,a)')       "ERROR: file="      , __FILE__, ", line=", __LINE__, ", txt=",#errtxt; stop; end if
#define CHKERRQ0(errval)                 if(errval /= 0)      then; write(*,fmt='(a,a,a,i5)')           "ERROR: file="      , __FILE__, ", line=", __LINE__; stop; end if
#define CHKERRQ1(errval,okval)           if(errval /= okval)  then; write(*,fmt='(a,a,a,i5,a,i5)')      "ERROR: file="      , __FILE__, ", line=", __LINE__, ", err=", errval; stop; end if
#define CHKERRQ2(errval,okval,errflag)   if(errval /= okval)  then; write(*,fmt='(a,a,a,i5,a,i5,a,i5)') "ERROR: file="      , __FILE__, ", line=", __LINE__, ", err=", errval, ",flag=", errflag; stop; end if
#define CHKERRLW(errval,lwval)           if(errval <  lwval)  then; write(*,fmt='(a,a,a,i5,a,i5)')      "ERROR: file="      , __FILE__, ", line=", __LINE__, ", err=", errval; stop; end if

! debug
#ifdef USE_DEBMODE
#define PRTFNC(fnctxt)                   if(mpi_proc_id == 0 .and. 2 <= verbose_mode) then; write(*,*) "  enter function: ",#fnctxt; end if
#else
#define PRTFNC(fnctxt)
#endif
