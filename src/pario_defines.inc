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
!> @file pario_defines.inc
!! @brief file for defines
!! @details
!! @author Jens Henrik Goebbert
!!
!! @}
!========================================

#define _MYTYPESIZE_ 8

! info
#define PRTINFO1(infotxt1)                   if(mpi_proc_id == 0) then; write(*,*) "  ",infotxt1; end if
#define PRTINFO2(infotxt1,infotxt2)          if(mpi_proc_id == 0) then; write(*,*) "  ",infotxt1,infotxt2; end if
#define PRTINFO3(infotxt1,infotxt2,infotxt3) if(mpi_proc_id == 0) then; write(*,*) "  ",infotxt1,infotxt2,infotxt3; end if

! verbose info
#define PRTVERBOSE1(infotxt1,verb)                   if(mpi_proc_id == 0 .and. verb <= verbose_mode) then; write(*,*) "  ",infotxt1; end if
#define PRTVERBOSE2(infotxt1,infotxt2,verb)          if(mpi_proc_id == 0 .and. verb <= verbose_mode) then; write(*,*) "  ",infotxt1,infotxt2; end if
#define PRTVERBOSE3(infotxt1,infotxt2,infotxt3,verb) if(mpi_proc_id == 0 .and. verb <= verbose_mode) then; write(*,*) "  ",infotxt1,infotxt2,infotxt3; end if
#define PRTVERBOSE4(infotxt1,infotxt2,infotxt3,infotxt4,verb) if(mpi_proc_id == 0 .and. verb <= verbose_mode) then; write(*,*) "  ",infotxt1,infotxt2,infotxt3,infotxt4; end if
#define PRTVERBOSE5(infotxt1,infotxt2,infotxt3,infotxt4,infotxt5,verb) if(mpi_proc_id == 0 .and. verb <= verbose_mode) then; write(*,*) "  ",infotxt1,infotxt2,infotxt3,infotxt4,infotxt5; end if

! warnings
#define PRTWARN1(warntxt)                if(.true.)           then; write(*,fmt='(a,a,a,i5,a,a)')       "WARNING: file="    , __FILE__, ", line=", __LINE__, ", txt=",  warntxt ; end if
#define CHKWARN0(warnval)                if(warnval /= 0)     then; write(*,fmt='(a,a,a,i5)')           "WARNING: file="    , __FILE__, ", line=", __LINE__; end if
#define CHKWARN1(warnval,okval)          if(warnval /= okval) then; write(*,fmt='(a,a,a,i5,a,i5)')      "WARNING: file="    , __FILE__, ", line=", __LINE__, ", warn=", warnval ; end if
#define CHKWARN2(warnval,okval,warnflag) if(warnval /= okval) then; write(*,fmt='(a,a,a,i5,a,i5,a,i5)') "WARNING: file="    , __FILE__, ", line=", __LINE__, ", warn=", warnval , ",flag=", warnflag; end if
#define CHKWARNLW(warnval,lwval)         if(warnval <  lwval) then; write(*,fmt='(a,a,a,i5,a,i5)')      "WARNING: file="    , __FILE__, ", line=", __LINE__, ", warn=", warnval ; end if

! errors
#define PRTERR1(errtxt)                  if(.true.)           then; write(*,fmt='(a,a,a,i5,a,a)')       "ERROR: file="      , __FILE__, ", line=", __LINE__, ", txt=", errtxt ; stop; end if
#define CHKERRQ0(errval)                 if(errval /= 0)      then; write(*,fmt='(a,a,a,i5)')           "ERROR: file="      , __FILE__, ", line=", __LINE__; stop; end if
#define CHKERRQ1(errval,okval)           if(errval /= okval)  then; write(*,fmt='(a,a,a,i5,a,i5)')      "ERROR: file="      , __FILE__, ", line=", __LINE__, ", err=", errval ; stop; end if
#define CHKERRQ2(errval,okval,errflag)   if(errval /= okval)  then; write(*,fmt='(a,a,a,i5,a,i5,a,i5)') "ERROR: file="      , __FILE__, ", line=", __LINE__, ", err=", errval , ",flag=", errflag; stop; end if
#define CHKERRLW(errval,lwval)           if(errval <  lwval)  then; write(*,fmt='(a,a,a,i5,a,i5)')      "ERROR: file="      , __FILE__, ", line=", __LINE__, ", err=", errval ; stop; end if

! debug
#ifdef USE_DEBMODE
#define PRTFNC(fnctxt)                   if(mpi_proc_id == 0) then; write(*,*) "  enter function: ",#fnctxt; end if
#else
#define PRTFNC(fnctxt)
#endif
