!> \author msleigh
!!
!! PURPOSE: Main controlling unit for SNES (Simple Neutron Transport Equation
!!          Solver)
!!
!! STRUCTURE
!! 1. Initialise variables
!! 2. Get input data and allocate storage
!! 3. Set up mesh and quadrature set
!! 4. Solve neutron transport equation
!! 5. Write output
!! 6. Deallocate storage

PROGRAM main

  USE allocstor_mod
  USE deallocstor_mod
  USE getkinds_mod
  USE initmesh_mod
  USE iterate_mod
  USE printflux1_mod
  USE printflux2_mod
  USE quadsets_mod
  USE readkeys_mod
  USE readline_mod, only: field
  USE readmats_mod
  USE readsrcs_mod
  USE setdata_mod

  IMPLICIT NONE

  CHARACTER(LEN=4), PARAMETER :: unitname = 'MAIN'
  REAL(KIND=rk),    PARAMETER :: version = 1.1

  CHARACTER(LEN=8)  :: datestr
  CHARACTER(LEN=10) :: timestr
  INTEGER(KIND=ik)  :: startcount
  INTEGER(KIND=ik)  :: endcount
  INTEGER(KIND=ik)  :: countrate
  INTEGER(KIND=ik)  :: countmax
  REAL(KIND=rk)     :: duration

  INTEGER(KIND=ik)   :: errstat
  CHARACTER(LEN=256) :: keywordfile
  CHARACTER(LEN=256) :: materialfile

  !----------------------------------------------------------------------------
  ! Timing
  !----------------------------------------------------------------------------

  CALL DATE_AND_TIME(datestr, timestr)
  WRITE(*,*)
  WRITE(*,*)
  WRITE(*,'(A53)')      '                  ####     #    #    ######     #### '
  WRITE(*,'(A53)')      '                 #         ##   #    #         #     '
  WRITE(*,'(A53)')      '                  ####     # #  #    #####      #### '
  WRITE(*,'(A53)')      '                      #    #  # #    #              #'
  WRITE(*,'(A53)')      '                 #    #    #   ##    #         #    #'
  WRITE(*,'(A53)')      '                  ####     #    #    ######     #### '
  WRITE(*,*)
  WRITE(*,*)
  WRITE(*,'(A37,F4.1)') '                         Version :',version
  WRITE(*,'(A52)')      '                          Author : msleigh'
  WRITE(*,'(A38,2(A2,A1),A6)') '                      Start Time : ', &
    timestr(1:2), ':', timestr(3:4), ':', timestr(5:10)
  WRITE(*,'(A38,2(A2,A1),A4)') '                      Start Date : ', &
    datestr(7:8), '/', datestr(5:6), '/', datestr(1:4)
  WRITE(*,*)
  WRITE(*,*)

  CALL SYSTEM_CLOCK(startcount)

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  errstat = 0_ik
  nffu_call = 0_ik

  !----------------------------------------------------------------------------
  ! 2. Get input data and allocate storage
  !----------------------------------------------------------------------------

  keywordfile = 'snes.in'

  CALL readkeys( &
    & keywordfile, &
    & errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Non-zero error code'
    WRITE(*,*) unitname, ': Aborting...'
    WRITE(*,*)
    STOP
  ENDIF

  CALL allocstor( &
    & errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Non-zero error code'
    WRITE(*,*) unitname, ': Aborting...'
    WRITE(*,*)
    STOP
  ENDIF

  materialfile = 'snes.in'

  CALL readmats( &
    & materialfile, &
    & errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Non-zero error code'
    WRITE(*,*) unitname, ': Aborting...'
    WRITE(*,*)
    STOP
  ENDIF

  IF (calctype == 2_ik) THEN
    CALL readsrcs( &
      & errstat)
    IF (errstat /= 0_ik) THEN
      WRITE(*,*) unitname, ': Non-zero error code'
      WRITE(*,*) unitname, ': Aborting...'
      WRITE(*,*)
      STOP
    ENDIF
  ENDIF

  !----------------------------------------------------------------------------
  ! 3. Set up spatial mesh
  !----------------------------------------------------------------------------

  CALL initmesh( &
    & errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Non-zero error code'
    WRITE(*,*) unitname, ': Aborting,..'
    WRITE(*,*)
    STOP
  ENDIF

  !----------------------------------------------------------------------------
  ! 4. Set up angular mesh
  !----------------------------------------------------------------------------

  CALL quadsets( &
    & errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Non-zero error code'
    WRITE(*,*) unitname, ': Aborting...'
    WRITE(*,*)
    STOP
  ENDIF

  !----------------------------------------------------------------------------
  ! 5. Solve neutron transport equation
  !----------------------------------------------------------------------------

  CALL iterate

  !----------------------------------------------------------------------------
  ! 6. Write output
  !----------------------------------------------------------------------------

  IF (printflux == 1_ik .OR. printflux == 3_ik) THEN
    CALL printflux1( &
      & errstat)
    IF (errstat /= 0_ik) THEN
      WRITE(*,*) unitname, ': Non-zero error code'
      WRITE(*,*) unitname, ': Aborting...'
      WRITE(*,*)
      STOP
    ENDIF
  ENDIF
  
  IF (printflux > 1_ik) THEN
    CALL printflux2( &
      & errstat)
    IF (errstat /= 0_ik) THEN
      WRITE(*,*) unitname, ': Non-zero error code'
      WRITE(*,*) unitname, ': Aborting...'
      WRITE(*,*)
      STOP
    ENDIF
  ENDIF

  WRITE(*,'(A14,I12)') 'NFFU calls:   ', nffu_call
  WRITE(*,*)

  !----------------------------------------------------------------------------
  ! Deallocate storage
  !----------------------------------------------------------------------------

  CALL deallocstor( &
    & errstat)

  IF (ALLOCATED(mats))            errstat = -1_ik
  IF (ALLOCATED(firstcell_mat))   errstat = -1_ik
  IF (ALLOCATED(lastcell_mat))    errstat = -1_ik
  IF (ALLOCATED(firstcell_src))   errstat = -1_ik
  IF (ALLOCATED(lastcell_src))    errstat = -1_ik
  IF (ALLOCATED(value_src))       errstat = -1_ik
  IF (ALLOCATED(numcellsinreg))   errstat = -1_ik
  IF (ALLOCATED(widthcellsinreg)) errstat = -1_ik
  IF (ALLOCATED(centre))          errstat = -1_ik
  IF (ALLOCATED(origin))          errstat = -1_ik
  IF (ALLOCATED(width))           errstat = -1_ik
  IF (ALLOCATED(matnum))          errstat = -1_ik
  IF (ALLOCATED(sigma_s))         errstat = -1_ik
  IF (ALLOCATED(sigma_f))         errstat = -1_ik
  IF (ALLOCATED(sigma_t))         errstat = -1_ik
  IF (ALLOCATED(scalflux))        errstat = -1_ik
  IF (ALLOCATED(scalflux_inner))  errstat = -1_ik
  IF (ALLOCATED(scalflux_outer))  errstat = -1_ik
  IF (ALLOCATED(source_f))        errstat = -1_ik
  IF (ALLOCATED(source_g))        errstat = -1_ik
  IF (ALLOCATED(source_i))        errstat = -1_ik
  IF (ALLOCATED(source_s))        errstat = -1_ik
  IF (ALLOCATED(source_t))        errstat = -1_ik
  IF (ALLOCATED(mu))              errstat = -1_ik
  IF (ALLOCATED(wgt))             errstat = -1_ik
  IF (ALLOCATED(field))           errstat = -1_ik

  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Non-zero error code'
    WRITE(*,*) unitname, ': Aborting...'
    WRITE(*,*)
    STOP
  ENDIF

  !----------------------------------------------------------------------------
  ! Timing
  !----------------------------------------------------------------------------

  CALL SYSTEM_CLOCK(endcount,countrate,countmax)
  IF (endcount > startcount) THEN
    duration = (endcount-startcount)/REAL(countrate)
  ELSE
    duration = (endcount-startcount+countmax)/REAL(countrate)
  ENDIF
  WRITE(*,'(A14,F12.3)') 'Duration:      ', duration
  WRITE(*,*)
  WRITE(*,*)
  WRITE(*,*)

  CALL DATE_AND_TIME(datestr,timestr)
  WRITE(*,'(A38,2(A2,A1),A6)') '                            End Time : ', &
    timestr(1:2), ':', timestr(3:4), ':', timestr(5:10)
  WRITE(*,'(A38,2(A2,A1),A4)') '                            End Date : ', &
    datestr(7:8), '/', datestr(5:6), '/', datestr(1:4)
  WRITE(*,*)

END PROGRAM main
