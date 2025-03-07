!! author: msleigh
!! date: 2002
!!
!! Deallocates allocatable storage

MODULE deallocstor_mod
  !! Handles memory deallocation for storage

PRIVATE
PUBLIC :: deallocstor

CONTAINS

  SUBROUTINE deallocstor( &
    !! Deallocates all allocated storage
    & errstat)

  USE getkinds_mod, only: ik
  USE setdata_mod, only: mats, firstcell_mat, lastcell_mat, firstcell_src, &
                       & lastcell_src, value_src, numcellsinreg, &
                       & widthcellsinreg, centre, origin, width, matnum, &
                       & sigma_s, sigma_f, sigma_t, scalflux, scalflux_inner, &
                       & scalflux_outer, source_s, source_f, source_g, &
                       & source_i, source_t, mu, wgt

  IMPLICIT NONE

  CHARACTER(LEN=11), PARAMETER :: unitname = 'DEALLOCSTOR'

  ! Arguments
  INTEGER(KIND=ik), INTENT(OUT) :: errstat !! Local error status

  INTEGER(KIND=ik) :: allocation_status

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  errstat = 0_ik

  !----------------------------------------------------------------------------
  ! 2. Deallocate storage
  !----------------------------------------------------------------------------

  IF (ALLOCATED(mats)) DEALLOCATE(mats,STAT=allocation_status)
  IF (ALLOCATED(firstcell_mat)) DEALLOCATE(firstcell_mat, &
      & STAT=allocation_status)
  IF (ALLOCATED(lastcell_mat)) DEALLOCATE(lastcell_mat,STAT=allocation_status)
  IF (ALLOCATED(firstcell_src)) DEALLOCATE(firstcell_src, &
      & STAT=allocation_status)
  IF (ALLOCATED(lastcell_src)) DEALLOCATE(lastcell_src,STAT=allocation_status)
  IF (ALLOCATED(value_src)) DEALLOCATE(value_src,STAT=allocation_status)
  IF (ALLOCATED(numcellsinreg)) DEALLOCATE(numcellsinreg, &
      & STAT=allocation_status)
  IF (ALLOCATED(widthcellsinreg)) DEALLOCATE(widthcellsinreg, &
      & STAT=allocation_status)
  IF (ALLOCATED(centre)) DEALLOCATE(centre,STAT=allocation_status)
  IF (ALLOCATED(origin)) DEALLOCATE(origin,STAT=allocation_status)
  IF (ALLOCATED(width)) DEALLOCATE(width,STAT=allocation_status)
  IF (ALLOCATED(matnum)) DEALLOCATE(matnum,STAT=allocation_status)
  IF (ALLOCATED(sigma_s)) DEALLOCATE(sigma_s,STAT=allocation_status)
  IF (ALLOCATED(sigma_f)) DEALLOCATE(sigma_f,STAT=allocation_status)
  IF (ALLOCATED(sigma_t)) DEALLOCATE(sigma_t,STAT=allocation_status)
  IF (ALLOCATED(scalflux)) DEALLOCATE(scalflux,STAT=allocation_status)
  IF (ALLOCATED(scalflux_inner)) DEALLOCATE(scalflux_inner, &
      & STAT=allocation_status)
  IF (ALLOCATED(scalflux_outer)) DEALLOCATE(scalflux_outer, &
      & STAT=allocation_status)
  IF (ALLOCATED(source_f)) DEALLOCATE(source_f,STAT=allocation_status)
  IF (ALLOCATED(source_g)) DEALLOCATE(source_g,STAT=allocation_status)
  IF (ALLOCATED(source_i)) DEALLOCATE(source_i,STAT=allocation_status)
  IF (ALLOCATED(source_s)) DEALLOCATE(source_s,STAT=allocation_status)
  IF (ALLOCATED(source_t)) DEALLOCATE(source_t,STAT=allocation_status)
  IF (ALLOCATED(mu)) DEALLOCATE(mu,STAT=allocation_status)
  IF (ALLOCATED(wgt)) DEALLOCATE(wgt,STAT=allocation_status)

  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Error', errstat, 'deallocating arrays'
    RETURN
  ENDIF

  RETURN
  END SUBROUTINE deallocstor

END MODULE deallocstor_mod
