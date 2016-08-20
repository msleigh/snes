MODULE sweep_mod

CONTAINS


 SUBROUTINE sweep( &
   & group)

!    UNIT TYPE:     subroutine
!    UNIT NAME:     sweep
!    MODULE NAME;   sweep_mod
!    FILE NAME:     sweep.f
!     LANGUAGE:        Fortran   90
!    AUTHOR:           Michael Sleigh

!    PURPOSE: Performs sweep over all cells and directions

!    STRUCTURE
!    1. Do sweeps

 USE getkinds_mod
 USE setdata_mod

 IMPLICIT   NONE


 ! Arguments
 INTEGER(KIND=ik), INTENT(IN) ::          group    ! Energy group being solved for

 ! Counters
 INTEGER(KIND=ik) ::      cell
 INTEGER(KIND=ik) ::      dir
 INTEGER(KIND=ik) ::      sweepdir
 INTEGER(KIND=ik) ::      begincell
 INTEGER(KIND=ik) ::      endcell
 INTEGER(KIND=ik) ::      stepcell

 ! Fluxes
 REAL(KIND=rk) ::                            angflux
 REAL(KIND=rk) ::                            angfluxout
 REAL(KIND=rk),DIMENSION(numdirs) ::         angfluxin

 ! Constants
 REAL(KIND=rk) :: a
 REAL(KIND=rk) :: d
REAL(KIND=rk) ::   s

REAL(KIND=rk) ::   ds
REAL(KIND=rk) ::   dds
REAL(KIND=rk) ::   ul
REAL(KIND=rk) ::   u2
REAL(KIND=rk) ::   u3
REAL(KIND=rk) ::   denom
REAL(KIND=rk) ::   qi
REAL(KIND=rk) ::   qo



!  1. Do sweeps



! Loop over all sweeps (there are 2 sweeps in a ID system)
DO sweepdir = 1,2

  ! Sweep 1 is right-to-left, sweep 2 is left-to-right
  IF (sweepdir .EQ. 1) THEN
    begincell = numcells
    endcell = 1
    stepcell = -1
    angfluxin(:) = rh_eflux
  ELSE
    begincell = 1
    endcell = numcells
    stepcell = 1
    IF (lhbc == 1) angfluxin(:) = lh_eflux
  ENDIF


  ! Loop over all cells
  DO cell = begincell,endcell,stepcell

    ! Determine constants
    d = width(cell)
    s = sigma_t(cell,group)
    ds = d*s
    dds = d*ds
    IF (sweepdir == 1) THEN
      qi = source_t(cell,2_ik)
      qo = source_t(cell,1_ik)
    ELSE
      qi = source_t(cell,1_ik)
      qo = source_t(cell,2_ik)
    ENDIF
    ! Loop over all positive direction cosines in quadrature set
    DO dir = 1,numdirs

        ! Determine constants
        ul = mu(dir)
        u2 = 2_ik*mu(dir)
        u3 = 3_ik*mu(dir)
        denom = 1.0_rk/(u2*u3 + 4_ik*ul*ds + ds*ds) ! Guessing that ds^ds should be ds*ds

        ! Calculate angular fluxes
        angfluxout       = u2*(u3 + 2_ik*ds)*angfluxin (dir) + d*ul*(qi - qo) + dds*qi
        angfluxout       = angfluxout*denom
        angfluxin(dir) = u2*(u3 - ds)*angfluxin(dir) + d*u3*(qi + qo) + dds*qo ! Guess ^ shuld be *
        angfluxin(dir) = angfluxin(dir)*denom
        angflux         = 0.5_rk*(angfluxout + angfluxin (dir))

        ! Fix up negative fluxes
        IF (nffu .AND. (angfluxout < 0.0_rk)) THEN
          nffu_call = nffu_call + 1_ik
          angfluxout = 0.0_rk
        ENDIF


        ! Add ang fluxes to scalar fluxes
        scalflux(cell,group,0_ik) = scalflux(cell,group,0_ik) + wgt(dir)*angflux
        IF (sweepdir == 1_ik) THEN
          scalflux(cell,group,1_ik) =  scalflux(cell,group,1_ik) + wgt(dir)*angfluxin(dir)
          scalflux(cell,group,2_ik) =  scalflux(cell,group,2_ik) + wgt(dir)*angfluxout
        ELSE
          scalflux(cell,group,1_ik) =    scalflux(cell,group,1_ik) + wgt(dir)*angfluxout
          scalflux(cell,group,2_ik) =    scalflux(cell,group,2_ik) + wgt(dir)*angfluxin(dir)
        ENDIF


   ENDDO


  ENDDO


ENDDO


! Normalise weighted sum of angular fluxes by sum of weights
a = 1_ik/(2_ik*sum(wgt)) ! Check????
scalflux(:,group,0_ik) = scalflux(:,group,0_ik) * a

scalflux(:,group,1_ik) = scalflux(:,group,1_ik) * a
scalflux(:,group,2_ik) = scalflux(:,group,2_ik) * a
  RETURN
 END SUBROUTINE sweep
END MODULE sweep_mod
