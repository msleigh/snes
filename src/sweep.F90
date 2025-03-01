!! author: msleigh
!! date: 2002
!!
!! Performs sweep over all cells and directions

MODULE sweep_mod

PRIVATE
PUBLIC :: sweep

CONTAINS

  SUBROUTINE sweep( &
    & group)
    !! AUTHOR: msleigh
    !!
    !! Performs sweep over all cells and directions

  USE getkinds_mod
  USE setdata_mod

  IMPLICIT NONE

  ! Arguments
  INTEGER(KIND=ik), INTENT(IN) :: group !! Energy group being solved for

  ! Counters
  INTEGER(KIND=ik) :: cell
  INTEGER(KIND=ik) :: dir
  INTEGER(KIND=ik) :: sweepdir
  INTEGER(KIND=ik) :: begincell
  INTEGER(KIND=ik) :: endcell
  INTEGER(KIND=ik) :: stepcell

  ! Fluxes
  REAL(KIND=rk)                     :: angflux
  REAL(KIND=rk)                     :: angfluxout
  REAL(KIND=rk), DIMENSION(numdirs) :: angfluxin

  ! Constants
  REAL(KIND=rk) :: dx    ! Cell width
  REAL(KIND=rk) :: xs    ! Total macro X-S
  REAL(KIND=rk) :: u1    ! Mu
  REAL(KIND=rk) :: w1    ! Wgt
  REAL(KIND=rk) :: denom
#ifdef CODETYPE
  REAL(KIND=rk) :: c1
#else
  REAL(KIND=rk) :: ds
  REAL(KIND=rk) :: dd
  REAL(KIND=rk) :: u2
  REAL(KIND=rk) :: u3
  REAL(KIND=rk) :: qi
  REAL(KIND=rk) :: qo
#endif

  !----------------------------------------------------------------------------
  ! 1. Do sweeps
  !----------------------------------------------------------------------------

  ! Loop over all sweeps (there are 2 sweeps in a 1D system)
  DO sweepdir = 1_ik, numsweeps

    ! Sweep 1 is right-to-left, sweep 2 is left-to-right
    IF (sweepdir == 1_ik) THEN
      begincell = numcells
      endcell = 1_ik
      stepcell = -1_ik
      angfluxin(:) = rh_eflux
    ELSE
      begincell = 1_ik
      endcell = numcells
      stepcell = 1_ik
      IF (lhbc == 1_ik) angfluxin(:) = lh_eflux
    ENDIF

    ! Loop over all cells
    DO cell = begincell, endcell, stepcell

      ! Determine constants
      dx = width(cell)
      xs = sigma_t(cell,group)
#ifndef CODETYPE
      ds = dx*xs
      dd = dx*ds
      IF (sweepdir == 1_ik) THEN
        qi = source_t(cell,2_ik)
        qo = source_t(cell,1_ik)
      ELSE
        qi = source_t(cell,1_ik)
        qo = source_t(cell,2_ik)
      ENDIF
#endif

      ! Loop over all positive direction cosines in quadrature set
      DO dir = 1_ik, numdirs

        ! Determine constants
        u1 = mu(dir)
        w1 = wgt(dir)
#ifdef CODETYPE
        c1 = dx/(2.0_rk*u1)
        denom = 1.0_rk + c1*xs
#else
        u2 = 2_ik*mu(dir)
        u3 = 3_ik*mu(dir)
        denom = 1.0_rk/(u2*u3 + 4_ik*u1*ds + ds*ds)
#endif

        ! Calculate angular fluxes
#ifdef CODETYPE
        angflux = (angfluxin(dir) + c1*source_t(cell,1_ik))/denom
        angfluxout = 2.0_rk*angflux - angfluxin(dir)
#else
        angfluxout     = u2*(u3 + 2_ik*ds)*angfluxin(dir) + dx*u1*(qi-qo) + dd*qi
        angfluxout     = angfluxout*denom
        angfluxin(dir) = u2*(u3 - ds)*angfluxin(dir) + dx*u3*(qi + qo) + dd*qo
        angfluxin(dir) = angfluxin(dir)*denom
        angflux        = 0.5_rk*(angfluxout + angfluxin(dir))
#endif

        ! Fix up negative fluxes
        IF (nffu .AND. (angfluxout < 0.0_rk)) THEN
          nffu_call = nffu_call + 1_ik
          angfluxout = 0.0_rk
#ifdef CODETYPE
          angflux = dx*(source_t(cell,1_ik)) + 2_ik*u1*angfluxin(dir)
          angflux = angflux/(2_ik*u1 + xs*dx)
#endif
        ENDIF

        ! Add ang fluxes to scalar fluxes
        scalflux(cell,group,0_ik) = scalflux(cell,group,0_ik) + w1*angflux
#ifdef CODETYPE
        IF (printflux > 1_ik) THEN
          IF (sweepdir == 1_ik) THEN
            scalflux(cell,group,1_ik) = scalflux(cell,group,1_ik) + w1*angfluxout
            scalflux(cell,group,2_ik) = scalflux(cell,group,2_ik) + w1*angfluxin(dir)
          ELSE
            scalflux(cell,group,1_ik) = scalflux(cell,group,1_ik) + w1*angfluxin(dir)
            scalflux(cell,group,2_ik) = scalflux(cell,group,2_ik) + w1*angfluxout
          ENDIF
        ENDIF
#else
        IF (sweepdir == 1_ik) THEN
          scalflux(cell,group,1_ik) = scalflux(cell,group,1_ik) + w1*angfluxin(dir)
          scalflux(cell,group,2_ik) = scalflux(cell,group,2_ik) + w1*angfluxout
        ELSE
          scalflux(cell,group,1_ik) = scalflux(cell,group,1_ik) + w1*angfluxout
          scalflux(cell,group,2_ik) = scalflux(cell,group,2_ik) + w1*angfluxin(dir)
        ENDIF
#endif

#ifdef CODETYPE
        angfluxin(dir) = angfluxout
#endif

      ENDDO ! Loop over directions

    ENDDO ! Loop over cells

  ENDDO ! Loop over sweeps

  RETURN
  END SUBROUTINE sweep

END MODULE sweep_mod
