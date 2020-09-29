MODULE sweep_mod

PRIVATE
PUBLIC :: sweep

CONTAINS

  !> \author msleigh
  !!
  !! PURPOSE: Performs sweep over all cells and directions
  !!
  !! STRUCTURE
  !! 1. Do sweeps

  SUBROUTINE sweep( &
    & group)

  USE getkinds_mod
  USE setdata_mod

  IMPLICIT NONE

  ! Arguments
  INTEGER(KIND=ik), INTENT(IN) :: group !< Energy group being solved for

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
  REAL(KIND=rk) :: qt    ! Total source
  REAL(KIND=rk) :: u1    ! Mu
  REAL(KIND=rk) :: w1    ! Wgt
  REAL(KIND=rk) :: c1
  REAL(KIND=rk) :: denom

  !----------------------------------------------------------------------------
  ! 1. Do sweeps
  !----------------------------------------------------------------------------

  ! Loop over all sweeps (there are 2 sweeps in a 1D system)
  DO sweepdir = 1_ik, numsweeps

    ! Sweep 1 is right-to-1eft, sweep 2 is left-to-right
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
      qt = source_t(cell,1_ik)

      ! Loop over all positive direction cosines in quadrature set
      DO dir = 1_ik, numdirs

        ! Determine constants
        u1 = mu(dir)
        w1 = wgt(dir)
        c1 = dx/(2.0_rk*u1)
        denom = 1.0_rk + c1*xs

        ! Calculate angular fluxes
        angflux = (angfluxin(dir) + c1*qt)/denom
        angfluxout = 2.0_rk*angflux - angfluxin(dir)

        ! Fix up negative fluxes
        IF (nffu .AND. (angfluxout < 0.0_rk)) THEN
          nffu_call = nffu_call + 1_ik
          angfluxout = 0.0_rk
          angflux = dx*qt + 2_ik*u1*angfluxin(dir)
          angflux = angflux/(2_ik*u1 + xs*dx)
        ENDIF

        ! Add ang fluxes to scalar fluxes
        scalflux(cell,group,0_ik) = scalflux(cell,group,0_ik) + w1*angflux
        IF (printflux > 1_ik) THEN
          IF (sweepdir == 1_ik) THEN                  ! RIGHT-TO-LEFT
            scalflux(cell,group,1_ik) = scalflux(cell,group,1_ik) &
                                & + w1*angfluxout
            scalflux(cell,group,2_ik) = scalflux(cell,group,2_ik) &
                                & + w1*angfluxin(dir)
          ELSE                                        ! LEFT-TO-RIGHT
            scalflux(cell,group,1_ik) = scalflux(cell,group,1_ik) &
                                & + w1*angfluxin(dir)
            scalflux(cell,group,2_ik) = scalflux(cell,group,2_ik) &
                                & + w1*angfluxout
          ENDIF
        ENDIF

        angfluxin(dir) = angfluxout

      ENDDO ! Loop over directions

    ENDDO   ! Loop over cells

  ENDDO     ! Loop over sweeps

  RETURN
  END SUBROUTINE sweep

END MODULE sweep_mod