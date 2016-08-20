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
  REAL(KIND=rk) :: a
  REAL(KIND=rk) :: d
  REAL(KIND=rk) :: s
  REAL(KIND=rk) :: u1
  REAL(KIND=rk) :: denom
  REAL(KIND=rk) :: q

  !----------------------------------------------------------------------------
  ! 1. Do sweeps
  !----------------------------------------------------------------------------

  ! Loop over all sweeps (there are 2 sweeps in a 1D system)
  DO sweepdir = 1, 2

    ! Sweep 1 is right-to-1eft, sweep 2 is left-to-right
    IF (sweepdir == 1) THEN
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
      q = source_t(cell,1_ik)

      ! Loop over all positive direction cosines in quadrature set
      DO dir = 1, numdirs

        ! Determine constants
        u1 = mu(dir)
        a = d/(2.0_rk*u1)
        denom = 1.0_rk + a*s

        ! Calculate angular fluxes
        angflux = (angfluxin(dir) + a*q)/denom
        angfluxout = 2.0_rk*angflux - angfluxin(dir)

        ! Fix up negative fluxes
        IF (nffu .AND. (angfluxout < 0.0_rk)) THEN
          nffu_call = nffu_call + 1_ik
          angfluxout = 0.0_rk
          angflux = width(cell)*source_t(cell,1_ik) + 2_ik*mu(dir)*angfluxin(dir)
          angflux = angflux/(2_ik*mu(dir) + sigma_t(cell,group)*width(cell))
        ENDIF

        ! Add ang fluxes to scalar fluxes
        scalflux(cell,group,0_ik) = scalflux(cell,group,0_ik) + wgt(dir)*angflux
        IF (printflux > 1) THEN
          IF (sweepdir == 1_ik) THEN                  ! RIGHT-TO-LEFT
            scalflux(cell,group,1_ik) = scalflux(cell,group,1_ik) &
                                & + wgt(dir)*angfluxout
            scalflux(cell,group,2_ik) = scalflux(cell,group,2_ik) &
                                & + wgt(dir)*angfluxin(dir)
          ELSE                                        ! LEFT-TO-RIGHT
            scalflux(cell,group,1_ik) = scalflux(cell,group,1_ik) &
                                & + wgt(dir)*angfluxin(dir)
            scalflux(cell,group,2_ik) = scalflux(cell,group,2_ik) &
                                & + wgt(dir)*angfluxout
          ENDIF
        ENDIF

        angfluxin(dir) = angfluxout

      ENDDO

    ENDDO

  ENDDO

  ! ! Normalise weighted sum of angular fluxes by sum of weights
  ! a = 1_ik/(2_ik*SUM(wgt))
  ! scalflux(:,group) = scalflux(:,group) * a
  ! IF (printflux > 1) THEN
  !   scalfluxl(:,group) = scalfluxl(:,group) * a
  !   scalfluxr(:,group) = scalfluxr(:,group) * a
  ! ENDIF

  RETURN
  END SUBROUTINE sweep

END MODULE sweep_mod
