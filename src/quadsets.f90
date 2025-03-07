!! author: msleigh
!! date: 2002
!!
!! Defines quadrature sets for Sn approximations

MODULE quadsets_mod
  !! Defines quadrature sets for Sn approximations

PRIVATE
PUBLIC :: quadsets

CONTAINS

  SUBROUTINE quadsets( &
    & errstat)
    !! Defines quadrature sets for Sn approximations

  USE getkinds_mod
  USE setdata_mod

  IMPLICIT NONE

  ! Arguments
  INTEGER(KIND=ik), INTENT(OUT) :: errstat !! Local error status

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  errstat = 0_ik

  !----------------------------------------------------------------------------
  ! 2. Define quadrature set
  !----------------------------------------------------------------------------

  SELECT CASE (numdirs)
    CASE(1)              ! S2
      mu(1)  = 1.0_rk
      wgt(1) = 1.0_rk
    CASE(2)              ! S4
      mu(1)  = 0.3500212_rk
      mu(2)  = 0.8688903_rk
      wgt(1) = 0.6666666_rk
      wgt(2) = 0.3333333_rk
    CASE(3)              ! S6
      mu(1)  = 0.2666355_rk
      mu(2)  = 0.6815076_rk
      mu(3)  = 0.9261808_rk
      wgt(1) = 0.5094597_rk
      wgt(2) = 0.3144142_rk
      wgt(3) = 0.1761263_rk
    CASE(4)              ! S8
      mu(1)  = 0.2182179_rk
      mu(2)  = 0.5773503_rk
      mu(3)  = 0.7867958_rk
      mu(4)  = 0.9511897_rk
      wgt(1) = 0.4234568_rk
      wgt(2) = 0.2740740_rk
      wgt(3) = 0.1814814_rk
      wgt(4) = 0.1209877_rk
    CASE(6)              ! S12
      mu (1) = 0.1672126_rk
      mu (2) = 0.4595476_rk
      mu (3) = 0.6280191_rk
      mu (4) = 0.7600210_rk
      mu (5) = 0.8722706_rk
      mu (6) = 0.9716377_rk
      wgt(1) = 0.3279628_rk
      wgt(2) = 0.2381773_rk
      wgt(3) = 0.1263780_rk
      wgt(4) = 0.1249573_rk
      wgt(5) = 0.1117622_rk
      wgt(6) = 0.0707626_rk
    CASE(8)              ! S16
      mu(1)  = 0.1389568_rk
      mu(2)  = 0.3922893_rk
      mu(3)  = 0.5370966_rk
      mu(4)  = 0.6504264_rk
      mu(5)  = 0.7467506_rk
      mu(6)  = 0.8319966_rk
      mu(7)  = 0.9092855_rk
      mu(8)  = 0.9805009_rk
      wgt(1) = 0.2743402_rk
      wgt(2) = 0.2181700_rk
      wgt(3) = 0.0884188_rk
      wgt(4) = 0.1287509_rk
      wgt(5) = 0.0801592_rk
      wgt(6) = 0.0785138_rk
      wgt(7) = 0.0826592_rk
      wgt(8) = 0.0489872_rk
  END SELECT

  wgt = wgt/(2.0_rk*SUM(wgt))

  RETURN
  END SUBROUTINE quadsets

END MODULE quadsets_mod
