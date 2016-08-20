MODULE quadsets_mod

PRIVATE
PUBLIC :: quadsets

CONTAINS

  !> \author msleigh
  !!
  !! NOTES:   1D within-group non-multiplying version
  !!
  !! PURPOSE: Defines quadrature sets for SN approximations
  !!
  !! STRUCTURE
  !! 1. Initialise variables
  !! 2. Define quadrature set

  SUBROUTINE quadsets( &
    & errstat)

  USE getkinds_mod
  USE setdata_mod

  IMPLICIT NONE

  ! Arguments
  INTEGER(KIND=ik), INTENT(OUT) :: errstat !<

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  errstat = 0_ik

  !----------------------------------------------------------------------------
  ! 2. Define quadrature set
  !----------------------------------------------------------------------------

  SELECT CASE (numdirs)
    CASE(1)              ! S2
      mu(1)  = 1.0
      wgt(1) = 1.0
    CASE(2)              ! S4
      mu(1)  = 0.3500212
      mu(2)  = 0.8688903
      wgt(1) = 0.6666666
      wgt(2) = 0.3333333
    CASE(3)              ! S6
      mu(1)  = 0.2666355
      mu(2)  = 0.6815076
      mu(3)  = 0.9261808
      wgt(1) = 0.5094597
      wgt(2) = 0.3144142
      wgt(3) = 0.1761263
    CASE(4)              ! S8
      mu(1)  = 0.2182179
      mu(2)  = 0.5773503
      mu(3)  = 0.7867958
      mu(4)  = 0.9511897
      wgt(1) = 0.4234568
      wgt(2) = 0.2740740
      wgt(3) = 0.1814814
      wgt(4) = 0.1209877
    CASE(6)              ! S12
      mu (1) = 0.1672126
      mu (2) = 0.4595476
      mu (3) = 0.6280191
      mu (4) = 0.7600210
      mu (5) = 0.8722706
      mu (6) = 0.9716377
      wgt(1) = 0.3279628
      wgt(2) = 0.2381773
      wgt(3) = 0.1263780
      wgt(4) = 0.1249573
      wgt(5) = 0.1117622
      wgt(6) = 0.0707626
    CASE(8)              ! S16
      mu(1)  = 0.1389568
      mu(2)  = 0.3922893
      mu(3)  = 0.5370966
      mu(4)  = 0.6504264
      mu(5)  = 0.7467506
      mu(6)  = 0.8319966
      mu(7)  = 0.9092855
      mu(8)  = 0.9805009
      wgt(1) = 0.2743402
      wgt(2) = 0.2181700
      wgt(3) = 0.0884188
      wgt(4) = 0.1287509
      wgt(5) = 0.0801592
      wgt(6) = 0.0785138
      wgt(7) = 0.0826592
      wgt(8) = 0.0489872
  END SELECT

  wgt = wgt/(2.0_rk*SUM(wgt))

  RETURN
  END SUBROUTINE quadsets

END MODULE quadsets_mod
