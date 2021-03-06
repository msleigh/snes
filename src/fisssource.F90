MODULE fisssource_mod

PRIVATE
PUBLIC :: fisssource

CONTAINS

  !> \author msleigh
  !!
  !! PURPOSE: Calculates fission source for current outer iteration
  !!
  !! STRUCTURE
  !! 1. Initialise variables
  !! 2. Set up fission source term

  SUBROUTINE fisssource( &
    & sfs)

  USE getkinds_mod
  USE setdata_mod

  IMPLICIT NONE

  ! Arguments
  REAL(KIND=rk), INTENT(OUT) :: sfs !< Sum of the fission source

  ! Counters
  INTEGER(KIND=ik) :: j
  INTEGER(KIND=ik) :: group        ! Energy group being considered
  INTEGER(KIND=ik) :: group_primed ! Energy group of incident neutrons
  INTEGER(KIND=ik) :: node

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  sfs = 0.0_rk

  !----------------------------------------------------------------------------
  ! 2. Set up fission source term
  !----------------------------------------------------------------------------

  DO node = 1_ik, numnodes
    DO group = 1_ik, numgroups
      DO j = 1_ik, numcells

        source_f(j,group,node) = 0.0_rk

        DO group_primed = 1_ik, numgroups

          source_f(j,group,node) = source_f(j,group,node) + &
            & (sigma_f(j,group,group_primed) * &
#ifdef CODETYPE
            & scalflux(j,group_primed,0_ik))
#else
            & scalflux_outer(j,group_primed,node))
#endif

        ENDDO

        sfs = sfs + source_f(j,group,node)

      ENDDO
    ENDDO
  ENDDO

  ! In DG version, average left and right sources, which have been summed in
  ! loop above
  sfs = sfs / REAL(numnodes)

  RETURN
  END SUBROUTINE fisssource

END MODULE fisssource_mod
