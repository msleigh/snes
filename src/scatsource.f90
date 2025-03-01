!! author: msleigh
!! date: 2002
!!
!! Sets up source term for current iteration

MODULE scatsource_mod
  !! Sets up the scattering source term for the current iteration

PRIVATE
PUBLIC :: scatsource

CONTAINS

  !!
  !! PURPOSE: Sets up source term for current iteration
  !!
  !! STRUCTURE
  !! 1. Add group-to-group scattering (g' -> g where g' < g) to group source

  SUBROUTINE scatsource( &
    & group)

  USE getkinds_mod
  USE setdata_mod

  IMPLICIT NONE

  ! Arguments
  INTEGER(KIND=ik), INTENT(IN) :: group !< Energy group being considered

  ! Counters
  INTEGER(KIND=ik) :: j
  INTEGER(KIND=ik) :: group_primed ! Energy group of incident neutrons
  INTEGER(KIND=ik) :: node

  !----------------------------------------------------------------------------
  ! 1. Add group-to-group scattering (g' -> g where g' < g) to group source
  !----------------------------------------------------------------------------

  IF (group > 1_ik) THEN
    DO node = 1_ik, numnodes
      DO j = 1_ik, numcells
        DO group_primed = 1_ik, group-1_ik
          source_g(j,node) = source_g(j,node) + &
            & (sigma_s(j,group,group_primed) * &
            & scalflux_outer(j,group_primed,node))
        ENDDO
      ENDDO
    ENDDO
  ENDIF

  RETURN
  END SUBROUTINE scatsource

END MODULE scatsource_mod
