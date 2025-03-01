!! author: msleigh
!! date: 2002
!!
!! Updates within-group scattering source for current group/inner

MODULE updatesource_mod
  !! Updates the scattering source for the current group and iteration

PRIVATE
PUBLIC :: updatesource

CONTAINS

  !!
  !! PURPOSE: Updates within-group scattering source for current group/inner
  !!
  !! STRUCTURE
  !! 1. Update within-group scattering source for current group

  SUBROUTINE updatesource( &
    !! Updates the scattering source for the specified energy group
    & group)

  USE getkinds_mod
  USE setdata_mod

  IMPLICIT NONE

  ! Arguments
  INTEGER(KIND=ik), INTENT(IN) :: group !< Energy group being considered

  ! Counters
  INTEGER(KIND=ik) :: j
  INTEGER(KIND=ik) :: node

  !----------------------------------------------------------------------------
  ! 1. Calculate within-group scattering source
  !----------------------------------------------------------------------------

  DO node = 1_ik, numnodes
      DO j = 1_ik, numcells

        source_s(j,node) = &
          & (sigma_s(j,group,group) * &
          & scalflux_inner(j,node))

      ENDDO
  ENDDO

  RETURN
  END SUBROUTINE updatesource

END MODULE updatesource_mod
