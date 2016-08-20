MODULE updatesource_mod

PRIVATE
PUBLIC :: updatesource

CONTAINS

  !> \author msleigh
  !!
  !! PURPOSE: Updates within-group scattering source for current group/inner
  !!
  !! STRUCTURE
  !! 1. Update within-group scattering source for current group

  SUBROUTINE updatesource( &
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
      DO j = 1, numcells

        source_s(j,node) = &
          & (sigma_s(j,group,group) * &
          & scalflux_inner(j,node))

      ENDDO
  ENDDO

  RETURN
  END SUBROUTINE updatesource

END MODULE updatesource_mod
