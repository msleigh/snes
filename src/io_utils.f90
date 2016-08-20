MODULE io_utils_mod

PRIVATE
PUBLIC :: get_free_lun

CONTAINS

  !> \author msleigh
  !!
  !! PURPOSE: Gets free logical unit number (LUN)
  !!
  !! STRUCTURE
  !! 1. Initialise variables
  !! 2. Find free LUN

  SUBROUTINE get_free_lun( &
    & logical_unit_number, &
    & errstat)

  USE getkinds_mod

  IMPLICIT NONE

  CHARACTER(LEN=12), PARAMETER :: unitname = 'GET_FREE_LUN'

  INTEGER(KIND=ik), INTENT(OUT) :: logical_unit_number !<
  INTEGER(KIND=ik), INTENT(OUT) :: errstat             !<

  LOGICAL :: lun_in_use

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  errstat = 0
  logical_unit_number = 0
  lun_in_use = .TRUE.

  !----------------------------------------------------------------------------
  ! 2. Find free LUN
  !----------------------------------------------------------------------------

  DO WHILE (lun_in_use .AND. logical_unit_number < 100)
    logical_unit_number = logical_unit_number + 1
    INQUIRE( &
      & UNIT=logical_unit_number, &
      & OPENED=lun_in_use)
  ENDDO

  IF (lun_in_use .AND. logical_unit_number == 100) THEN
    WRITE(*,*) unitname, ': ERROR - no free logical unit numbers available'
    errstat = -1
    RETURN
  ENDIF

  RETURN
  END SUBROUTINE get_free_lun

END MODULE io_utils_mod

