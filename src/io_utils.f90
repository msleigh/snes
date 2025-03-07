!! author: msleigh
!! date: 2002
!!
!! Provides I/O utility functions

MODULE io_utils_mod
  !! Contains input/output utility functions

PRIVATE
PUBLIC :: get_free_lun

CONTAINS

  SUBROUTINE get_free_lun( &
    !! Finds a free logical unit number for file operations
    & logical_unit_number, &
    & errstat)

  USE getkinds_mod

  IMPLICIT NONE

  CHARACTER(LEN=12), PARAMETER :: unitname = 'GET_FREE_LUN'

  INTEGER(KIND=ik), INTENT(OUT) :: logical_unit_number !! Logical unit number to be assigned
  INTEGER(KIND=ik), INTENT(OUT) :: errstat             !! Local error status indicator

  LOGICAL :: lun_in_use

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  errstat = 0_ik
  logical_unit_number = 0_ik
  lun_in_use = .TRUE.

  !----------------------------------------------------------------------------
  ! 2. Find free LUN
  !----------------------------------------------------------------------------

  DO WHILE (lun_in_use .AND. logical_unit_number < 100_ik)
    logical_unit_number = logical_unit_number + 1_ik
    INQUIRE( &
      & UNIT=logical_unit_number, &
      & OPENED=lun_in_use)
  ENDDO

  IF (lun_in_use .AND. logical_unit_number == 100_ik) THEN
    WRITE(*,*) unitname, ': ERROR - no free logical unit numbers available'
    errstat = -1_ik
    RETURN
  ENDIF

  RETURN
  END SUBROUTINE get_free_lun

END MODULE io_utils_mod
