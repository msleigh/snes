!! author: msleigh
!! date: 2002
!!
!! Provides functions to convert strings to real and integer types

MODULE typechange_mod
  !! Provides functions to convert strings to numerical types
USE getkinds_mod

PRIVATE
PUBLIC :: toreal
PUBLIC :: toint

CONTAINS

  !! Function to return real representation of input string

  FUNCTION toreal(string) RESULT (toreal_result)
    !! Converts a string to a real number

    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: string !< String to convert
    REAL(KIND=rk) :: toreal_result

    READ(string,*) toreal_result

  END FUNCTION toreal

  !----------------------------------------------------------------------------

  !! Function to return integer representation of input string

  FUNCTION toint(string) RESULT (toint_result)
    !! Converts a string to an integer

    IMPLICIT NONE
    CHARACTER (LEN=*), INTENT(IN) :: string !< String to convert
    INTEGER(KIND=ik) :: toint_result

    READ(string,*) toint_result

  END FUNCTION toint

END MODULE typechange_mod
