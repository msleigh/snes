!! author: msleigh
!! date: 2002
!!
!! Provides utilities for changing case in strings

MODULE casechange_mod
  !! Provides utilities for changing case in strings

USE getkinds_mod

PRIVATE
PUBLIC :: tolower

CONTAINS

  FUNCTION tolower(string) RESULT (tolower_result)
    !! Converts a string to lowercase using ASCII values

  USE getkinds_mod

  IMPLICIT NONE

  ! Arguments
  CHARACTER (LEN=*), INTENT(in) :: string !! Input string to be converted to lowercase

  CHARACTER (LEN=LEN(string)) :: tolower_result !! Output string in lower case
  INTEGER(KIND=ik) :: i, ii

  DO i = 1_ik, LEN(string)
    ii = IACHAR(string(i:i))
    SELECT CASE (ii)
      CASE (65:90)            ! ii represents an upper CASE letter in ASCII
        tolower_result(i:i) = ACHAR(ii+32)
      CASE DEFAULT
        tolower_result(i:i) = string(i:i)
    END SELECT
  ENDDO

  END FUNCTION tolower

END MODULE casechange_mod
