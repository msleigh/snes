MODULE casechange_mod

USE getkinds_mod

PRIVATE
PUBLIC :: tolower

CONTAINS

  !> \brief Function to return lower CASE of input string
  !!
  !! Uses ASCII collating sequence
  !! UPPER=LOWER-32 for this sequence

  FUNCTION tolower(string) RESULT (tolower_result)

  USE getkinds_mod

  IMPLICIT NONE

  ! Arguments
  CHARACTER (LEN=*), INTENT(in) :: string !< Input string

  CHARACTER (LEN=LEN(string)) :: tolower_result
  INTEGER(KIND=ik) :: i, ii

  DO i = 1, LEN(string)
    ii = IACHAR(string(i:i))
    SELECT CASE (ii)
      CASE (65:90)            ! ii represents an upper CASE letter in ASCII
        tolower_result(i:i) = ACHAR(ii+32)
      CASE DEFAULT
        tolower_result(i:i) = string(i:i)
    END SELECT
  ENDDO

  END FUNCTION tolower

!-----------------------------------------------------------------------------

  !> \brief Function to return upper CASE of input string
  !!
  !! Uses ASCII collating sequence
  !! UPPER=LOWER-32 for this sequence

  FUNCTION toupper(string) RESULT (toupper_result)

  USE getkinds_mod

  IMPLICIT NONE

  ! Arguments
  CHARACTER (LEN=*), INTENT(IN) :: string !< Input string

  CHARACTER (LEN=LEN(string)) :: toupper_result
  INTEGER(KIND=ik) :: i,ii

  DO i = 1, LEN(string)
    ii = IACHAR(string(i:i))
    SELECT CASE (ii)
      CASE (97:122)        ! ii represents a lower case letter in ASCII
        toupper_result(i:i) = ACHAR(ii-32)
      CASE DEFAULT
        toupper_result(i:i) = string(i:i)
    END SELECT
  ENDDO

  END FUNCTION toupper

END MODULE casechange_mod
