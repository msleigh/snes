!> \author msleigh
!!
!! NOTES:   1D within-group non-multiplying version
!!
!! PURPOSE: Declares kind type parameters

MODULE getkinds_mod

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: rk
  PUBLIC :: ik

  INTEGER, PARAMETER :: real_kind = 8
  INTEGER, PARAMETER :: int_kind = 4

  ! Short versions
  INTEGER, PARAMETER :: rk = real_kind
  INTEGER, PARAMETER :: ik = int_kind

END MODULE getkinds_mod
