!! author: msleigh
!! date: 2002
!!
!! Declares kind type parameters

MODULE getkinds_mod
  !! Provides kind parameters for numerical precision

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
