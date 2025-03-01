!! author: msleigh
!! date: 2002
!!
!! Writes scalar flux output to ASCII file

MODULE printflux1_mod

PRIVATE
PUBLIC :: printflux1

CONTAINS

  !> \author msleigh
  !!
  !! PURPOSE: Writes scalar flux output to ASCII file
  !!
  !! STRUCTURE
  !! 1. Initialise variables
  !! 2. Open output file
  !! 3. Write data
  !! 4. Close output file

  SUBROUTINE printflux1( &
    & errstat)

  USE getkinds_mod
  USE io_utils_mod
  USE setdata_mod

  IMPLICIT NONE

  CHARACTER(LEN=10), PARAMETER :: unitname = 'PRINTFLUX1'

  ! Arguments
  INTEGER(KIND=ik), INTENT(OUT) :: errstat !< Error status

  ! I/O
  INTEGER(KIND=ik) :: outlun

  ! Counters
  INTEGER(KIND=ik) :: cell
  INTEGER(KIND=ik) :: group

  !----------------------------------------------------------------------------
  !  1. Initialise variables
  !----------------------------------------------------------------------------

  errstat = 0_ik

  !----------------------------------------------------------------------------
  ! 2. Open output file
  !----------------------------------------------------------------------------

  WRITE(*,'(A)') 'Printing cell-centred scalar fluxes to file flux1.dat'
  WRITE(*,*)

  CALL get_free_lun(outlun,errstat)
  OPEN( &
    & UNIT=outlun, &
    & ACTION='WRITE', &
    & FILE='flux1.dat', &
    & FORM='FORMATTED', &
    & STATUS='REPLACE', &
    & IOSTAT=errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname,': Error code ', errstat,' opening file flux1.dat'
    RETURN
  ENDIF

  !----------------------------------------------------------------------------
  ! 3. Write data
  !----------------------------------------------------------------------------

  WRITE(outlun,'(A62)') &
    & '# TITLE=[Scalar flux against position for 1D slab calculation]'
  WRITE(outlun,'(A17)') '# XLABEL=[x (cm)]'
  WRITE(outlun,'(A41)') '# YLABEL=[Scalar flux (cm^{-2}.s^{-1})]'
  WRITE(outlun,ADVANCE='NO',FMT='(A10)') '# SERIES=['
  DO group = 1_ik, numgroups-1_ik
    WRITE(outlun,ADVANCE='NO',FMT='(A5,1X,I1)') 'Group', group, ','
  ENDDO
  WRITE(outlun,ADVANCE='YES',FMT='(A5,1X,I1,A1)') 'Group', numgroups, ']'
  WRITE(outlun,'(A11,F7.3,A1,F7.3,A1)') '# XLIMITS=[',xmin,',',xmax,']'
  WRITE(outlun,'(A15,F7.3,A1)') '# YLIMITS=[0.0,',MAXVAL(scalflux(:,:,0_ik)),']'
  WRITE(outlun,'(A1)') '#'

  DO cell = 1_ik, numcells
    WRITE( &
      & UNIT=outlun, &
      & ADVANCE='NO', &
      & FMT='(F28.6)') &
      & centre(cell)
    DO group = 1_ik, numgroups-1_ik
      WRITE( &
        & UNIT=outlun,&
        & ADVANCE='NO', &
        & FMT='(F28.20)') &
        & scalflux(cell,group,0_ik)
    ENDDO
    WRITE( &
      & UNIT=outlun, &
      & ADVANCE='YES', &
      & FMT='(F28.20)') &
      & scalflux(cell,numgroups,0_ik)
  ENDDO

  !----------------------------------------------------------------------------
  !   4. Close output file
  !----------------------------------------------------------------------------

  CLOSE( &
    & UNIT=outlun, &
    & IOSTAT=errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Error code ', errstat, ' closing file flux1.dat'
    RETURN
  ENDIF

  RETURN
  END SUBROUTINE printflux1

END MODULE printflux1_mod
