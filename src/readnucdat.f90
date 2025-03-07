!! author: msleigh
!! date: 2002
!!
!! Extracts nuclear data from ASCII input file

MODULE readnucdat_mod
  !! Extracts nuclear data from ASCII input file

PRIVATE
PUBLIC :: readnucdat

CONTAINS

  SUBROUTINE readnucdat( &
    !! Reads nuclear data for a given material
    & id, &
    & mat, &
    & fission, &
    & errstat)

  USE casechange_mod
  USE getkinds_mod
  USE io_utils_mod
  USE readline_mod
  USE setdata_mod
  USE typechange_mod

  IMPLICIT NONE

  CHARACTER(LEN=10), PARAMETER  :: unitname = 'READNUCDAT'

  ! Arguments
  CHARACTER(LEN=8), INTENT(IN)  :: id      !! Nuclide ID code
  INTEGER(KIND=ik), INTENT(IN)  :: mat     !! Material number
  LOGICAL,          INTENT(OUT) :: fission !! Fission flag
  INTEGER(KIND=ik), INTENT(OUT) :: errstat !! Error status indicator

  ! Counters
  INTEGER(KIND=ik) :: group        ! Energy group
  INTEGER(KIND=ik) :: group_primed ! Energy group which is source of scatter

  ! I/O
  INTEGER(KIND=ik)  :: inlun
  INTEGER(KIND=ik)  :: linetype
  CHARACTER(LEN=12) :: nucdatfile

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  errstat = 0_ik
  inputerror = .FALSE.

  !----------------------------------------------------------------------------
  ! 2. Read nuclear data
  !----------------------------------------------------------------------------

  ! Open nuclear data file for this material
  nucdatfile = id // '.dat'
  CALL get_free_lun(inlun,errstat)
  OPEN( &
    & UNIT=inlun, &
    & ACTION='READ', &
    & FILE=TRIM(nucdatfile), &
    & FORM='FORMATTED', &
    & STATUS='OLD', &
    & IOSTAT=errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Error code   ', errstat, ' opening file ', &
      & TRIM(nucdatfile)
    WRITE(*,*)
    RETURN
  ENDIF

  ! Read nuclear data for this material
  DO
    CALL readline( &
      & linetype, &
      & inlun, &
      & .FALSE.)
    SELECT CASE (linetype)
      CASE (0_ik)        ! Comment or blank line
        CONTINUE
      CASE (1_ik)        ! End of file
        EXIT
      CASE (2_ik)        ! Error on read
        WRITE(*,*) unitname, ': Error reading nuclear data file: ', &
          & TRIM(nucdatfile)
        WRITE(*,*)
        errstat = -1_ik
        RETURN
      CASE (3_ik)        ! Line contains valid input

        field(1) = tolower(field(1))

        SELECT CASE (field(1))

          ! Read atomic weight
          CASE ('weight')
            mats(mat)%atomweight = toreal(field(2))

          ! Read total X-Ss
          CASE('-1')
            DO group = 1_ik, numgroups
              mats(mat)%microxstot(group) = toreal(field(group+2))
            ENDDO

          ! Read scattering X-Ss
          CASE('-2')
            group_primed = toint(field(2))
            IF (group_primed > numgroups) THEN
              WRITE(*,*) unitname, &
                & ': File contains data for more groups than declared: ', &
                & TRIM(nucdatfile)
              WRITE(*,*)
              inputerror = .TRUE.
            ENDIF
            DO group = 1_ik, numgroups
              mats(mat)%microxsscat(group,group_primed) = &
                & toreal(field(group+2))
            ENDDO

          ! Read fission X-Ss
          CASE('-3')
            group_primed = toint(field(2))
            IF (group_primed > numgroups) THEN
              WRITE(*,*) unitname, &
                & ': File contains data for more groups than declared: ', &
                & TRIM(nucdatfile)
              WRITE(*,*)
              inputerror = .TRUE.
            ENDIF
            DO group = 1_ik, numgroups
              mats(mat)%microxsfiss(group,group_primed) = &
                & toreal(field(group+2))
              IF (mats(mat)%microxsfiss(group,group_primed) > 0.0_rk) &
                & fission = .TRUE.
            ENDDO

        END SELECT
    END SELECT
  ENDDO

  ! Close nuclear data file for this material
  CLOSE( &
    & UNIT=inlun, &
    & IOSTAT=errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Error code ', errstat, ' closing file ', &
      & TRIM(nucdatfile)
    RETURN
  ENDIF

  IF (inputerror) THEN
    WRITE(*,*) unitname, ': Error in input'
    WRITE(*,*)
    errstat = -1_ik
    RETURN
  ENDIF

  RETURN
  END SUBROUTINE readnucdat

END MODULE readnucdat_mod
