!! author: msleigh
!! date: 2002
!!
!! Extracts source parameters from ASCII input file

MODULE readsrcs_mod
  !! Extracts source parameters from ASCII input file

PRIVATE
PUBLIC :: readsrcs

CONTAINS

  !!
  !! STRUCTURE
  !! 1. Initialise variables
  !! 2. Read and check source parameters from input data

  SUBROUTINE readsrcs( &
    !! Reads source parameters from an input file
    & errstat)

  USE casechange_mod
  USE getkinds_mod
  USE io_utils_mod
  USE readline_mod
  USE setdata_mod, ONLY: value_src, inputerror, numcells, numgroups, numsrcs, &
                       & firstcell_src, lastcell_src
  USE typechange_mod

  IMPLICIT NONE

  CHARACTER(LEN=8), PARAMETER :: unitname = 'READSRCS'

  ! Arguments
  INTEGER(KIND=ik), INTENT(OUT) :: errstat !< Error status

  ! Counters
  INTEGER(KIND=ik) :: group
  INTEGER(KIND=ik) :: src

  INTEGER(KIND=ik) :: inlun
  INTEGER(KIND=ik) :: linetype
  LOGICAL          :: value_supplied

  WRITE(*,'(A)') &
    & '==============================================================================='
  WRITE(*,'(A)') 'SOURCE CHARACTERISTICS'
  WRITE(*,'(A)') &
    & '==============================================================================='
  WRITE(*,*)

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  errstat = 0_ik

  !----------------------------------------------------------------------------
  ! 2. Open keyword input file
  !----------------------------------------------------------------------------

  CALL get_free_lun(inlun,errstat)
  OPEN( &
    & UNIT=inlun, &
    & ACTION='READ', &
    & FILE='snes.in', &
    & FORM='FORMATTED', &
    & STATUS='OLD', &
    & IOSTAT=errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Error code ', errstat, ' opening file snes.in'
    RETURN
  ENDIF

  !----------------------------------------------------------------------------
  ! 3. Read and check source parameters from input data
  !----------------------------------------------------------------------------

  DO
    CALL readline( &
      & linetype, &
      & inlun, &
      & .FALSE.)
    SELECT CASE (linetype)
      CASE (0_ik)         ! Comment or blank line
        CONTINUE
      CASE (1_ik)         ! End of file
        EXIT
      CASE (2_ik)         ! Error on read
        WRITE(*,*) unitname, ': ERROR: Error from readline'
        errstat = -1_ik
        RETURN
      CASE (3_ik)         ! Line contains valid input

        field(1) = tolower(field(1))

        IF (field(1) == 'src') THEN
          src = toint(field(2))
          IF (src > numsrcs .OR. src < 1_ik) THEN
            WRITE(*,*) unitname, ': ERROR: Invalid source number given ', src
            errstat = -1_ik
            RETURN
          ENDIF
          DO
            CALL readline( &
              & linetype, &
              & inlun, &
              & .FALSE.)
            SELECT CASE (linetype)
              CASE (0_ik)         ! Comment or blank line
                CONTINUE
              CASE (1_ik)         ! End of file
                WRITE(*,*) unitname, &
                  & ': ERROR: End of file reached before endsrc keyword ', &
                  & '(source ', src, ')'
                errstat = -1_ik
                RETURN
              CASE (2_ik)         ! Error on read
                WRITE(*,*) unitname, &
                  & ': ERROR: Error from readline reading source ', src
                errstat = -1_ik
                RETURN
              CASE (3_ik)         ! Line contains valid input
                field(1) = tolower(field(1))
                SELECT CASE (field(1))
                  CASE ('firstcell')
                    firstcell_src(src) = toint(field(2))
                  CASE ('lastcell')
                    lastcell_src(src) = toint(field(2))
                  CASE ('value')
                    value_supplied = .FALSE.
                    DO group = 1_ik, numgroups
                      value_src(src,group) = toreal(field(group+1))
                      IF (value_src(src,group) > 0.0_rk) value_supplied = .TRUE.
                    ENDDO
                    IF (.NOT.value_supplied) THEN
                      WRITE(*,*) unitname, &
                        & ': No value supplied for source: ', src
                      WRITE(*,*)
                      inputerror = .TRUE.
                    ENDIF
                  CASE ('endsrc')
                    EXIT
                END SELECT
            END SELECT
          ENDDO
        ENDIF
    END SELECT
  ENDDO

  !----------------------------------------------------------------------------
  ! 4. Close keyword input file
  !----------------------------------------------------------------------------

  CLOSE( &
    & UNIT=inlun, &
    & IOSTAT=errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Error code ', errstat, ' closing file snes.in'
    RETURN
  ENDIF

  DO src = 1_ik, numsrcs
    IF (firstcell_src(src) < 1_ik .OR. firstcell_src(src) > numcells) THEN
      WRITE(*,*) unitname, ': First cell of source ', src, ' out of range'
      errstat = -1_ik
      RETURN
    ENDIF
    IF (lastcell_src(src) < 1_ik .OR. lastcell_src(src) > numcells) THEN
      WRITE(*,*) unitname, ': Last cell of source ', src, ' out of range'
      errstat = -1_ik
      RETURN
    ENDIF
    IF (lastcell_src(src) < firstcell_src(src)) THEN
      WRITE(*,*) unitname, ': Last cell of source ', src, ' lower than first'
      errstat = -1_ik
      RETURN
    ENDIF
    DO group = 1, numgroups
      IF (value_src(src,group) < 0.0_rk) THEN
        WRITE(*,*) unitname, ': Negative value given for source ', src
        errstat = -1_ik
        RETURN
      ENDIF
    ENDDO
  ENDDO

  IF (numsrcs == 0_ik) THEN
    WRITE(*,'(A19)') 'NO IMPOSED SOURCES'
  ELSE
    WRITE(*,'(A6)',ADVANCE='NO') 'Source'
    DO group = 1_ik, numgroups-1_ik
      WRITE(*,'(A7,I4)',ADVANCE='NO') 'Group', group
    ENDDO
    WRITE(*,'(A7,I4)',ADVANCE='YES') 'Group', numgroups
    WRITE(*,'(A6)',ADVANCE='NO') '      '
    DO group = 1_ik, numgroups-1_ik
      WRITE(*,'(A11)',ADVANCE='NO') '          '
    ENDDO
    WRITE(*,'(A11)',ADVANCE='YES') '           '
    WRITE(*,*)
    DO src = 1_ik, numsrcs
      WRITE(*,'(I6)',ADVANCE='NO') src
      DO group = 1_ik, numgroups-1_ik
        WRITE(*,'(F11.6)',ADVANCE='NO') value_src(src,group)
      ENDDO
      WRITE(*,'(F11.6)',ADVANCE='YES') value_src(src,numgroups)
    ENDDO
  ENDIF
  WRITE(*,*)

  IF (inputerror) THEN
    WRITE(*,*) unitname, ': Error in input'
    WRITE(*,*)
    errstat = -1_ik
    RETURN
  ENDIF

  RETURN
  END SUBROUTINE readsrcs

END MODULE readsrcs_mod
