!> author: msleigh
!> date: 2002
!>
!> Extracts keyword parameters from ASCII input file

MODULE readkeys_mod

PRIVATE
PUBLIC :: readkeys

CONTAINS

  !> \author msleigh
  !!
  !! PURPOSE: Extracts keyword parameters from ASCII input file
  !!
  !! STRUCTURE
  !! 1. Initialise variables
  !! 2. Extract keyword parameters from data
  !! 3. Check validity of keyword parameters
  !! 4. Print information

  SUBROUTINE readkeys( &
    & filename, &
    & errstat)

  USE casechange_mod
  USE getkinds_mod
  USE io_utils_mod
  USE readline_mod
  USE setdata_mod
  USE typechange_mod

  IMPLICIT NONE

  CHARACTER(LEN=8), PARAMETER :: unitname = 'READKEYS'

  ! Arguments
  CHARACTER(LEN=256), INTENT(IN)  :: filename !< Name of input file
  INTEGER(KIND=ik),   INTENT(OUT) :: errstat  !< Error status

  INTEGER(KIND=ik) :: inlun
  INTEGER(KIND=ik) :: linetype
  INTEGER(KIND=ik) :: snorder

  REAL(KIND=rk), PARAMETER :: eps = 1.e-24_rk

  WRITE(*,'(A)') &
    & '================================================================================'
  WRITE(*,'(A)') ' KEYWORD INPUT'
  WRITE(*,'(A)') &
    & '================================================================================'
  WRITE(*,*)

  !----------------------------------------------------------------------------
  ! 1. Set defaults for control parameters
  !----------------------------------------------------------------------------

  ! Force user definition
  numcells  = -1_ik
  numgroups = -1_ik
  nummats   = -1_ik
  numregs   = -1_ik
  snorder   = -1_ik
  calctype  = -1_ik

  ! Set defaults
  epsinner      = 1.0E-06_rk
  epsouter      = 1.0E-06_rk
  forceinners   = .FALSE.
  imaxinner     = 20_ik
  imaxouter     = 20_ik
  longmeshprint = .FALSE.
  macro         = .FALSE.
#ifdef CODETYPE
  nffu          = .TRUE.
  numnodes      = 1_ik
#else
  nffu          = .FALSE.
  numnodes      = 2_ik
#endif
  numsrcs       = 0_ik
  printflux     = 0_ik
  printmatnum   = .FALSE.
  printnucdat   = .FALSE.
  quadset       = 's'
  xmin          = 0.0_rk

  lhbc          = 0_ik
  lh_eflux      = 0.0_rk
  rh_eflux      = 0.0_rk

  errstat       = 0_ik
  inputerror    = .FALSE.

  !----------------------------------------------------------------------------
  ! 2. Open keyword input file
  !----------------------------------------------------------------------------

  CALL get_free_lun(inlun,errstat)
  OPEN( &
    & UNIT=inlun, &
    & ACTION='READ', &
    & FILE=TRIM(ADJUSTL(filename)), &
    & FORM='FORMATTED', &
    & STATUS='OLD', &
    & IOSTAT=errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Error code ', errstat, ' opening file ', &
      & TRIM(ADJUSTL(filename))
    WRITE(*,*)
    RETURN
  ENDIF

  !----------------------------------------------------------------------------
  ! 3. Extract keyword parameters from input data
  !----------------------------------------------------------------------------

  DO

    CALL readline( &
      & linetype, &
      & inlun, &
      & .FALSE.)

    SELECT CASE (linetype)
      CASE (0_ik)          ! Comment or blank line
        CONTINUE
      CASE (1_ik)          !  End of file
        EXIT
      CASE (2_ik)          !  Error on read
        WRITE(*,*) unitname, ': Error reading file ', TRIM(ADJUSTL(filename))
        WRITE(*,*)
        errstat = -1_ik
        RETURN
      CASE (3_ik)          ! Line contains valid input

        field(1) = tolower(field(1))

        SELECT CASE (field(1))

          ! Normal use parameters
          ! ---------------------
          CASE ('calctype')
            calctype = toint(field(2))
          CASE ('cells')
            numcells = toint(field(2))
          CASE ('groups')
            numgroups = toint(field(2))
          CASE ('materials')
            nummats = toint(field(2))
          CASE ('regions')
            numregs = toint(field(2))
          CASE ('sn')
            snorder = toint(field(2))

          ! Control parameters
          ! ------------------
          CASE ('epsinner')
            epsinner = toreal(field(2))
          CASE ('epsouter')
            epsouter = toreal(field(2))
          CASE ('forceinners')
            IF (toint(field(2)) == 0) THEN
              forceinners = .FALSE.
            ELSEIF (toint(field(2)) == 1) THEN
              forceinners = .TRUE.
            ELSE
              WRITE(*,*) unitname, &
                & ': Invalid value supplied for keyword FORCEINNERS: ', &
                & TRIM(field(2))
              WRITE(*,*)
              inputerror = .TRUE.
            ENDIF
          CASE ('imaxinner')
            imaxinner = toint(field(2))
          CASE ('imaxouter')
            imaxouter = toint(field(2))
          CASE ('longmeshprint')
            IF (toint(field(2)) == 1) THEN
              longmeshprint = .TRUE.
            ELSEIF (toint(field(2)) == 0) THEN
              longmeshprint = .FALSE.
            ELSE
              WRITE(*,*) unitname, &
                & ': Invalid value supplied for keyword LONGMESHPRINT: ', &
                & field(2)
              WRITE(*,*)
              inputerror = .TRUE.
            ENDIF
          CASE ('macroscopic')
            IF (toint(field(2)) == 1) THEN
              macro = .TRUE.
            ELSEIF (toint(field(2)) == 0) THEN
              macro = .FALSE.
            ELSE
              WRITE(*,*) unitname, &
                & ': Invalid value supplied for keyword FORCEINNERS: ', &
                & TRIM(field(2))
              WRITE(*,*)
              inputerror = .TRUE.
            ENDIF
          CASE ('nffu')
            IF (toint(field(2)) == 1) THEN
              nffu = .TRUE.
            ELSEIF (toint(field(2)) == 0) THEN
              nffu = .FALSE.
            ELSE
              WRITE(*,*) unitname, &
                & ': Invalid value supplied for keyword NFFU: ', &
                & TRIM(field(2))
              WRITE(*,*)
              inputerror = .TRUE.
            ENDIF
          CASE ('printflux')
            printflux = toint(field(2))
          CASE ('printmatnum')
            IF (toint(field(2)) == 0) THEN
              printmatnum = .FALSE.
            ELSEIF (toint(field(2)) == 1) THEN
              printmatnum = .TRUE.
            ELSE
              WRITE(*,*) unitname, &
                & ': Invalid value supplied for keyword PRINTMATNUM: ', &
                & TRIM(field(2))
              WRITE(*,*)
              inputerror = .TRUE.
            ENDIF
          CASE ('printnucdat')
            IF (toint(field(2)) == 0) THEN
              printnucdat = .FALSE.
            ELSEIF (toint(field(2)) == 1) THEN
              printnucdat = .TRUE.
            ELSE
              WRITE(*,*) unitname, &
                & ' Invalid value supplied for keyword PRINTNUCDAT: ', &
                & TRIM(field(2))
              WRITE(*,*)
              inputerror = .TRUE.
            ENDIF
          CASE ('quadset')
            quadset = tolower(field(2))
          CASE ('sources')
            numsrcs = toint(field(2))
          CASE ('xmin')
            xmin = toreal(field(2))

          ! Boundary conditions
          ! -------------------
          CASE ('lhbc')
            lhbc = toint(field(2))
          CASE ('lh_eflux')
            lh_eflux = toreal(field(2))
          CASE ('rh_eflux')
            rh_eflux = toreal(field(2))

        END SELECT
    END SELECT
  ENDDO

  !----------------------------------------------------------------------------
  !   4. Close input file
  !----------------------------------------------------------------------------

  CLOSE( &
    & UNIT=inlun, &
    & IOSTAT=errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Error code ', errstat, ' closing file ', &
      & TRIM(ADJUSTL(filename))
    WRITE(*,*)
    RETURN
  ENDIF

  !----------------------------------------------------------------------------
  ! 5. Check validity of keywords
  !----------------------------------------------------------------------------

  ! Normal use parameters
  ! ---------------------

  IF (calctype < 1_ik .OR. calctype > 2_ik) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword CALCTYPE: ', &
      & calctype
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF (numcells < 1_ik) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword CELLS: ', &
      & numcells
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF (numgroups < 1_ik) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword GROUPS: ', &
      & numgroups
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF ((snorder /=  2_ik) .AND. &
    & (snorder /=  4_ik) .AND. &
    & (snorder /=  8_ik) .AND. &
    & (snorder /= 12_ik) .AND. &
    & (snorder /= 16_ik)) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword SN: ', snorder
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF
  numdirs = NINT(REAL(snorder)*0.5_rk)

  IF (nummats < 1_ik .OR. nummats > numcells) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword MATERIALS: ', &
      & nummats
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF (numregs < 1_ik .OR. numregs > numcells) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword REGIONS: ', &
      & numregs
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  ! Control parameters
  ! ------------------

  IF (epsinner <= 0.0_rk .OR. epsinner > 1.0_rk) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword EPSINNER: ', &
      & epsinner
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF (epsouter <= 0.0_rk .OR. epsouter > 1.0_rk) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword EPSOUTER: ', &
      & epsouter
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF (imaxinner < 1_ik) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword IMAXINNER: ', &
      & imaxinner
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF (imaxouter < 1_ik) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword IMAXOUTER: ', &
      & imaxouter
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF (numsrcs < 0_ik .OR. numsrcs > numcells) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword SOURCES: ', &
      & numsrcs
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF (printflux < 0_ik .OR. printflux > 3_ik) THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword PRINTFLUX: ', &
      & printflux
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF (TRIM(quadset) /= 's' .AND. quadset /= 'r') THEN
    WRITE(*,*) unitname, ': Invalid value supplied for keyword QUADSET: ', &
      & quadset
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  ! Boundary conditions
  ! -------------------

  IF (lhbc /= 0_ik .AND. lhbc /= 1_ik) THEN
    WRITE(*,*) unitname, ': Invalid left-hand boundary condition supplied: ', &
      lhbc
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF
  IF (lh_eflux < 0.0_rk) THEN
    WRITE(*,*) unitname, ': Invalid left-hand edge flux supplied'
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF
  IF (rh_eflux < 0.0_rk) THEN
    WRITE(*,*) unitname,': Invalid right-hand edge flux supplied'
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  ! Interdependencies
  ! -----------------

  IF ((calctype == 1_ik) .AND. (numsrcs > 0_ik)) THEN
    WRITE(*,*) unitname, ': Imposed source supplied for eigenvalue calculation'
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF ((calctype == 2_ik) .AND. (lh_eflux < eps) &
    & .AND. (rh_eflux < eps) .AND. (numsrcs == 0_ik)) THEN
    WRITE(*,*) unitname, &
      & ': No sources or incoming fluxes supplied for flux calculation'
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF ((calctype == 2_ik) .AND. (printflux == 0_ik)) THEN
    WRITE(*,*) unitname, ': Flux print switched off for flux calculation'
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  IF ((lh_eflux > 0.0_rk) .AND. (lhbc == 0_ik)) THEN
    WRITE(*,*) unitname, &
      & ': Left-hand edge flux supplied for reflective boundary'
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  !----------------------------------------------------------------------------
  ! 6. Print information
  !----------------------------------------------------------------------------

  WRITE(*,'(A22)')        ' Normal Use Parameters'
  WRITE(*,'(A22)')        ' ---------------------'
  WRITE(*,'(A33,I22)')    ' Number of cells:                 ', numcells
  WRITE(*,'(A33,I22)')    ' Number of energy groups:         ', numgroups
  WRITE(*,'(A33,I22)')    ' Number of materials:             ', nummats
  WRITE(*,'(A33,I22)')    ' Number of mesh regions:          ', numregs
  WRITE(*,'(A33,I22)')    ' Sn order:                        ', snorder
  IF (calctype == 1_ik) THEN
    WRITE(*,'(A33,A22)')  ' Type of calculation:             ', 'eigenvalue'
  ELSE
    WRITE(*,'(A33,A22)')  ' Type of calculation:             ', 'flux'
  ENDIF
  WRITE(*,*)

  WRITE(*,'(A19)')        ' Control Parameters'
  WRITE(*,'(A19)')        ' ------------------'
  WRITE(*,'(A33,ES22.4)') ' Convergence criterion (inners):  ', epsinner
  WRITE(*,'(A33,ES22.4)') ' Convergence criterion (outers):  ', epsouter
  IF (forceinners) THEN
    WRITE(*,'(A33,A22)')  ' Inners forced to converge:       ', 'yes'
  ELSE
    WRITE(*,'(A33,A22)')  ' Inners forced to converge:       ', 'no'
  ENDIF
  WRITE(*,'(A33,I22)')    ' Max. number of inner iterations: ', imaxinner
  WRITE(*,'(A33,I22)')    ' Max. number of outer iterations: ', imaxouter
  IF (longmeshprint) THEN
    WRITE(*,'(A33,A22)')  ' Mesh print:                      ', 'long'
  ELSE
    WRITE(*,'(A33,A22)')  ' Mesh print:                      ', 'short'
  ENDIF
  IF (macro) THEN
    WRITE(*,'(A33,A22)')  ' Type of cross-section:           ', 'macroscopic'
  ELSE
    WRITE(*,'(A33,A22)')  ' Type of cross-section:           ', 'microscopic'
  ENDIF
  IF (nffu) THEN
    WRITE(*,'(A33,A22)')  ' Negative flux fix-up:            ', 'on'
  ELSE
    WRITE(*,'(A33,A22)')  ' Negative flux fix-up:            ', 'off'
  ENDIF
  IF (printflux == 0_ik) THEN
    WRITE(*,'(A33,A22)')  ' Type of flux print:              ', 'none'
  ELSEIF (printflux == 1_ik) THEN
    WRITE(*,'(A33,A22)')  ' Type of flux print:              ', 'cell fluxes'
  ELSEIF (printflux == 2_ik) THEN
    WRITE(*,'(A33,A22)')  ' Type of flux print:              ', 'edge fluxes'
  ELSE
    WRITE(*,'(A33,A22)')  ' Type of flux print:              ', 'cell fluxes'
    WRITE(*,'(A33,A22)')  '                                  ', 'edge fluxes'
  ENDIF
  IF (printmatnum) THEN
    WRITE(*,'(A33,A22)')  ' Material number print requested: ', 'yes'
  ELSE
    WRITE(*,'(A33,A22)')  ' Material number print requested: ', 'no'
  ENDIF
  IF (printnucdat) THEN
    WRITE(*,'(A33,A22)')  ' Nuclear data print requested:    ', 'yes'
  ELSE
    WRITE(*,'(A33,A22)')  ' Nuclear data print requested:    ', 'no'
  ENDIF
  WRITE(*,'(A33,F22.4)')  ' Left-hand edge x-coordinate:     ', xmin
  WRITE(*,'(A33,I22)')    ' Number of sources:               ', numsrcs
  WRITE(*,*)

  WRITE(*,'(A20)')        ' Boundary Conditions'
  WRITE(*,'(A20)')        ' -------------------'
  IF (lhbc == 0) THEN
    WRITE(*,'(A33,A22)')  ' Left-hand boundary condition:    ', 'reflective'
  ELSE
    WRITE(*,'(A33,A22)')  ' Left-hand boundary condition:    ', 'transmissive'
  ENDIF
  WRITE(*,'(A33,F22.4)')  ' Left-hand incoming flux:         ', lh_eflux
  WRITE(*,'(A33,F22.4)')  ' Right-hand incoming flux:        ', rh_eflux
  WRITE(*,*)

  IF (inputerror) THEN
    WRITE(*,*) unitname, ': Error in input'
    WRITE(*,*)
    errstat = -1_ik
    RETURN
  ENDIF

  RETURN
  END SUBROUTINE readkeys

END MODULE readkeys_mod
