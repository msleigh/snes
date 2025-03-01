!! author: msleigh
!! date: 2002
!!
!! Constructs the mesh by assigning data to cell arrays

MODULE initmesh_mod
  !! Initializes the computational mesh

PRIVATE
PUBLIC :: initmesh

CONTAINS

  !> \author msleigh
  !!
  !! PURPOSE: Constructs the mesh by assigning data to cell arrays
  !!
  !! STRUCTURE
  !! 1. Initialise variables
  !! 2. Construct mesh geometry
  !! 3. Construct mesh material properties
  !! 4. Print information

  SUBROUTINE initmesh( &
    & errstat)

  USE casechange_mod
  USE getkinds_mod
  USE io_utils_mod
  USE readline_mod
  USE setdata_mod
  USE typechange_mod

  IMPLICIT NONE

  CHARACTER(LEN=8), PARAMETER :: unitname = 'INITMESH'

  ! Arguments
  INTEGER(KIND=ik), INTENT(OUT) :: errstat !< Error status

  ! Counters
  INTEGER(KIND=ik) :: cell
  INTEGER(KIND=ik) :: group
  INTEGER(KIND=ik) :: group_primed
  INTEGER(KIND=ik) :: mat
  INTEGER(KIND=ik) :: reg
  INTEGER(KIND=ik) :: cellinreg
  INTEGER(KIND=ik) :: src
  INTEGER(KIND=ik) :: inlun
  INTEGER(KIND=ik) :: linetype

  CHARACTER(LEN=7), PARAMETER :: meshdumpfile = 'snes.in'

  WRITE(*,'(A)') &
    & '==============================================================================='
  WRITE(*,'(A)') 'MESH GEOMETRY'
  WRITE(*,'(A)') &
    & '==============================================================================='
  WRITE(*,*)

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  errstat = 0_ik

  !----------------------------------------------------------------------------
  ! 2. Open mesh dump input file
  !----------------------------------------------------------------------------

  ! Open keyword input file
  CALL get_free_lun(inlun,errstat)
  OPEN( &
    & UNIT=inlun, &
    & ACTION='READ', &
    & FILE=TRIM(meshdumpfile), &
    & FORM='FORMATTED', &
    & STATUS='OLD', &
    & IOSTAT=errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Error code ', errstat, ' opening file ', &
      & TRIM(meshdumpfile)
    RETURN
  ENDIF

  !----------------------------------------------------------------------------
  ! 3. Read mesh geometry
  !----------------------------------------------------------------------------

  cell = 0_ik
  reg  = 0_ik

  DO
    CALL readline( &
      & linetype, &
      & inlun, &
      & .FALSE.)
    SELECT CASE (linetype)
      CASE (0_ik)           ! Comment or blank line
        CONTINUE
      CASE (1_ik)           ! End of file
        EXIT
      CASE (2_ik)           ! Error on read
        WRITE(*,*) unitname, ': Error from readline reading geometry file ', &
          & TRIM(meshdumpfile)
        errstat = -1_ik
        RETURN
      CASE (3_ik)           ! Line contains valid input
        field(1) = tolower(field(1))
        IF (field(1) == 'region') THEN
          reg = reg + 1_ik
          IF (reg > numregs) THEN
            WRITE(*,*) unitname, ': ERROR: Invalid region number given ', reg
            errstat = -1_ik
            RETURN
          ENDIF
          DO
            CALL readline( &
              & linetype, &
              & inlun, &
              & .FALSE.)
            SELECT CASE (linetype)
              CASE (0_ik)        ! Comment or blank line
                CONTINUE
              CASE (1_ik)        ! End of file
                WRITE(*,*) unitname, &
                  ': ERROR: End of file reached before endregion keyword (region ', &
                  reg, ')'
                errstat = -1_ik
                RETURN
              CASE (2_ik)        ! Error on read
                WRITE(*,*) unitname, &
                  ': ERROR: Error from readline reading region ', reg
                  errstat = -1_ik
                  RETURN
              CASE (3_ik)        ! Line contains valid input
                field(1) = tolower(field(1))
                SELECT CASE (field(1))
                  CASE ('nxr')
                    numcellsinreg(reg) = toint(field(2))
                  CASE ('dxr')
                    widthcellsinreg(reg) = toreal(field(2))
                  CASE ('endregion')
                    EXIT
                END SELECT
            END SELECT
          ENDDO
          DO cellinreg = 1_ik, numcellsinreg(reg)
            cell = cell + 1_ik
            width(cell) = widthcellsinreg(reg)
          ENDDO
        ENDIF
    END SELECT
  ENDDO

  !----------------------------------------------------------------------------
  ! 4. Close mesh dump input file
  !----------------------------------------------------------------------------

  CLOSE( &
    & UNIT=inlun, &
    & IOSTAT=errstat)
  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Error code ', errstat, ' closing file ', &
      & TRIM(meshdumpfile)
    RETURN
  ENDIF

  !----------------------------------------------------------------------------
  ! 5. Check region data
  !----------------------------------------------------------------------------

  ! Check all data are here
  IF (reg < numregs) THEN
    WRITE(*,*) unitname, &
      & ': WARNING - region keywords only provided for first ', reg, &
      & ' regions out of ', numregs
    errstat = -1_ik
    RETURN
  ENDIF

  !----------------------------------------------------------------------------
  ! 6. Calculate cell centres and origins
  !----------------------------------------------------------------------------

  origin(1) = xmin
  centre(1) = xmin + width(1)*0.5_rk
  DO cell = 2_ik, numcells
    origin(cell) = origin(cell-1_ik) + width(cell-1_ik)
    centre(cell) = origin(cell) + width(cell)*0.5_rk
  ENDDO
  xmax = origin(numcells) + width(numcells)

#ifdef CODETYPE
  !----------------------------------------------------------------------------
  ! 7. Construct mesh material properties
  !----------------------------------------------------------------------------

  DO mat = 1_ik, nummats
    matnum(firstcell_mat(mat):lastcell_mat(mat)) = mat
  ENDDO

  DO cell = 1_ik, numcells
    DO group = 1_ik, numgroups
      DO group_primed = 1_ik, numgroups
        sigma_s(cell,group,group_primed) = &
          & mats(matnum(cell))%macroxsscat(group,group_primed)
        sigma_f(cell,group,group_primed) = &
          & mats(matnum(cell))%macroxsfiss(group,group_primed)
      ENDDO
      sigma_t(cell,group) = mats(matnum(cell))%macroxstot(group)
    ENDDO
  ENDDO

#else
  !----------------------------------------------------------------------------
  !  7. Construct mesh material properties
  !----------------------------------------------------------------------------

  DO group = 1_ik, numgroups
    DO mat = 1_ik, nummats
      DO group_primed = 1_ik, numgroups
        sigma_s(firstcell_mat(mat):lastcell_mat(mat),group,group_primed) = &
          & mats(mat)%macroxsscat(group,group_primed)
        sigma_f(firstcell_mat(mat):lastcell_mat(mat),group,group_primed) = &
          & mats(mat)%macroxsfiss(group,group_primed)
      ENDDO
      sigma_t(firstcell_mat(mat):lastcell_mat(mat),group) = &
        & mats(mat)%macroxstot(group)
    ENDDO
  ENDDO
#endif

  !----------------------------------------------------------------------------
  !  8. Set up mesh sources
  !----------------------------------------------------------------------------

  DO group = 1_ik, numgroups
    DO src = 1_ik, numsrcs
      source_i(firstcell_src(src):lastcell_src(src),group) = value_src(src,group)
    ENDDO
  ENDDO

  !----------------------------------------------------------------------------
  ! 9. Print mesh geometry
  !----------------------------------------------------------------------------

  WRITE(*,'(A30,F16.6)') 'Left-hand edge x-coordinate:  ', xmin
  WRITE(*,'(A30,F16.6)') 'Right-hand edge x-coordinate: ', xmax
  WRITE(*,'(A30,F16.6)') 'Total problem width:          ', xmax - xmin
  WRITE(*,*)

  IF (longmeshprint) THEN
    WRITE(*,'(A4,4A16)') 'Cell', 'Origin (cm)', 'Centre (cm)', 'Width (cm)', &
      'Material no.'
    WRITE(*,'(A4,4A16)') '----', '-----------', '-----------', '----------', &
      '------------'
    DO cell = 1_ik, numcells
#ifdef CODETYPE
      WRITE(*,'(I4,3F16.6,I16)') cell, origin(cell), centre(cell), &
        width(cell), matnum(cell)
#else
      WRITE(*,'(I4,3F16.6,I16)') cell, origin(cell), centre(cell), &
        width(cell)
#endif
    ENDDO
    WRITE(*,*)
  ENDIF

  RETURN
  END SUBROUTINE initmesh

END MODULE initmesh_mod
