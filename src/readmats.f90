MODULE readmats_mod

PRIVATE
PUBLIC :: readmats

CONTAINS

  !> \author msleigh
  !!
  !! PURPOSE: Extracts material parameters from ASCII input file
  !!
  !! STRUCTURE
  !! 1. Initialise variables
  !! 2. Read and check material parameters from input data

  SUBROUTINE readmats( &
    & filename, &
    & errstat)

  USE casechange_mod
  USE getkinds_mod
  USE io_utils_mod
  USE readline_mod
  USE readnucdat_mod
  USE setdata_mod
  USE typechange_mod

  IMPLICIT NONE

  CHARACTER(LEN=8), PARAMETER :: unitname = 'READMATS'

  ! Arguments
  CHARACTER(LEN=256), INTENT(IN)  :: filename !<
  INTEGER(KIND=ik),   INTENT(OUT) :: errstat  !<

  ! Counters
  INTEGER(KIND=ik) :: group        ! Energy group
  INTEGER(KIND=ik) :: group_primed ! Energy group which is source of scatter
  INTEGER(KIND=ik) :: mat

  REAL(KIND=rk) ::    group_check  ! Value not index

  ! Nuclear data file
  CHARACTER(LEN=8), DIMENSION(nummats) :: nucdat_id

  ! Physics
  REAL(KIND=rk), PARAMETER :: avogadro = 6.022137E+23_rk
  REAL(KIND=rk)            :: numberdensity

  ! I/O
  INTEGER(KIND=ik) :: inlun
  INTEGER(KIND=ik) :: linetype

  LOGICAL :: fission

  REAL(KIND=rk), PARAMETER :: eps = 1.e-24_rk

  WRITE(*,'(A)') &
    & '==============================================================================='
  WRITE(*,'(A)') ' NUCLEAR DATA'
  WRITE(*,'(A)') &
    & '==============================================================================='
  WRITE(*,*)

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  errstat = 0_ik
  inputerror = .FALSE.
  nucdat_id(:) = ''

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
  ! 3. Read material parameters from input data
  !----------------------------------------------------------------------------

  mat = 0_ik

  DO
    CALL readline( &
      & linetype, &
      & inlun, &
      & .FALSE.)
    SELECT CASE (linetype)
      CASE (0_ik)           !  Comment or blank line
        CONTINUE
      CASE (1_ik)           !  End of file
        EXIT
      CASE (2_ik)           !  Error on read
        WRITE(*,*) unitname, ': Error reading file: ', TRIM(ADJUSTL(filename))
        WRITE(*,*)
        errstat = -1_ik
        RETURN
      CASE (3_ik)           ! Line contains valid input

        field(1) = tolower(field(1))

        IF (field(1) == 'mat') THEN
          mat = mat + 1_ik
          IF (mat > nummats) THEN
            WRITE(*,*) unitname, &
              & ': No. of materials listed > no. of materials declared'
            WRITE(*,*)
            inputerror = .TRUE.
          ENDIF

          mats(mat)%mat_id = toint(field(2))

          DO
            CALL readline( &
              & linetype, &
              & inlun, &
              & .FALSE.)
            SELECT CASE (linetype)
              CASE (0_ik)           ! Comment or blank line
                CONTINUE
              CASE (1_ik)           ! End of file
                WRITE(*,*) unitname, &
                  & ': End of file reached before endmat keyword (material ', &
                  & mat,')'
                WRITE(*,*)
                errstat = -1_ik
                RETURN
              CASE (2_ik)           ! Error on read
                WRITE(*,*) unitname, ': Error reading material ', mat
                WRITE(*,*)
                errstat = -1_ik
                RETURN
              CASE (3_ik)           ! Line contains valid input
                field(1) = tolower(field(1))
                SELECT CASE (field(1))
                  CASE ('void')
                    mats(mat)%rho = 0.0_rk
                    nucdat_id(mat) = 'void'
                  CASE ('firstcell')
                    firstcell_mat(mat) = toint(field(2))
                  CASE ('lastcell')
                    lastcell_mat(mat) = toint(field(2))
                  CASE ('density')
                    mats(mat)%rho = toreal(field(2))
                  CASE ('fileid')
                    nucdat_id(mat) = tolower(field(2))
                  CASE ('endmat')
                    EXIT
                END SELECT
            END SELECT
          ENDDO

          ! Check data for this material
          IF ((firstcell_mat(mat) < 1_ik) .OR. (firstcell_mat(mat) > &
            & numcells)) THEN
            WRITE(*,*) unitname, ': First cell of material ', mat, &
              & ' out of range'
            WRITE(*,*)
            inputerror = .TRUE.
          ENDIF
          IF ((lastcell_mat(mat) < 1_ik) .OR. (lastcell_mat(mat) > numcells)) &
            & THEN
            WRITE(*,*) unitname, ': Last cell of material ', mat, &
              & ' out of range'
            WRITE(*,*)
            inputerror = .TRUE.
          ENDIF
          IF (lastcell_mat(mat) < firstcell_mat(mat)) THEN
            WRITE(*,*) unitname, ': Last cell of material ', mat, &
              & ' lower than first'
            WRITE(*,*)
            inputerror = .TRUE.
          ENDIF
          IF ((.NOT.macro) .AND. (mats(mat)%rho <= 0.0_rk) .AND. &
            & (nucdat_id(mat) /= 'void')) THEN
            WRITE(*,*) unitname, &
              & ': Zero density supplied for non-void material ', mat
            WRITE(*,*)
            inputerror = .TRUE.
          ENDIF
          IF ((LEN_TRIM(nucdat_id(mat)) /= 8) .AND. (nucdat_id(mat) /= &
            & 'void')) THEN
            WRITE(*,*) unitname, &
              & ': Invalid nuclear data file ID supplied for material ', mat
            WRITE(*,*)
            inputerror = .TRUE.
          ENDIF

        ENDIF
    END SELECT
  ENDDO

  IF (mat < nummats) THEN
    WRITE(*,*) unitname, &
      & ': Material keywords only provided for first ', mat, &
      & ' materials out of ', nummats
    WRITE(*,*)
    inputerror = .TRUE.
  ENDIF

  !----------------------------------------------------------------------------
  ! 4. Close keyword input file
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
  ! 5. Read nuclear data for all materials
  !----------------------------------------------------------------------------

  fission = .FALSE.
  DO mat = 1_ik, nummats
    ! Allocate storage for X-Ss
    ALLOCATE( &
      & mats(mat)%microxstot(numgroups),            &
      & mats(mat)%microxsfiss(numgroups,numgroups), &
      & mats(mat)%microxsscat(numgroups,numgroups), &
      & mats(mat)%macroxstot(numgroups),            &
      & mats(mat)%macroxsfiss(numgroups,numgroups), &
      & mats(mat)%macroxsscat(numgroups,numgroups), &
      & STAT=errstat)

    IF (nucdat_id(mat) == 'void') THEN

      ! Material is void
      mats(mat)%rho = 0.0_rk
      mats(mat)%atomweight = 0.0_rk
      mats(mat)%microxstot(:) = 0.0_rk
      mats(mat)%microxsscat(:,:) = 0.0_rk
      mats(mat)%microxsfiss(:,:) = 0.0_rk
      mats(mat)%macroxstot(:) = 0.0_rk
      mats(mat)%macroxsscat(:,:) = 0.0_rk
      mats(mat)%macroxsfiss(:,:) = 0.0_rk

    ELSE

      ! Read nuclear data for this material
      CALL readnucdat( &
        & nucdat_id(mat), &
        & mat, &
        & fission, &
        & errstat)

      ! Check for presence of fission X-Ss in flux calculation
      IF ((calctype == 2_ik) .AND. (fission)) THEN
        WRITE(*,*) unitname, &
          & ': Fission cross-sections specified for flux calculation'
        WRITE(*,*)
        inputerror = .TRUE.
      ENDIF

      ! Check data for this material
      DO group = 1_ik, numgroups
        group_check = 0.0_rk
        DO group_primed = 1_ik, numgroups
          group_check = group_check + mats(mat)%microxsscat(group,group_primed)
        ENDDO
        IF (group_check >= mats(mat)%microxstot(group)) THEN
          WRITE(*,*) unitname, ': Scattering X-S for group ', group, &
            & ' in material ', mat, ' not less than total X-S'
          WRITE(*,*)
        ENDIF
      ENDDO

      ! Calculate number density of nuclei given density & atomic weight
      IF ((macro) .OR. (mats(mat)%atomweight < eps)) THEN
        numberdensity = 1.0_rk
      ELSE
        numberdensity = (1.0E-24_rk)*mats(mat)%rho*avogadro/mats(mat)%atomweight
      ENDIF

      ! Convert microscopic cross-sections to macroscopic
      mats(mat)%macroxstot(:)    = numberdensity*mats(mat)%microxstot(:)
      mats(mat)%macroxsscat(:,:) = numberdensity*mats(mat)%microxsscat(:,:)
      mats(mat)%macroxsfiss(:,:) = numberdensity*mats(mat)%microxsfiss(:,:)

    ENDIF
  ENDDO

  !----------------------------------------------------------------------------
  ! 6. Print nuclear data for all materials
  !----------------------------------------------------------------------------

  DO mat = 1_ik, nummats

    WRITE(*,'(A9,I4)') ' MATERIAL ', mat
    WRITE(*,'(A13)') '               '
    WRITE(*,*)

    WRITE(*,'(A20,A2)',ADVANCE='NO') 'Quantity (cm^-1)', '|'
    DO group_primed = 1_ik, numgroups-1_ik
      WRITE(*,'(I10,A2)',ADVANCE='NO') group_primed, '|'
    ENDDO
    WRITE(*,'(I10)',ADVANCE='YES') numgroups

    WRITE(*,'(A20,A2)',ADVANCE='NO') '--------------------', '|'
    DO group_primed = 1_ik, numgroups-1_ik
      WRITE(*,'(A10,A2)',ADVANCE='NO') '----------', '|'
    ENDDO
    WRITE(*,'(A10,A2)',ADVANCE='YES') '----------', '|'

    WRITE(*,'(A20,A2)',ADVANCE='NO') 'Total XS', '|'
    DO group_primed = 1_ik, numgroups-1_ik
      WRITE(*,'(F10.6,A2)',ADVANCE='NO') mats(mat)%macroxstot(group_primed), &
        & '|'
    ENDDO
    WRITE(*,'(F10.6)',ADVANCE='YES') mats(mat)%macroxstot(numgroups)

    DO group = 1_ik, numgroups
      WRITE(*,'(A16,I4,A2)',ADVANCE='NO') 'Scatter -> group', group, '|'
      DO group_primed = 1_ik, numgroups-1_ik
        WRITE(*,'(F10.6,A2)',ADVANCE='NO') &
          & mats(mat)%macroxsscat(group,group_primed), '|'
      ENDDO
      WRITE(*,'(F10.6)',ADVANCE='YES') mats(mat)%macroxsscat(group,numgroups)
    ENDDO

    DO group = 1_ik, numgroups
      WRITE(*,'(A16,I4,A2)',ADVANCE='NO') 'Fission -> group', group, '|'
      DO group_primed = 1_ik, numgroups-1_ik
        WRITE(*,'(F10.6,A2)',ADVANCE='NO') &
          & mats(mat)%macroxsfiss(group,group_primed), '|'
      ENDDO
      WRITE(*,'(F10.6)',ADVANCE='YES') mats(mat)%macroxsfiss(group,numgroups)
    ENDDO

    WRITE(*,*)

  ENDDO

  IF (inputerror) THEN
    WRITE(*,*) unitname, ': Error in input'
    WRITE(*,*)
    errstat = -1_ik
    RETURN
  ENDIF

  RETURN
  END SUBROUTINE readmats

END MODULE readmats_mod
