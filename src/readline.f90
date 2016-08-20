MODULE readline_mod

USE getkinds_mod

IMPLICIT NONE

PRIVATE
PUBLIC :: readline

! Public
CHARACTER(LEN=80), DIMENSION(:), ALLOCATABLE, PUBLIC :: field
INTEGER(KIND=ik), PUBLIC                             :: num_fields

CONTAINS

  !> \author msleigh
  !!
  !! PURPOSE: Routine to read in a line of input from file instream
  !!          Line is split into field() with each element of field
  !!          containing character representation of each white-space-
  !!          delimited field in variable line
  !!          Variable num_fields contains number of fields in field()
  !!          Allows comment character # to appear anywhere on input line
  !!          Output argument 'line_type' is integer specifying type of line
  !!          read, i.e. line_type=0 all comment or blank line
  !!                     line_type=1 end of file
  !!                     line_type=2 error on read
  !!                     line_type=3 line contains input
  !!          Output argument printout is flag for printing out line read
  !!                     .TRUE. is print
  !!                     .FALSE. is do not print
  !!
  !! STRUCTURE
  !! 1. Initialise variables
  !! 2. Extract keyword parameters from data
  !! 3. Check validity of keyword parameters
  !! 4. Print information

  SUBROUTINE readline( &
    & line_type, &
    & instream, &
    & printout)

  USE getkinds_mod

  IMPLICIT NONE
  
  CHARACTER(LEN=8), PARAMETER :: unitname = 'readline'
  
  ! Arguments
  INTEGER(kind=ik),  INTENT(OUT) :: line_type !< Output argument
  INTEGER(kind=ik),  INTENT(IN)  :: instream  !< Input stream
  LOGICAL, OPTIONAL, INTENT(IN)  :: printout  !< Flag to print or not
  
  INTEGER(KIND=ik), DIMENSION(:,:), ALLOCATABLE :: fp
  LOGICAL                                       :: pp
  INTEGER(KIND=ik)                              :: allocation_status
  INTEGER(KIND=ik)                              :: in_status
  INTEGER(KIND=ik)                              :: line_length
  INTEGER(KIND=ik)                              :: loc_com
  INTEGER(KIND=ik)                              :: fld
  CHARACTER(LEN=120)                            :: line
  
  ! Counters
  INTEGER(KIND=ik) :: i

  allocation_status = 0_ik
  
  allocation_status = 0_ik
   
  !----------------------------------------------------------------------------
  ! 1. Test for optional argument 'printout' and set working variable 'pp'
  !----------------------------------------------------------------------------

  IF (PRESENT(printout)) then
    pp = printout
  ELSE
    pp = .TRUE.
  ENDIF

  !----------------------------------------------------------------------------
  ! 2. Clear out dynamic arrays used for parsing input line
  !----------------------------------------------------------------------------

  IF (ALLOCATED(field)) DEALLOCATE(field,STAT=allocation_status)
  IF (allocation_status > 0) THEN
    WRITE(*,*) 'Deallocation error - field', allocation_status
    STOP
  ENDIF
  IF (ALLOCATED(fp)) WRITE(*,*) 'IMPOSSIBLE ERROR'

  !----------------------------------------------------------------------------
  ! 3. Read line into character variable 'line' & process
  !----------------------------------------------------------------------------

  READ(instream,FMT='(a)',IOSTAT=in_status) line
  IF (in_status > 0 ) THEN    ! read error
    line_type = 2_ik
    RETURN
  ELSEIF (in_status < 0) THEN ! end of file
    line_type = 1_ik
    RETURN
  ENDIF

  ! Convert tabs to spaces
  DO i = 1_ik, LEN(line)
    IF (line(i:i) == ACHAR(9)) line(i:i) = ' '
  ENDDO

  ! Remove initial spaces
  line = ADJUSTL(line)
  
  ! Compress multiple spaces to one
  blank_loop: DO
    i = INDEX(TRIM(line),"  ")
    IF (i == 0) EXIT
    line(i:) = line(i+1:)
  ENDDO blank_loop
  line_length = LEN_TRIM(line)
  
  ! Write out adjusted input
  IF (pp) WRITE(*,*) unitname, ': ', TRIM(line), '$'
  
  ! Check for zero-length lines and comment lines
  IF (line_length == 0 .OR. line(1:1) == '#' ) THEN
    line_type = 0_ik
    RETURN
  ELSE
    line_type = 3_ik
  ENDIF

  ! Remove embedded comments
  loc_com = INDEX(line,'#')
  IF (loc_com /= 0) THEN
    line(loc_com:) = ' '
    line_length = LEN_TRIM(line)
  ENDIF

  !----------------------------------------------------------------------------
  ! 4. Read line into character array 'field'
  !----------------------------------------------------------------------------

  ! Determine number of blank-separated fields
  num_fields = 1_ik
  DO i = 1_ik, line_length
    IF (line(i:i) == ' ') THEN
      num_fields = num_fields + 1_ik
    ENDIF
  ENDDO

  ! Allocate output arrays
  ALLOCATE(field(num_fields),STAT=allocation_status)
  IF (allocation_status > 0) THEN
    WRITE(*,*) 'Allocation error - field'
    STOP
  ENDIF
  ALLOCATE(fp(num_fields,2),STAT=allocation_status)
  IF (allocation_status > 0) THEN
    WRITE(*,*) 'Allocation error - fp'
    STOP
  ENDIF

  fld = 1_ik
  fp(fld,1) = 1_ik
  DO i = 1_ik, line_length
    IF (line(i:i) == ' ') THEN
      fp(fld,2) = i - 1
      fp(fld+1,1) = i + 1
      fld = fld + 1_ik
    ENDIF
  ENDDO
  fp(num_fields,2) = line_length
  DO i = 1_ik, num_fields
    field(i) = line(fp(i,1):fp(i,2))
  ENDDO

  DEALLOCATE(fp,STAT=allocation_status)
  IF (allocation_status > 0) THEN
    WRITE(*,*) 'Deallocation error - fp'
    STOP
  ENDIF

  RETURN
  END SUBROUTINE readline

END MODULE readline_mod
