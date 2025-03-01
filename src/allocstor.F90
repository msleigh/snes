!! author: msleigh
!! date: 2002
!!
!! Allocates sizes to global arrays

MODULE allocstor_mod
  !! Handles memory allocation for storage
  !! author: msleigh
  !! date: 2002
  !!
  !! Allocates sizes to global arrays

PRIVATE
PUBLIC :: allocstor

CONTAINS

  SUBROUTINE allocstor( &
    & errstat)
    !! author: msleigh
    !! date: 2002
    !!
    !! Allocates sizes to global arrays

  USE getkinds_mod
  USE setdata_mod

  IMPLICIT NONE

  CHARACTER(LEN=9), PARAMETER :: unitname = 'ALLOCSTOR'

  ! Arguments
  INTEGER(KIND=ik), INTENT(OUT) :: errstat !! Local error status

  !---------------------------------------------------------------------------
  !> 1. Initialise variables
  !---------------------------------------------------------------------------

  errstat = 0_ik

  !---------------------------------------------------------------------------
  !> 2. Allocate storage
  !---------------------------------------------------------------------------

  ALLOCATE( &
    & mats(nummats), &
    & STAT=errstat)

  ! Arrays for materials
  ALLOCATE( &
    & firstcell_mat(nummats), &
    & lastcell_mat(nummats), &
    & STAT=errstat)
  firstcell_mat(:) = 0_ik
  lastcell_mat(:)  = 0_ik

  ! Arrays for sources
  IF (numsrcs > 0_ik) THEN
    ALLOCATE( &
      & firstcell_src(numsrcs), &
      & lastcell_src(numsrcs), &
      & value_src(numsrcs,numgroups), &
      & STAT=errstat)
    firstcell_src(:) = 0_ik
    lastcell_src(:)  = 0_ik
    value_src(:,:)   = 0.0_rk
  ENDIF

  ! Arrays for mesh geometry
  ALLOCATE (&
    & numcellsinreg(numregs), &
    & widthcellsinreg(numregs), &
    & centre(numcells), &
    & origin(numcells), &
    & width(numcells), &
#ifdef CODETYPE
    & matnum(numcells), &
#endif
    & STAT=errstat)
  numcellsinreg(:)   = 0_ik
  widthcellsinreg(:) = 0.0_rk
  centre(:)          = 0.0_rk
  origin(:)          = 0.0_rk
  width(:)           = 0.0_rk
#ifdef CODETYPE
  matnum(:)          = 0_ik
#endif

  ! Arrays for sources and fluxes
  ALLOCATE( &
    & sigma_s(numcells,numgroups,numgroups), &
    & sigma_f(numcells,numgroups,numgroups), &
    & sigma_t(numcells,numgroups), &
    & scalflux(numcells,numgroups,0:2), &
    & scalflux_inner(numcells,0:numnodes), &
    & scalflux_outer(numcells,numgroups,0:numnodes), &
    & source_f(numcells,numgroups,numnodes), &
    & source_g(numcells,numnodes), &
    & source_i(numcells,numgroups), &
    & source_s(numcells,numnodes), &
    & source_t(numcells,numnodes), &
    & STAT=errstat)
  sigma_s(:,:,:)        = 0.0_rk
  sigma_f(:,:,:)        = 0.0_rk
  sigma_t(:,:)          = 0.0_rk
  scalflux(:,:,:)       = 0.0_rk
  scalflux_inner(:,:)   = 0.0_rk
  scalflux_outer(:,:,:) = 0.0_rk
  source_f(:,:,:)       = 0.0_rk
  source_g(:,:)         = 0.0_rk
  source_i(:,:)         = 0.0_rk
  source_s(:,:)         = 0.0_rk
  source_t(:,:)         = 0.0_rk

  ! Arrays by index direction
  ALLOCATE( &
    & mu(numdirs), &
    & wgt(numdirs), &
    & STAT=errstat)
  mu(:)  = 0.0_rk
  wgt(:) = 0.0_rk

  IF (errstat /= 0_ik) THEN
    WRITE(*,*) unitname, ': Error', errstat, 'allocating arrays'
    RETURN
  ENDIF

  RETURN
  END SUBROUTINE allocstor

END MODULE allocstor_mod
