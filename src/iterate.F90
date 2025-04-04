!! author: msleigh
!! date: 2002
!!
!! Controls iteration process

MODULE iterate_mod
  !! Handles iterative solution processes

PRIVATE
PUBLIC :: iterate

CONTAINS

  SUBROUTINE iterate
    !! Controls the iterative solution process
    !! Controls the iterative solution process

  ! STRUCTURE
  ! 1. Initialise variables
  ! 2. Set up iteration
  ! 3. Set up source
  ! 4. Do sweep
  ! 5. Test for convergence

  USE getkinds_mod
  USE setdata_mod
  USE fisssource_mod
  USE scatsource_mod
  USE updatesource_mod
  USE sweep_mod

  IMPLICIT NONE

  CHARACTER(LEN=7), PARAMETER :: unitname = 'ITERATE'

  ! Convergence
  LOGICAL          :: converged_inner
  LOGICAL          :: converged_outer
  REAL(KIND=rk)    :: error
  INTEGER(KIND=ik) :: outer_exit_stat

  ! k_effective
  REAL(KIND=rk) :: keffold

  ! Counters
  INTEGER(KIND=ik)                       :: j
  INTEGER(KIND=ik)                       :: group
  INTEGER(KIND=ik)                       :: node
  INTEGER(KIND=ik)                       :: outeriter
  INTEGER(KIND=ik), DIMENSION(numgroups) :: inneriter

  REAL(KIND=rk), PARAMETER :: eps = 1.e-24_rk

  WRITE(*,'(A)') &
    & '==============================================================================='
  WRITE(*,'(A)') 'ITERATIONS'
  WRITE(*,'(A)') &
    & '==============================================================================='
  WRITE(*,*)

  !----------------------------------------------------------------------------
  ! 1. Initialise variables
  !----------------------------------------------------------------------------

  keff = 1.0_rk

#ifdef SNES
  scalflux(:,:,0_ik) = 1.0_rk
  if (printflux > 1_ik) scalflux(:,:,1_ik) = 1.0_rk
  if (printflux > 1_ik) scalflux(:,:,2_ik) = 1.0_rk
#else
  scalflux(:,:,:) = 1.0_rk
#endif

  inneriter(:) = 0_ik
  outeriter    = 0_ik

  !----------------------------------------------------------------------------
  ! 2. Begin fission source (outer) iterations
  !----------------------------------------------------------------------------

  outer_exit_stat = -1_ik
  converged_outer = .FALSE.
  outeriter = 0_ik

  outer_loop: DO

    ! Increment outer iteration counter
    outeriter = outeriter + 1_ik
    IF (outeriter > imaxouter) THEN
      outer_exit_stat = 1_ik
      EXIT outer_loop
    ENDIF

    ! Store scalar flux from previous outer iteration
#ifdef SNES
    scalflux_outer(:,:,1_ik) = scalflux(:,:,0_ik)
#else
    scalflux_outer(:,:,:) = scalflux(:,:,:)
#endif

    IF (calctype == 1_ik) then

      keffold = keff

      ! Set up fission source and get k_effective
      CALL fisssource( &
        & keff)

      ! Scale fission source
      IF (keff < eps) THEN
        outer_exit_stat = 2_ik
        EXIT outer_loop
      ELSE
#ifdef SNES
        source_f(:,:,1_ik) = source_f(:,:,1_ik)/keff
#else
        source_f(:,:,:) = source_f(:,:,:)/keff
#endif

      ENDIF

    ENDIF

    !--------------------------------------------------------------------------
    ! 3. Begin loop over energy groups
    !--------------------------------------------------------------------------

    group_loop: DO group = 1_ik, numgroups

      ! Set group source equal to imposed source + fission source

      ! Imposed source (source_i) must be zero for eigenvalue calcs (calctype = 1)
      ! In the routine init_mesh.f, source_i is set equal to value_src
      ! If calctype is 1, readsrcs.f is not called, therefore value_src is zero

      ! Fission source (source_f) must be zero for flux calcs (calctype = 2)
      ! In the routine fisssource.f, source_f is constructed from fluxes & X-Ss
      ! If calctype is 2, fisssource.f is not called, therefore source_f is zero

#ifdef SNES
      source_g(:,1_ik) = source_f(:,group,1_ik) + source_i(:,group)
#else
    DO node = 1_ik, numnodes
      source_g(:,node) = source_f(:,group,node) + source_i(:,group)
    ENDDO
#endif

      call scatsource( &
        & group)

      !------------------------------------------------------------------------
      ! 4. Begin scattering source (inner) iterations
      !------------------------------------------------------------------------

      converged_inner = .FALSE.
      inneriter(group) = 0_ik

      inner_loop: DO

        ! Increment inner iteration counter
        inneriter(group) = inneriter(group) + 1_ik
        IF (inneriter(group) > imaxinner) then
          IF (calctype == 1_ik) then
            inneriter(group) = 1000_ik
            EXIT inner_loop
          ELSE
            WRITE(*,*) unitname, &
              & ': Inners not converged - increase parameter IMAXINNERS'
            WRITE(*,*)
            RETURN
          ENDIF
        ENDIF

        ! Save scalar flux from previous inner iteration
#ifdef SNES
        scalflux_inner(:,1_ik) = scalflux(:,group,0_ik)
#else
        scalflux_inner(:,:) = scalflux(:,group,:)
#endif

        ! Set up scattering source
        CALL updatesource( &
          & group)

        ! Calculate total sources
        DO node = 1_ik, numnodes
            source_t(:,node) = source_g(:,node) + source_s(:,node)
        ENDDO

        ! Do the sweep
#ifdef SNES
        scalflux(:,group,0_ik) = 0.0_rk
        IF (printflux > 1_ik) scalflux(:,group,1_ik) = 0.0_rk ! Left
        IF (printflux > 1_ik) scalflux(:,group,2_ik) = 0.0_rk ! Right
#else
      scalflux(:,group,:) = 0.0_rk
#endif
        CALL sweep( &
          & group)

        ! Test for inner convergence
        converged_inner = .TRUE.
        conv_loop_inner: DO j = 1_ik, numcells
#ifdef SNES
          error = ABS(1.0_rk-scalflux_inner(j,1_ik)/scalflux(j,group,0_ik))
#else
          error = ABS(1.0_rk-scalflux_inner(j,0_ik)/scalflux(j,group,0_ik))
#endif
          IF (error > epsinner) THEN
            converged_inner = .FALSE.
            EXIT conv_loop_inner
          ENDIF
        ENDDO conv_loop_inner

        IF (converged_inner) THEN
          EXIT inner_loop
        ENDIF

      !------------------------------------------------------------------------
      ! 5. End scattering source (inner) iterations
      !------------------------------------------------------------------------

      ENDDO inner_loop

    !--------------------------------------------------------------------------
    ! 6. End loop over energy groups
    !--------------------------------------------------------------------------

    ENDDO group_loop

    WRITE(*,'(A16,I3,A1)',ADVANCE='NO') 'OUTER ITERATION ', outeriter, '|'
    DO group = 1_ik, numgroups - 1_ik
      WRITE(*,'(I3)',ADVANCE='NO') inneriter(group)
    ENDDO
    WRITE(*,'(I3)',ADVANCE='YES') inneriter(numgroups)

    !--------------------------------------------------------------------------
    ! 7. Test for outer convergence
    !--------------------------------------------------------------------------

    IF (calctype == 1_ik) THEN
      converged_outer = .TRUE.
      conv_loop_outer: DO j = 1_ik, numcells
        DO group = 1_ik, numgroups
#ifdef SNES
          error = ABS(1.0_rk-scalflux_outer(j,group,1_ik)/scalflux(j,group,0_ik))
#else
          error = ABS(1.0_rk-scalflux_outer(j,group,0_ik)/scalflux(j,group,0_ik))
#endif
          IF (error > epsouter) THEN
            converged_outer = .FALSE.
            EXIT conv_loop_outer
          ENDIF
        ENDDO
      ENDDO conv_loop_outer

      IF (converged_outer) THEN
        IF (converged_inner) THEN
          WRITE(*,*)
          WRITE(*,'(A21,I4,A17)') 'Problem converged in ', outeriter, &
            & ' outer iterations'
          outer_exit_stat = 0_ik
          EXIT outer_loop
        ELSE
          WRITE(*,'(A)') 'Inners not converged - forcing another outer'
        ENDIF
      ENDIF
    ELSE
      EXIT outer_loop
    ENDIF

  !--------------------------------------------------------------------------
  ! 8. End fission source (outer) iterations
  !--------------------------------------------------------------------------

  ENDDO outer_loop
  WRITE(*,*)

  !----------------------------------------------------------------------------
  ! 9. Determine final keff
  !----------------------------------------------------------------------------

  SELECT CASE (outer_exit_stat)
    CASE(0_ik)
      CALL fisssource( &
        & keff)
      WRITE(*,'(A14,F12.8)') 'K EFFECTIVE = ', keff
    CASE(1_ik)
      WRITE(*,'(A32,I3,A17)') 'Convergence not achieved within ', imaxouter, &
        & ' outer iterations'
    CASE(2_ik)
      WRITE(*,'(A33)') 'Fission source is zero everywhere'
  END SELECT
  WRITE(*,*)

  RETURN
  END SUBROUTINE iterate

END MODULE iterate_mod
