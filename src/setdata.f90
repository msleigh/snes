!> \author msleigh
!!
!! PURPOSE: Declares global data

MODULE setdata_mod

USE getkinds_mod

IMPLICIT NONE

PRIVATE
PUBLIC :: mattype

!----------------------------------------------------------------------------
! 1. Declare parameters
!----------------------------------------------------------------------------

! Error codes
LOGICAL,          PUBLIC :: inputerror

! Iterations criteria
REAL(KIND=rk),    PUBLIC :: epsinner      ! Convergence criterion for inner iters
REAL(KIND=rk),    PUBLIC :: epsouter      ! Convergence criterion for inner iters
INTEGER(KIND=ik), PUBLIC :: imaxinner     ! Maximum no. of inner iterations
INTEGER(KIND=ik), PUBLIC :: imaxouter     ! Maximum no. of outer iterations

! Switches
INTEGER(KIND=ik), PUBLIC :: calctype      ! Calculation type
                                          !   1 = eigenvalue
                                          !   2 = fixed-source
LOGICAL,          PUBLIC :: forceinners   ! Force inner iters to converge
LOGICAL,          PUBLIC :: longmeshprint ! Long or short print of mesh
LOGICAL,          PUBLIC :: macro         ! Flag for macroscopic cross-sections
LOGICAL,          PUBLIC :: nffu          ! Flag for negative flux fix-up
INTEGER(KIND=ik), PUBLIC :: printflux     ! Flux output control
                                          !   0 = no flux print
                                          !   1 = cell-centred fluxes
                                          !   2 = cell edge & centre fluxes
                                          !   3 = both flux print options
LOGICAL,          PUBLIC :: printmatnum   ! Material number print
LOGICAL,          PUBLIC :: printnucdat   ! Nuclear data print
CHARACTER(LEN=8), PUBLIC :: quadset

! Boundary conditions
INTEGER(KIND=ik), PUBLIC :: lhbc          ! Left boundary condition (0 = reflective)
REAL(KIND=rk),    PUBLIC :: lh_eflux      ! Left incoming ang flux (if transmissive)
REAL(KIND=rk),    PUBLIC :: rh_eflux      ! Right-hand incoming ang flux

! Discretisation parameters
INTEGER(KIND=ik), PUBLIC :: numcells      ! Total number of cells
INTEGER(KIND=ik), PUBLIC :: numdirs       ! Number of positive directions
INTEGER(KIND=ik), PUBLIC :: numgroups     ! Number of energy groups
INTEGER(KIND=ik), PUBLIC :: numnodes      ! Number of nodes in cell
                                          !   1 = diamond difference
                                          !   2 = linear discontinuous (1 = left, 2 = right)

! Problem set-up
INTEGER(KIND=ik), PUBLIC :: nummats       ! Number of materials
INTEGER(KIND=ik), PUBLIC :: numregs       ! Number of regions
INTEGER(KIND=ik), PUBLIC :: numsrcs       ! Number of imposed sources
REAL(KIND=rk),    PUBLIC :: xmin          ! Co-ordinate of left-hand boundary

! Calculated quantities
REAL(KIND=rk),    PUBLIC :: keff          ! Criticality eigenvalue
INTEGER(KIND=rk), PUBLIC :: nffu_call     ! No of calls to nffu algorithm
REAL(KIND=rk),    PUBLIC :: xmax          ! Co-ordinate of right-hand boundary

! Internal parameters
INTEGER(KIND=ik), PARAMETER, PUBLIC :: numsweeps = 2_ik

!----------------------------------------------------------------------------
! 2. Declare arrays
!----------------------------------------------------------------------------

! Material array of derived type. This 1D structure holds attributes of
! each material listed in keyword file. These are:
!     arbitrary ID no. for the material (user-defined)
!     density (g/cm^-3)
!     atomic weight (g/mol)
!     microscopic total cross-sections (barns)
!     microscopic fission cross-sections (barns)
!     microscopic scattering cross-sections (barns)
!     macroscopic total cross-sections (cm^-1)
!     macroscopic fission cross-sections (cm^-1)
!     macroscopic scattering cross-sections (cm^-1)
! IDs and densities read in from keyword file, microscopic cross-sections
! from nuclear data file (whose filename is also listed in keyword
! file but not stored), and macroscopic cross-sections are calculated.

TYPE mattype
  INTEGER(KIND=rk)                       :: mat_id
  REAL(KIND=rk)                          :: rho
  REAL(KIND=rk)                          :: atomweight
  REAL(KIND=rk), DIMENSION(:),   POINTER :: microxstot
  REAL(KIND=rk), DIMENSION(:,:), POINTER :: microxsfiss
  REAL(KIND=rk), DIMENSION(:,:), POINTER :: microxsscat
  REAL(KIND=rk), DIMENSION(:),   POINTER :: macroxstot
  REAL(KIND=rk), DIMENSION(:,:), POINTER :: macroxsfiss
  REAL(KIND=rk), DIMENSION(:,:), POINTER :: macroxsscat
END TYPE mattype

TYPE(mattype),    ALLOCATABLE, DIMENSION(:),     PUBLIC :: mats

INTEGER(KIND=ik), ALLOCATABLE, DIMENSION(:),     PUBLIC :: firstcell_mat
INTEGER(KIND=ik), ALLOCATABLE, DIMENSION(:),     PUBLIC :: lastcell_mat

INTEGER(KIND=ik), ALLOCATABLE, DIMENSION(:),     PUBLIC :: firstcell_src
INTEGER(KIND=ik), ALLOCATABLE, DIMENSION(:),     PUBLIC :: lastcell_src
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:),   PUBLIC :: value_src

INTEGER(KIND=ik), ALLOCATABLE, DIMENSION(:),     PUBLIC :: numcellsinreg
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:),     PUBLIC :: widthcellsinreg
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:),     PUBLIC :: centre
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:),     PUBLIC :: origin
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:),     PUBLIC :: width
INTEGER(KIND=ik), ALLOCATABLE, DIMENSION(:),     PUBLIC :: matnum

REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:,:), PUBLIC :: sigma_s
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:,:), PUBLIC :: sigma_f
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:),   PUBLIC :: sigma_t

! Arrays to store total, scattering and fission sources

REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:,:), PUBLIC :: source_f
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:),   PUBLIC :: source_g
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:),   PUBLIC :: source_i
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:),   PUBLIC :: source_s
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:),   PUBLIC :: source_t

REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:,:), PUBLIC :: scalflux
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:),   PUBLIC :: scalflux_inner
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:,:,:), PUBLIC :: scalflux_outer

REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:),     PUBLIC :: mu
REAL(KIND=rk),    ALLOCATABLE, DIMENSION(:),     PUBLIC :: wgt

END MODULE setdata_mod
