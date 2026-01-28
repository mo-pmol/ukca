! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description:
!   Module providing subroutines to put GLOMAP-mode diagnostics into
!   the STASHwork array
!
! Part of the UKCA model, a community model supported by the
! Met Office and NCAS, with components provided initially
! by The University of Cambridge, University of Leeds,
! University of Oxford, and the Met. Office.
! See www.ukca.ac.uk
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: UKCA
!
! Code Description:
!   Language:  FORTRAN 90
!   This code is written to UMDP3 v8 programming standards.
!
! ----------------------------------------------------------------------
!
MODULE ukca_mode_diags_mod

USE ukca_config_constants_mod, ONLY: avogadro, boltzmann
USE ukca_constants,         ONLY: mmw, m_air
USE ukca_mode_setup,        ONLY: nmodes, ncp_max,                             &
                                  cp_su, cp_bc, cp_oc, cp_cl, cp_du,           &
                                  cp_no3, cp_nh4, cp_nn, cp_mp
USE ukca_config_specification_mod, ONLY: glomap_variables
USE ukca_fieldname_mod, ONLY: diagname_pm10_dry, diagname_pm2p5_dry,           &
                              diagname_pm10_wet, diagname_pm2p5_wet,           &
                              diagname_pm10_bc, diagname_pm2p5_bc,             &
                              diagname_pm10_oc, diagname_pm2p5_oc,             &
                              diagname_pm10_so4, diagname_pm2p5_so4,           &
                              diagname_pm10_du, diagname_pm2p5_du,             &
                              diagname_pm10_ss, diagname_pm2p5_ss,             &
                              diagname_pm10_nh4, diagname_pm2p5_nh4,           &
                              diagname_pm10_no3, diagname_pm2p5_no3,           &
                              diagname_pm10_nn, diagname_pm2p5_nn,             &
                              diagname_pm10_mp, diagname_pm2p5_mp,             &
                              maxlen_diagname
USE errormessagelength_mod, ONLY: errormessagelength
USE ereport_mod,            ONLY: ereport
USE umPrintMgr,             ONLY: umPrint, umMessage
USE parkind1,               ONLY: jprb, jpim      ! for Dr Hook tracing
USE yomhook,                ONLY: lhook, dr_hook  ! for Dr Hook tracing

IMPLICIT NONE
PRIVATE

CHARACTER(LEN=*), PARAMETER :: ModuleName='UKCA_MODE_DIAGS_MOD'

LOGICAL, SAVE, PUBLIC :: l_ukca_cmip6_diags=.FALSE.
! Set to true to enable CMIP6 diagnostics. This is done by the parent via
! direct use of this module pending the addition of full diagnostic support to
! the UKCA API

LOGICAL, SAVE, PUBLIC :: l_ukca_pm_diags=.FALSE.
! Set to true to enable PM10 or PM2.5 38 diagnostics. This is done by the
! parent via direct use of this module pending the addition of full diagnostic
! support to the UKCA API

REAL, ALLOCATABLE, PUBLIC :: mdwat_diag(:,:)
! Molecular concentration of water in each mode (molecules per particle)

REAL, ALLOCATABLE, PUBLIC :: wetdp_diag(:,:)
! Geometric mean wet diameter of particles in each mode (m)

INTEGER, PARAMETER, PUBLIC :: n_size_cat = 2  ! Number of PM size categories
                                              ! (1=PM10, 2=PM2.5)
INTEGER, PARAMETER, PUBLIC :: n_diag_cp = 9   ! Number of component diagnostics
                                       ! available for each PM size category

! Structure to hold requested PM diagnostics CF names
TYPE, PUBLIC :: pm_diag_struct
  CHARACTER(LEN=maxlen_diagname) :: diagname_total_dry(n_size_cat)
                                             ! Diagnostic names of total PM dry mass
  CHARACTER(LEN=maxlen_diagname) :: diagname_total_wet(n_size_cat)
                                             ! Diagnostic names of total PM wet mass
  CHARACTER(LEN=maxlen_diagname) :: diagname_component(n_diag_cp,n_size_cat)
                                             ! Diagnostic names of contribution to PM
  INTEGER :: i_ref_component(n_diag_cp) ! Reference component number
END TYPE pm_diag_struct

TYPE(pm_diag_struct), PARAMETER, PUBLIC :: pm_diag = pm_diag_struct(           &
  [diagname_pm10_dry, diagname_pm2p5_dry],                                     &
  [diagname_pm10_wet, diagname_pm2p5_wet],                                     &
  RESHAPE([diagname_pm10_so4,   diagname_pm10_bc, diagname_pm10_oc,            &
            diagname_pm10_ss,   diagname_pm10_du, diagname_pm10_no3,           &
            diagname_pm10_nn,   diagname_pm10_nh4, diagname_pm10_mp,           &
            diagname_pm2p5_so4, diagname_pm2p5_bc, diagname_pm2p5_oc,          &
            diagname_pm2p5_ss,  diagname_pm2p5_du, diagname_pm2p5_no3,         &
            diagname_pm2p5_nn,  diagname_pm2p5_nh4, diagname_pm2p5_mp],        &
            [n_diag_cp,2]),                                                    &
          [cp_su, cp_bc, cp_oc, cp_cl, cp_du, cp_no3, cp_nn, cp_nh4, cp_mp])

! Structure to hold the indices of PM diagnostics requested
TYPE, PUBLIC :: i_diag_req_struct
  INTEGER, ALLOCATABLE :: dry(:,:)
  INTEGER, ALLOCATABLE :: wet(:,:)
  INTEGER, ALLOCATABLE :: component(:,:,:)
END TYPE i_diag_req_struct

PUBLIC :: ukca_mode_diags_alloc
PUBLIC :: ukca_mode_diags
PUBLIC :: ukca_mode_diags_pm_req

CONTAINS

! ----------------------------------------------------------------------
SUBROUTINE ukca_mode_diags_alloc(nbox)
! Description:
!   Allocate arrays for copies of water content and wet diameter fields
!   that are provided by the aerosol scheme and required as input to
!   diagnostic calculations in UKCA_MODE_DIAGS.
! ----------------------------------------------------------------------

IMPLICIT NONE

! Number of elements
INTEGER, INTENT(IN) :: nbox

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='UKCA_MODE_DIAGS_ALLOC'


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Allocate diagnostic field for copy of aerosol water array if required
IF (.NOT. ALLOCATED(mdwat_diag) .AND.                                          &
    (l_ukca_cmip6_diags .OR. l_ukca_pm_diags))                                 &
  ALLOCATE(mdwat_diag(nbox,nmodes))
! Allocate diagnostic field for copy of wet particle diameter array if required
IF (.NOT. ALLOCATED(wetdp_diag) .AND. l_ukca_pm_diags)                         &
  ALLOCATE(wetdp_diag(nbox,nmodes))

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE ukca_mode_diags_alloc


! ----------------------------------------------------------------------
SUBROUTINE ukca_mode_diags(error_code_ptr,                                     &
                           row_length, rows, model_levels,                     &
                           nbox, n_mode_tracers,                               &
                           p_theta_levels,                                     &
                           t_theta_levels, mode_tracers,                       &
                           interf_z, len_stashwork38, stashwork38,             &
                           diagnostics, pm_request, i_diag_req,                &
                           error_message, error_routine)
! Description:
!   Obtain number densities and component material concentrations for
!   each mode to use in calculating diagnostics, derive the required
!   diagnostics and put them in the STASHwork array.
! ----------------------------------------------------------------------

USE ukca_mode_tracer_maps_mod, ONLY: nmr_index, mmr_index
USE asad_mod,                  ONLY: jpctr
USE ukca_types_mod,            ONLY: log_small
USE ukca_diagnostics_type_mod, ONLY: diagnostics_type
USE ukca_pm_diags_mod,         ONLY: pm_request_struct
USE ukca_error_mod, ONLY: maxlen_message, maxlen_procname

IMPLICIT NONE

INTEGER, POINTER, INTENT(IN) :: error_code_ptr

! UKCA domain dimensions
INTEGER, INTENT(IN) :: row_length
INTEGER, INTENT(IN) :: rows
INTEGER, INTENT(IN) :: model_levels

! Number of elements
INTEGER, INTENT(IN) :: nbox

! Number of MODE tracers
INTEGER, INTENT(IN) :: n_mode_tracers

! Pressure on theta levels
REAL, INTENT(IN)    :: p_theta_levels(row_length, rows, model_levels)

! Temperature on theta levels
REAL, INTENT(IN)    :: t_theta_levels(row_length, rows, model_levels)

! MODE tracer array
REAL, INTENT(IN)    :: mode_tracers(row_length, rows, model_levels,            &
                                    n_mode_tracers)

! Height of interface levels above surface (m)
REAL, INTENT(IN)    :: interf_z(row_length, rows, 0:model_levels)

! Length of diagnostics array
INTEGER, INTENT(IN) :: len_stashwork38

! Work array for STASH
REAL, INTENT(IN OUT) :: stashwork38(len_stashwork38)

! Type to hold data for servicing diagnostic requests
TYPE(diagnostics_type), INTENT(IN OUT) :: diagnostics

! Diagnostics request structure
TYPE(pm_request_struct), INTENT(IN OUT) :: pm_request
TYPE(i_diag_req_struct), INTENT(IN OUT) :: i_diag_req

CHARACTER(LEN=maxlen_message), OPTIONAL, INTENT(OUT) :: error_message
CHARACTER(LEN=maxlen_procname), OPTIONAL, INTENT(OUT) :: error_routine

! Local variables

! Caution - pointers to TYPE glomap_variables%
!           have been included here to make the code easier to read
!           take care when making changes involving pointers
LOGICAL, POINTER :: component(:,:)
REAL,    POINTER :: mfrac_0(:,:)
REAL,    POINTER :: mm(:)
REAL,    POINTER :: mmid(:)
LOGICAL, POINTER :: mode(:)
INTEGER, POINTER :: ncp

INTEGER :: ifirst         ! index of first mode tracer in nmr_index, mmr_index
INTEGER :: imode          ! loop counter for modes
INTEGER :: icp            ! loop counter for components
INTEGER :: itra           ! tracer number
INTEGER :: icode          ! error code
REAL    :: pmid(nbox)     ! Air pressure at mid-point (Pa)
REAL    :: tmid(nbox)     ! Temperature at mid-point (K)
REAL    :: tr_rs(nbox)    ! Local variable to hold re-shaped aerosol tracers
REAL    :: aird(nbox)     ! Number density of air (cm^-3)
REAL    :: nd(nbox,nmodes)! Aerosol particle number density for mode (cm^-3)
REAL    :: md(nbox,nmodes,glomap_variables%ncp)
                          ! Average component concentration of aerosol particle
                          ! in mode (molecules.particle^-1)
LOGICAL (KIND=log_small) :: mask1(nbox)              ! To mask negatives
CHARACTER(LEN=errormessagelength)   :: cmessage      ! Error return message

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='UKCA_MODE_DIAGS'


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

error_code_ptr = 0
IF (PRESENT(error_message)) error_message = ''
IF (PRESENT(error_routine)) error_routine = ''

! Caution - pointers to TYPE glomap_variables%
!           have been included here to make the code easier to read
!           take care when making changes involving pointers
component   => glomap_variables%component
mfrac_0     => glomap_variables%mfrac_0
mm          => glomap_variables%mm
mmid        => glomap_variables%mmid
mode        => glomap_variables%mode
ncp         => glomap_variables%ncp

icode = 0

! -------------------------------------------------------------
! Check for availability of diagnostic fields required as input
! -------------------------------------------------------------

IF (.NOT. ALLOCATED(mdwat_diag)) THEN
  icode = 1
  cmessage = ' Aerosol water content diagnostics not found'
  WRITE(umMessage,'(A70)') cmessage
  CALL umPrint(umMessage,src=RoutineName)
  CALL ereport(RoutineName,icode,cmessage)
END IF

IF (.NOT. ALLOCATED(wetdp_diag) .AND. l_ukca_pm_diags) THEN
  icode = 3
  cmessage = ' Wet particle diameter diagnostics not found'
  WRITE(umMessage,'(A70)') cmessage
  CALL umPrint(umMessage,src=RoutineName)
  CALL ereport(RoutineName,icode,cmessage)
END IF

! ----------------------------------------------
! Obtain ND and MD arrays from mode tracer array
! ----------------------------------------------

tmid(:)=RESHAPE(t_theta_levels(:,:,:),[nbox])
pmid(:)=RESHAPE(p_theta_levels(:,:,:),[nbox])

! calculate molecular concentration of air (/cm3)
aird(:)=pmid(:)/(tmid(:)*boltzmann*1.0e6)

!  Find index of 1st mode tracer, as nmr_index and
!   mmr_index index all ukca tracers
ifirst = jpctr + 1
DO imode=1,nmodes
  IF (mode(imode)) THEN
    itra = nmr_index(imode) - ifirst + 1
    tr_rs(:)=RESHAPE(mode_tracers(:,:,:,itra),[nbox])
    mask1(:)=(tr_rs(:) < 0.0)
    WHERE (mask1(:))
      tr_rs(:)=0.0
    END WHERE
    ! .. above sets tr_rs to zero if negative
    nd(:,imode)=tr_rs(:)*aird(:)
    ! .. above sets ND (particles per cc) from advected number-mixing-ratio

    DO icp=1,ncp
      IF (component(imode,icp)) THEN
        itra = mmr_index(imode,icp) - ifirst + 1
        tr_rs(:)=RESHAPE(mode_tracers(:,:,:,itra),[nbox])
        mask1(:)=(tr_rs(:) < 0.0)
        WHERE (mask1(:))
          tr_rs(:)=0.0
        END WHERE
        ! .. above sets tr_rs to zero if negative
        !        mask1(:)=(nd(:,imode) > num_eps(imode))
        mask1(:)=(nd(:,imode) > 1e-30)
        WHERE (mask1(:))
          md(:,imode,icp)=(m_air/mm(icp))*aird(:)*tr_rs(:)/nd(:,imode)
        ELSE WHERE
          md(:,imode,icp)=mmid(imode)*mfrac_0(imode,icp)
        END WHERE
        ! above sets MD (molecules per particle) from advected mass-mix-ratio
        ! .. note that only "trusts" values where ND>NUM_EPS
      ELSE
        md(:,imode,icp)=0.0
      END IF
    END DO    ! loop over cpts

  END IF    ! mode

END DO  ! loop over modes

! --------------------------------------------------------------------
! Calculate required diagnostics and copy them to the STASHwork array
! --------------------------------------------------------------------

IF (l_ukca_cmip6_diags) THEN
  CALL mode_diags_cmip6(row_length, rows, model_levels,                        &
                        nbox, aird, nd, md, interf_z,                          &
                        len_stashwork38, stashwork38)
END IF

IF (l_ukca_pm_diags) THEN
  CALL mode_diags_pm(error_code_ptr,                                           &
                     row_length, rows, model_levels,                           &
                     nbox, nd, md, diagnostics,                                &
                     pm_request, i_diag_req,                                   &
                     error_message=error_message,                              &
                     error_routine=error_routine)

  IF (error_code_ptr > 0) THEN
    IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
    RETURN
  END IF
END IF

! --------------------------------------------------------------------
! Deallocate arrays
! --------------------------------------------------------------------
IF (ALLOCATED(wetdp_diag)) DEALLOCATE(wetdp_diag)
IF (ALLOCATED(mdwat_diag)) DEALLOCATE(mdwat_diag)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN

END SUBROUTINE ukca_mode_diags


! ----------------------------------------------------------------------
SUBROUTINE mode_diags_cmip6(row_length, rows, model_levels, nbox, aird,        &
                            nd, md, interf_z, len_stashwork38, stashwork38)
! Description:
!   Calculate the required CMIP6 diagnostics and copy them to the
!   STASHwork array.
! ----------------------------------------------------------------------

USE ukca_um_legacy_mod, ONLY:                                                  &
  stashcode_so4_nuc_sol,  stashcode_so4_ait_sol,  stashcode_so4_acc_sol,       &
  stashcode_so4_cor_sol,  stashcode_bc_ait_sol,   stashcode_bc_acc_sol,        &
  stashcode_bc_cor_sol,   stashcode_bc_ait_insol, stashcode_oc_nuc_sol,        &
  stashcode_oc_nuc_sol,   stashcode_oc_ait_sol,   stashcode_oc_acc_sol,        &
  stashcode_oc_cor_sol,   stashcode_oc_ait_insol, stashcode_ss_acc_sol,        &
  stashcode_ss_cor_sol,   stashcode_du_acc_sol,   stashcode_du_cor_sol,        &
  stashcode_du_acc_insol, stashcode_du_cor_insol, stashcode_du_sup_insol,      &
  stashcode_n_nuc_sol,    stashcode_n_ait_sol,    stashcode_n_acc_sol,         &
  stashcode_n_cor_sol,    stashcode_n_ait_insol,  stashcode_n_acc_insol,       &
  stashcode_n_cor_insol,  stashcode_n_sup_insol,                               &
  stashcode_h2o_nuc_sol,  stashcode_h2o_ait_sol,  stashcode_h2o_acc_sol,       &
  stashcode_h2o_cor_sol,  stashcode_h2o_total,    stashcode_so4_nuc_sol_load,  &
  stashcode_so4_ait_sol_load,  stashcode_so4_acc_sol_load,                     &
  stashcode_so4_cor_sol_load,  stashcode_so4_total_load,                       &
  stashcode_bc_ait_sol_load,   stashcode_bc_acc_sol_load,                      &
  stashcode_bc_cor_sol_load,   stashcode_bc_ait_insol_load,                    &
  stashcode_bc_total_load,     stashcode_oc_nuc_sol_load,                      &
  stashcode_oc_ait_sol_load,   stashcode_oc_acc_sol_load,                      &
  stashcode_oc_cor_sol_load,   stashcode_oc_ait_insol_load,                    &
  stashcode_oc_total_load,     stashcode_ss_acc_sol_load,                      &
  stashcode_ss_cor_sol_load,   stashcode_ss_total_load,                        &
  stashcode_du_acc_sol_load,   stashcode_du_cor_sol_load,                      &
  stashcode_du_acc_insol_load, stashcode_du_cor_insol_load,                    &
  stashcode_du_sup_insol_load, stashcode_du_total_load,                        &
  stashcode_h2o_nuc_sol_load,  stashcode_h2o_ait_sol_load,                     &
  stashcode_h2o_acc_sol_load,  stashcode_h2o_cor_sol_load,                     &
  stashcode_h2o_total_load,    stashcode_h2o_mmr,                              &
  stashcode_nh4_ait_sol, stashcode_nh4_acc_sol,                                &
  stashcode_nh4_cor_sol, stashcode_no3_ait_sol,                                &
  stashcode_no3_acc_sol, stashcode_no3_cor_sol,                                &
  stashcode_nn_acc_sol , stashcode_nn_cor_sol ,                                &
  stashcode_nh4_ait_sol_load,                                                  &
  stashcode_nh4_acc_sol_load, stashcode_nh4_cor_sol_load,                      &
  stashcode_no3_ait_sol_load,                                                  &
  stashcode_no3_acc_sol_load, stashcode_no3_cor_sol_load,                      &
  stashcode_nn_acc_sol_load , stashcode_nn_cor_sol_load ,                      &
  stashcode_nh4_total_load, stashcode_no3_total_load, stashcode_nn_total_load, &
  stashcode_mp_ait_sol,   stashcode_mp_acc_sol,   stashcode_mp_cor_sol,        &
  stashcode_mp_ait_insol, stashcode_mp_acc_insol, stashcode_mp_cor_insol,      &
  stashcode_mp_sup_insol, stashcode_mp_sup_insol_load,                         &
  stashcode_mp_ait_sol_load,   stashcode_mp_acc_sol_load,                      &
  stashcode_mp_cor_sol_load,   stashcode_mp_ait_insol_load,                    &
  stashcode_mp_acc_insol_load, stashcode_mp_cor_insol_load,                    &
  stashcode_mp_total_load,                                                     &
  len_stlist, stindex, stlist, num_stash_levels, stash_levels, si, sf,         &
  si_last, stashcode_glomap_sec,                                               &
  copydiag, copydiag_3d

IMPLICIT NONE

! UKCA domain dimensions
INTEGER, INTENT(IN) :: row_length
INTEGER, INTENT(IN) :: rows
INTEGER, INTENT(IN) :: model_levels

! Number of elements
INTEGER, INTENT(IN) :: nbox

! Number density of air (cm^-3)
REAL, INTENT(IN)    :: aird(nbox)

! Aerosol particle number density for mode (cm^-3)
REAL, INTENT(IN)    :: nd(nbox,nmodes)

! Average component concentration of aerosol particle in mode
! (molecules.particle^-1)
REAL, INTENT(IN)    :: md(nbox,nmodes,glomap_variables%ncp)

! Height of interface levels above surface (m)
REAL, INTENT(IN)    :: interf_z(row_length, rows, 0:model_levels)

! Diagnostics array
INTEGER, INTENT(IN) :: len_stashwork38

! Work array for STASH
REAL, INTENT(IN OUT) :: stashwork38(len_stashwork38)

! Local variables

! Caution - pointers to TYPE glomap_variables%
!           have been included here to make the code easier to read
!           take care when making changes involving pointers
LOGICAL, POINTER :: component(:,:)
REAL,    POINTER :: mm (:)
LOGICAL, POINTER :: mode (:)
INTEGER, POINTER :: modesol(:)
INTEGER, POINTER :: ncp

! Table to locate STASH item numbers from mode and component
! Final components are unused (SO, NO3, NH4, Na, Cl) currently
INTEGER, PARAMETER :: item_component_cmip6(nmodes,ncp_max) =                   &
  RESHAPE( [                                                                   &
! SO4
  stashcode_so4_nuc_sol, stashcode_so4_ait_sol, stashcode_so4_acc_sol,         &
  stashcode_so4_cor_sol, -1,                    -1,                            &
  -1                   , -1,                                                   &
! BC
  -1,                    stashcode_bc_ait_sol,  stashcode_bc_acc_sol,          &
  stashcode_bc_cor_sol,  stashcode_bc_ait_insol,-1,                            &
  -1                   , -1,                                                   &
! OC
  stashcode_oc_nuc_sol,  stashcode_oc_ait_sol,  stashcode_oc_acc_sol,          &
  stashcode_oc_cor_sol,  stashcode_oc_ait_insol,-1,                            &
  -1                  ,  -1,                                                   &
! NaCl
  -1,                    -1,                    stashcode_ss_acc_sol,          &
  stashcode_ss_cor_sol,  -1,                    -1,                            &
  -1                  ,  -1,                                                   &
! Dust
  -1,                    -1,                    stashcode_du_acc_sol,          &
  stashcode_du_cor_sol,  -1,                    stashcode_du_acc_insol,        &
  stashcode_du_cor_insol, stashcode_du_sup_insol,                              &
! Secondary organic
  -1,                    -1,                    -1,                            &
  -1,                    -1,                    -1,                            &
  -1,                    -1,                                                   &
! NO3
  -1,                    stashcode_no3_ait_sol, stashcode_no3_acc_sol,         &
  stashcode_no3_cor_sol,                    -1,                    -1,         &
  -1                   , -1,                                                   &
! NaNO3
  -1,                                       -1,  stashcode_nn_acc_sol,         &
   stashcode_nn_cor_sol,                    -1,                    -1,         &
  -1                   , -1,                                                   &
! NH4
  -1,                    stashcode_nh4_ait_sol, stashcode_nh4_acc_sol,         &
  stashcode_nh4_cor_sol,                    -1,                    -1,         &
  -1                   , -1,                                                   &
  ! Microplastics
  -1,                    stashcode_mp_ait_sol,  stashcode_mp_acc_sol,          &
  stashcode_mp_cor_sol,  stashcode_mp_ait_insol,stashcode_mp_acc_insol,        &
  stashcode_mp_cor_insol,stashcode_mp_sup_insol                                &
  ], [nmodes, ncp_max])

! Table to locate STASH item numbers for mode and component loads
! Final components are unused (SO,NO3,NH4,Na,Cl) currently
INTEGER, PARAMETER :: item_load_cmip6(nmodes,ncp_max) =                        &
  RESHAPE( [                                                                   &
! SO4
  stashcode_so4_nuc_sol_load, stashcode_so4_ait_sol_load,                      &
  stashcode_so4_acc_sol_load, stashcode_so4_cor_sol_load,                      &
  -1,                         -1,                                              &
  -1,                         -1,                                              &
! BC
  -1,                          stashcode_bc_ait_sol_load,                      &
  stashcode_bc_acc_sol_load,   stashcode_bc_cor_sol_load,                      &
  stashcode_bc_ait_insol_load, -1,                                             &
  -1,                          -1,                                             &
! OC
  stashcode_oc_nuc_sol_load  , stashcode_oc_ait_sol_load,                      &
  stashcode_oc_acc_sol_load  , stashcode_oc_cor_sol_load,                      &
  stashcode_oc_ait_insol_load, -1,                                             &
  -1,                          -1,                                             &
! NaCl
  -1,                          -1,                                             &
  stashcode_ss_acc_sol_load,   stashcode_ss_cor_sol_load,                      &
  -1,                          -1,                                             &
  -1,                          -1,                                             &
! Dust
  -1,                          -1,                                             &
  stashcode_du_acc_sol_load,   stashcode_du_cor_sol_load,                      &
  -1,                          stashcode_du_acc_insol_load,                    &
  stashcode_du_cor_insol_load, stashcode_du_sup_insol_load,                    &
! SO
  -1,                         -1,                                              &
  -1,                         -1,                                              &
  -1,                         -1,                                              &
  -1,                         -1,                                              &
!  NO3
  -1,                         stashcode_no3_ait_sol_load,                      &
  stashcode_no3_acc_sol_load, stashcode_no3_cor_sol_load,                      &
  -1,                         -1,                                              &
  -1,                         -1,                                              &
!  NaNO3
  -1,                                                 -1,                      &
   stashcode_nn_acc_sol_load,  stashcode_nn_cor_sol_load,                      &
  -1,                         -1,                                              &
  -1,                         -1,                                              &
!  NH4
  -1,                         stashcode_nh4_ait_sol_load,                      &
  stashcode_nh4_acc_sol_load, stashcode_nh4_cor_sol_load,                      &
  -1,                         -1,                                              &
  -1,                         -1,                                              &
! Microplastics
  -1,                          stashcode_mp_ait_sol_load,                      &
  stashcode_mp_acc_sol_load,   stashcode_mp_cor_sol_load,                      &
  stashcode_mp_ait_insol_load, stashcode_mp_acc_insol_load,                    &
  stashcode_mp_cor_insol_load, stashcode_mp_sup_insol_load                     &
  ], [nmodes, ncp_max])

! Table to locate STASH item numbers for aerosol water density
INTEGER :: item_water_cmip6(nmodes) = [                                        &
  stashcode_h2o_nuc_sol, stashcode_h2o_ait_sol, stashcode_h2o_acc_sol,         &
  stashcode_h2o_cor_sol, -1,                    -1,                            &
  -1,                    -1  ]

! Table to locate STASH item numbers for aerosol water loads
INTEGER :: item_water_load_cmip6(nmodes) = [                                   &
  stashcode_h2o_nuc_sol_load, stashcode_h2o_ait_sol_load,                      &
  stashcode_h2o_acc_sol_load, stashcode_h2o_cor_sol_load,                      &
  -1,                         -1,                                              &
  -1,                         -1  ]

! Table to locate STASH item numbers for number density
INTEGER :: item_number_cmip6(nmodes) =                                         &
 [stashcode_n_nuc_sol,   stashcode_n_ait_sol,   stashcode_n_acc_sol,           &
   stashcode_n_cor_sol,   stashcode_n_ait_insol, stashcode_n_acc_insol,        &
   stashcode_n_cor_insol, stashcode_n_sup_insol]

! Table to locate STASH item numbers for total aerosol loads
! Positions for SO, NO3, and NH4 unused currently
INTEGER, PARAMETER :: item_aerosol_load_cmip6(ncp_max+1) = [                   &
  stashcode_so4_total_load, stashcode_bc_total_load,                           &
  stashcode_oc_total_load,  stashcode_ss_total_load,                           &
  stashcode_du_total_load,  -1,                                                &
  stashcode_no3_total_load, stashcode_nh4_total_load,                          &
  stashcode_nn_total_load, stashcode_mp_total_load,                            &
  stashcode_h2o_total_load  ]

INTEGER :: n_soluble      ! No of soluble modes
INTEGER :: section        ! stash section
INTEGER :: tsection       ! stash section * 1000
INTEGER :: item           ! stash item
INTEGER :: imode          ! loop counter for modes
INTEGER :: icp            ! loop counter for components
INTEGER :: icp2           ! loop counter for H2O load
INTEGER :: icode          ! error code
INTEGER :: im_index       ! internal model index
INTEGER :: k              ! loop counter
CHARACTER(LEN=errormessagelength)   :: cmessage      ! Error return message

REAL, PARAMETER :: cm3_per_m3 = 1.0e6       ! cm^3 per m^3

REAL    :: field(nbox)                      ! Output field (1D)
REAL    :: field3d(row_length,rows,model_levels)   ! Output field (3D)
REAL    :: dz(row_length,rows,model_levels) ! Depth of each layer (m)

REAL    :: aerosol_component_density(row_length,rows,model_levels,             &
                                     nmodes,glomap_variables%ncp+1)
                                            ! aerosol cpnt. density (kg.m^-3)
REAL    :: aerosol_number_density(row_length,rows,model_levels,nmodes)
                                            ! aerosol no. density (m^-3)
REAL    :: aerosol_component_load(row_length,rows,nmodes,                      &
                                            glomap_variables%ncp+1)
                                            ! integrated aerosol load (kg.m^-2)
REAL    :: aerosol_total_load(row_length,rows,                                 &
                                            glomap_variables%ncp+1)
                                            ! total aerosol load of each
                                            ! component + h2o (kg.m^-2)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='MODE_DIAGS_CMIP6'


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

! Caution - pointers to TYPE glomap_variables%
!           have been included here to make the code easier to read
!           take care when making changes involving pointers
component   => glomap_variables%component
mm          => glomap_variables%mm
mode        => glomap_variables%mode
modesol     => glomap_variables%modesol
ncp         => glomap_variables%ncp

im_index = 1
icode = 0

!  -------------------------------------------------------
!  CMIP6 Diagnostics for aerosol mass and number densities
!  -------------------------------------------------------

section = stashcode_glomap_sec
tsection = stashcode_glomap_sec*1000
n_soluble = SUM(modesol)

! -----------------------------------------------------
! Calculate aerosol component and number density arrays
! -----------------------------------------------------

aerosol_component_density = 0.0
aerosol_number_density = 0.0
field = 0.0

DO k=1,model_levels
  dz(:,:,k) = interf_z(:,:,k) - interf_z(:,:,k-1)
END DO

DO imode=1,nmodes
  IF (mode(imode)) THEN

    ! Number densities
    field(:) = nd(:,imode)*cm3_per_m3   ! Number per m^3
    aerosol_number_density(:,:,:,imode) = RESHAPE(field(:),                    &
                                             [row_length,rows,model_levels])
    ! Component densities
    DO icp = 1,ncp
      IF (component(imode,icp)) THEN
        field(:) = mm(icp)*md(:,imode,icp)*nd(:,imode)*cm3_per_m3/avogadro
        aerosol_component_density(:,:,:,imode,icp) = RESHAPE(field(:),         &
                                             [row_length,rows,model_levels])

      END IF
    END DO     ! icp
  END IF     ! mode

  !  H2O component density
  IF (modesol(imode) == 1) THEN
    field(:) = mmw*mdwat_diag(:,imode)*nd(:,imode)*cm3_per_m3/avogadro
    aerosol_component_density(:,:,:,imode,ncp+1) = RESHAPE(field(:),           &
                                           [row_length,rows,model_levels])
  END IF
END DO  ! imode


! Two-dimensional aerosol loads (kg.m^-2)
! ---------------------------------------

aerosol_component_load = 0.0
DO imode=1,nmodes
  IF (mode(imode)) THEN
    DO icp = 1,ncp
      IF (component(imode,icp)) THEN
        item = item_load_cmip6(imode,icp)   ! find correct stash item no.
        item = item - tsection
        IF (item < 0) THEN
          icode = 11
          cmessage = ' No valid item number identified for this mode'//        &
                     ' and component'
          WRITE(umMessage,'(A70,2I6)') cmessage,imode,icp
          CALL umPrint(umMessage,src=RoutineName)
          CALL ereport(RoutineName,icode,cmessage)
        END IF
        IF (sf(item,section)) THEN
          DO k = 1,model_levels
            aerosol_component_load(:,:,imode,icp) =                            &
                  aerosol_component_load(:,:,imode,icp) +                      &
                  aerosol_component_density(:,:,k,imode,icp) * dz(:,:,k)
          END DO
          CALL copydiag(stashwork38(si(item,section,im_index):                 &
               si_last(item,section,im_index)),                                &
               aerosol_component_load(:,:,imode,icp),row_length,rows)
        END IF
      END IF
    END DO     ! icp
    ! H2O
    IF (modesol(imode) == 1) THEN
      item = item_water_load_cmip6(imode)   ! find correct stash item no.
      item = item - tsection
      IF (item < 0) THEN
        icode = 12
        cmessage = ' No valid item number identified for this mode'//          &
                   ' and component'
        WRITE(umMessage,'(A70,2I6)') cmessage,imode,icp
        CALL umPrint(umMessage,src=RoutineName)
        CALL ereport(RoutineName,icode,cmessage)
      END IF
      IF (sf(item,section)) THEN
        DO k = 1,model_levels
          aerosol_component_load(:,:,imode,ncp+1) =                            &
                aerosol_component_load(:,:,imode,ncp+1) +                      &
                aerosol_component_density(:,:,k,imode,ncp+1) * dz(:,:,k)
        END DO
        CALL copydiag(stashwork38(si(item,section,im_index):                   &
             si_last(item,section,im_index)),                                  &
             aerosol_component_load(:,:,imode,icp),row_length,rows)
      END IF
    END IF     ! mode
  END IF     ! modesol
END DO    ! imode

! Total aerosol loads in each component
! -------------------------------------

aerosol_total_load = 0.0
DO imode=1,nmodes
  IF (mode(imode)) THEN
    DO icp = 1,ncp
      IF (component(imode,icp)) THEN
        DO k = 1,model_levels
          aerosol_total_load(:,:,icp) = aerosol_total_load(:,:,icp) +          &
            aerosol_component_density(:,:,k,imode,icp) * dz(:,:,k)
        END DO
      END IF
    END DO  ! icp
    ! H2O
    DO k = 1,model_levels
      aerosol_total_load(:,:,ncp+1) = aerosol_total_load(:,:,ncp+1) +          &
        aerosol_component_density(:,:,k,imode,ncp+1) * dz(:,:,k)
    END DO
  END IF
END DO   ! imode

! Total aerosol loads of components
DO icp=1,ncp
  IF (ANY(component(:,icp))) THEN
    item = item_aerosol_load_cmip6(icp)
    item = item - tsection
    IF (item < 0) THEN
      icode = 13
      cmessage = ' No valid item number identified for this component'
      WRITE(umMessage,'(A70,I6)') cmessage,icp
      CALL umPrint(umMessage,src=RoutineName)
      CALL ereport(RoutineName,icode,cmessage)
    END IF
    IF (sf(item,section)) THEN
      CALL copydiag(stashwork38(si(item,section,im_index):                     &
           si_last(item,section,im_index)),                                    &
           aerosol_total_load(:,:,icp),row_length,rows)
    END IF
  END IF
END DO   ! icp

! H2O load
icp = ncp_max + 1
item = item_aerosol_load_cmip6(icp)
item = item - tsection
IF (item < 0) THEN
  icode = 14
  cmessage = ' No valid item number identified for this component'
  WRITE(umMessage,'(A70,I6)') cmessage,icp
  CALL umPrint(umMessage,src=RoutineName)
  CALL ereport(RoutineName,icode,cmessage)
END IF

icp2 = ncp+1
IF (sf(item,section)) THEN
  CALL copydiag(stashwork38(si(item,section,im_index):                         &
       si_last(item,section,im_index)),                                        &
       aerosol_total_load(:,:,icp2),row_length,rows)
END IF

! Copy items into STASHwork array:
! --------------------------------

! 1) aerosol components in kg/m^3
! -------------------------------
DO imode=1,nmodes
  IF (mode(imode)) THEN
    DO icp = 1,ncp
      IF (component(imode,icp)) THEN
        item = item_component_cmip6(imode,icp)   ! find correct stash item no.
        item = item - tsection
        IF (item < 0) THEN
          icode = 15
          cmessage = ' No valid item number identified for this mode'//        &
                     ' and component'
          WRITE(umMessage,'(A70,2I6)') cmessage,imode,icp
          CALL umPrint(umMessage,src=RoutineName)
          CALL ereport(RoutineName,icode,cmessage)
        END IF

        IF (sf(item,section)) THEN
          CALL copydiag_3d(stashwork38(si(item,section,im_index):              &
            si_last(item,section,im_index)),                                   &
            aerosol_component_density(:,:,:,imode,icp),                        &
            row_length,rows,model_levels,                                      &
            stlist(:,stindex(1,item,section,im_index)),len_stlist,             &
            stash_levels,num_stash_levels+1)
        END IF       ! sf
      END IF        ! component
    END DO         ! icp
  END IF          ! mode(imode)
END DO           ! imode

! 2) aerosol water in kg/m^3
! --------------------------
DO imode=1,nmodes
  IF (mode(imode) .AND. modesol(imode) == 1) THEN
    item = item_water_cmip6(imode)   ! find correct stash item no.
    item = item - tsection
    IF (item < 0) THEN
      icode = 16
      cmessage = ' No valid item number identified for this mode'
      WRITE(umMessage,'(A70,2I6)') cmessage,imode,ncp+1
      CALL umPrint(umMessage,src=RoutineName)
      CALL ereport(RoutineName,icode,cmessage)
    END IF

    IF (sf(item,section)) THEN
      CALL copydiag_3d(stashwork38(si(item,section,im_index):                  &
        si_last(item,section,im_index)),                                       &
        aerosol_component_density(:,:,:,imode,ncp+1),                          &
        row_length,rows,model_levels,                                          &
        stlist(:,stindex(1,item,section,im_index)),len_stlist,                 &
        stash_levels,num_stash_levels+1)
    END IF       ! sf
  END IF       ! mode(imode)
END DO       ! imode

! Total aerosol water from all soluble modes
! ------------------------------------------
item = stashcode_h2o_total - tsection
IF (sf(item,section)) THEN
  field3d(:,:,:) = 0.0
  DO imode=1,nmodes
    IF (mode(imode) .AND. modesol(imode) == 1) THEN
      field3d(:,:,:) = field3d(:,:,:) +                                        &
                       aerosol_component_density(:,:,:,imode,ncp+1)
    END IF
  END DO

  CALL copydiag_3d(stashwork38(si(item,section,im_index):                      &
    si_last(item,section,im_index)),                                           &
    field3d(:,:,:),                                                            &
    row_length,rows,model_levels,                                              &
    stlist(:,stindex(1,item,section,im_index)),len_stlist,                     &
    stash_levels,num_stash_levels+1)
END IF   ! sf

! 3) aerosol number density in m^-3
! ---------------------------------
DO imode=1,nmodes
  IF (mode(imode)) THEN
    item = item_number_cmip6(imode)
    item = item - tsection
    IF (item < 0) THEN
      icode = 17
      cmessage = ' No valid item number identified for this mode'
      WRITE(umMessage,'(A70,I6)') cmessage,imode
      CALL umPrint(umMessage,src=RoutineName)
      CALL ereport(RoutineName,icode,cmessage)
    END IF

    IF (sf(item,section)) THEN
      CALL copydiag_3d(stashwork38(si(item,section,im_index):                  &
        si_last(item,section,im_index)),                                       &
        aerosol_number_density(:,:,:,imode),                                   &
        row_length,rows,model_levels,                                          &
        stlist(:,stindex(1,item,section,im_index)),len_stlist,                 &
        stash_levels,num_stash_levels+1)
    END IF       ! sf
  END IF       ! mode(imode)
END DO       ! imode

! 4) Aerosol water mass mixing ratio
! ----------------------------------

item = stashcode_h2o_mmr - tsection
IF (sf(item,section)) THEN
  field(:) = 0.0
  DO imode = 1,n_soluble
    ! aerosol water over all soluble modes
    field(:) = field(:) + mdwat_diag(:,imode)*nd(:,imode)
  END DO
  field(:) = field(:)*mmw/(aird(:)*m_air)  ! kg/kg
  field3d = RESHAPE(field,[row_length,rows,model_levels])

  CALL copydiag_3d(stashwork38(si(item,section,im_index):                      &
    si_last(item,section,im_index)),                                           &
    field3d,                                                                   &
    row_length,rows,model_levels,                                              &
    stlist(:,stindex(1,item,section,im_index)),len_stlist,                     &
    stash_levels,num_stash_levels+1)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE mode_diags_cmip6

! ----------------------------------------------------------------------
SUBROUTINE ukca_mode_diags_pm_req(error_code_ptr,                              &
                                  diagnostics,                                 &
                                  pm_request,                                  &
                                  i_diag_req,                                  &
                                  l_ukca_pm_diags_req,                         &
                                  error_message, error_routine)

! Description:
!   Determine if PM diagnostics will be populated within UKCA from
!   the request configuration. This routine will determine the
!   value of l_ukca_pm_diags at runtime.

USE ukca_pm_diags_mod, ONLY: pm_request_struct
USE ukca_diagnostics_type_mod, ONLY: diagnostics_type, n_diag_group
USE ukca_diagnostics_output_mod, ONLY: seek_active_requests
USE ukca_fieldname_mod, ONLY: maxlen_diagname
USE ukca_error_mod, ONLY: maxlen_message, maxlen_procname
USE ukca_mode_setup, ONLY : ncp_max

IMPLICIT NONE

! Error code pointer
INTEGER, POINTER, INTENT(IN) :: error_code_ptr

! Diagnostic request info and pointers to parent arrays for diagnostic output
TYPE(diagnostics_type), INTENT(IN OUT) :: diagnostics
TYPE(pm_request_struct), INTENT(IN OUT) :: pm_request
TYPE(i_diag_req_struct), INTENT(IN OUT) :: i_diag_req

! Logical flag to indicate if UKCA PM diagnostics are requested
LOGICAL, INTENT(IN OUT) :: l_ukca_pm_diags_req

CHARACTER(LEN=maxlen_message), OPTIONAL, INTENT(OUT) :: error_message
CHARACTER(LEN=maxlen_procname), OPTIONAL, INTENT(OUT) :: error_routine

! Local variables
INTEGER :: i_size_cat           ! Loop counter for PM size category
INTEGER :: icp                  ! Loop counter for components
INTEGER :: i_diag_cp            ! Loop counter for PM component diagnostics
                                ! request
LOGICAL :: l_check_group(n_diag_group) ! True to check group for active
                                       ! request
LOGICAL :: l_active_requests    ! True if any active requests found
CHARACTER(LEN=maxlen_diagname) :: diagname  ! Internal variable for diagnostic
                                            ! name

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='MODE_DIAGS_PM_REQ'

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

error_code_ptr = 0
IF (PRESENT(error_message)) error_message = ''
IF (PRESENT(error_routine)) error_routine = RoutineName

! Initialize PM request flags
IF (.NOT. ALLOCATED(pm_request%l_total_dry)) ALLOCATE(pm_request%l_total_dry(n_size_cat))
IF (.NOT. ALLOCATED(pm_request%l_total_wet)) ALLOCATE(pm_request%l_total_wet(n_size_cat))
IF (.NOT. ALLOCATED(pm_request%l_component)) ALLOCATE(pm_request%l_component(ncp_max, n_size_cat))

pm_request%l_total_dry(:) = .FALSE.
pm_request%l_total_wet(:) = .FALSE.
pm_request%l_component(:,:) = .FALSE.

! Initialise i_diag with the indices of the requested
! PM diagnostics
IF (.NOT. ALLOCATED(i_diag_req%dry)) ALLOCATE(i_diag_req%dry(n_diag_group, n_size_cat))
IF (.NOT. ALLOCATED(i_diag_req%wet)) ALLOCATE(i_diag_req%wet(n_diag_group, n_size_cat))
IF (.NOT. ALLOCATED(i_diag_req%component)) ALLOCATE(i_diag_req%component(n_diag_group, ncp_max, n_size_cat))

! Determine which PM diagnostics must be calculated based on requests,
! set PM request flags accordingly and allocate storage (don't allocate above
! maximum index to be used)

! Set l_check_group to True for both surface and full height diagnostics
l_check_group = .TRUE.
! ------------------------------------------------------------------------------
!Total PM dry mass
! ------------------------------------------------------------------------------
DO i_size_cat = 1, n_size_cat
  diagname = pm_diag%diagname_total_dry(i_size_cat)
  CALL seek_active_requests(diagname, diagnostics, l_check_group,              &
                            i_diag_req%dry(:, i_size_cat),                     &
                            l_active_requests)
  IF (l_active_requests) THEN
    pm_request%l_total_dry(i_size_cat) = .TRUE.
  END IF
END DO
! ------------------------------------------------------------------------------
! Total PM wet mass
! ------------------------------------------------------------------------------
DO i_size_cat = 1, n_size_cat
  diagname = pm_diag%diagname_total_wet(i_size_cat)
  CALL seek_active_requests(diagname, diagnostics, l_check_group,              &
                            i_diag_req%wet(:, i_size_cat),                     &
                            l_active_requests)
  IF (l_active_requests) THEN
    pm_request%l_total_wet(i_size_cat) = .TRUE.
  END IF
END DO
! ------------------------------------------------------------------------------
! PM Component contribution requests.
! ------------------------------------------------------------------------------
DO i_size_cat = 1, n_size_cat
  DO i_diag_cp = 1, n_diag_cp
    diagname = pm_diag%diagname_component(i_diag_cp, i_size_cat)
    CALL seek_active_requests(diagname, diagnostics, l_check_group,            &
                              i_diag_req%component(:, i_diag_cp, i_size_cat),  &
                              l_active_requests)
    IF (l_active_requests) THEN
      icp = pm_diag%i_ref_component(i_diag_cp)
      pm_request%l_component(icp, i_size_cat) = .TRUE.
    END IF
  END DO
END DO

! Set the l_ukca_pm_diags_req flag to True if any PM diagnostics are
! requested.
IF (ANY(pm_request%l_total_dry) .OR. ANY(pm_request%l_total_wet) .OR.          &
    ANY(pm_request%l_component)) THEN
  l_ukca_pm_diags_req = .TRUE.
ELSE
  l_ukca_pm_diags_req = .FALSE.
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE ukca_mode_diags_pm_req

! ----------------------------------------------------------------------
SUBROUTINE mode_diags_pm(error_code_ptr,                                       &
                         row_length, rows, model_levels, nbox, nd, md,         &
                         diagnostics,                                          &
                         pm_request, i_diag_req,                               &
                         error_message, error_routine)

! Description:
!   Calculate the required PM10 and PM2.5 diagnostics and copy them to
!   the diagnostic array.
! ----------------------------------------------------------------------

USE ukca_pm_diags_mod,    ONLY: ukca_pm_diags, pm_request_struct
USE ukca_fieldname_mod, ONLY: maxlen_diagname
USE ukca_diagnostics_type_mod, ONLY: diagnostics_type, n_diag_group
USE ukca_diagnostics_output_mod, ONLY: update_diagnostics_3d_real
USE ukca_error_mod, ONLY: maxlen_message, maxlen_procname

IMPLICIT NONE

INTEGER, POINTER, INTENT(IN) :: error_code_ptr

! UKCA domain dimensions
INTEGER, INTENT(IN) :: row_length
INTEGER, INTENT(IN) :: rows
INTEGER, INTENT(IN) :: model_levels

! Number of elements
INTEGER, INTENT(IN) :: nbox

! Aerosol particle number density for mode (cm^-3)
REAL, INTENT(IN)    :: nd(nbox,nmodes)

! Average component concentration of aerosol particle in mode
! (molecules.particle^-1)
REAL, INTENT(IN)    :: md(nbox,nmodes,glomap_variables%ncp)

! Diagnostic request info and pointers to parent arrays for diagnostic output
TYPE(diagnostics_type), INTENT(IN OUT) :: diagnostics
TYPE(pm_request_struct), INTENT(IN OUT) :: pm_request
TYPE(i_diag_req_struct), INTENT(IN OUT) :: i_diag_req

CHARACTER(LEN=maxlen_message), OPTIONAL, INTENT(OUT) :: error_message
CHARACTER(LEN=maxlen_procname), OPTIONAL, INTENT(OUT) :: error_routine

! Local variables
INTEGER :: i_size_cat     ! Loop counter for PM size category
INTEGER :: i_size_cat_max ! Highest PM size category index to be used
INTEGER :: icp            ! Loop counter for components
INTEGER :: icp_max        ! Highest component index to be used for component
                          ! contributions
INTEGER :: i_diag_cp      ! Loop counter
INTEGER :: icode          ! Error code
REAL, PARAMETER :: d_cutoff(n_size_cat) = [10.0e-6, 2.5e-6]
                          ! Size limits for particulate matter (m)
REAL    :: field3d(row_length,rows,model_levels)   ! Output field (3D)

LOGICAL :: l_active_requests              ! True if any active requests found
CHARACTER(LEN=maxlen_diagname) :: diagname ! Internal variable for diagnostic name

! PM diagnostics from 'ukca_pm_diags' (ug m-3)
REAL, ALLOCATABLE :: pm_dry(:,:)         ! Total PM dry mass by size category
REAL, ALLOCATABLE :: pm_wet(:,:)         ! Total PM wet mass by size category
REAL, ALLOCATABLE :: pm_component(:,:,:) ! Component contributions to PM by
                                         ! component number and size category

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='MODE_DIAGS_PM'


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

error_code_ptr = 0
IF (PRESENT(error_message)) error_message = ''
IF (PRESENT(error_routine)) error_routine = RoutineName

! Determine which PM diagnostics must be calculated based on requests,
! using PM request flags obtained previously by ukca_mode_diags_pm_req
! and allocate storage (don't allocate above maximum index to be used)

!Total PM dry mass
i_size_cat_max = 0
DO i_size_cat = 1, n_size_cat
  IF (pm_request%l_total_dry(i_size_cat) .EQV. .TRUE.) THEN
    i_size_cat_max = i_size_cat
  END IF
END DO
IF (i_size_cat_max > 0) THEN
  ALLOCATE(pm_dry(nbox,i_size_cat_max))
ELSE
  ALLOCATE(pm_dry(1,1))
END IF

! Total PM wet mass
i_size_cat_max = 0
DO i_size_cat = 1, n_size_cat
  IF (pm_request%l_total_dry(i_size_cat) .EQV. .TRUE.) THEN
    i_size_cat_max = i_size_cat
  END IF
END DO

IF (i_size_cat_max > 0) THEN
  ALLOCATE(pm_wet(nbox, i_size_cat_max))
ELSE
  ALLOCATE(pm_wet(1,1))
END IF

! Component contribution requests.
i_size_cat_max = 0
icp_max = 0
DO i_size_cat = 1, n_size_cat
  DO i_diag_cp = 1, n_diag_cp
    icp = pm_diag%i_ref_component(i_diag_cp)
    IF (pm_request%l_component(icp, i_size_cat) .EQV. .TRUE.) THEN
      i_size_cat_max = i_size_cat
      IF (icp > icp_max) icp_max = icp
    END IF
  END DO
END DO
IF (icp > 0) THEN
  ALLOCATE(pm_component(nbox,icp_max,i_size_cat_max))
ELSE
  ALLOCATE(pm_component(1,1,1))
END IF

! Derive the PM diagnostics needed for producing the required
! PM diagnostics names

CALL ukca_pm_diags(nbox,nd,md,mdwat_diag,wetdp_diag,d_cutoff,pm_request,       &
                   pm_dry,pm_wet,pm_component)

! Copy required items to diagnostic array

DO i_size_cat = 1,n_size_cat

  ! Total dry mass for PM size category
  diagname = pm_diag%diagname_total_dry(i_size_cat)
  IF (pm_request%l_total_dry(i_size_cat)) THEN
    field3d = RESHAPE(pm_dry(:,i_size_cat),                                    &
                      [row_length,rows,model_levels])

    CALL update_diagnostics_3d_real(                                           &
           error_code_ptr, diagname,                                           &
           field3d, diagnostics,                                               &
           i_diag_req=i_diag_req%dry(:, i_size_cat),                           &
           error_message=error_message,                                        &
           error_routine=error_routine)

    IF ( error_code_ptr > 0 ) THEN
      IF (lhook) CALL dr_hook(ModuleName//';'//RoutineName,zhook_out,zhook_handle)
      RETURN
    END IF

  END IF

  ! Total wet mass for PM size category
  diagname = pm_diag%diagname_total_wet(i_size_cat)
  IF (pm_request%l_total_wet(i_size_cat)) THEN
    field3d = RESHAPE(pm_wet(:,i_size_cat),                                    &
                      [row_length,rows,model_levels])

    CALL update_diagnostics_3d_real(                                           &
      error_code_ptr, diagname,                                                &
      field3d, diagnostics,                                                    &
      i_diag_req=i_diag_req%wet(:, i_size_cat),                                &
      error_message=error_message,                                             &
      error_routine=error_routine)

    IF ( error_code_ptr > 0 ) THEN
      IF (lhook) CALL dr_hook(ModuleName//';'//RoutineName,zhook_out,zhook_handle)
      RETURN
    END IF

  END IF

  ! Component contributions to PM size category
  DO i_diag_cp = 1,n_diag_cp
    diagname = pm_diag%diagname_component(i_diag_cp,i_size_cat)
    icp = pm_diag%i_ref_component(i_diag_cp)

    IF (pm_request%l_component(icp, i_size_cat)) THEN
      field3d = RESHAPE(pm_component(:,icp,i_size_cat),                        &
                        [row_length,rows,model_levels])

      CALL update_diagnostics_3d_real(                                         &
        error_code_ptr, diagname,                                              &
        field3d, diagnostics,                                                  &
        i_diag_req=i_diag_req%component(:, i_diag_cp, i_size_cat),             &
        error_message=error_message,                                           &
        error_routine=error_routine)

      IF ( error_code_ptr > 0 ) THEN
        IF (lhook) CALL dr_hook(ModuleName//';'//RoutineName,zhook_out,zhook_handle)
        RETURN
      END IF

    END IF
  END DO

END DO

DEALLOCATE(pm_request%l_total_dry)
DEALLOCATE(pm_request%l_total_wet)
DEALLOCATE(pm_request%l_component)
DEALLOCATE(i_diag_req%component)
DEALLOCATE(i_diag_req%wet)
DEALLOCATE(i_diag_req%dry)
DEALLOCATE(pm_dry)
DEALLOCATE(pm_wet)
DEALLOCATE(pm_component)

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE mode_diags_pm

END MODULE ukca_mode_diags_mod
