! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description:
!
!   Module containing code for creating and populating a master list
!   holding information about each diagnostic recognised by UKCA's
!   diagnostic handling system.
!
!   Public procedures provided:
!
!     create_master_diagnostics_list
!       - set up the master list of recognised diagnostics
!     set_diagnostic_availabilities
!       - determine the availability of each diagnostic in the master list
!         given the UKCA configuration
!
! Part of the UKCA model, a community model supported by the
! Met Office and NCAS, with components provided initially
! by The University of Cambridge, University of Leeds,
! University of Oxford and The Met. Office.  See www.ukca.ac.uk
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: UKCA
!
! Code Description:
!   Language:  Fortran 2003
!   This code is written to UMDP3 programming standards.
!
! ----------------------------------------------------------------------

MODULE ukca_diagnostics_master_mod

USE ukca_diagnostics_type_mod, ONLY: bounds_type, diag_entry_type,             &
                                     n_diag_group, dgroup_flat_real,           &
                                     dgroup_fullht_real

USE ukca_fieldname_mod, ONLY: maxlen_diagname,                                 &
                              diagname_jrate_no2, diagname_jrate_o3a,          &
                              diagname_jrate_o3b, diagname_jrate_o2b,          &
                              diagname_rxnflux_oh_ch4_trop,                    &
                              diagname_p_tropopause,                           &
                              diagname_o3_column_du,                           &
                              diagname_plumeria_height,                        &
                              diagname_pm10_dry, diagname_pm2p5_dry,           &
                              diagname_pm10_wet, diagname_pm2p5_wet,           &
                              diagname_pm10_bc, diagname_pm2p5_bc,             &
                              diagname_pm10_oc, diagname_pm2p5_oc,             &
                              diagname_pm10_so4, diagname_pm2p5_so4,           &
                              diagname_pm10_du, diagname_pm2p5_du,             &
                              diagname_pm10_ss, diagname_pm2p5_ss,             &
                              diagname_pm10_nh4, diagname_pm2p5_nh4,           &
                              diagname_pm10_no3, diagname_pm2p5_no3,           &
                              diagname_pm10_nn, diagname_pm2p5_nn,             &
                              diagname_pm10_mp, diagname_pm2p5_mp


USE ukca_missing_data_mod, ONLY: imdi

USE ukca_config_specification_mod,  ONLY: ukca_config
USE ukca_error_mod, ONLY: maxlen_message, maxlen_procname,                     &
                          errcode_ukca_internal_fault, error_report

IMPLICIT NONE

PRIVATE

CHARACTER(LEN=*), PARAMETER :: ModuleName = 'UKCA_DIAGNOSTICS_MASTER_MOD'

! Public procedures
PUBLIC create_master_diagnostics_list, set_diagnostic_availabilities


! --- Module variables ---

INTEGER, PARAMETER :: n_diag_tot = 30          ! Total no. of UKCA diagnostics

! Array bounds for each diagnostic group
TYPE(bounds_type), PUBLIC :: bound_info(n_diag_group)

! Master list of all UKCA diagnostics (excluding UM-only diagnostics)
TYPE(diag_entry_type), ALLOCATABLE, SAVE, PUBLIC :: master_diag_list(:)

CONTAINS

! ----------------------------------------------------------------------
SUBROUTINE create_master_diagnostics_list(error_code_ptr, error_message,       &
                                          error_routine)
! ----------------------------------------------------------------------
! Description:
!   Create and initialise master diagnostics list.
! ----------------------------------------------------------------------

USE yomhook,   ONLY: lhook, dr_hook
USE parkind1,  ONLY: jprb, jpim

IMPLICIT NONE

! Subroutine arguments

INTEGER, POINTER, INTENT(IN) :: error_code_ptr  ! Pointer to return code

CHARACTER(LEN=maxlen_message), OPTIONAL, INTENT(OUT) :: error_message
CHARACTER(LEN=maxlen_procname), OPTIONAL, INTENT(OUT) :: error_routine

! Local variables

INTEGER :: n

CHARACTER(LEN=maxlen_message) :: message_txt  ! Buffer for output message

! Dr Hook

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CREATE_MASTER_DIAGNOSTICS_LIST'

! End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName, zhook_in, zhook_handle)

error_code_ptr = 0
IF (PRESENT(error_message)) error_message = ''
IF (PRESENT(error_routine)) error_routine = ''

! Create the master diagnostics list
! For each diagnostic set name, group and switch to indicate whether it is only
! available on chemistry time steps. This switch will be used to return the
! status of the diagnostic on non-chemistry time steps as 'skipped'.

n = 0
ALLOCATE(master_diag_list(n_diag_tot))
CALL create_diagnostic(diagname_jrate_no2, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_jrate_o3a, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_jrate_o3b, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_jrate_o2b, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_rxnflux_oh_ch4_trop, dgroup_fullht_real,       &
                       .TRUE., n)
CALL create_diagnostic(diagname_p_tropopause, dgroup_flat_real, .FALSE., n)
CALL create_diagnostic(diagname_o3_column_du, dgroup_fullht_real, .FALSE., n)
CALL create_diagnostic(diagname_plumeria_height, dgroup_flat_real, .FALSE., n)
CALL create_diagnostic(diagname_pm10_dry, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm2p5_dry, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm10_wet, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm2p5_wet, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm10_so4, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm2p5_so4, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm10_bc, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm2p5_bc, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm10_oc, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm2p5_oc, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm10_ss, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm2p5_ss, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm10_du, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm2p5_du, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm10_nh4, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm2p5_nh4, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm10_no3, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm2p5_no3, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm10_nn, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm2p5_nn, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm10_mp, dgroup_fullht_real, .TRUE., n)
CALL create_diagnostic(diagname_pm2p5_mp, dgroup_fullht_real, .TRUE., n)

IF (n /= n_diag_tot) THEN
  WRITE(message_txt,'(A,I0,A,I0)')                                             &
    'Number of diagnostics being created (', n,                                &
    ') differs from that expected: n_diag_tot = ', n_diag_tot
  error_code_ptr = errcode_ukca_internal_fault
  CALL error_report(ukca_config%i_error_method, error_code_ptr, message_txt,   &
                    RoutineName, msg_out=error_message, locn_out=error_routine)
  IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName, zhook_out, zhook_handle)
  RETURN
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName, zhook_out, zhook_handle)
RETURN

END SUBROUTINE create_master_diagnostics_list

! ----------------------------------------------------------------------
SUBROUTINE create_diagnostic(varname, group, l_chem_timestep, n_diag)
! ----------------------------------------------------------------------
! Description:
!   Create an entry for a named diagnostic in the master list.
! ----------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments

CHARACTER(LEN=*), INTENT(IN) :: varname      ! Diagnostic name

INTEGER, INTENT(IN) :: group                 ! Diagnostic group

LOGICAL, INTENT(IN) :: l_chem_timestep       ! T if only available on chemistry
                                             ! time steps

INTEGER, INTENT(IN OUT) :: n_diag            ! Index of created diagnostic
                                             ! in the master list

! Local variables

INTEGER :: group_alt                         ! Alternative group to request in
INTEGER, PARAMETER :: asad_id = imdi         ! Default ASAD id (used for
                                             ! non-ASAD diagnostics)

LOGICAL, PARAMETER :: l_available = .FALSE.  ! Default availability

! End of header

! Set alternative group: full height diagnostic fields can satisfy flat field
! diagnostic requests (level 1 subfield is supplied)
IF (group == dgroup_fullht_real) THEN
  group_alt = dgroup_flat_real
ELSE
  group_alt = 0
END IF

n_diag = n_diag + 1
IF (n_diag <= n_diag_tot)                                                      &
  master_diag_list(n_diag) =                                                   &
    diag_entry_type(varname, group, group_alt, bound_info(group),              &
                    l_available, l_chem_timestep, asad_id)

RETURN
END SUBROUTINE create_diagnostic

! ----------------------------------------------------------------------
SUBROUTINE set_diagnostic_availabilities(error_code_ptr,                       &
                                         ukca_config, glomap_config,           &
                                         advt, n, error_message,               &
                                         error_routine)
! ----------------------------------------------------------------------
! Description:
!   Determine the availability of each diagnostic in the master list
!   given the UKCA configuration
! ----------------------------------------------------------------------

USE ukca_config_specification_mod, ONLY: ukca_config_spec_type,                &
                                         glomap_config_spec_type,              &
                                         glomap_variables,                     &
                                         calc_ozonecol

USE ukca_mode_setup,               ONLY: nmodes, cp_su, cp_bc, cp_oc, cp_cl,   &
                                         cp_nh4, cp_no3, cp_nn, cp_du, cp_mp
USE ukca_chem_defs_mod, ONLY: ratj_defs
USE asad_flux_dat,      ONLY: asad_chemical_fluxes, stashcode_ukca_chem_diag
USE ukca_mode_setup, ONLY : ncp_max

USE yomhook,   ONLY: lhook, dr_hook
USE parkind1,  ONLY: jprb, jpim

IMPLICIT NONE

! Subroutine arguments

INTEGER, POINTER, INTENT(IN) :: error_code_ptr  ! Pointer to return code

TYPE(ukca_config_spec_type), INTENT(IN OUT) :: ukca_config
                                                ! UKCA configuration data
TYPE(glomap_config_spec_type), INTENT(IN OUT) :: glomap_config
                                                ! GLOMAP configuration data

CHARACTER(LEN=*), INTENT(IN) :: advt(:)         ! Advected chemical species

INTEGER, INTENT(OUT) :: n                       ! Number of available
                                                ! diagnostics

CHARACTER(LEN=maxlen_message), OPTIONAL, INTENT(OUT) :: error_message
CHARACTER(LEN=maxlen_procname), OPTIONAL, INTENT(OUT) :: error_routine

! Local variables

INTEGER :: item
INTEGER :: stash_value
INTEGER :: i
INTEGER :: j
INTEGER :: imodes ! loop counter for modes
INTEGER :: icp    ! loop counter for components
LOGICAL, DIMENSION(ncp_max) :: l_pm_components ! Component availability
! (initialised to .FALSE. for each component)

! Dr Hook

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1

REAL(KIND=jprb) :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'SET_DIAGNOSTIC_AVAILABILITIES'
! End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName, zhook_in, zhook_handle)

error_code_ptr = 0
IF (PRESENT(error_message)) error_message = ''
IF (PRESENT(error_routine)) error_routine = ''

! Define the availability of PM diagnostics based on the GLOMAP configuration
! settings.
l_pm_components = .FALSE.

! Set availability of each diagnostic in the master diagnostics list given the
! UKCA configuration settings. Keep count of number available for later use.

n = 0
DO i = 1, SIZE(master_diag_list)

  SELECT CASE (master_diag_list(i)%varname)

  CASE (diagname_jrate_no2)
    master_diag_list(i)%l_available = ukca_config%l_use_photolysis .AND.       &
      ANY(ratj_defs(:)%fname == 'jno2')
  CASE (diagname_jrate_o3a)
    master_diag_list(i)%l_available = ukca_config%l_use_photolysis .AND.       &
      ANY(ratj_defs(:)%fname == 'jo3a')
  CASE (diagname_jrate_o3b)
    master_diag_list(i)%l_available = ukca_config%l_use_photolysis .AND.       &
       ANY(ratj_defs(:)%fname == 'jo3b')
  CASE (diagname_jrate_o2b)
    master_diag_list(i)%l_available = ukca_config%l_use_photolysis .AND.       &
       ANY(ratj_defs(:)%fname == 'jo2b')
  CASE (diagname_p_tropopause)
    master_diag_list(i)%l_available = (ukca_config%model_levels > 1)
  CASE (diagname_o3_column_du)
    master_diag_list(i)%l_available = ASSOCIATED(calc_ozonecol) .AND.          &
                                      (ANY(advt(:) == 'O3        '))
  CASE (diagname_plumeria_height)
    master_diag_list(i)%l_available = ukca_config%l_ukca_so2ems_plumeria
  ! Dry deposition diagnostics
  CASE (diagname_pm10_dry)
    master_diag_list(i)%l_available = .TRUE.
  CASE (diagname_pm2p5_dry)
    master_diag_list(i)%l_available = .TRUE.
  ! Wet deposition diagnostics
  CASE (diagname_pm10_wet)
    master_diag_list(i)%l_available = .TRUE.
  CASE (diagname_pm2p5_wet)
    master_diag_list(i)%l_available = .TRUE.
  ! Sulphate diagnostics (cp_su=1)
  CASE (diagname_pm10_so4)
    master_diag_list(i)%l_available = .TRUE.
  CASE (diagname_pm2p5_so4)
    master_diag_list(i)%l_available = .TRUE.
  ! Black carbon diagnostics (cp_bc=2)
  CASE (diagname_pm10_bc)
    master_diag_list(i)%l_available = .TRUE.
  CASE (diagname_pm2p5_bc)
    master_diag_list(i)%l_available = .TRUE.
  ! Particle organic matter diagnostics (cp_oc=3 )
  CASE (diagname_pm10_oc)
    master_diag_list(i)%l_available = .TRUE.
  CASE (diagname_pm2p5_oc)
    master_diag_list(i)%l_available = .TRUE.
  ! Sea salt diagnostics (cp_cl=4 )
  CASE (diagname_pm10_ss)
    master_diag_list(i)%l_available = .TRUE.
  CASE (diagname_pm2p5_ss)
    master_diag_list(i)%l_available = .TRUE.
! Dust diagnostics (cp_du=5)
  CASE (diagname_pm10_du)
    master_diag_list(i)%l_available = .TRUE.
  CASE (diagname_pm2p5_du)
    master_diag_list(i)%l_available = .TRUE.
  ! Nitrate diagnostics (cp_no3=7)
  CASE (diagname_pm10_no3)
    master_diag_list(i)%l_available = .TRUE.
  CASE (diagname_pm2p5_no3)
    master_diag_list(i)%l_available = .TRUE.
  ! Sodium Nitrate diagnostics (cp_nn=8)
  CASE (diagname_pm10_nn)
    master_diag_list(i)%l_available = .TRUE.
  CASE (diagname_pm2p5_nn)
    master_diag_list(i)%l_available = .TRUE.
  ! Ammonium diagnostics (cp_nh4=9 )
  CASE (diagname_pm10_nh4)
    master_diag_list(i)%l_available = .TRUE.
  CASE (diagname_pm2p5_nh4)
    master_diag_list(i)%l_available = .TRUE.
  ! Microplastic diagnostics (cp_mp=10)
  CASE (diagname_pm10_mp)
    master_diag_list(i)%l_available = .TRUE.
  CASE (diagname_pm2p5_mp)
    master_diag_list(i)%l_available = .TRUE.

  CASE DEFAULT

    ! Assume this is an ASAD framework diagnostic. It will thus be marked
    ! unavailable if the ASAD diagnostic framework is not in use. If it is
    ! in use then look up the id code associated with the diagnostic.
    ! If this code is present in the ASAD chemical fluxes array the diagnostic
    ! is available. (All id codes are currently assumed to be UM Section-50
    ! STASH codes.)

    master_diag_list(i)%l_available = .FALSE.
    IF (ukca_config%l_asad_chem_diags_support) THEN
      item = stash_item_code(master_diag_list(i)%varname)
      IF (item /= imdi) THEN
        stash_value = stashcode_ukca_chem_diag * 1000 + item
        IF (ANY(asad_chemical_fluxes(:)%stash_number == stash_value))          &
          master_diag_list(i)%l_available = .TRUE.
        master_diag_list(i)%asad_id = stash_value
      ELSE
        error_code_ptr = errcode_ukca_internal_fault
        CALL error_report(ukca_config%i_error_method, error_code_ptr,          &
          'No ASAD id code defined for a recognised ASAD diagnostic: ' //      &
          master_diag_list(i)%varname, RoutineName,                            &
          msg_out=error_message, locn_out=error_routine)
        IF (lhook) THEN
          CALL dr_hook(ModuleName//':'//RoutineName, zhook_out, zhook_handle)
        END IF
        RETURN
      END IF
    END IF

  END SELECT
  ! Increment number of available diagnostics if applicable
  IF (master_diag_list(i)%l_available) n = n + 1

END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName, zhook_out, zhook_handle)
RETURN

END SUBROUTINE set_diagnostic_availabilities

! ----------------------------------------------------------------------
INTEGER FUNCTION stash_item_code(diagname)
! ----------------------------------------------------------------------
! Description:
!   Returns STASH item number associated with the given diagnostic name
!   for any ASAD diagnostic available via UKCA's own diagnostic handling
!   system, i.e. independently of the UM STASH system.
! ----------------------------------------------------------------------

IMPLICIT NONE

! Function argument
CHARACTER(LEN=maxlen_diagname) :: diagname

! Local variables

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'STASH_ITEM_CODE'

SELECT CASE (diagname)
CASE (diagname_rxnflux_oh_ch4_trop)
  stash_item_code = 41
CASE DEFAULT
  stash_item_code = imdi
END SELECT

RETURN
END FUNCTION stash_item_code

END MODULE ukca_diagnostics_master_mod
