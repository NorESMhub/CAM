
module check_energy

!---------------------------------------------------------------------------------
! Purpose:
!
! Module to check
!   1. vertically integrated total energy and water conservation for each
!      column within the physical parameterizations
!
!   2. global mean total energy conservation between the physics output state
!      and the input state on the next time step.
!
!   3. add a globally uniform heating term to account for any change of total energy in 2.
!
! Author: Byron Boville  Oct 31, 2002
!
! Modifications:
!   03.03.29  Boville  Add global energy check and fixer.
!
!---------------------------------------------------------------------------------

  use shr_kind_mod,    only: r8 => shr_kind_r8
  use ppgrid,          only: pcols, pver, begchunk, endchunk
  use spmd_utils,      only: masterproc

  use physconst,       only: gravit, rga, latvap, latice, cpair, rair
  use air_composition, only: cpairv, cp_or_cv_dycore
  use physics_types,   only: physics_state, physics_tend, physics_ptend, physics_ptend_init
  use constituents,    only: cnst_get_ind, pcnst, cnst_name, cnst_get_type_byind
  use cam_logfile,     only: iulog

  implicit none
  private

  ! Public types:
  public check_tracers_data

  ! Public methods - not CCPP-ized
  public :: check_tracers_init              ! initialize tracer integrals and cumulative boundary fluxes
  public :: check_tracers_chng              ! check changes in integrals against cumulative boundary fluxes
  public :: tot_energy_phys                 ! calculate and output total energy and axial angular momentum diagnostics

  ! These subroutines cannot be CCPP-ized
  public :: check_energy_readnl             ! read namelist values
  public :: check_energy_register           ! register fields in physics buffer
  public :: check_energy_init               ! initialization of module
  public :: check_energy_gmean              ! global means of physics input and output total energy
  public :: check_energy_get_integrals      ! get energy integrals computed in check_energy_gmean

  ! Public methods - CAM interfaces to CCPP version:
  public :: check_energy_cam_chng           ! check changes in integrals against cumulative boundary fluxes
  public :: check_energy_timestep_init      ! timestep initialization of energy integrals and cumulative boundary fluxes
                                            ! name is retained for FV3 compatibility

  public :: check_energy_cam_fix            ! add heating rate required for global mean total energy conservation

  ! This routine adjusts enthalpy if compute_enthalpy_flux = .true.
  public :: enthalpy_adjustment

  ! Private module data
  logical  :: print_energy_errors = .false.

  ! used for check_energy_gmean
  real(r8) :: teout_glob   ! global mean energy of output state
  real(r8) :: teinp_glob   ! global mean energy of input state
  real(r8) :: tedif_glob   ! global mean energy difference
  real(r8) :: psurf_glob   ! global mean surface pressure
  real(r8) :: ptopb_glob   ! global mean top boundary pressure
  real(r8) :: heat_glob    ! global mean heating rate

  ! Physics buffer indices

  integer, public  :: teout_idx  = 0       ! teout index in physics buffer
  integer, public  :: dtcore_idx = 0       ! dtcore index in physics buffer
  integer, public  :: dqcore_idx = 0       ! dqcore index in physics buffer
  integer, public  :: ducore_idx = 0       ! ducore index in physics buffer
  integer, public  :: dvcore_idx = 0       ! dvcore index in physics buffer

  type check_tracers_data
     real(r8) :: tracer(pcols,pcnst)       ! initial vertically integrated total (kinetic + static) energy
     real(r8) :: tracer_tnd(pcols,pcnst)   ! cumulative boundary flux of total energy
     integer :: count(pcnst)               ! count of values with significant imbalances
  end type check_tracers_data


!===============================================================================
contains
!===============================================================================

subroutine check_energy_readnl(nlfile)

   use namelist_utils,  only: find_group_name
   use units,           only: getunit, freeunit
   use spmd_utils,      only: mpicom, mstrid=>masterprocid, mpi_logical
   use cam_abortutils,  only: endrun

   ! update the CCPP-ized namelist option
   use check_energy_chng, only: check_energy_chng_init

   character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

   ! Local variables
   integer :: unitn, ierr
   character(len=*), parameter :: sub = 'check_energy_readnl'

   namelist /check_energy_nl/ print_energy_errors
   !-----------------------------------------------------------------------------

   ! Read namelist
   if (masterproc) then
      unitn = getunit()
      open( unitn, file=trim(nlfile), status='old' )
      call find_group_name(unitn, 'check_energy_nl', status=ierr)
      if (ierr == 0) then
         read(unitn, check_energy_nl, iostat=ierr)
         if (ierr /= 0) then
            call endrun(sub//': FATAL: reading namelist')
         end if
      end if
      close(unitn)
      call freeunit(unitn)
   end if

   call mpi_bcast(print_energy_errors, 1, mpi_logical, mstrid, mpicom, ierr)
   if (ierr /= 0) call endrun(sub//": FATAL: mpi_bcast: print_energy_errors")

   if (masterproc) then
      write(iulog,*) 'check_energy options:'
      write(iulog,*) '  print_energy_errors =', print_energy_errors
   end if

   ! update the CCPP-ized namelist option
   call check_energy_chng_init(print_energy_errors_in=print_energy_errors)

end subroutine check_energy_readnl

!===============================================================================

  subroutine check_energy_register()
!
! Register fields in the physics buffer.
!
!-----------------------------------------------------------------------

    use physics_buffer, only : pbuf_add_field, dtype_r8, dyn_time_lvls
    use physics_buffer, only : pbuf_register_subcol
    use subcol_utils,   only : is_subcol_on

!-----------------------------------------------------------------------

! Request physics buffer space for fields that persist across timesteps.

    call pbuf_add_field('TEOUT', 'global',dtype_r8 , (/pcols,dyn_time_lvls/),      teout_idx)
    call pbuf_add_field('DTCORE','global',dtype_r8,  (/pcols,pver,dyn_time_lvls/),dtcore_idx)
    ! DQCORE refers to dycore tendency of water vapor
    call pbuf_add_field('DQCORE','global',dtype_r8,  (/pcols,pver,dyn_time_lvls/),dqcore_idx)
    call pbuf_add_field('DUCORE','global',dtype_r8,  (/pcols,pver,dyn_time_lvls/),ducore_idx)
    call pbuf_add_field('DVCORE','global',dtype_r8,  (/pcols,pver,dyn_time_lvls/),dvcore_idx)
    if(is_subcol_on()) then
      call pbuf_register_subcol('TEOUT', 'phys_register', teout_idx)
      call pbuf_register_subcol('DTCORE', 'phys_register', dtcore_idx)
      call pbuf_register_subcol('DQCORE', 'phys_register', dqcore_idx)
      call pbuf_register_subcol('DUCORE', 'phys_register', ducore_idx)
      call pbuf_register_subcol('DVCORE', 'phys_register', dvcore_idx)
    end if

  end subroutine check_energy_register

!================================================================================================

  subroutine check_energy_init()
!
! Initialize the energy conservation module
!
!-----------------------------------------------------------------------
    use cam_history,       only: addfld, add_default, horiz_only
    use phys_control,      only: phys_getopts

    implicit none

    logical          :: history_budget, history_waccm
    integer          :: history_budget_histfile_num ! output history file number for budget fields

!-----------------------------------------------------------------------

    call phys_getopts( history_budget_out = history_budget, &
                       history_budget_histfile_num_out = history_budget_histfile_num, &
                       history_waccm_out = history_waccm )

! register history variables
    call addfld('TEINP',  horiz_only,  'A', 'J/m2', 'Total energy of physics input')
    call addfld('TEOUT',  horiz_only,  'A', 'J/m2', 'Total energy of physics output')
    call addfld('TEFIX',  horiz_only,  'A', 'J/m2', 'Total energy after fixer')
    call addfld('EFIX',   horiz_only,  'A', 'W/m2', 'Effective sensible heat flux due to energy fixer')
    call addfld('DTCORE', (/ 'lev' /), 'A', 'K/s' , 'T tendency due to dynamical core')
    call addfld('DQCORE', (/ 'lev' /), 'A', 'kg/kg/s' , 'Water vapor tendency due to dynamical core')

    if ( history_budget ) then
       call add_default ('DTCORE', history_budget_histfile_num, ' ')
    end if
    if ( history_waccm ) then
       call add_default ('DTCORE', 1, ' ')
    end if

  end subroutine check_energy_init

!===============================================================================
  subroutine check_tracers_init(state, tracerint)

!-----------------------------------------------------------------------
! Compute initial values of tracers integrals,
! zero cumulative tendencies
!-----------------------------------------------------------------------

!------------------------------Arguments--------------------------------

    type(physics_state),   intent(in)    :: state
    type(check_tracers_data), intent(out)   :: tracerint

!---------------------------Local storage-------------------------------

    real(r8) :: tr(pcols)                          ! vertical integral of tracer
    real(r8) :: trpdel(pcols, pver)                ! pdel for tracer

    integer ncol                                   ! number of atmospheric columns
    integer  i,k,m                                 ! column, level,constituent indices
    integer :: ixcldice, ixcldliq                  ! CLDICE and CLDLIQ indices
    integer :: ixrain, ixsnow                      ! RAINQM and SNOWQM indices
    integer :: ixgrau                              ! GRAUQM index
!-----------------------------------------------------------------------

    ncol  = state%ncol
    call cnst_get_ind('CLDICE', ixcldice, abort=.false.)
    call cnst_get_ind('CLDLIQ', ixcldliq, abort=.false.)
    call cnst_get_ind('RAINQM', ixrain,   abort=.false.)
    call cnst_get_ind('SNOWQM', ixsnow,   abort=.false.)
    call cnst_get_ind('GRAUQM', ixgrau,   abort=.false.)


    do m = 1,pcnst

       if ( any(m == (/ 1, ixcldliq, ixcldice, &
                           ixrain,   ixsnow, ixgrau /)) ) exit   ! dont process water substances
                                                                 ! they are checked in check_energy

       if (cnst_get_type_byind(m).eq.'dry') then
          trpdel(:ncol,:) = state%pdeldry(:ncol,:)
       else
          trpdel(:ncol,:) = state%pdel(:ncol,:)
       endif

       ! Compute vertical integrals of tracer
       tr = 0._r8
       do k = 1, pver
          do i = 1, ncol
             tr(i) = tr(i) + state%q(i,k,m)*trpdel(i,k)*rga
          end do
       end do

       ! Compute vertical integrals of frozen static tracers and total water.
       do i = 1, ncol
          tracerint%tracer(i,m) = tr(i)
       end do

       ! zero cummulative boundary fluxes
       tracerint%tracer_tnd(:ncol,m) = 0._r8

       tracerint%count(m) = 0

    end do

    return
  end subroutine check_tracers_init

!===============================================================================
  subroutine check_tracers_chng(state, tracerint, name, nstep, ztodt, cflx)

!-----------------------------------------------------------------------
! Check that the tracers and water change matches the boundary fluxes
! these checks are not save when there are tracers transformations, as
! they only check to see whether a mass change in the column is
! associated with a flux
!-----------------------------------------------------------------------

    use cam_abortutils, only: endrun


    implicit none

!------------------------------Arguments--------------------------------

    type(physics_state)    , intent(in   ) :: state
    type(check_tracers_data), intent(inout) :: tracerint! tracers integrals and boundary fluxes
    character*(*),intent(in) :: name               ! parameterization name for fluxes
    integer , intent(in   ) :: nstep               ! current timestep number
    real(r8), intent(in   ) :: ztodt               ! 2 delta t (model time increment)
    real(r8), intent(in   ) :: cflx(pcols,pcnst)       ! boundary flux of tracers       (kg/m2/s)

!---------------------------Local storage-------------------------------

    real(r8) :: tracer_inp(pcols,pcnst)                   ! total tracer of new (input) state
    real(r8) :: tracer_xpd(pcols,pcnst)                   ! expected value (w0 + dt*boundary_flux)
    real(r8) :: tracer_dif(pcols,pcnst)                   ! tracer_inp - original tracer
    real(r8) :: tracer_tnd(pcols,pcnst)                   ! tendency from last process
    real(r8) :: tracer_rer(pcols,pcnst)                   ! relative error in tracer column

    real(r8) :: tr(pcols)                           ! vertical integral of tracer
    real(r8) :: trpdel(pcols, pver)                       ! pdel for tracer

    integer lchnk                                  ! chunk identifier
    integer ncol                                   ! number of atmospheric columns
    integer  i,k                                   ! column, level indices
    integer :: ixcldice, ixcldliq                  ! CLDICE and CLDLIQ indices
    integer :: ixrain, ixsnow                      ! RAINQM and SNOWQM indices
    integer :: ixgrau                              ! GRAUQM index
    integer :: m                            ! tracer index
    character(len=8) :: tracname   ! tracername
!-----------------------------------------------------------------------

    lchnk = state%lchnk
    ncol  = state%ncol
    call cnst_get_ind('CLDICE', ixcldice, abort=.false.)
    call cnst_get_ind('CLDLIQ', ixcldliq, abort=.false.)
    call cnst_get_ind('RAINQM', ixrain,   abort=.false.)
    call cnst_get_ind('SNOWQM', ixsnow,   abort=.false.)
    call cnst_get_ind('GRAUQM', ixgrau,   abort=.false.)

    do m = 1,pcnst

       if ( any(m == (/ 1, ixcldliq, ixcldice, &
                           ixrain,   ixsnow, ixgrau /)) ) exit   ! dont process water substances
                                                                 ! they are checked in check_energy
       tracname = cnst_name(m)
       if (cnst_get_type_byind(m).eq.'dry') then
          trpdel(:ncol,:) = state%pdeldry(:ncol,:)
       else
          trpdel(:ncol,:) = state%pdel(:ncol,:)
       endif

       ! Compute vertical integrals tracers
       tr = 0._r8
       do k = 1, pver
          do i = 1, ncol
             tr(i) = tr(i) + state%q(i,k,m)*trpdel(i,k)*rga
          end do
       end do

       ! Compute vertical integrals of tracer
       do i = 1, ncol
          tracer_inp(i,m) = tr(i)
       end do

       ! compute expected values and tendencies
       do i = 1, ncol
          ! change in tracers
          tracer_dif(i,m) = tracer_inp(i,m) - tracerint%tracer(i,m)

          ! expected tendencies from boundary fluxes for last process
          tracer_tnd(i,m) = cflx(i,m)

          ! cummulative tendencies from boundary fluxes
          tracerint%tracer_tnd(i,m) = tracerint%tracer_tnd(i,m) + tracer_tnd(i,m)

          ! expected new values from original values plus boundary fluxes
          tracer_xpd(i,m) = tracerint%tracer(i,m) + tracerint%tracer_tnd(i,m)*ztodt

          ! relative error, expected value - input value / original
          tracer_rer(i,m) = (tracer_xpd(i,m) - tracer_inp(i,m)) / tracerint%tracer(i,m)
       end do

!! final loop for error checking
!    do i = 1, ncol

!! error messages
!       if (abs(enrgy_rer(i)) > 1.E-14 .or. abs(water_rer(i)) > 1.E-14) then
!          tracerint%count = tracerint%count + 1
!          write(iulog,*) "significant conservations error after ", name,        &
!               " count", tracerint%count, " nstep", nstep, "chunk", lchnk, "col", i
!          write(iulog,*) enrgy_inp(i),enrgy_xpd(i),enrgy_dif(i),tracerint%enrgy_tnd(i)*ztodt,  &
!               enrgy_tnd(i)*ztodt,enrgy_rer(i)
!          write(iulog,*) water_inp(i),water_xpd(i),water_dif(i),tracerint%water_tnd(i)*ztodt,  &
!               water_tnd(i)*ztodt,water_rer(i)
!       end if
!    end do


       ! final loop for error checking
       if ( maxval(tracer_rer) > 1.E-14_r8 ) then
          write(iulog,*) "CHECK_TRACERS TRACER large rel error"
          write(iulog,*) tracer_rer
       endif

       do i = 1, ncol
          ! error messages
          if (abs(tracer_rer(i,m)) > 1.E-14_r8 ) then
             tracerint%count = tracerint%count + 1
             write(iulog,*) "CHECK_TRACERS TRACER significant conservation error after ", name,        &
                  " count", tracerint%count, " nstep", nstep, "chunk", lchnk, "col",i
             write(iulog,*)' process name, tracname, index ',  name, tracname, m
             write(iulog,*)" input integral              ",tracer_inp(i,m)
             write(iulog,*)" expected integral           ", tracer_xpd(i,m)
             write(iulog,*)" input - inital integral     ",tracer_dif(i,m)
             write(iulog,*)" cumulative tend      ",tracerint%tracer_tnd(i,m)*ztodt
             write(iulog,*)" process tend         ",tracer_tnd(i,m)*ztodt
             write(iulog,*)" relative error       ",tracer_rer(i,m)
             call endrun()
          end if
       end do
    end do

    return
  end subroutine check_tracers_chng

!#######################################################################

  subroutine tot_energy_phys(state, outfld_name_suffix,vc)
    use physconst,       only: rga,rearth,omega
    use cam_thermo,      only: get_hydrostatic_energy,thermo_budget_num_vars,thermo_budget_vars, &
                               wvidx,wlidx,wiidx,seidx,poidx,keidx,moidx,mridx,ttidx,teidx
    use cam_history,     only: outfld
    use dyn_tests_utils, only: vc_physics
    use cam_thermo_formula, only: ENERGY_FORMULA_DYCORE_SE, ENERGY_FORMULA_DYCORE_MPAS

    use cam_abortutils,  only: endrun
    use cam_history_support, only: max_fieldname_len
    use cam_budget,      only: thermo_budget_history
!------------------------------Arguments--------------------------------

    type(physics_state), intent(inout) :: state
    character(len=*),    intent(in)    :: outfld_name_suffix ! suffix for "outfld"
    integer, optional,   intent(in)    :: vc                 ! vertical coordinate (controls energy formula to use)

!---------------------------Local storage-------------------------------
    real(r8) :: se(pcols)                          ! Dry Static energy (J/m2)
    real(r8) :: po(pcols)                          ! surface potential or potential energy (J/m2)
    real(r8) :: ke(pcols)                          ! kinetic energy    (J/m2)
    real(r8) :: wv(pcols)                          ! column integrated vapor       (kg/m2)
    real(r8) :: liq(pcols)                         ! column integrated liquid      (kg/m2)
    real(r8) :: ice(pcols)                         ! column integrated ice         (kg/m2)
    real(r8) :: tt(pcols)                          ! column integrated test tracer (kg/m2)
    real(r8) :: mr(pcols)                          ! column integrated wind axial angular momentum (kg*m2/s)
    real(r8) :: mo(pcols)                          ! column integrated mass axial angular momentum (kg*m2/s)
    real(r8) :: tt_tmp,mr_tmp,mo_tmp,cos_lat
    real(r8) :: mr_cnst, mo_cnst
    real(r8) :: cp_or_cv(pcols,pver)               ! cp for pressure-based vcoord and cv for height vcoord
    real(r8) :: temp(pcols,pver)                   ! temperature
    real(r8) :: scaling(pcols,pver)                ! scaling for conversion of temperature increment

    integer :: lchnk                               ! chunk identifier
    integer :: ncol                                ! number of atmospheric columns
    integer :: i,k                                 ! column, level indices
    integer :: vc_loc                              ! local vertical coordinate variable
    integer :: ixtt                                ! test tracer index
    character(len=max_fieldname_len) :: name_out(thermo_budget_num_vars)

!-----------------------------------------------------------------------

    if (.not.thermo_budget_history) return

    do i=1,thermo_budget_num_vars
       name_out(i)=trim(thermo_budget_vars(i))//'_'//trim(outfld_name_suffix)
    end do

    lchnk = state%lchnk
    ncol  = state%ncol

    ! The "vertical coordinate" parameter is equivalent to the dynamical core
    ! energy formula parameter, which controls the dycore energy formula used
    ! by get_hydrostatic_energy.
    if (present(vc)) then
      vc_loc = vc
    else
      vc_loc = vc_physics
    end if

    if (state%psetcols == pcols) then
      if (vc_loc == ENERGY_FORMULA_DYCORE_MPAS .or. vc_loc == ENERGY_FORMULA_DYCORE_SE) then
        cp_or_cv(:ncol,:) = cp_or_cv_dycore(:ncol,:,lchnk)
      else
        cp_or_cv(:ncol,:) = cpairv(:ncol,:,lchnk)
      end if
    else
      call endrun('tot_energy_phys: energy diagnostics not implemented/tested for subcolumns')
    end if

    if (vc_loc == ENERGY_FORMULA_DYCORE_MPAS .or. vc_loc == ENERGY_FORMULA_DYCORE_SE) then
      scaling(:ncol,:) = cpairv(:ncol,:,lchnk)/cp_or_cv(:ncol,:)!scaling for energy consistency
    else
      scaling(:ncol,:) = 1.0_r8 !internal energy / enthalpy same as CAM physics
    end if
    ! scale accumulated temperature increment for internal energy / enthalpy consistency
    temp(1:ncol,:) = state%temp_ini(1:ncol,:)+scaling(1:ncol,:)*(state%T(1:ncol,:)- state%temp_ini(1:ncol,:))
    call get_hydrostatic_energy(state%q(1:ncol,1:pver,1:pcnst),.true.,               &
         state%pdel(1:ncol,1:pver), cp_or_cv(1:ncol,1:pver),                         &
         state%u(1:ncol,1:pver), state%v(1:ncol,1:pver), temp(1:ncol,1:pver),        &
         vc_loc, ptop=state%pintdry(1:ncol,1), phis = state%phis(1:ncol),            &
         z_mid = state%z_ini(1:ncol,:), se = se(1:ncol),                             &
         po = po(1:ncol), ke = ke(1:ncol), wv = wv(1:ncol), liq = liq(1:ncol),       &
         ice = ice(1:ncol))

    call cnst_get_ind('TT_LW' , ixtt    , abort=.false.)
    tt    = 0._r8
    if (ixtt > 1) then
      if (name_out(ttidx) == 'TT_pAM'.or.name_out(ttidx) == 'TT_zAM') then
        !
        ! after dme_adjust mixing ratios are all wet
        !
        do k = 1, pver
          do i = 1, ncol
            tt_tmp   = state%q(i,k,ixtt)*state%pdel(i,k)*rga
            tt   (i) = tt(i)    + tt_tmp
          end do
        end do
      else
        do k = 1, pver
          do i = 1, ncol
            tt_tmp   = state%q(i,k,ixtt)*state%pdeldry(i,k)*rga
            tt   (i) = tt(i)    + tt_tmp
          end do
        end do
      end if
    end if

    call outfld(name_out(seidx)  ,se      , pcols   ,lchnk   )
    call outfld(name_out(poidx)  ,po      , pcols   ,lchnk   )
    call outfld(name_out(keidx)  ,ke      , pcols   ,lchnk   )
    call outfld(name_out(wvidx)  ,wv      , pcols   ,lchnk   )
    call outfld(name_out(wlidx)  ,liq     , pcols   ,lchnk   )
    call outfld(name_out(wiidx)  ,ice     , pcols   ,lchnk   )
    call outfld(name_out(ttidx)  ,tt      , pcols   ,lchnk   )
    call outfld(name_out(teidx)  ,se+ke+po, pcols   ,lchnk   )
    !
    ! Axial angular momentum diagnostics
    !
    ! Code follows
    !
    ! Lauritzen et al., (2014): Held-Suarez simulations with the Community Atmosphere Model
    ! Spectral Element (CAM-SE) dynamical core: A global axial angularmomentum analysis using Eulerian
    ! and floating Lagrangian vertical coordinates. J. Adv. Model. Earth Syst. 6,129-140,
    ! doi:10.1002/2013MS000268
    !
    ! MR is equation (6) without \Delta A and sum over areas (areas are in units of radians**2)
    ! MO is equation (7) without \Delta A and sum over areas (areas are in units of radians**2)
    !

    mr_cnst = rga*rearth**3
    mo_cnst = rga*omega*rearth**4

    mr = 0.0_r8
    mo = 0.0_r8
    do k = 1, pver
       do i = 1, ncol
          cos_lat = cos(state%lat(i))
          mr_tmp = mr_cnst*state%u(i,k)*state%pdel(i,k)*cos_lat
          mo_tmp = mo_cnst*state%pdel(i,k)*cos_lat**2

          mr(i) = mr(i) + mr_tmp
          mo(i) = mo(i) + mo_tmp
       end do
    end do

    call outfld(name_out(mridx)  ,mr, pcols,lchnk   )
    call outfld(name_out(moidx)  ,mo, pcols,lchnk   )

  end subroutine tot_energy_phys

  ! Compute global mean total energy of physics input and output states
  ! computed consistently with dynamical core vertical coordinate
  ! (under hydrostatic assumption)
  !
  ! This subroutine cannot use the CCPP-ized equivalent because
  ! it is dependent on chunks.
  subroutine check_energy_gmean(state, pbuf2d, dtime, nstep)
    use physics_buffer,  only: physics_buffer_desc, pbuf_get_field, pbuf_get_chunk
    use physics_types,   only: dyn_te_idx
    use ppgrid,          only: begchunk, endchunk
    use spmd_utils,      only: masterproc
    use cam_logfile,     only: iulog
    use gmean_mod,       only: gmean
    use physconst,       only: gravit

    type(physics_state), intent(in), dimension(begchunk:endchunk) :: state
    type(physics_buffer_desc), pointer                            :: pbuf2d(:,:)

    real(r8), intent(in) :: dtime        ! physics time step
    integer , intent(in) :: nstep        ! current timestep number

    integer :: ncol                      ! number of active columns
    integer :: lchnk                     ! chunk index

    real(r8) :: te(pcols,begchunk:endchunk,4)
                                         ! total energy of input/output states (copy)
    real(r8) :: te_glob(4)               ! global means of total energy
    real(r8), pointer :: teout(:)

    ! Copy total energy out of input and output states
    do lchnk = begchunk, endchunk
       ncol = state(lchnk)%ncol
       ! input energy using dynamical core energy formula
       te(:ncol,lchnk,1) = state(lchnk)%te_ini(:ncol,dyn_te_idx)
       ! output energy
       call pbuf_get_field(pbuf_get_chunk(pbuf2d,lchnk),teout_idx, teout)

       te(:ncol,lchnk,2) = teout(1:ncol)
       ! surface pressure for heating rate
       te(:ncol,lchnk,3) = state(lchnk)%pint(:ncol,pver+1)
       ! model top pressure for heating rate (not constant for z-based vertical coordinate!)
       te(:ncol,lchnk,4) = state(lchnk)%pint(:ncol,1)
    end do

    ! Compute global means of input and output energies and of
    ! surface pressure for heating rate (assume uniform ptop)
    call gmean(te, te_glob, 4)

    if (begchunk .le. endchunk) then
       teinp_glob = te_glob(1)
       teout_glob = te_glob(2)
       psurf_glob = te_glob(3)
       ptopb_glob = te_glob(4)

       ! Global mean total energy difference
       tedif_glob =  teinp_glob - teout_glob
       heat_glob  = -tedif_glob/dtime * gravit / (psurf_glob - ptopb_glob)
       if (masterproc) then
          write(iulog,'(1x,a9,1x,i8,5(1x,e25.17))') "nstep, te", nstep, teinp_glob, teout_glob, &
               heat_glob, psurf_glob, ptopb_glob
       end if
    else
       heat_glob = 0._r8
    end if  !  (begchunk .le. endchunk)

  end subroutine check_energy_gmean

  ! Return energy integrals (module variables)
  subroutine check_energy_get_integrals(tedif_glob_out, heat_glob_out)
     real(r8), intent(out), optional :: tedif_glob_out
     real(r8), intent(out), optional :: heat_glob_out

   if ( present(tedif_glob_out) ) then
      tedif_glob_out = tedif_glob
   endif

   if ( present(heat_glob_out) ) then
      heat_glob_out = heat_glob
   endif
  end subroutine check_energy_get_integrals

  ! Compute initial values of energy and water integrals,
  ! zero cumulative tendencies
  subroutine check_energy_timestep_init(state, tend, pbuf, col_type)
    use physics_buffer,  only: physics_buffer_desc, pbuf_set_field
    use cam_abortutils,  only: endrun
    use dyn_tests_utils, only: vc_physics, vc_dycore
    use cam_thermo_formula, only: ENERGY_FORMULA_DYCORE_SE, ENERGY_FORMULA_DYCORE_MPAS
    use physics_types,   only: physics_tend
    use physics_types,   only: phys_te_idx, dyn_te_idx
    use time_manager,    only: is_first_step
    use physconst,       only: cpair, rair
    use air_composition, only: cpairv, cp_or_cv_dycore

    ! CCPP-ized subroutine
    use check_energy_chng, only: check_energy_chng_timestep_init

    type(physics_state),   intent(inout)    :: state
    type(physics_tend ),   intent(inout)    :: tend
    type(physics_buffer_desc), pointer      :: pbuf(:)
    integer, optional                       :: col_type  ! Flag indicating whether using grid or subcolumns

    real(r8)  :: local_cp_phys(state%psetcols,pver)
    real(r8)  :: local_cp_or_cv_dycore(state%psetcols,pver)
    real(r8)  :: teout(state%ncol) ! dummy teout argument
    integer   :: lchnk     ! chunk identifier
    integer   :: ncol      ! number of atmospheric columns
    character(len=512) :: errmsg
    integer            :: errflg

    lchnk = state%lchnk
    ncol  = state%ncol

    ! The code below is split into not-subcolumns and subcolumns code, as there is different handling of the
    ! cp passed into the hydrostatic energy call. CAM-SIMA does not support subcolumns, so we keep this special
    ! handling inside this CAM interface. (hplin, 9/9/24)
    if(state%psetcols == pcols) then
        ! No subcolumns
        local_cp_phys(:ncol,:) = cpairv(:ncol,:,lchnk)
        local_cp_or_cv_dycore(:ncol,:) = cp_or_cv_dycore(:ncol,:,lchnk)
    else if (state%psetcols > pcols) then
        ! Subcolumns code
        ! Subcolumns specific error handling
        if(.not. all(cpairv(:,:,lchnk) == cpair)) then
            call endrun('check_energy_timestep_init: cpairv is not allowed to vary when subcolumns are turned on')
        endif

        local_cp_phys(1:ncol,:) = cpair

        if (vc_dycore == ENERGY_FORMULA_DYCORE_MPAS) then
            ! MPAS specific hydrostatic energy computation (internal energy)
            local_cp_or_cv_dycore(:ncol,:) = cpair-rair
        else if(vc_dycore == ENERGY_FORMULA_DYCORE_SE) then
            ! SE specific hydrostatic energy (enthalpy)
            local_cp_or_cv_dycore(:ncol,:) = cpair
        else
            ! cp_or_cv is not used in the underlying subroutine, zero it out to be sure
            local_cp_or_cv_dycore(:ncol,:) = 0.0_r8
        endif
    end if

    ! Call CCPP-ized underlying subroutine.
    call check_energy_chng_timestep_init( &
        ncol            = ncol, &
        pver            = pver, &
        pcnst           = pcnst, &
        is_first_timestep = is_first_step(), &
        q               = state%q(1:ncol,1:pver,1:pcnst), &
        pdel            = state%pdel(1:ncol,1:pver), &
        u               = state%u(1:ncol,1:pver), &
        v               = state%v(1:ncol,1:pver), &
        T               = state%T(1:ncol,1:pver), &
        pintdry         = state%pintdry(1:ncol,1:pver), &
        phis            = state%phis(1:ncol), &
        zm              = state%zm(1:ncol,:), &
        cp_phys         = local_cp_phys(1:ncol,:), &
        cp_or_cv_dycore = local_cp_or_cv_dycore(1:ncol,:), &
        te_ini_phys     = state%te_ini(1:ncol,phys_te_idx), &
        te_ini_dyn      = state%te_ini(1:ncol,dyn_te_idx),  &
        tw_ini          = state%tw_ini(1:ncol),             &
        te_cur_phys     = state%te_cur(1:ncol,phys_te_idx), &
        te_cur_dyn      = state%te_cur(1:ncol,dyn_te_idx),  &
        tw_cur          = state%tw_cur(1:ncol),             &
        tend_te_tnd     = tend%te_tnd(1:ncol),              &
        tend_tw_tnd     = tend%tw_tnd(1:ncol),              &
        temp_ini        = state%temp_ini(:ncol,:),          &
        z_ini           = state%z_ini(:ncol,:),             &
        count           = state%count,                      &
        teout           = teout(1:ncol),                    & ! dummy argument - actual teout written to pbuf directly below
        energy_formula_physics = vc_physics,                &
        energy_formula_dycore  = vc_dycore,                 &
        errmsg          = errmsg, &
        errflg          = errflg  &
    )

    ! initialize physics buffer
    if (is_first_step()) then
       call pbuf_set_field(pbuf, teout_idx, state%te_ini(:,dyn_te_idx), col_type=col_type)
    end if

  end subroutine check_energy_timestep_init

  ! Check that the energy and water change matches the boundary fluxes
  subroutine check_energy_cam_chng(state, tend, name, nstep, ztodt,        &
       flx_vap, flx_cnd, flx_ice, flx_sen)
    use dyn_tests_utils,    only: vc_physics, vc_dycore
    use cam_thermo_formula, only: ENERGY_FORMULA_DYCORE_SE, ENERGY_FORMULA_DYCORE_MPAS
    use cam_abortutils,     only: endrun
    use physics_types,      only: phys_te_idx, dyn_te_idx
    use physics_types,      only: physics_tend
    use physconst,          only: cpair, rair, latice, latvap
    use air_composition,    only: cpairv, cp_or_cv_dycore

    ! CCPP-ized subroutine
    use check_energy_chng,  only: check_energy_chng_run

    type(physics_state), intent(inout) :: state
    type(physics_tend ), intent(inout) :: tend
    character*(*),intent(in) :: name               ! parameterization name for fluxes
    integer , intent(in) :: nstep                  ! current timestep number
    real(r8), intent(in) :: ztodt                  ! physics timestep (s)
    real(r8), intent(in) :: flx_vap(:)             ! (pcols) - boundary flux of vapor (kg/m2/s)
    real(r8), intent(in) :: flx_cnd(:)             ! (pcols) - boundary flux of lwe liquid+ice (m/s)
    real(r8), intent(in) :: flx_ice(:)             ! (pcols) - boundary flux of lwe ice (m/s)
    real(r8), intent(in) :: flx_sen(:)             ! (pcols) - boundary flux of sensible heat (W/m2)

    integer :: lchnk                               ! chunk identifier
    integer :: ncol                                ! number of atmospheric columns
    real(r8)  :: local_cp_phys(state%psetcols,pver)
    real(r8)  :: local_cp_or_cv_dycore(state%psetcols,pver)
    real(r8)  :: scaling_dycore(state%ncol,pver)
    character(len=512) :: errmsg
    integer            :: errflg

    lchnk = state%lchnk
    ncol  = state%ncol

    if(state%psetcols == pcols) then
        ! No subcolumns
        local_cp_phys(:ncol,:) = cpairv(:ncol,:,lchnk)

        ! Only if using MPAS or SE energy formula cp_or_cv_dycore is nonzero.
        if(vc_dycore == ENERGY_FORMULA_DYCORE_MPAS .or. vc_dycore == ENERGY_FORMULA_DYCORE_SE) then
            local_cp_or_cv_dycore(:ncol,:) = cp_or_cv_dycore(:ncol,:,lchnk)

            scaling_dycore(:ncol,:)  = cpairv(:ncol,:,lchnk)/local_cp_or_cv_dycore(:ncol,:) ! cp/cv scaling
        endif
    else if(state%psetcols > pcols) then
        ! Subcolumns
        if(.not. all(cpairv(:,:,:) == cpair)) then
            call endrun('check_energy_chng: cpairv is not allowed to vary when subcolumns are turned on')
        endif
        local_cp_phys(:,:) = cpair
        ! Note: cp_or_cv set above for pressure coordinate
        if (vc_dycore == ENERGY_FORMULA_DYCORE_MPAS) then
            ! compute cv if vertical coordinate is height: cv = cp - R
            local_cp_or_cv_dycore(:ncol,:) = cpair-rair
            scaling_dycore(:ncol,:)  = cpairv(:ncol,:,lchnk)/local_cp_or_cv_dycore(:ncol,:) ! cp/cv scaling
        else if (vc_dycore == ENERGY_FORMULA_DYCORE_SE) then
            ! SE specific hydrostatic energy
            local_cp_or_cv_dycore(:ncol,:) = cpair
            scaling_dycore(:ncol,:) = 1.0_r8
        else
            ! Moist pressure... use phys formula, cp_or_cv_dycore is unused. Reset for safety
            local_cp_or_cv_dycore(:ncol,:) = 0.0_r8
            scaling_dycore(:ncol,:)  = 0.0_r8
        end if
    endif

    ! Call CCPP-ized underlying subroutine.
    call check_energy_chng_run( &
        ncol            = ncol, &
        pver            = pver, &
        pcnst           = pcnst, &
        iulog           = iulog, &
        q               = state%q(1:ncol,1:pver,1:pcnst), &
        pdel            = state%pdel(1:ncol,1:pver), &
        u               = state%u(1:ncol,1:pver), &
        v               = state%v(1:ncol,1:pver), &
        T               = state%T(1:ncol,1:pver), &
        pintdry         = state%pintdry(1:ncol,1:pver), &
        phis            = state%phis(1:ncol), &
        zm              = state%zm(1:ncol,:), &
        cp_phys         = local_cp_phys(1:ncol,:), &
        cp_or_cv_dycore = local_cp_or_cv_dycore(1:ncol,:),  &
        scaling_dycore  = scaling_dycore(1:ncol,:),         &
        te_cur_phys     = state%te_cur(1:ncol,phys_te_idx), &
        te_cur_dyn      = state%te_cur(1:ncol,dyn_te_idx),  &
        tw_cur          = state%tw_cur(1:ncol),             &
        tend_te_tnd     = tend%te_tnd(1:ncol),              &
        tend_tw_tnd     = tend%tw_tnd(1:ncol),              &
        temp_ini        = state%temp_ini(:ncol,:),          &
        z_ini           = state%z_ini(:ncol,:),             &
        count           = state%count,                      &
        ztodt           = ztodt,                            &
        latice          = latice,                           &
        latvap          = latvap,                           &
        energy_formula_physics = vc_physics,                &
        energy_formula_dycore  = vc_dycore,                 &
        name            = name,       &
        flx_vap         = flx_vap,    &
        flx_cnd         = flx_cnd,    &
        flx_ice         = flx_ice,    &
        flx_sen         = flx_sen,    &
        errmsg          = errmsg, &
        errflg          = errflg  &
    )

  end subroutine check_energy_cam_chng

  ! Add heating rate required for global mean total energy conservation
  subroutine check_energy_cam_fix(state, ptend, nstep, eshflx)
    use physics_types,    only: physics_ptend, physics_ptend_init
    use physconst,        only: gravit

    ! SCAM support
    use scamMod,          only: single_column, use_camiop, heat_glob_scm
    use cam_history,      only: write_camiop
    use cam_history,      only: outfld

    ! CCPP-ized subroutine
    use check_energy_fix, only: check_energy_fix_run

    type(physics_state), intent(in)    :: state
    type(physics_ptend), intent(out)   :: ptend

    integer , intent(in)  :: nstep          ! time step number
    real(r8), intent(out) :: eshflx(pcols)  ! effective sensible heat flux

    integer     :: ncol                     ! number of atmospheric columns in chunk
    integer     :: lchnk                    ! chunk number
    real(r8)    :: heat_out(pcols)
    character(len=64) :: dummy_scheme_name  ! dummy scheme name for CCPP-ized scheme

    integer            :: errflg
    character(len=512) :: errmsg

    lchnk = state%lchnk
    ncol  = state%ncol

    call physics_ptend_init(ptend, state%psetcols, 'chkenergyfix', ls=.true.)

#if ( defined OFFLINE_DYN )
    ! disable the energy fix for offline driver
    heat_glob = 0._r8
#endif

    ! Special handling of energy fix for SCAM - supplied via CAMIOP - zero's for normal IOPs
    if (single_column) then
       if (use_camiop) then
          heat_glob = heat_glob_scm(1)
       else
          heat_glob = 0._r8
       endif
    endif

    if (nstep > 0 .and. write_camiop) then
      heat_out(:ncol) = heat_glob
      call outfld('heat_glob',  heat_out(:ncol), pcols, lchnk)
    endif

    ! Call the CCPP-ized subroutine (for non-SCAM)
    ! to compute the effective sensible heat flux and save to ptend%s
    call check_energy_fix_run( &
        ncol        = ncol, &
        pver        = pver, &
        pint        = state%pint(:ncol,:), &
        gravit      = gravit, &
        heat_glob   = heat_glob, &
        ptend_s     = ptend%s(:ncol,:), &
        eshflx      = eshflx(:ncol), &
        scheme_name = dummy_scheme_name, &
        errmsg      = errmsg, &
        errflg      = errflg  &
    )

  end subroutine check_energy_cam_fix

!===============================================================================

  subroutine enthalpy_adjustment(ncol, lchnk, state, cam_in, cam_out, pbuf, ztodt, itim_old,&
       qini,totliqini,toticeini,tend)

    ! This routine is called by routine tphysac and is only called if compute_enthalpy_flux is .true.

    use camsrfexch,      only: cam_in_t, cam_out_t, get_prec_vars
    use physics_buffer,  only: pbuf_get_index, physics_buffer_desc, pbuf_set_field, pbuf_get_field
    use cam_abortutils,  only: endrun
    use air_composition, only: hliq_idx, hice_idx, fliq_idx, fice_idx, num_enthalpy_vars
    use air_composition, only: cpairv, cp_or_cv_dycore, te_init
    use air_composition, only: thermodynamic_active_species_liq_num,thermodynamic_active_species_liq_idx
    use air_composition, only: thermodynamic_active_species_ice_num,thermodynamic_active_species_ice_idx
    use physconst,       only: cpliq, cpice, cpwv, tmelt
    use air_composition, only: t00a, h00a
    use physconst,       only: rga, latvap, latice
    use dyn_tests_utils, only: vc_dycore
    use cam_thermo,      only: get_hydrostatic_energy
    use physics_types,   only: physics_dme_adjust_camnor, dyn_te_idx
    use cam_thermo,      only: cam_thermo_water_update
    use cam_history,     only: outfld
    use cam_budget,      only: thermo_budget_history
    use time_manager,    only: get_nstep

    ! Arguments
    integer,                         intent(in)    :: ncol, lchnk
    type(physics_state),             intent(inout) :: state
    type(cam_in_t),                  intent(in   ) :: cam_in
    type(cam_out_t),                 intent(inout) :: cam_out
    type(physics_buffer_desc),       pointer       :: pbuf(:)
    real(r8),                        intent(in)    :: ztodt
    integer,                         intent(in)    :: itim_old
    real(r8), dimension(pcols,pver), intent(in)    :: qini, totliqini, toticeini
    type(physics_tend )    ,         intent(inout) :: tend

    ! Local variables
    integer:: enthalpy_prec_bc_idx, enthalpy_prec_ac_idx, enthalpy_evop_idx
    real(r8), dimension(:,:), pointer            :: enthalpy_prec_bc
    real(r8), dimension(pcols,num_enthalpy_vars) :: enthalpy_prec_ac
    real(r8), dimension(pcols)                   :: fliq_tot, fice_tot

    integer:: dp_ntprp_idx, dp_ntsnp_idx
    real(r8), dimension(:,:), pointer :: dp_ntprp, dp_ntsnp
    integer:: qrain_mg_idx,qsnow_mg_idx
    real(r8), dimension(:,:), pointer :: qrain_mg, qsnow_mg

    real(r8), dimension(pcols)      :: te        , se        , po        , ke
    real(r8), dimension(pcols)      :: te_endphys, se_endphys, po_endphys, ke_endphys
    real(r8), dimension(pcols)      :: te_dme    , se_dme    , po_dme    , ke_dme
    real(r8), dimension(pcols)      :: te_enth_fix      , se_enth_fix        , po_enth_fix    , ke_enth_fix
    real(r8), dimension(pcols)      :: fct_bc_tot, fct_ac_tot
    real(r8), dimension(pcols)      :: enthalpy_heating_fix_bc, enthalpy_heating_fix_ac

    real(r8), dimension(pcols)      :: dEdt_physics
    real(r8), dimension(pcols)      :: dEdt_dme
    real(r8), dimension(pcols)      :: dEdt_cpdycore
    real(r8), dimension(pcols)      :: dEdt_enth_fix, dEdt_efix
    real(r8), dimension(pcols)      :: constant_latent_heat_surface  !xxx diagnostics
    real(r8), dimension(pcols)      :: variable_latent_heat_surface_cpice_term !xxx diagnostics
    real(r8), dimension(pcols)      :: variable_latent_heat_surface_ls_term !xxx diagnostics
    real(r8), dimension(pcols)      :: variable_latent_heat_surface_lf_term !xxx diagnostics
    real(r8), dimension(pcols)      :: enthalpy_flux_atm, enthalpy_flux_ocn
    real(r8), dimension(pcols,pver) :: tmp_t, pdel_rf, qinp, totliqinp, toticeinp
    real(r8), dimension(pcols)      :: zero, dsema, dcp_heat, iedme
    real(r8), dimension(pcols)      :: water_flux_bc, water_flux_ac, enthalpy_flux_bc, enthalpy_flux_ac
    real(r8), dimension(pcols)      :: eflx_out
    real(r8), dimension(pcols)      :: mflx_out
    real(r8), dimension(pcols)      :: hevap_atm, hevap_ocn
    real(r8), dimension(pcols)      :: tevp, tprc, nocnfrc

    real(r8), dimension(pcols,pver) :: rnsrc_pbc, snsrc_pbc
    real(r8), dimension(pcols,pver) :: rnsrc_pac, snsrc_pac
    real(r8), dimension(pcols,pver) :: rnsrc_tot, snsrc_tot
    real(r8), dimension(pcols)      :: watrerr,rainerr,snowerr

    integer nstep, ixq, m, m_cnst
    real(r8), dimension(pcols,pver) :: fct_bc, fct_ac
    real(r8), dimension(pcols,pver) :: scale_cpdry_cpdycore, ttend_hfix
    real(r8), parameter :: eps=1.E-10_r8
    logical , parameter :: debug_enthalpy=.false.
    integer :: i, k
    real(r8):: tot, wgt_bc, wgt_ac
    !-----------------------------------------------------------------------------

    nstep = get_nstep()
    zero(:)=0._r8

    ! scale temperature for consistency with dycore (partial adj. after cp update done implicitly in dme)
    do k = 1, pver
       do i = 1, ncol
          scale_cpdry_cpdycore(i,k) = cpairv(i,k,lchnk)/cp_or_cv_dycore(i,k,lchnk)
          state%T  (i,k) = state%temp_ini(i,k)+scale_cpdry_cpdycore(i,k)*(state%T(i,k)- state%temp_ini(i,k))
          tend%dtdt(i,k) = scale_cpdry_cpdycore(i,k)*tend%dtdt(i,k)
       end do
    end do

    !-------------------------------------------------------------------------------------------
    ! from this point onwards computation consistent with variable latent heat total energy formula
    ! Equation 78 in https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2022MS003117
    !-------------------------------------------------------------------------------------------

    !=== start computation of material enthalpy fluxes ===
    ! evaporation enthalpy flux
    enthalpy_evop_idx    = pbuf_get_index('ENTHALPY_EVOP'   , errcode=i)
    if (enthalpy_evop_idx==0) then
       call endrun("pbufs for enthalpy evap flux not allocated")
    end if

    ! using merged quantities, for atmospheric mat.enthalpy flux (used in check_energy)
    if (minval(cam_in%ts(:ncol)).gt.0._r8) then
       hevap_atm(:ncol) = cam_in%cflx(:ncol,1)*(cpwv*(cam_in%ts (:ncol)-t00a)+(cpliq*t00a+h00a))   ! into atm
       tevp(:ncol)= cam_in%ts(:ncol)
       ! for ocean-only  mat.enthalpy flux (passed to ocean)
       hevap_ocn (:ncol)= cam_in%evap_ocn(:ncol)*(cpwv*(cam_in%sst(:ncol)-t00a)+(cpliq*t00a+h00a))
    else ! not great but better than zeros
       hevap_atm (:ncol)= cam_in%cflx(:ncol,1)*(cpwv*(state%t(:ncol,pver)-t00a)+(cpliq*t00a+h00a)) ! into atm
       tevp      (:ncol)= state%t(:ncol,pver)
       hevap_ocn (:ncol)= hevap_atm(:ncol) ! out of ocn
    endif
    call pbuf_set_field(pbuf, enthalpy_evop_idx, hevap_ocn)

    !------------------------------------------------------------------
    ! compute precipitation fluxes and set associated physics buffers
    !------------------------------------------------------------------
    enthalpy_prec_bc_idx = pbuf_get_index('ENTHALPY_PREC_BC', errcode=i)
    enthalpy_prec_ac_idx = pbuf_get_index('ENTHALPY_PREC_AC', errcode=i)
    if (enthalpy_prec_bc_idx==0.or.enthalpy_prec_ac_idx==0) then
       call endrun("pbufs for enthalpy precip flux not allocated")
    end if
    call pbuf_get_field(pbuf, enthalpy_prec_bc_idx, enthalpy_prec_bc)
    call get_prec_vars(ncol,pbuf,fliq=fliq_tot,fice=fice_tot)

    ! fliq_tot holds liquid precipitation from tphysbc and tphysac; idem for ice
    enthalpy_prec_ac(:ncol,fice_idx) = fice_tot(:ncol)-enthalpy_prec_bc(:ncol,fice_idx)
    enthalpy_prec_ac(:ncol,fliq_idx) = fliq_tot(:ncol)-enthalpy_prec_bc(:ncol,fliq_idx)

    ! compute precipitation enthalpy fluxes from tphysbc
    tprc(:ncol) = cam_out%tbot(:ncol)

    ! correct for reference T of latent heats (liquid reference state)
    enthalpy_prec_ac(:ncol,hice_idx) =  -enthalpy_prec_ac(:ncol,fice_idx)*(cpice*(tprc(:ncol)-t00a)+(cpliq*t00a+h00a))
    enthalpy_prec_ac(:ncol,hliq_idx) =  -enthalpy_prec_ac(:ncol,fliq_idx)*(cpliq*(tprc(:ncol)-t00a)+(cpliq*t00a+h00a))
    call pbuf_set_field(pbuf, enthalpy_prec_ac_idx, enthalpy_prec_ac)

    ! compute total enthalpy flux
    enthalpy_flux_bc (:ncol) = enthalpy_prec_bc(:ncol,hliq_idx)+enthalpy_prec_bc(:ncol,hice_idx)
    enthalpy_flux_ac (:ncol) = enthalpy_prec_ac(:ncol,hliq_idx)+enthalpy_prec_ac(:ncol,hice_idx) &
         + hevap_atm(:ncol)
    water_flux_bc(:ncol) = enthalpy_prec_bc(:ncol,fliq_idx)+enthalpy_prec_bc(:ncol,fice_idx)
    water_flux_ac(:ncol) = enthalpy_prec_ac(:ncol,fliq_idx)+enthalpy_prec_ac(:ncol,fice_idx) &
         - cam_in%cflx(:ncol,1)
    enthalpy_flux_atm(:ncol) = enthalpy_prec_bc(:ncol,hliq_idx)+enthalpy_prec_bc(:ncol,hice_idx) &
         + enthalpy_prec_ac(:ncol,hliq_idx)+enthalpy_prec_ac(:ncol,hice_idx) &
         + hevap_atm(:ncol)
    enthalpy_flux_ocn(:ncol) = enthalpy_prec_bc(:ncol,hliq_idx)+enthalpy_prec_bc(:ncol,hice_idx) &
         + enthalpy_prec_ac(:ncol,hliq_idx)+enthalpy_prec_ac(:ncol,hice_idx) &
         + hevap_ocn(:ncol)
    enthalpy_flux_ocn(:ncol) = cam_in%ocnfrac(:ncol)*enthalpy_flux_ocn(:ncol)

    if (debug_enthalpy) then
       call outfld("enth_prec_ac_hice"  , enthalpy_prec_ac(:,hice_idx)     , pcols   ,lchnk   )
       call outfld("enth_prec_ac_hliq"  , enthalpy_prec_ac(:,hliq_idx)     , pcols   ,lchnk   )
       call outfld("enth_prec_bc_hice"  , enthalpy_prec_bc(:,hice_idx)     , pcols   ,lchnk   )
       call outfld("enth_prec_bc_hliq"  , enthalpy_prec_bc(:,hliq_idx)     , pcols   ,lchnk   )
       call outfld("enth_prec_ac_fice"  , enthalpy_prec_ac(:,fice_idx)     , pcols   ,lchnk   )
       call outfld("enth_prec_ac_fliq"  , enthalpy_prec_ac(:,fliq_idx)     , pcols   ,lchnk   )
       call outfld("enth_prec_bc_fice"  , enthalpy_prec_bc(:,fice_idx)     , pcols   ,lchnk   )
       call outfld("enth_prec_bc_fliq"  , enthalpy_prec_bc(:,fliq_idx)     , pcols   ,lchnk   )
       call outfld("enth_hevap_atm"     , hevap_atm       (:)              , pcols   ,lchnk   )
       call outfld("enth_hevap_ocn"     , hevap_ocn       (:)              , pcols   ,lchnk   )
    endif
    !=== end computation of material enthalpy fluxes ===

    !+++ diags
    ! compute total energy after physics using equation 78
    call get_hydrostatic_energy(state%q(1:ncol,1:pver,1:pcnst),.true.,            &
         state%pdel(1:ncol,1:pver), cp_or_cv_dycore(:ncol,:,lchnk),               &
         state%u(1:ncol,1:pver), state%v(1:ncol,1:pver), state%T(1:ncol,1:pver),&
         vc_dycore, ptop=state%pintdry(1:ncol,1), phis = state%phis(1:ncol),     &
         te = te_endphys(:ncol), se=se_endphys(:ncol), po=po_endphys(:ncol), ke=ke_endphys(:ncol))
    ! the column integrated total energy change should match accumlated te_tnd:
    !                         dEdt_physics=te_tnd
    call outfld ('te_tnd',tend%te_tnd  , pcols, lchnk)
    dEdt_physics(:ncol) = (te_endphys(:ncol)-te_init(:ncol,1,lchnk))/ztodt
    call outfld ('dEdt_physics', dEdt_physics, pcols, lchnk)
    !--- sgaid

    !+ get pbuf fields for precip
    dp_ntprp_idx = pbuf_get_index('dp_ntprp',errcode=i) !prec production from ZM
    dp_ntsnp_idx = pbuf_get_index('dp_ntsnp',errcode=i) !snow production from ZM
    call pbuf_get_field(pbuf, dp_ntprp_idx , dp_ntprp)
    call pbuf_get_field(pbuf, dp_ntsnp_idx , dp_ntsnp)
    qrain_mg_idx = pbuf_get_index('qrain_mg',errcode=i) !rain production from MG
    qsnow_mg_idx = pbuf_get_index('qsnow_mg',errcode=i) !snow production from MG
    call pbuf_get_field(pbuf, qrain_mg_idx, qrain_mg)
    call pbuf_get_field(pbuf, qsnow_mg_idx, qsnow_mg)
    rnsrc_pbc(:ncol,:) = dp_ntprp(:ncol,:)-dp_ntsnp(:ncol,:)
    snsrc_pbc(:ncol,:) = dp_ntsnp(:ncol,:)
    rnsrc_pac(:ncol,:) = qrain_mg(:ncol,:)
    snsrc_pac(:ncol,:) = qsnow_mg(:ncol,:)
    rnsrc_tot(:ncol,:) = rnsrc_pbc(:ncol,:)+rnsrc_pac(:ncol,:)
    snsrc_tot(:ncol,:) = snsrc_pbc(:ncol,:)+snsrc_pac(:ncol,:)
    !- picerp rof sdleif fubp teg

    !  Adjust the dry mass in each layer back to the value of physics input state
    !  Adjust air specific enthalpy accordingly. Diagnose boundary enthalpy flux.
    !  Author: Thomas Toniazzo (17.07.21)
    call physics_dme_adjust_camnor(state, tend, qini, totliqini, toticeini, ztodt, &
          ntrnprd=rnsrc_tot*ztodt, &
          ntsnprd=snsrc_tot*ztodt, &
          tevap=tevp, &
          tprec=tprc, &
          mflx=water_flux_bc+water_flux_ac,  &
          eflx=enthalpy_flux_atm, &
          mflx_out=mflx_out, &
          eflx_out=eflx_out, &
          ent_tnd=dsema, &
          pdel_rf=pdel_rf )

    call outfld('IETEND_DME', dsema            , pcols, lchnk)
    call outfld('EFLX'      , enthalpy_flux_atm                 , pcols, lchnk)
    call outfld('MFLX'      , water_flux_bc+water_flux_ac       , pcols, lchnk)

    ! compute and store new column-integrated enthalpy and associated tendency
    call get_hydrostatic_energy(state%q(1:ncol,1:pver,1:pcnst),.true.,          &
         state%pdel(1:ncol,1:pver), cp_or_cv_dycore(:ncol,:,lchnk),                           &
         state%u(1:ncol,1:pver), state%v(1:ncol,1:pver), state%T(1:ncol,1:pver),&
         vc_dycore, ptop=state%pintdry(1:ncol,1), phis = state%phis(1:ncol),    &
         te = te(:ncol), se=se(:ncol), po=po(:ncol), ke=ke(:ncol))

    ! Save final energy for use with global fixer in next timestep -- note sign conventions, and coupling-dependent options
    ! subtract from te the h flux (sign: into atm) that is *not* passed to surface components
    ! and also remove enthalpy of run-off (if added to BLOM)
    state%te_cur(:ncol,dyn_te_idx) = te(:ncol) &
         - ztodt*(enthalpy_flux_atm(:ncol) - enthalpy_flux_ocn(:ncol) - cam_in%hrof(:ncol))
    tend%te_tnd(:ncol) = tend%te_tnd(:ncol) + (enthalpy_flux_ocn(:ncol) + cam_in%hrof(:ncol))  ! B. with run-off

    if (thermo_budget_history) then
       call tot_energy_phys(state, 'phAM')
       call tot_energy_phys(state, 'dyAM', vc=vc_dycore)
    endif

    call pbuf_set_field(pbuf, teout_idx, state%te_cur(:,dyn_te_idx), (/1,itim_old/),(/pcols,1/))
    ! the amount of total energy we need energy fixer to fix (associated with enthalpy flux)
    dEdt_efix(:ncol) = (state%te_cur(:ncol,dyn_te_idx)-te         (:ncol))/ztodt
    call outfld("dEdt_efix_physics"  ,  dEdt_efix  , pcols   ,lchnk   )

 end subroutine enthalpy_adjustment

end module check_energy
