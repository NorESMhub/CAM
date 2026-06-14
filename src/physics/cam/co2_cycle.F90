module co2_cycle

   !----------------------------------------------------------------------------
   !
   ! Purpose:
   ! Provides distributions of CO2_LND, CO2_OCN, CO2_FF, CO2
   ! Surface flux from CO2_LND and CO2_OCN provided by the mediator.
   ! Surface flux from CO2_FFF can be read from a file.
   !
   ! Author: Jeff Lee, Keith Lindsay
   !         Mariana Vertenstein, Refactored for NUOPC Stream functionality
   !
   !----------------------------------------------------------------------------

   use shr_kind_mod,  only: r8=>shr_kind_r8

   implicit none
   private

   ! Public interfaces
   public co2_cycle_readnl    ! read the namelist
   public co2_register        ! register consituents
   public co2_transport       ! turn on co2 tracers transport
   public co2_implements_cnst ! returns true if consituent is implemented by this package
   public co2_init_cnst       ! initialize mixing ratios if not read from initial file
   public co2_init            ! initialize (history) variables
   public co2_cycle_set_ptend ! set tendency from aircraft emissions

   ! Namelist variables
   logical                    :: co2_flag              = .false. ! true => turn on co2 code, namelist variable
   logical, public, protected :: co2_readFlux_fuel     = .false. ! true => read fuel     co2 flux from date file, namelist variable
   logical, public, protected :: co2_readFlux_aircraft = .false. ! true => read aircraft co2 flux from date file, namelist variable

   !-------------------------------------------------------------------------------
   ! new constituents
   !-------------------------------------------------------------------------------

   integer, parameter         :: ncnst=4    ! number of constituents implemented
   integer, public, protected :: c_i(ncnst) ! global index for new constituents

   character(len=7), dimension(ncnst), parameter :: & ! constituent names
        c_names = (/'CO2_OCN', 'CO2_FFF', 'CO2_LND', 'CO2    '/)

   integer :: co2_fff_glo_ind = -1 ! global index of 'CO2_FFF'
   integer :: co2_glo_ind = -1     ! global index of 'CO2'
   integer :: idx_ac_CO2 = -1      ! pbuf index of aircraft CO2 field
   logical :: local_co2 = .false.  ! .true. if CO2 const. added in this module

!==============================================================================
contains
!==============================================================================

   subroutine co2_cycle_readnl(nlfile)

      !--------------------------------------------
      ! Purpose: Read co2_cycle_nl namelist group.
      !--------------------------------------------

      use namelist_utils,  only: find_group_name
      use spmd_utils,      only: masterproc, mpicom, masterprocid
      use spmd_utils,      only: mpi_logical, mpi_character, mpi_integer
      use cam_logfile,     only: iulog
      use cam_abortutils,  only: endrun
      use co2_data_flux,   only: co2_data_flux_readnl

      ! Arguments
      character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

      ! Local variables
      integer            :: unitn, ierr
      character(len=256) :: msg
      character(len=*), parameter :: subname = 'co2_cycle_readnl'

      namelist /co2_cycle_nl/       &
           co2_flag,                &
           co2_readFlux_aircraft,   & ! if true, read aircraft data
           co2_readFlux_fuel          ! if true, read fuel data
      !-------------------------------------------------------------------------

      if (masterproc) then
         open( newunit=unitn, file=trim(nlfile), status='old' )
         call find_group_name(unitn, 'co2_cycle_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, co2_cycle_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname // ':: ERROR reading co2_cycle_nl namelist')
            end if
         end if
         close(unitn)
      end if

      call mpi_bcast(co2_flag, 1, mpi_logical, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: co2_flag")
      call mpi_bcast(co2_readFlux_aircraft, 1, mpi_logical, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: co2_readFlux_aircraft")
      call mpi_bcast(co2_readFlux_fuel, 1, mpi_logical, masterprocid, mpicom, ierr)
      if (ierr /= 0) call endrun(subname//": FATAL: mpi_bcast: co2_readFlux_fuel")

      if (masterproc) then
         write(iulog, '(a, l4)') "co2_flag = ", co2_flag
         write(iulog, '(a, l4)')"co2_readFlux_aircraft = ", co2_readFlux_aircraft
         write(iulog, '(a, l4)')"co2_readFlux_fuel = ", co2_readFlux_fuel
      end if

      if (co2_readFlux_fuel) then
         call co2_data_flux_readnl(nlfile)
      end if

   end subroutine co2_cycle_readnl

   !============================================================================

   subroutine co2_register

      !-------------------------------------------------------------------------
      ! Purpose: register advected constituents
      !-------------------------------------------------------------------------

      use physconst,      only: mwco2, cpair
      use constituents,   only: cnst_get_ind, cnst_add
      use cam_abortutils, only: endrun

      ! Local variables
      real(r8), dimension(ncnst) :: &
           c_mw,    &! molecular weights
           c_cp,    &! heat capacities
           c_qmin    ! minimum mmr

      integer  :: icnst
      !-------------------------------------------------------------------------

      if (.not. co2_flag) return

      c_mw   = (/     mwco2,     mwco2,     mwco2,     mwco2 /)
      c_cp   = (/     cpair,     cpair,     cpair,     cpair /)
      c_qmin = (/ 1.e-20_r8, 1.e-20_r8, 1.e-20_r8, 1.e-20_r8 /)

      ! register any new CO2 constiuents as dry tracers, set indices
      ! This logic prevents duplicate CO2 tracers from being created.
      ! If a CO2 tracer already exists do not attempt register one and local_co2
      !   is set to .false. so that other code below (addfld calls,
      !   co2_implements_cnst) does not do anything with that consitituent.
      ! local_co2 = .true. means that the CO2 constituent was created by
      !   and is manged by this module.
      local_co2 = .false.
      do icnst = 1, ncnst
         call cnst_get_ind(c_names(icnst), c_i(icnst), abort=.false.)
         if (c_i(icnst) < 0) then
            call cnst_add(c_names(icnst), c_mw(icnst), c_cp(icnst), c_qmin(icnst), &
                 c_i(icnst), longname=c_names(icnst), mixtype='dry')
            if (trim(c_names(icnst)) == 'CO2') then
               local_co2 = .true.
            end if
         else if (trim(c_names(icnst)) /= 'CO2') then
            call endrun('co2_register: '//trim(c_names(icnst))//' already defined')
         end if
         select case (trim(c_names(icnst)))
         case ('CO2_FFF')
            co2_fff_glo_ind = c_i(icnst)
         case ('CO2')
            co2_glo_ind = c_i(icnst)
         end select
      end do

   end subroutine co2_register

   !============================================================================

   logical function co2_transport()

      !-------------------------------------------------------------------------
      ! Purpose: return true if this package is active
      !-------------------------------------------------------------------------

      !-------------------------------------------------------------------------

      co2_transport = co2_flag

   end function co2_transport

   !============================================================================

   logical function co2_implements_cnst(name)

      !-------------------------------------------------------------------------
      ! Purpose: return true if specified constituent is
      !           implemented by this package
      !-------------------------------------------------------------------------

      ! Arguments
      character(len=*), intent(in) :: name  ! constituent name

      ! Local variables
      integer :: mind
      !-------------------------------------------------------------------------

      co2_implements_cnst = .false.

      if (.not. co2_flag) return

      do mind = 1, ncnst
         if (name == c_names(mind)) then
            if ((trim(name) /= 'CO2') .or. local_co2) then
               co2_implements_cnst = .true.
            end if
            return
         end if
      end do

   end function co2_implements_cnst

   !============================================================================

   subroutine co2_init_cnst(name, latvals, lonvals, mask, q)

      !-------------------------------------------------------------------------
      ! Purpose:
      ! Set initial values of CO2_OCN, CO2_FFF, CO2_LND, CO2
      ! Need to be called from process_inidat in inidat.F90
      ! (or, initialize co2 in co2_timestep_init)
      !-------------------------------------------------------------------------

      use chem_surfvals,  only: chem_surfvals_get

      ! Arguments
      character(len=*), intent(in)  :: name       ! constituent name
      real(r8),         intent(in)  :: latvals(:) ! lat in degrees (ncol)
      real(r8),         intent(in)  :: lonvals(:) ! lon in degrees (ncol)
      logical,          intent(in)  :: mask(:)    ! Only initialize where .true.
      real(r8),         intent(out) :: q(:,:)     ! kg tracer/kg dry air (gcol, plev)

      ! Local variables
      integer :: kindx
      !-------------------------------------------------------------------------

      if (.not. co2_flag) return

      do kindx = 1, size(q, 2)
         select case (name)
         case ('CO2_OCN')
            where(mask)
               q(:, kindx) = chem_surfvals_get('CO2MMR')
            end where
         case ('CO2_FFF')
            where(mask)
               q(:, kindx) = chem_surfvals_get('CO2MMR')
            end where
         case ('CO2_LND')
            where(mask)
               q(:, kindx) = chem_surfvals_get('CO2MMR')
            end where
         case ('CO2')
            where(mask)
               q(:, kindx) = chem_surfvals_get('CO2MMR')
            end where
         end select
      end do

   end subroutine co2_init_cnst

   !============================================================================

   subroutine co2_init

      !-------------------------------------------------------------------------
      ! Purpose: initialize co2,
      !          declare history variables,
      !          read co2 flux form fuel, as data_flux_fuel
      !-------------------------------------------------------------------------

      use cam_history,    only: addfld, add_default, horiz_only
      use constituents,   only: cnst_name, cnst_longname, sflxnam
      use physics_buffer, only: pbuf_get_index

      ! Local variables
      integer :: m, mm
      !-------------------------------------------------------------------------

      if (.not. co2_flag) return

      ! Add constituents and fluxes to history file
      do m = 1, ncnst
         mm = c_i(m)

         call addfld(trim(cnst_name(mm))//'_BOT', horiz_only,  'A', 'kg/kg',   trim(cnst_longname(mm))//', Bottom Layer')
         if (co2_implements_cnst(cnst_name(mm))) then
            call addfld(cnst_name(mm),            (/ 'lev' /), 'A', 'kg/kg',   cnst_longname(mm))
            call addfld(sflxnam(mm),              horiz_only,  'A', 'kg/m2/s', trim(cnst_name(mm))//' surface flux')
         end if

         call add_default(cnst_name(mm), 1, ' ')
         call add_default(sflxnam(mm),   1, ' ')

         ! The addfld call for the 'TM*' fields are made by default in the
         ! constituent_burden module.
         call add_default('TM'//trim(cnst_name(mm)), 1, ' ')
      end do

      ! Find and store the aircraft CO2 index
      ! We can ignore the error code, idx_ac_CO2 is unchanged on error
      idx_ac_CO2 = pbuf_get_index('ac_CO2', errcode=mm)

   end subroutine co2_init

   !============================================================================
   subroutine co2_cycle_set_ptend(state, pbuf, ptend)

      !-------------------------------------------------------------------------
      ! Purpose:
      ! Set ptend, using aircraft CO2 emissions in ac_CO2 from pbuf
      !-------------------------------------------------------------------------

      use physics_types,  only: physics_state, physics_ptend, physics_ptend_init
      use physics_buffer, only: physics_buffer_desc, pbuf_get_field
      use constituents,   only: pcnst
      use ppgrid,         only: pver
      use physconst,      only: gravit

      ! Arguments
      type(physics_state), intent(in)    :: state
      type(physics_buffer_desc), pointer :: pbuf(:)
      type(physics_ptend), intent(out)   :: ptend     ! indivdual parameterization tendencies

      ! Local variables
      logical :: lq(pcnst)
      integer :: ifld, ncol, k
      real(r8), pointer :: ac_CO2(:,:)
      !----------------------------------------------------------------------------

      if (.not. co2_flag .or. .not. co2_readFlux_aircraft) then
         call physics_ptend_init(ptend, state%psetcols, 'none')
         return
      end if

      ! aircraft fluxes are added to 'CO2_FFF' and 'CO2' tendencies
      lq(:)               = .false.
      lq(co2_fff_glo_ind) = .true.
      lq(co2_glo_ind)     = .true.

      call physics_ptend_init(ptend, state%psetcols, 'co2_cycle_ac', lq=lq)

      if (idx_ac_CO2 > 0) then
         call pbuf_get_field(pbuf, idx_ac_CO2, ac_CO2)

         ! [ac_CO2] = 'kg m-2 s-1'
         ! [ptend%q] = 'kg kg-1 s-1'
         ncol = state%ncol
         do k = 1, pver
            ptend%q(:ncol,k,co2_fff_glo_ind) = gravit * state%rpdeldry(:ncol,k) * ac_CO2(:ncol,k)
            ptend%q(:ncol,k,co2_glo_ind)     = gravit * state%rpdeldry(:ncol,k) * ac_CO2(:ncol,k)
         end do
      end if

   end subroutine co2_cycle_set_ptend

end module co2_cycle
