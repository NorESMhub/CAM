
module constituent_burden

!-----------------------------------------------------------------------------------------
! Purpose: subroutines to generate constituent burden history variables
!
! Revision history:
! 2005-12-21  K. Lindsay       Original version
!-----------------------------------------------------------------------------------------

  use constituents, only: pcnst
  use cam_history_support,  only: fieldname_len
  use co2_cycle, only: c_i, co2_transport

  implicit none

! Public interfaces

  public constituent_burden_init
  public constituent_burden_comp

  private

  character(len=fieldname_len) :: burdennam(pcnst)  ! name of burden history variables
  integer                      :: co2_cnst_ind = -1 ! >0 if CO2 is a constituent

!=========================================================================================

contains

!=========================================================================================

subroutine constituent_burden_init

   use cam_history,   only: addfld, horiz_only
   use constituents,  only: cnst_name, cnst_get_ind

   integer                      :: mind
   integer                      :: ncnst
   character(len=fieldname_len) :: burdennam_inst

   do mind = 2, pcnst
      burdennam = 'TM'//trim(cnst_name(mind))
      call addfld(burdennam(mind), horiz_only, 'A', 'kg/m2', &
           trim(cnst_name(mind)) // ' column burden')
   end do
   call cnst_get_ind('CO2', co2_cnst_ind, abort=.false.)
   if (co2_cnst_ind < 0) then
      call addfld('TMCO2', horiz_only, 'A', 'kg/m2', 'CO2 column burden')
   end if
   if (co2_transport()) then
      ncnst = size(c_i)
      do mind = 1, ncnst
         burdennam_inst = 'TM'//trim(cnst_name(c_i(mind)))//'_INST'
         call addfld(burdennam_inst, horiz_only, 'A', 'kg/m2', &
              trim(cnst_name(c_i(mind))) // ' column burden for instantaneous output')
      end do
   else
      burdennam_inst = 'TMCO2_INST'
      call addfld(burdennam_inst, horiz_only, 'A', 'kg/m2', &
           'CO2 column burden for instantaneous output')
   end if

end subroutine constituent_burden_init

!=========================================================================================

subroutine constituent_burden_comp(state)

  use physics_types, only: physics_state
  use shr_kind_mod,  only: r8 => shr_kind_r8
  use constituents,  only: cnst_type
  use ppgrid,        only: pcols
  use physconst,     only: rga
  use cam_history,   only: outfld, hist_fld_active
  use chem_surfvals, only: chem_surfvals_get

!-----------------------------------------------------------------------
!
! Arguments
!
   type(physics_state), intent(inout) :: state
!
!---------------------------Local workspace-----------------------------

  real(r8) :: ftem(pcols)      ! temporary workspace

  integer                      :: mind, lchnk, ncol
  integer                      :: cind, ncnst
  character(len=fieldname_len) :: burdennam_inst

  lchnk = state%lchnk
  ncol  = state%ncol

  do mind = 2, pcnst
     if (.not. hist_fld_active(burdennam(mind))) cycle
     if (cnst_type(mind) .eq. 'dry') then
        ftem(:ncol) = sum(state%q(:ncol,:,mind) * state%pdeldry(:ncol,:), dim=2) * rga
     else
        ftem(:ncol) = sum(state%q(:ncol,:,mind) * state%pdel(:ncol,:), dim=2) * rga
     end if
     call outfld (burdennam(mind), ftem, pcols, lchnk)
  end do
  ! Compute special instantaneous values
  if (co2_transport()) then
     ncnst = size(c_i)
     do mind = 1, ncnst
        burdennam_inst = burdennam(c_i(mind))//'_INST'
        if (.not. hist_fld_active(burdennam_inst)) cycle
        cind = c_i(mind)
        if (cnst_type(cind) .eq. 'dry') then
           ftem(:ncol) = sum(state%q(:ncol,:,cind) * state%pdeldry(:ncol,:), dim=2) * rga
        else
           ftem(:ncol) = sum(state%q(:ncol,:,cind) * state%pdel(:ncol,:), dim=2) * rga
        end if
        call outfld(burdennam_inst, ftem(:ncol), ncol, lchnk)
     end do
  else if (hist_fld_active('TMCO2') .or. hist_fld_active('TMCO2_INST')) then
     if (co2_cnst_ind > 0) then
        if (cnst_type(co2_cnst_ind) .eq. 'dry') then
           ftem(:ncol) = sum(state%q(:ncol,:,co2_cnst_ind) * state%pdeldry(:ncol,:), dim=2) * rga
        else
           ftem(:ncol) = sum(state%q(:ncol,:,co2_cnst_ind) * state%pdel(:ncol,:), dim=2) * rga
        end if
     else
        ! There is no CO2 tracer, compute from co2mmr
        ftem(:ncol) = chem_surfvals_get('CO2MMR', lchnk, ncol) * sum(state%pdeldry(:ncol,:), dim=2) * rga
     end if
     if (co2_cnst_ind < 0) then
        ! Only output TMCO2 if it was not output in the burdennam loop above
        call outfld('TMCO2', ftem(:ncol), ncol, lchnk)
     end if
     call outfld('TMCO2_INST', ftem(:ncol), ncol, lchnk)
  end if

end subroutine constituent_burden_comp

!=========================================================================================

end module constituent_burden
