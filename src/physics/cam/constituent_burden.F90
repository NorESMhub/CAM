
module constituent_burden

!-----------------------------------------------------------------------------------------
! Purpose: subroutines to generate constituent burden history variables
!
! Revision history:
! 2005-12-21  K. Lindsay       Original version
!-----------------------------------------------------------------------------------------

  use constituents,         only: pcnst
  use cam_history_support,  only: fieldname_len

  implicit none

! Public interfaces

  public constituent_burden_init
  public constituent_burden_comp

  private

  character(len=fieldname_len) :: burdennam(pcnst)     ! name of burden history variables
  character(len=fieldname_len) :: burdennam_inst(pcnst) ! name of instantaneous burden history variables

  save

!=========================================================================================

contains

!=========================================================================================

subroutine constituent_burden_init

  use cam_history,   only: addfld, horiz_only
  use constituents,  only: cnst_name

  integer :: m

  do m = 2, pcnst
    burdennam(m) = 'TM'//trim(cnst_name(m))
    burdennam_inst(m) = 'TM'//trim(cnst_name(m))//'_INST'
    
    call addfld (burdennam(m), horiz_only, 'A', 'kg/m2', &
                 trim(cnst_name(m)) // ' column burden')
    call addfld (burdennam_inst(m), horiz_only, 'I', 'kg/m2', &
                  trim(cnst_name(m)) // ' column burden (instantaneous)')
  end do

end subroutine constituent_burden_init

!=========================================================================================

subroutine constituent_burden_comp(state)

  use physics_types, only: physics_state
  use shr_kind_mod,  only: r8 => shr_kind_r8
  use constituents,  only: cnst_type
  use ppgrid,        only: pcols
  use physconst,     only: rga
  use cam_history,   only: outfld, hist_fld_active

!-----------------------------------------------------------------------
!
! Arguments
!
   type(physics_state), intent(inout) :: state
!
!---------------------------Local workspace-----------------------------

  real(r8) :: ftem(pcols)      ! temporary workspace

  integer :: m, lchnk, ncol

  lchnk = state%lchnk
  ncol  = state%ncol

  do m = 2, pcnst
     if (.not. hist_fld_active(burdennam(m)) .and. &
         .not. hist_fld_active(burdennam_inst(m))) cycle
     if (cnst_type(m) .eq. 'dry') then
        ftem(:ncol) = sum(state%q(:ncol,:,m) * state%pdeldry(:ncol,:), dim=2) * rga
     else
        ftem(:ncol) = sum(state%q(:ncol,:,m) * state%pdel(:ncol,:), dim=2) * rga
     endif
     if (hist_fld_active(burdennam(m))) call outfld (burdennam(m), ftem, pcols, lchnk)
     if (hist_fld_active(burdennam_inst(m))) call outfld (burdennam_inst(m), ftem, pcols, lchnk)
  end do

end subroutine constituent_burden_comp

!=========================================================================================

end module constituent_burden

