module reset_physics
  use ccpp_kinds,     only: kind_phys
  use test_host_data, only: physics_state
contains
!> \section arg_table_reset_physics_run Argument Table
!! \htmlinclude arg_table_reset_physics_run.html
!!
  subroutine reset_physics_run(state, errmsg, errflg)
    type(physics_state), intent(inout) :: state
    character(len=512),  intent(  out) :: errmsg
    integer,             intent(  out) :: errflg

    errmsg = ''
    errflg = 0
    
    call state%reset()
    
  end subroutine reset_physics_run
  
end module reset_physics
