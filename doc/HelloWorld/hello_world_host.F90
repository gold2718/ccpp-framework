module hello_world_host

  use machine, only: kind_phys

  implicit none

  !> \section arg_table_hello_world_host  Argument Table
  !! \htmlinclude arg_table_hello_world_host.html
  !!
  integer, parameter :: ncols = 10
  integer, parameter :: pver = 5
  integer, parameter :: pverp = 6
  integer            :: ntimes_loop
  real(kind_phys)    :: temp_midpoints(ncols, pver)
  real(kind_phys)    :: temp_interfaces(ncols, pverp)
  real(kind_phys)    :: dt

CONTAINS

  !> \section arg_table_hello_world_sub  Argument Table
  !! \htmlinclude arg_table_hello_world_sub.html
  !!
  subroutine hello_world_sub()

    use CAM_ccpp_cap,     only: CAM_ccpp_physics_initialize
    use CAM_ccpp_cap,     only: CAM_ccpp_physics_timestep_initial
    use CAM_ccpp_cap,     only: CAM_ccpp_physics_run
    use CAM_ccpp_cap,     only: CAM_ccpp_physics_timestep_final
    use CAM_ccpp_cap,     only: CAM_ccpp_physics_finalize
    use ccpp_physics_api, only: ccpp_physics_suite_list
    use ccpp_physics_api, only: ccpp_physics_suite_part_list


    integer                         :: col_start, col_end
    integer                         :: index
    character(len=128), allocatable :: part_names(:)
    character(len=512)              :: errmsg
    integer                         :: errflg

    ! Use the suite information to setup the run
    call CAM_ccpp_physics_initialize('hello_world', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    ! Initialize the timestep
    call CAM_ccpp_physics_timestep_initial('hello_world', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    do col_start = 1, ncols, 5
      col_end = MIN(col_start + 4, ncols)

      call CAM_ccpp_physics_run('hello_world', 'physics', col_start, col_end, precl, vmr, total_dens, errmsg, errflg)
      if (errflg /= 0) then
        write(6, *) trim(errmsg)
        call ccpp_physics_suite_part_list('hello_world', part_names, errmsg, errflg)
        write(6, *) 'Available suite parts are:'
        do index = 1, size(part_names)
          write(6, *) trim(part_names(index))
        end do
        stop
      end if
    end do

    call CAM_ccpp_physics_timestep_final('hello_world', errmsg, errflg)

    call CAM_ccpp_physics_finalize('hello_world', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      write(6,'(a)') 'An error occurred in ccpp_timestep_final, Exiting...'
      stop
    end if

  end subroutine hello_world_sub

end module hello_world_host

program hello_world
  use hello_world_sub, only: hellow_world_sub
  call hello_world_sub()
end program hello_world
