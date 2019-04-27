module test_prog

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public test_host

CONTAINS

  !> \section arg_table_test_host  Argument Table
  !! \htmlinclude arg_table_test_host.html
  !!
  subroutine test_host()

    use test_host_mod,      only: ncols
    use test_host_ccpp_cap, only: test_host_ccpp_physics_initialize
    use test_host_ccpp_cap, only: test_host_ccpp_physics_timestep_initial
    use test_host_ccpp_cap, only: test_host_ccpp_physics_run
    use test_host_ccpp_cap, only: test_host_ccpp_physics_timestep_final
    use test_host_ccpp_cap, only: test_host_ccpp_physics_finalize
    use test_host_ccpp_cap, only: ccpp_physics_suite_list
    use test_host_ccpp_cap, only: ccpp_physics_suite_part_list
    use test_host_mod,      only: init_temp, compare_temp


    integer                         :: col_start, col_end
    integer                         :: index
    character(len=128), allocatable :: part_names(:)
    character(len=512)              :: errmsg
    integer                         :: errflg

    ! Initialize our 'data'
    call init_temp()

    ! Use the suite information to setup the run
    call test_host_ccpp_physics_initialize('temp_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    ! Initialize the timestep
    call test_host_ccpp_physics_timestep_initial('temp_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    do col_start = 1, ncols, 5
      col_end = MIN(col_start + 4, ncols)

      call test_host_ccpp_physics_run('temp_suite', 'physics', col_start, col_end, errmsg, errflg)
      if (errflg /= 0) then
        write(6, *) trim(errmsg)
        call ccpp_physics_suite_part_list('temp_suite', part_names, errmsg, errflg)
        write(6, *) 'Available suite parts are:'
        do index = 1, size(part_names)
          write(6, *) trim(part_names(index))
        end do
        stop
      end if
    end do

    call test_host_ccpp_physics_timestep_final('temp_suite', errmsg, errflg)

    call test_host_ccpp_physics_finalize('temp_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      write(6,'(a)') 'An error occurred in ccpp_timestep_final, Exiting...'
      stop
    end if

    if (compare_temp()) then
      write(6, *) 'Answers are correct!'
    else
      write(6, *) 'Answers are not correct!'
    end if

  end subroutine test_host

end module test_prog

program test
  use test_prog, only: test_host
  call test_host()
end program test
