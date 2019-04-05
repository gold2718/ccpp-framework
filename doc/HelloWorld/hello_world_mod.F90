module hello_world_mod

  use ccpp_kinds, only: kind_phys

  implicit none
  public

  !> \section arg_table_hello_world_mod  Argument Table
  !! \htmlinclude arg_table_hello_world_host.html
  !!
  integer, parameter :: ncols = 10
  integer, parameter :: pver = 5
  integer, parameter :: pverp = 6
  integer            :: ntimes_loop
  real(kind_phys)    :: temp_midpoints(ncols, pver)
  real(kind_phys)    :: temp_interfaces(ncols, pverp)
  real(kind_phys)    :: dt

end module hello_world_mod
