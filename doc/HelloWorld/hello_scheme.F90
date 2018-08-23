!Hello demonstration parameterization
!

MODULE hello_scheme

  USE machine, ONLY: rkind => kind_phys

  PRIVATE
  PUBLIC :: hello_scheme_init
  PUBLIC :: hello_scheme_run
  PUBLIC :: hello_scheme_finalize

CONTAINS

!! [ccpp-arg-table]
!!   name = hello_scheme_run
!!   type = scheme
!! [ ims ]
!!   standard_name = horizontal_loop_begin
!!   type = integer
!!   units = count
!!   intent = in
!! [ ime ]
!!   standard_name = horizontal_loop_end
!!   type = integer
!!   units = count
!!   intent = in
!! [ lev ]
!!   standard_name = vertical_layer_dimension
!!   type = integer
!!   units = count
!!   intent = in
!! [ ilev ]
!!   standard_name = vertical_level_dimension
!!   type = integer
!!   units = count
!!   intent = in
!! [ state_layer ]
!!   standard_name = potential_temperature
!!   long_name = potential temperature
!!   units = K
!!   dimensions = (horizontal_loop_begin:horizontal_loop_end, vertical_layer_dimension)
!!   type = real
!!   kind = kind_phys
!!   intent = inout
!! [ state_level ]
!!   standard_name = potential_temperature
!!   long_name = potential temperature
!!   units = K
!!   dimensions = (horizontal_loop_begin:horizontal_loop_end, vertical_level_dimension)
!!   type = real
!!   kind = kind_phys
!!   intent = out
!! [ timestep ]
!!   standard_name = time_step_for_physics
!!   long_name = time step
!!   units = s
!!   dimensions = ()
!!   type = real
!!   kind = kind_phys
!!   intent = in
!! [ errmsg ]
!!   standard_name = ccpp_error_message
!!   long_name = Error message for error handling in CCPP
!!   units = 1
!!   dimensions = ()
!!   type = character
!!   kind = len=512
!!   intent = out
!! [ errflg ]
!!   standard_name = ccpp_error_flag
!!   long_name = Error flag for error handling in CCPP
!!   units = flag
!!   dimensions = ()
!!   type = integer
!!   intent = out
  SUBROUTINE hello_scheme_run(ims, ime, lev, ilev, state_layer, state_level,
    timestep, errmsg, errflg)
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------

   integer,            intent(in)  :: ims, ime, lev, ilev
   REAL(kind_phys),    intent(in)  :: state_layer(lev)
   REAL(kind_phys),    INTENT(out) :: state_level(ilev)
   character(len=512), intent(out) :: errmsg
   integer,            intent(out) errflg
!----------------------------------------------------------------

    errmsg = ''
    errflg = 0


  END SUBROUTINE hello_scheme_run

!> \section arg_table_hello_scheme_init  Argument Table
!!
!! [ccpp-arg-table]
!!   name = hello_scheme_run
!!   type = scheme
!! [ errmsg ]
!!   standard_name = ccpp_error_message
!!   long_name = Error message for error handling in CCPP
!!   units = 1
!!   dimensions = ()
!!   type = character
!!   kind = len=512
!!   intent = out
!! [ errflg ]
!!   standard_name = ccpp_error_flag
!!   long_name = Error flag for error handling in CCPP
!!   units = flag
!!   dimensions = ()
!!   type = integer
!!   intent = out
  subroutine hello_scheme_init (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine hello_scheme_init

!> \section arg_table_hello_scheme_finalize  Argument Table
!!
!! [ccpp-arg-table]
!!   name = hello_scheme_run
!!   type = scheme
!! [ errmsg ]
!!   standard_name = ccpp_error_message
!!   long_name = Error message for error handling in CCPP
!!   units = 1
!!   dimensions = ()
!!   type = character
!!   kind = len=512
!!   intent = out
!! [ errflg ]
!!   standard_name = ccpp_error_flag
!!   long_name = Error flag for error handling in CCPP
!!   units = flag
!!   dimensions = ()
!!   type = integer
!!   intent = out
  subroutine hello_scheme_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine hello_scheme_finalize

END MODULE hello_scheme
