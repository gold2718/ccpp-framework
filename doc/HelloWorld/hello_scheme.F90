!Hello demonstration parameterization
!

MODULE hello_scheme

  USE machine, ONLY: rkind => kind_phys

  PRIVATE
  PUBLIC :: hello_scheme_init
  PUBLIC :: hello_scheme_run
  PUBLIC :: hello_scheme_finalize

CONTAINS

!> \section arg_table_hello_scheme_run  Argument Table
!! \htmlinclude arg_table_hello_scheme_run.html
!!
  SUBROUTINE hello_scheme_run(ims, ime, lev, ilev, state_layer, state_level, &
    timestep, errmsg, errflg)
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------

   integer,            intent(in)  :: ims, ime, lev, ilev
   REAL(kind_phys),    intent(in)  :: state_layer(lev)
   REAL(kind_phys),    INTENT(out) :: state_level(ilev)
   character(len=512), intent(out) :: errmsg
   integer,            intent(out) :: errflg
!----------------------------------------------------------------

    errmsg = ''
    errflg = 0

  END SUBROUTINE hello_scheme_run

!> \section arg_table_hello_scheme_init  Argument Table
!! \htmlinclude arg_table_hello_scheme_init.html
!!
  subroutine hello_scheme_init (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine hello_scheme_init

!> \section arg_table_hello_scheme_finalize  Argument Table
!! \htmlinclude arg_table_hello_scheme_finalize.html
!!
  subroutine hello_scheme_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine hello_scheme_finalize

END MODULE hello_scheme
