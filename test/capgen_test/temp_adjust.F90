!Hello demonstration parameterization
!

MODULE temp_adjust

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: temp_adjust_init
  PUBLIC :: temp_adjust_run
  PUBLIC :: temp_adjust_finalize

CONTAINS

!> \section arg_table_temp_adjust_run  Argument Table
!! \htmlinclude arg_table_temp_adjust_run.html
!!
  SUBROUTINE temp_adjust_run(ncol, temp_layer, timestep, errmsg, errflg)

   integer,            intent(in)    :: ncol
   REAL(kind_phys),    intent(inout) :: temp_layer(:)
   real(kind_phys),    intent(in)    :: timestep
   character(len=512), intent(out)   :: errmsg
   integer,            intent(out)   :: errflg
!----------------------------------------------------------------

   integer :: col_index

    errmsg = ''
    errflg = 0

    do col_index = 1, ncol
      temp_layer(col_index) = temp_layer(col_index) + 1.0_kind_phys
    end do

  END SUBROUTINE temp_adjust_run

!> \section arg_table_temp_adjust_init  Argument Table
!! \htmlinclude arg_table_temp_adjust_init.html
!!
  subroutine temp_adjust_init (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_adjust_init

!> \section arg_table_temp_adjust_finalize  Argument Table
!! \htmlinclude arg_table_temp_adjust_finalize.html
!!
  subroutine temp_adjust_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine temp_adjust_finalize

END MODULE temp_adjust
