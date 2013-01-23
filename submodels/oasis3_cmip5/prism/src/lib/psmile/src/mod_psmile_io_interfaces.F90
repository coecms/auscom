!-----------------------------------------------------------------------
! BOP
!
! !MODULE:  mod_psmile_io_interfaces
! !REMARKS: Programed by Reiner Vogelsang, SGI (reiner@sgi.com)
!
! !USES: psmile_os.h
! !DESCRIPTION:
!  Defines F90 interfaces of all PSMILe I/O routines
!
! !REVISION HISTORY:
! 2003.04.28 Reiner Vogelsang
!       - Screening of 4 byte real interfaces.
!
! EOP
!-----------------------------------------------------------------------
! $Id: mod_psmile_io_interfaces.F90,v 1.2 2003/08/06 12:11:55 valcke Exp $
!-----------------------------------------------------------------------
!
!RV: Set here some OS specific preprocessor flags
!
#include "psmile_os.h" 
!
       module mod_psmile_io_interfaces
       interface psmile_io_init_comp
         subroutine psmile_io_init_comp(id_error)
            use mod_kinds_model
            integer(kind=ip_intwp_p),intent(out)::id_error
         end subroutine psmile_io_init_comp
       end interface

       interface  psmile_def_domains
	 subroutine psmile_def_domains(id_error)
            use mod_kinds_model
           integer(kind=ip_intwp_p),intent(out)::id_error
	 end subroutine psmile_def_domains
       end interface

       interface psmile_def_files
         subroutine psmile_def_files(id_error)
            use mod_kinds_model
	   integer(kind=ip_intwp_p),intent(out)::id_error
         end subroutine psmile_def_files
       end interface

       interface psmile_def_metadata
         subroutine psmile_def_metadata(id_error)
            use mod_kinds_model
	   integer(kind=ip_intwp_p),intent(out)::id_error
         end subroutine psmile_def_metadata
       end interface

       interface psmile_close_files
         subroutine psmile_close_files(id_error)
            use mod_kinds_model
	   integer(kind=ip_intwp_p),intent(out)::id_error
         end subroutine psmile_close_files
       end interface

       interface psmile_io_cleanup
         subroutine psmile_io_cleanup(id_error)
            use mod_kinds_model
	   integer(kind=ip_intwp_p),intent(out)::id_error
         end subroutine psmile_io_cleanup
       end interface

       interface psmile_read

	 subroutine psmile_read_8(id_port_id,rd_field,id_newtime)
            use mod_kinds_model
	   integer(kind=ip_intwp_p),intent(in)::id_newtime,id_port_id
           REAL(kind=ip_double_p), DIMENSION(:) :: rd_field
	 end subroutine psmile_read_8

#ifndef __NO_4BYTE_REALS
   	 subroutine psmile_read_4(id_port_id,rd_field,id_newtime)
               use mod_kinds_model
   	   integer(kind=ip_intwp_p),intent(in)::id_newtime,id_port_id
              REAL(kind=ip_single_p), DIMENSION(:) :: rd_field
   	 end subroutine psmile_read_4
#endif

       end interface

       interface psmile_write
	 subroutine psmile_write_8(id_port_id,rd_field,id_newtime)
            use mod_kinds_model
	   integer(kind=ip_intwp_p),intent(in)::id_newtime,id_port_id
           REAL(kind=ip_double_p), DIMENSION(:) :: rd_field
	 end subroutine psmile_write_8

#ifndef __NO_4BYTE_REALS
   	 subroutine psmile_write_4(id_port_id,rd_field,id_newtime)
               use mod_kinds_model
   	   integer(kind=ip_intwp_p),intent(in)::id_newtime,id_port_id
              REAL(kind=ip_single_p), DIMENSION(:) :: rd_field
   	 end subroutine psmile_write_4
#endif 

       end interface

       interface indexi
	 subroutine indexi(n,arr,indx)
            use mod_kinds_model
	 integer(kind=ip_intwp_p),intent(in)::n
	 integer(kind=ip_intwp_p),intent(in)::arr(n)
	 integer(kind=ip_intwp_p),intent(out)::indx(n)
	 end subroutine indexi
       end interface

       interface combine_with_date
         subroutine combine_with_date(cd_in,cd_mode,id_initial_date,cd_on)
            use mod_kinds_model
         character(len=*),intent(in)::cd_in
         character(len=*),intent(in)::cd_mode
	 integer(kind=ip_intwp_p),intent(in)::id_initial_date(:)
         character(len=*),intent(out)::cd_on
         end subroutine combine_with_date
       end interface

       end module  mod_psmile_io_interfaces
