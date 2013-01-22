       module mod_psmile_date_and_time
         USE mod_kinds_model
!This data structure contains the initial date of the simulation, respectively
! the experiment.
!The meanings of the fields are
!	1	fullyear like 1978
!	2	month
!	3	day
!	4       hour
!	5	minutes
!	6	seconds

         integer(kind=ip_intwp_p)::ig_initial_date(1:6)
         logical::lg_date_is_initialized

       contains

         subroutine psmile_get_initial_date(id_datetime,id_error)
!------------------------------------------------------------------------
           USE mod_kinds_model
           implicit none
           integer(kind=ip_intwp_p),intent(out)::id_datetime(1:6),id_error

           !This is an initialization for test purposes only
           ig_initial_date(1)=2002
           ig_initial_date(2)=11 
           ig_initial_date(3)=8
           ig_initial_date(4)=13
           ig_initial_date(5)=7
           ig_initial_date(6)=30
           !End of test initialization

           id_error =0

           if(lg_date_is_initialized) then
             id_datetime=ig_initial_date
           else
             id_error=-999
           endif

         end subroutine psmile_get_initial_date

       end module mod_psmile_date_and_time
