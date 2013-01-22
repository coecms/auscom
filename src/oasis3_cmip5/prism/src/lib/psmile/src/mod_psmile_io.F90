
!#define __VERBOSE
#define __READ_BY_LOOKUP
!----------------------------------------------------------------------
! BOP
!
! !MODULE:  mod_psmile_io
! !REMARKS: Initially programed by Reiner Vogelsang, SGI GmbH, and integrated
!            into OASIS 3.0 and tested by Arnaud Caubel, CERFACS
!	    Bugs should be reported to reiner@sgi.com or Sophie.Valcke@cerfacs.fr.
! !REVISION HISTORY:
! 2003.01.31 Reiner Vogelsang Finalized initial version
! 2003.02.28 Arnaud Caubel Tests and correction of the READ_BY_LOOKUP
! 2003.04.24 Reiner Vogelsang Corrections with regard to CF naming convention.
! 2003.04.28 Reiner Vogelsang Added ProTex documentation header and 
!                             static CVS versions string.
! 2004.03.08 Sophie Valcke    Added the writting of bounds,lons and lats.
! 2004.04.07 Reiner Vogelsang Correction: bounds are written with
!                             number of corners as leading dimension
! 2004.19.06 Reiner Vogelsang Considering cyclic boundaries for longitudes.
!                             which lead to differences between corners
!                             and bounds by 360 degrees
! 2004.26.08 Reiner Vogelsang Correction of allocation scheme for arrays
!                             hosting the longitudes.
!
! !PUBLIC MEMBER FUNCTIONS:
! 
!       subroutine psmile_io_init_comp(id_error)
!             This subroutine finds a local communicator for the MPI tasks
!	      of a model component participating in coupling.
!
!       subroutine psmile_def_domains(id_error)
!	      This subroutine initializes the MPP_IO domains.
!
!       subroutine psmile_def_files(id_error)
!	      This subroutine open files acccording to the kewords
!
!       subroutine psmile_def_metadata(id_error)
!	      This subroutine writes CF metadata or finds a variable according
!             to CF metadata information.
!
!       subroutine psmile_close_files(id_error)
!             This subroutine closes all opened psmile_io files
!
!       subroutine psmile_io_cleanup(id_error)
!             This subroutine deallocates all PSMILe I/O related data
!	      and exits MPP_IO
!
!       subroutine psmile_read_8(id_port_id,rd_field,id_newtime)
!	      This subroutine reads REAL(KIND=4) data.
!
!       subroutine psmile_read_4(id_port_id,rd_field,id_newtime)
!	      This subroutine reads REAL(KIND=8) data.
!
!       subroutine psmile_write_4(id_port_id,rd_field,id_newtime)
!	      This subroutine writes REAL(KIND=4) data.
!
!       subroutine psmile_write_8(id_port_id,rd_field,id_newtime)
!	      This subroutine writes REAL(KIND=8) data.
!
!       subroutine indexi(n,arr,indx)
!	      This subroutine sorts by indexing.
!
!       subroutine combine_with_date(cd_in,cd_mode,id_initial_date,cd_on)
!	      This subroutines appends the date in ISO format to a strings. 
!       

       module mod_psmile_io
#if !defined key_noIO
! !USES:
       use mod_kinds_model
       use mpp_mod_oa
       use mpp_io_mod_oa
       use mpp_domains_mod_oa
! !PUBLIC TYPES:
!Highest unit number permittable by the OS.
       integer(kind=ip_intwp_p),parameter::PSMILE_MAX_UNIT=299
!Highest unit number reserved by the OS.
       integer(kind=ip_intwp_p),parameter::PSMILE_MAX_RESERVED_UNIT=104
!Lowest unit number reserved by the OS.
       integer(kind=ip_intwp_p),parameter::PSMILE_MIN_RESERVED_UNIT=100
       integer(kind=ip_intwp_p),parameter::PSMILE_MAX_CF_TABLE_ENTRIES=1024
       integer(kind=ip_intwp_p),allocatable::nbr_corners(:)

       integer(kind=ip_intwp_p)::ig_mpp_io_comm,ig_color,ig_mpp_io_comm_size,ig_mpp_io_comm_rank
       integer(kind=ip_intwp_p)::ig_stackmax=1310720,ig_stackmaxd=655360
       integer(kind=ip_intwp_p)::ig_layout(2)
!Global arrays to specify IO domains
       type(domain2D),allocatable::domain(:)
       type(domain1D),allocatable::xdom(:),ydom(:)
       logical,allocatable::lg_is_odd_distribution(:)
!Global arrarys for opening files
       integer(kind=ip_intwp_p),allocatable::ig_units(:)
       integer(kind=ip_intwp_p),allocatable::ig_action(:)
       integer(kind=ip_intwp_p),allocatable::ig_form(:)
       integer(kind=ip_intwp_p),allocatable::ig_threading(:)
       integer(kind=ip_intwp_p),allocatable::ig_fileset(:)
       character(len=128),allocatable::cg_my_input_file(:)
       character(len=128),allocatable::cg_output_file(:)


!Global arrays for netcdf metadata informations
       integer(kind=ip_intwp_p),allocatable::ig_var_index(:)
       integer(kind=ip_intwp_p),allocatable::ig_ntimes(:)
!RV: 22.01.2003
       TYPE time_stamps
!	    real(kind=ip_double_p),pointer::rl_times(:)
!AC:31/01
        real(kind=ip_double_p),dimension(:),pointer :: rl_times  
       END TYPE time_stamps
! !PUBLIC DATA MEMBERS:
       type(time_stamps),allocatable :: dg_times(:)

       type(fieldtype),allocatable::ig_vars(:),field(:),latij(:),lonij(:)
       type(fieldtype),allocatable::crnlatij(:), crnlonij(:)
       type(axistype),allocatable::x_axis(:),y_axis(:),z_axis(:),t_axis(:)
       type(axistype),allocatable::c_axis(:)
! These arrays are supposed to be part of a different module!
! However, no information is available in Oasis 3.0. So, let them live here
! for a while.
!RV
       character(len=64),allocatable::cports_lgname(:)
       character(len=64),allocatable::cports_units(:)
       character(len=64),allocatable::cfldlab(:),cfunitslab(:)
!RV
! !DESCRIPTION:
! This module contains the global data objects which hold informations for
! performing I/O on coupled fields and are the glue between routines defined
! within this file, mod_psmile_io.F90.
! !EOP
!-------------------------------------------------------------------------------
!$Id: mod_psmile_io.F90,v 1.8 2006/09/11 11:59:16 valcke Exp $
!-------------------------------------------------------------------------------
#endif
       end module mod_psmile_io

#if !defined key_noIO

       subroutine psmile_io_init_comp(id_error)
!--------------------------------------------------------------------------

!Routine must be called in prism_init_comp.
!The assumption is that each process of a model calls prism_init_comp.
       use mod_kinds_model
       use mod_prism_proto
       use mod_comprism_proto
       use mod_psmile_io

       implicit none
#include <mpif.h>

!Arguments:
!Error return code
       integer(kind=ip_intwp_p),intent(out)::id_error

!Local Variables:
!Don't touch the following character variable!
       character(len=80),save::cl_version_string= &
'$Id: mod_psmile_io.F90,v 1.8 2006/09/11 11:59:16 valcke Exp $'
!
!Communicator which covers the whole model.
       integer(kind=ip_intwp_p)::il_local_comm 

       integer(kind=ip_intwp_p)::il_i,il_key
       integer(kind=ip_intwp_p),allocatable::il_cpl_ranks(:)
       integer(kind=ip_intwp_p)::ig_local_comm_grp,il_ncplprocs,ig_local_comm_size,il_mynum

       integer(kind=ip_intwp_p)::il_file_unit,il_max_entry_id,il_no_of_entries
       integer(kind=ip_intwp_p)::il_pos
       character(len=64)       :: cl_cfname,cl_cfunit
       logical                 :: ll_exist


#ifdef __VERBOSE
       write(nulprt,*) 'Start - - psmile_io_init_comp'
#endif
       id_error=CLIM_OK
       il_local_comm=ig_local_comm

!Create a communicator which covers all processes of a model invoLved with
!coupling.
       il_ncplprocs=kbcplproc(ig_mynummod)
       allocate(il_cpl_ranks(1:il_ncplprocs))

       do il_i=0,il_ncplprocs-1
	 il_cpl_ranks(il_i+1)=il_i
       enddo

       il_key=1
       call MPI_Comm_rank(il_local_comm,il_mynum,id_error)

!
!Create a color  by finding the cpl  ranks  in the model communicator.
!
       if(ANY(il_cpl_ranks.eq.il_mynum)) then
         ig_color=1000*ig_mynummod
       else
	 ig_color=1
       endif
#ifdef __VERBOSE
       write(nulprt,*)'psmile_io_init_comp: rank ',il_mynum,' color ',ig_color
#endif

!
!Do the splitting.
!
#ifdef __VERBOSE
       write(nulprt,*) 'psmile_io_init_comp: Communicator splitting'
#endif
       call MPI_Comm_split(il_local_comm,ig_color,il_key,ig_mpp_io_comm,id_error)
       call MPI_Comm_rank(ig_mpp_io_comm,ig_mpp_io_comm_rank,id_error)
       call MPI_Comm_size(ig_mpp_io_comm,ig_mpp_io_comm_size,id_error)

!
!Initialize FMS mpp_init,mpp_io,mpp_domains
!
       if (ig_color .eq. 1000*ig_mynummod ) then
#ifdef __VERBOSE
         write(nulprt,*) 'psmile_io_init_comp: MPP_INIT : Model',ig_mynummod &
                         ,ig_mpp_io_comm_rank
#endif
         call mpp_init(mpp_comm=ig_mpp_io_comm,logfile=trim(cnaprt))
         call mpp_domains_set_stack_size(ig_stackmaxd)
!rv
!rv Oasis 3 uses for the tracefiles the I/O units >= 1000. Since
!rv mpp_io stores informations regarding to file specs in an array of
!rv structs that array has to be allocated apropiately.
!rv maxresunit specifies that the top maxresunits are excluded from the list
!rv of the files to be closed on mpp_io_exit.
!rv
         call mpp_io_init(maxunit=nulprt,maxresunit=25)
         call mpp_set_stack_size(ig_stackmax)
       endif

!After this point I assume that I/O routines are called by processes
!involved with coupling only.

!RV   At this point I am reading the CF name table definitions.

       inquire(file='cf_name_table.txt',exist=ll_exist)
       if(ll_exist) then
#ifdef __VERBOSE
         write(nulprt,*) 'psmile_io_init_comp: Reading CF name table!'
         call flush(nulprt)
#endif

         il_file_unit=nulprt+1
         open(file='cf_name_table.txt' &
             ,unit=il_file_unit &
             ,form='formatted' &
             ,status='old')

         read(unit=il_file_unit,fmt=*,iostat=id_error)
         read(unit=il_file_unit,fmt=*,iostat=id_error) &
             il_max_entry_id, il_no_of_entries

         if(id_error.ne.0) then
           write(nulprt,*) &
                 'psmile_io_init_comp:cf_name_table.txt:' &
                ,' Reading of first record failed!'
           call flush(nulprt)
           call MPI_Abort(mpi_comm,0,mpi_error)
         endif

         if(il_max_entry_id.gt.0.and. &
            il_max_entry_id.le.PSMILE_MAX_CF_TABLE_ENTRIES) then

           allocate(cfldlab(1:il_max_entry_id),STAT=id_error)
           if(id_error.ne.0) then
             write(nulprt,*) &
                  'psmile_io_init_comp: Allocation of cfldlab failed!'
             call flush(nulprt)
             call MPI_Abort(mpi_comm,0,mpi_error)
           endif

           allocate(cfunitslab(1:il_max_entry_id),STAT=id_error)
           if(id_error.ne.0) then
             write(nulprt,*) &
                  'psmile_io_init_comp: Allocation of cfunitslab failed!'
                  call flush(nulprt)
             call MPI_Abort(mpi_comm,0,mpi_error)
            endif

         else

           write(nulprt,*) &
                 'psmile_io_init_comp:cf_name_table.txt:' &
                ,'Your max. numlab is out of range !'
           call flush(nulprt)
           call MPI_Abort(mpi_comm,0,mpi_error)

         endif


         read(unit=il_file_unit,fmt=*,iostat=id_error)
         do il_i=1,il_no_of_entries

           read(unit=il_file_unit,fmt=*,iostat=id_error) &
              il_pos,cl_cfname,cl_cfunit

           if(id_error.eq.0) then
             if(il_pos.le.il_max_entry_id) then
               cfldlab(il_pos)=trim(cl_cfname)
               cfunitslab(il_pos)=trim(cl_cfunit)
             else
               write(nulprt,*) &
                   'psmile_io_init_comp:cf_name_table.txt:' &
                  ,'Record ',il_i,': numlab =',il_pos,' out of range!'
               call flush(nulprt)
               call MPI_Abort(mpi_comm,0,mpi_error)
             endif
           else
             write(nulprt,*) &
                 'psmile_io_init_comp:cf_name_table.txt:' &
                ,'Reading record ',il_i,' failed!'
             call flush(nulprt)
             call MPI_Abort(mpi_comm,0,mpi_error)
           endif
           
         enddo

       endif



!RV
       deallocate(il_cpl_ranks)
#ifdef __VERBOSE
       write(nulprt,*) 'End - - psmile_io_init_comp'
#endif

       return
       end

       subroutine psmile_def_domains(id_error)
!--------------------------------------------------------------------------
!
!	Routine defines the IO domain partitioning
!       Called at the end of prism_enddef.
!
       use mod_kinds_model      
       use mod_prism_proto
       use mod_comprism_proto
       use mod_psmile_io
       use mod_psmile_io_interfaces, only:indexi
       implicit none
       
#include <mpif.h>

       integer(kind=ip_intwp_p),intent(out)::id_error
       
       integer(kind=ip_intwp_p)::il_port,il_i,il_j,il_ldx
       integer(kind=ip_intwp_p),allocatable::il_gextents(:,:,:)
       integer(kind=ip_intwp_p),allocatable::il_offsets(:,:,:)
       integer(kind=ip_intwp_p),allocatable::il_extentsx(:)
       integer(kind=ip_intwp_p),allocatable::il_extentsy(:)
       integer(kind=ip_intwp_p),allocatable::il_offsetx(:)
       integer(kind=ip_intwp_p),allocatable::il_offsety(:)
       logical,allocatable::ll_mask(:,:)
       integer(kind=ip_intwp_p),allocatable::il_pelist(:)
       integer(kind=ip_intwp_p)::il_ngx,il_ngy
       integer(kind=ip_intwp_p)::il_recvcount=1,il_sum

!Assumption: Processes beloging to the same model have the same 
!             strategy per port. Each process involved  in the
!              coupling has the same number of ports

#ifdef __VERBOSE
       write(nulprt,*)'Start - - psmile_def_domains'
#endif

       id_error=CLIM_ok

!Global arrays 
       allocate(domain(1:nports))
       allocate(xdom(1:nports))
       allocate(ydom(1:nports))
       allocate(lg_is_odd_distribution(1:nports))

!Local arrays
!il_gextents(:,1,:) carries extents in x, il_gextents(:,2,:) extents in y
       allocate(il_gextents(0:ig_mpp_io_comm_size-1,1:2,1:nports))
       allocate(il_offsets(1:ig_mpp_io_comm_size,1:2,1:nports))
       allocate(il_pelist(0:ig_mpp_io_comm_size-1))
       allocate(il_extentsx(0:ig_mpp_io_comm_size-1))
       allocate(il_extentsy(0:ig_mpp_io_comm_size-1))
       allocate(il_offsetx(0:ig_mpp_io_comm_size-1))
       allocate(il_offsety(0:ig_mpp_io_comm_size-1))
       allocate(ll_mask(0:ig_mpp_io_comm_size-1,0:ig_mpp_io_comm_size-1))
       
       if(ig_color/ig_mynummod.ne.1000) then
         WRITE(nulprt,*)'psmile_def_domains: Called by proc which is not' &
                       ,' involved with coupling!'
         call flush(nulprt)
         call MPI_ABORT(mpi_comm,0,mpi_err)
       endif

       do il_port=1,nports
#ifdef __VERBOSE
        WRITE(nulprt,*)'ig_def_state ',ig_def_state(il_port)  
#endif
       IF (ig_def_state(il_port) .eq. ip_ignout .or. &
            ig_def_state(il_port) .eq. ip_expout .or. &
            ig_def_state(il_port) .eq. ip_output .or. &
            ig_def_state(il_port) .eq. ip_input ) THEN
#ifdef __VERBOSE
       WRITE(nulprt,*)'psmile_def_domains: port: ',il_port
       call flush(nulprt)
#endif

         if( mydist(CLIM_Strategy,il_port) .eq. CLIM_Serial) then

#ifdef __VERBOSE
           WRITE(nulprt,*)'psmile_def_domains: port: ',il_port,' CLIM_Serial'
#endif

           if(ig_mpp_io_comm_size.eq.1)then
#ifdef __VERBOSE
              WRITE(nulprt,*)'psmile_def_domains: port: ',il_port &
                         ,'(/1,mydist( CLIM_Segments+2,il_port),1,1/)' &
                         ,1,mydist(CLIM_Segments+2,il_port),1,1
!             call flush(nulprt)
#endif
             ig_layout(1:2)=1
             call mpp_define_domains((/1,mydist( CLIM_Segments+2,il_port),1,1/)&
                                    , ig_layout,domain(il_port))
             call mpp_get_domain_components( domain(il_port), xdom(il_port),&
                                             ydom(il_port) )
           else
	     write(nulprt, * ) 'psmile_def_domains: no. of procs for I/O ',&
                             '> 1 and CLIM_Serial does not make sense! '
             call MPI_ABORT(mpi_comm,0,mpi_err)

           endif
            
         else if( mydist(CLIM_Strategy,il_port) .eq. CLIM_Apple) then

#ifdef __VERBOSE
           WRITE(nulprt,*)'psmile_def_domains: port: ',il_port,' CLIM_Apple',mydist(:,il_port)
#endif

!Assuming 1D decomposition. At the this point this is the only information
!which I have considering CLIM_Apple!

           ig_layout(1)=ig_mpp_io_comm_size
           ig_layout(2)=1

!Build a list of the local extents which exists on the procs involved with
!coupling
           call MPI_Allgather(mydist(CLIM_Length+1,il_port),1,MPI_INTEGER,&
                             il_gextents(0,1,il_port),il_recvcount,MPI_INTEGER,&
                              ig_mpp_io_comm,id_error)
           call MPI_Allgather(mydist(CLIM_Offset+1,il_port),1,MPI_INTEGER,&
                              il_offsets(1,1,il_port),il_recvcount,MPI_INTEGER,&
                              ig_mpp_io_comm,id_error)

!Sort the extents and the PE-list according to the offsets

           call indexi(ig_mpp_io_comm_size &
                      ,il_offsets(1:ig_mpp_io_comm_size,1,il_port),il_pelist)

           il_pelist(:)=il_pelist(:)-1
           il_extentsx(0:ig_mpp_io_comm_size-1)=&
                     il_gextents(il_pelist(:),1,il_port)

!The max global index for the current 1D decomposition.
           il_sum=sum(il_extentsx(0:ig_mpp_io_comm_size-1))

#ifdef __VERBOSE
           WRITE(nulprt,*)'psmile_def_domains: port: ',il_port &
                         ,'(/1,il_sum,1,1/),pelist',1,il_sum,1,1,il_pelist &
                         ,'extents',il_extentsx
#endif
!Define a pseudo 2D domain
           il_extentsy(0)=1
           call mpp_define_domains((/1,il_sum,1,1/),(/ig_mpp_io_comm_size,1/),&
                                  domain(il_port),&
                                  pelist=il_pelist,&
                                  xextent=il_extentsx,&
                                  yextent=il_extentsy(0:0))
           call mpp_get_domain_components( domain(il_port), xdom(il_port),&
                                           ydom(il_port) )

         else if( mydist(CLIM_Strategy,il_port) .eq. CLIM_Box) then

#ifdef __VERBOSE
           WRITE(nulprt,*)'psmile_def_domains: port: ',il_port,' CLIM_Box'
!           WRITE(nulprt,*)'psmile_def_domains: port: ',il_port,' CLIM_Box', &
!           mydist(:,il_port),'::',myport(:,il_port)
#endif
!Build a list of the local extents which exists on the procs involved with
!coupling
!Block sizes in x
           call MPI_Allgather(mydist(CLIM_SizeX+1,il_port),1,MPI_INTEGER,&
                             il_gextents(0,1,il_port),il_recvcount,MPI_INTEGER,&
                              ig_mpp_io_comm,id_error)
!Block sizes in y
           call MPI_Allgather(mydist(CLIM_Segments,il_port),1,MPI_INTEGER,&
                             il_gextents(0,2,il_port),il_recvcount,MPI_INTEGER,&
                              ig_mpp_io_comm,id_error)
!Offsets of the blocks
           call MPI_Allgather(mydist(CLIM_Offset+1,il_port),1,MPI_INTEGER,&
                              il_offsets(1,1,il_port),il_recvcount,MPI_INTEGER,&
                              ig_mpp_io_comm,id_error)

           call indexi(ig_mpp_io_comm_size &
                      ,il_offsets(1:ig_mpp_io_comm_size,1,il_port),&
                           il_pelist)
           il_pelist(:)=il_pelist(:)-1

           il_extentsx(0:ig_mpp_io_comm_size-1)=&
                       il_gextents(il_pelist(:),1,il_port)
           il_extentsy(0:ig_mpp_io_comm_size-1)=&
                       il_gextents(il_pelist(:),2,il_port)
#ifdef __VERBOSE
!           write(nulprt,*)'psmile_def_domains:il_extentsx',il_extentsx
!           write(nulprt,*)'psmile_def_domains:il_extentsy',il_extentsy
#endif
! At this point I have to think about the domain decomposition
! 1st assumption : The leading dimension is a global parameter for
!                  the model. So, I don't do further checks on that.

           il_ldx=mydist(CLIM_Segments+3,il_port)-mydist(CLIM_Offset+1,il_port)
#ifdef __VERBOSE
!           write(nulprt,*)'psmile_def_domains: LDX = ',il_ldx
#endif


!Count the number of offsets which are less than LDX to get the partitioning
!in x-direction
           ig_layout(1)=&
           count(mask=il_offsets(:,1,il_port).lt.il_ldx)

           if(ig_layout(1).gt.0) then
             ig_layout(2)=ig_mpp_io_comm_size/ig_layout(1)
             if(ig_layout(1)*ig_layout(2).ne.ig_mpp_io_comm_size) then
               lg_is_odd_distribution(il_port)=.true.
             else
!check within a column y  for different extents of y
               lg_is_odd_distribution(il_port)=.false.
               do il_j=0,ig_layout(2)-1
                 do il_i=1,ig_layout(1)-1
                   lg_is_odd_distribution(il_port)= &
                   il_extentsy(ig_layout(1)*il_j).ne.&
                     il_extentsy(il_i+ig_layout(1)*il_j)
#ifdef __VERBOSE
!                 write(nulprt,*)'i j extentsy ',il_i,il_j, &
!                 il_extentsy(ig_layout(1)*il_j), &
!                 il_extentsy(il_i+ig_layout(1)*il_j)
#endif
                 enddo
               enddo

!check within a column x  for different extents of x
               do il_i=0,ig_layout(2)-1
                 do il_j=1,ig_layout(1)-1
                   lg_is_odd_distribution(il_port)= &
                   il_extentsx(il_i).ne.il_extentsx(il_i+ig_layout(1)*il_j)
#ifdef __VERBOSE
!                 write(nulprt,*)'i j extentsy ',il_i,il_j, &
!                 il_extentsx(il_i), &
!                 il_extentsx(il_i+ig_layout(1)*il_j)
#endif
                 enddo
               enddo

             endif

!Okay, fine! There are global divisors of the x- and y-axis!
             if(.not.lg_is_odd_distribution(il_port)) then


               il_ngx= il_ldx
               il_ngy=il_offsets(il_pelist(ig_mpp_io_comm_size-1)+1,1,il_port)&
                    /il_ngx  + il_extentsy(ig_mpp_io_comm_size-1)

#ifdef __VERBOSE
               WRITE(nulprt,*)'psmile_def_domains: port: ',il_port &
                             ,'(/1,il_ngx,1,il_ngy/),pelist' &
                             ,1,il_ngx,1,il_ngy,il_pelist
#endif

               call mpp_define_domains((/1,il_ngx,1,il_ngy/) &
                                      ,ig_layout,domain(il_port) &
                   ,xextent=il_extentsx(0:ig_mpp_io_comm_size-1:ig_layout(2)) & 
                   ,yextent=il_extentsy(0:ig_mpp_io_comm_size-1:ig_layout(1)) &
			              ,pelist=il_pelist)
               call mpp_get_domain_components( domain(il_port), xdom(il_port),&
                                           ydom(il_port) )

             else

!Okay, at this point it seems that have we have a block-structured
!distribution. Need to calculate the offsets of the blocks in x- and y-
!direction. These offsets migth be provided by prism_set_offset.
               ll_mask(:,:)=.false.
               do il_i=0,ig_mpp_io_comm_size-1
               ll_mask(il_pelist(il_i),il_pelist(il_i))=.true.
               enddo
               il_ngx=il_ldx
               il_offsetx(:)=il_offsets(il_pelist(0:ig_mpp_io_comm_size-1)+1,1,il_port) &
                            -il_ngx* &
                            (il_offsets(il_pelist(0:ig_mpp_io_comm_size-1)+1,1,il_port) &
			    /il_ngx)
               il_offsety(:)=il_offsets(il_pelist(0:ig_mpp_io_comm_size-1)+1,1,il_port) &
                            /il_ngx
               il_ngy=il_offsety(ig_mpp_io_comm_size-1) &
                     +il_extentsy(ig_mpp_io_comm_size-1)
               ig_layout(1)=ig_mpp_io_comm_size
               ig_layout(2)=ig_mpp_io_comm_size

#ifdef __VERBOSE
               WRITE(nulprt,*)'psmile_def_domains: port: ',il_port &
                             ,' :fall back: (/1,il_ngx,1,il_ngy/),pelist' &
                             ,1,il_ngx,1,il_ngy,il_pelist,il_offsetx(:) &
                             ,il_offsety(:)
#endif

               call mpp_define_domains((/1,il_ngx,1,il_ngy/) &
                                      ,ig_layout,domain(il_port) &
                                      ,xextent=il_extentsx &
                                      ,yextent=il_extentsy & 
                                      ,pelist=il_pelist,maskmap=ll_mask &
                                      ,offsetx=il_offsetx,offsety=il_offsety)
               call mpp_get_domain_components( domain(il_port), xdom(il_port),&
                                           ydom(il_port) )
             endif

           else

	     write(nulprt, * ) 'psmile_def_domains: ig_layout(1) =0! '
             call MPI_ABORT(mpi_comm,0,mpi_err)
           endif

         else if( mydist(CLIM_Strategy,il_port) .eq. CLIM_Orange) then

	   write(nulprt, * ) 'psmile_def_domains: Strategy CLIM_orange',&
                             'not yet implemented for I/O!'
           call MPI_ABORT(mpi_comm,0,mpi_err)

         endif
         endif
       enddo

!Cleanup
       deallocate(il_pelist)
       deallocate(il_extentsx)
       deallocate(il_extentsy)
       deallocate(il_gextents)
       deallocate(il_offsets)
       deallocate(il_offsetx)
       deallocate(il_offsety)
       deallocate(ll_mask)

#ifdef __VERBOSE
       WRITE(nulprt,*)'End - - psmile_def_domains:'
       call flush(nulprt)
#endif

       
       end subroutine psmile_def_domains

       subroutine psmile_def_files(id_error)
!--------------------------------------------------------------------------
!
!      This subroutine open files acccording to the kewords 
!      OUTPUT: write by model issueing prism_put
!      INPUT:  read by  issueing prism_get
!      EXPOUT: write by model issueing  prism_put and prims_get after receive.
!	       Usage for debugging data exchange model->Oasis->model
!      IGNOUT: write by model issueing  prism_put and prims_get after receive.
!	       Usage for debugging data exchange model->model
!
!       Called at the end of prism_enddef.
!
       use mod_kinds_model	
       use mod_prism_proto
       use mod_comprism_proto
       use mod_psmile_io
       use mod_psmile_io_interfaces,only:combine_with_date
       use mod_psmile_date_and_time

       implicit none

#include <mpif.h>

       integer(kind=ip_intwp_p),intent(out)::id_error
!Local variables
       integer(kind=ip_intwp_p)::il_i,il_port,il_unit
!Test       integer(kind=ip_intwp_p),allocatable::il_units(:)
       logical:: ll_opened,ll_exist

#ifdef __VERBOSE
       write(nulprt,*)'Start - - psmile_def_files'
       call flush(nulprt)
#endif

       if(ig_color/ig_mynummod.ne.1000) then
         WRITE(nulprt,*)'psmile_def_files: Called by proc which is not' &
                       ,' involved with coupling!'
         call flush(nulprt)
         call MPI_ABORT(mpi_comm,0,mpi_err)
       endif

       if((.not.allocated(domain))) then
         WRITE(nulprt,*)'psmile_def_files:' &
                       ,' Not called after psmile_def_domains!' 
         call flush(nulprt)
         call MPI_ABORT(mpi_comm,0,mpi_err)
       endif
        

       id_error=CLIM_Ok

!Find unused units.
!Problem is here that mpi codes usually are scattering/collecting from/on one PE
! which read/write. The rank of that PE is not known  to PSMILE.
!Algorithm: Let each PE involved with coupling do a search beginning from 
!PSMILE_MAX_UNIT. Check if the result is the same on each couple task.
!Currently I don't know if the check is absolutely necessary.
!Moreover, I assume that MPI is doing private I/O for each task, means
!each opened file is handled individually per task by the OS .
!I made my experience with UNICOS where file handling where global per default
!running MPI in autotasking mode. In that case the user had to specify a
!certain FFIO flag to get the normal behaviour.

!Test  allocate(il_units(1:ig_mpp_io_comm_size))
       allocate(ig_units(1:nports))
!RV:22.01.2003
       ig_units=-1

       il_unit=PSMILE_MAX_UNIT
       do il_port=1,nports
          IF (ig_def_state(il_port) .eq. ip_ignout .or. &
            ig_def_state(il_port) .eq. ip_expout .or. &
            ig_def_state(il_port) .eq. ip_output .or. &
            ig_def_state(il_port) .eq. ip_input ) THEN
         do
           inquire(unit=il_unit,exist=ll_exist,opened=ll_opened)
           if( ll_exist .and. (.not.ll_opened)) exit
           il_unit=il_unit-1
           if(il_unit.eq.PSMILE_MAX_RESERVED_UNIT) &
              il_unit=PSMILE_MIN_RESERVED_UNIT-1
           if(il_unit.eq.7) then
             write(nulprt,*)'psmile_def_files: Could not find unit!'
             call MPI_ABORT(mpi_comm,0,mpi_err)
           endif
         enddo
         ig_units(il_port)=il_unit

#ifdef __VERBOSE
         write(nulprt,*)'psmile_def_files: Found FORTRAN I/O unit ', il_unit
         call flush(nulprt)
#endif

         il_unit=il_unit-1
         endif
       enddo

!Opening files!
!I am opening the files such that read / write is performed on one file.
!One can easily change that to distributed i/o changing the fileset attribute.
        
       allocate(ig_action(1:nports))
       allocate(ig_form(1:nports))
       allocate(ig_threading(1:nports))
       allocate(ig_fileset(1:nports))
       allocate(cg_my_input_file(1:nports))
       allocate(cg_output_file(1:nports))

!       call_mpp_sync()
       do il_port=1,nports
         if(ig_def_state(il_port).eq.ip_input) then

           ig_action(il_port)=MPP_RDONLY
	   ig_form(il_port)=MPP_NETCDF
	   ig_threading(il_port)=MPP_MULTI
	   ig_fileset(il_port)=MPP_SINGLE

!I create here the name of the input file according to the convention
!cports(il_port)_in.<year>-<month>-<day>T<hh>:<mm>:ss.nc
!Arnaud, if you like you prefer approach of reading the name from the namcouple
!initialize cg_my_input_file accordingly and comment out the call of
!combine_with_date.

!           call combine_with_date(cports(il_port),'in',il_my_initial_date &
!                                 ,cg_my_input_file(il_port))

           call mpp_open( ig_units(il_port),trim(cg_def_inpfile(il_port)) &
                        , action=ig_action(il_port) &
                        , form=ig_form(il_port) &
                        , threading=ig_threading(il_port) &
                        , fileset=ig_fileset(il_port))
           
         elseif(ig_def_state(il_port).eq.ip_output) then

           ig_action(il_port)=MPP_OVERWR
	   ig_form(il_port)=MPP_NETCDF
	   ig_threading(il_port)=MPP_SINGLE

           call combine_with_date(cports(il_port),'out',ig_inidate &
                                 ,cg_output_file(il_port))

           call mpp_open( ig_units(il_port), trim(cg_output_file(il_port)) &
                        , action=ig_action(il_port) &
                        , form=ig_form(il_port) &
                        , threading=ig_threading(il_port))
	    

         elseif(ig_def_state(il_port).eq.ip_expout) then

           ig_action(il_port)=MPP_OVERWR
	   ig_form(il_port)=MPP_NETCDF
	   ig_threading(il_port)=MPP_SINGLE

           call combine_with_date(cports(il_port),'out',ig_inidate &
                                 ,cg_output_file(il_port))

           call mpp_open( ig_units(il_port),trim(cg_output_file(il_port)) &
                        , action=ig_action(il_port) &
                        , form=ig_form(il_port) &
                        , threading=ig_threading(il_port))

         elseif(ig_def_state(il_port).eq.ip_ignout) then

           ig_action(il_port)=MPP_OVERWR
	   ig_form(il_port)=MPP_NETCDF
	   ig_threading(il_port)=MPP_SINGLE

           call combine_with_date(cg_ignout_field(il_port),'out',ig_inidate &
                                 ,cg_output_file(il_port))

           call mpp_open( ig_units(il_port), trim(cg_output_file(il_port)) &
                        , action=ig_action(il_port) &
                        , form=ig_form(il_port) &
                        , threading=ig_threading(il_port))
         endif
       enddo

       
#ifdef __VERBOSE
       write(nulprt,*)'End - - psmile_def_files'
       call flush(nulprt)
#endif
       end subroutine psmile_def_files

       subroutine psmile_def_metadata(id_error)
!--------------------------------------------------------------------------
!
!This routine defines the metadata headers of netcdf files case or writing.
!In case or reading it defines the  index  to lookup a certain variable in
!a netcdf file
!       Called at the end of prism_enddef.
       use mod_kinds_model
       use mod_psmile_io_interfaces,only:combine_with_date
       use mod_prism_proto
       use mod_comprism_proto
       use mod_psmile_io
       implicit none

#include <mpif.h>

       integer(kind=ip_intwp_p),intent(out)::id_error
!Local
       character(len=64)::cl_varname
       integer(kind=ip_intwp_p)::il_port,il_i
       integer(kind=ip_intwp_p)::il_xbegin,il_xend,il_ybegin,il_yend
       integer(kind=ip_intwp_p)::il_ndim,il_nvar,il_natt,il_ntime
       integer(kind=ip_intwp_p)::il_crn,il_ii,il_jj
       
       type(fieldtype),allocatable::il_vars(:)
!
!      Dimensions of the lons and lats contained in the OASIS 3 grid file
       integer(kind=ip_intwp_p)::lens_lat(4),lens_lon(4)
       REAL(kind=ip_double_p), ALLOCATABLE :: lon(:,:),lat(:,:)
       REAL(kind=ip_double_p), ALLOCATABLE :: loncrn(:,:,:),latcrn(:,:,:)
       LOGICAL                 ::   lg_exist
       CHARACTER(len=8):: cg_clim_cgrdnamnc, clstrg
       INTEGER(kind=ip_intwp_p):: nc_grdid, il_varid, il_x, il_y
       REAL(kind=ip_double_p), ALLOCATABLE :: rl_lons(:,:), rl_lats(:,:)
       REAL(kind=ip_double_p), ALLOCATABLE :: rl_lonscrn(:,:,:), rl_latscrn(:,:,:)
       REAL(kind=ip_double_p):: difflon
       REAL(kind=ip_double_p):: threesixty
       LOGICAL                 ::   llylat,llxlon

#ifdef __VERBOSE
       write(nulprt,*)'Start -- psmile_def_metada'
       call flush(nulprt)
#endif

       if(ig_color/ig_mynummod.ne.1000) then
         WRITE(nulprt,*)'psmile_def_metdata: Called by proc which is not' &
                       ,' involved with coupling!'
         call flush(nulprt)
         call MPI_ABORT(mpi_comm,0,mpi_err)
       endif

       if((.not.allocated(ig_units)).and.(.not.allocated(domain))) then
         WRITE(nulprt,*)'psmile_def_metdata:' &
                       ,' Not called after psmile_def_domains' &
                       ,' and psmile_def_files!'
         call flush(nulprt)
         call MPI_ABORT(mpi_comm,0,mpi_err)
       endif
        

       id_error=CLIM_Ok
       threesixty=360.0

       allocate(ig_var_index(nports))
       allocate(ig_vars(1:nports))
! 2005-07-20, Luis Kornblueh, MPIMet
! for later association test the pointer must be initialized, otherwise
! the program might crash - that is a very common problem! mpp_io is providing
! an initial axis which is extended by the nullifyd data pointer. 
       ig_vars(:) = default_field
       allocate(ig_ntimes(1:nports))

!RV:22.01.2003
#ifdef __READ_BY_LOOKUP
       allocate(dg_times(1:nports))
!AC:31/01
       DO il_i = 1,nports
          nullify(dg_times(il_i)%rl_times)
       ENDDO
#endif

       allocate(x_axis(1:nports))
! 2005-07-20, Luis Kornblueh, MPIMet
       x_axis(:) = default_axis
       allocate(y_axis(1:nports))
! 2005-07-20, Luis Kornblueh, MPIMet
       y_axis(:) = default_axis
       allocate(c_axis(1:nports))
! 2005-07-20, Luis Kornblueh, MPIMet
       c_axis(:) = default_axis
#ifdef __VERBOSE
       write(nulprt,*)'psmile_def_metada: Number of corners: ',ig_noc
       call flush(nulprt)
#endif
       allocate(nbr_corners(1:nports))
       nbr_corners=ig_noc
!Was not allocated. Oasis sends 2D arrays. In case of bundles or 3d arrays
!we have to declare z_axis + plus one for bundles.
!Left here as a reminder!
!       allocate(z_axis(1:nports))
! 2005-07-20, Luis Kornblueh, MPIMet
!       z_axis(:) = default_axis
       allocate(t_axis(1:nports))
! 2005-07-20, Luis Kornblueh, MPIMet
       t_axis(:) = default_axis
       allocate(field(1:nports))
! 2005-07-20, Luis Kornblueh, MPIMet
       field(:) = default_field
       allocate(lonij(1:nports))
! 2005-07-20, Luis Kornblueh, MPIMet
       lonij(:) = default_field
       allocate(latij(1:nports))
! 2005-07-20, Luis Kornblueh, MPIMet
       latij(:) = default_field
       allocate(crnlonij(1:nports))
! 2005-07-20, Luis Kornblueh, MPIMet
       crnlonij(:) = default_field
       allocate(crnlatij(1:nports))
! 2005-07-20, Luis Kornblueh, MPIMet
       crnlatij(:) = default_field

!The following files are allocated here since no informations
!are given in Oasis regarding to long names and units. I am filling them with
!dummy informations. I put the data structure into mod_psmile_io.
       allocate(cports_lgname(1:nports))
       allocate(cports_units(1:nports))
       cports_lgname='BLANK'
       cports_units='1'
       

       DO il_port=1,nports
	  IF(ig_def_state(il_port).EQ.ip_input) THEN

!Get information with regard to no. of dimensions, variables, attributes
!and time stamps.
	    call mpp_get_info(ig_units(il_port)&
			     ,il_ndim,il_nvar,il_natt,il_ntime)
	    
            ig_ntimes(il_port)=il_ntime

            allocate(il_vars(1:il_nvar))
!RV,16.12.2002:Get the all ids of the fields within the file.
            call mpp_get_fields (ig_units(il_port),il_vars(:))

!Loop over variables and find the index of cports(il_port).
            ig_var_index(il_port)=-1
	    do il_i=1,il_nvar

	      call mpp_get_atts(il_vars(il_i),name=cl_varname)
	      if(trim(cl_varname).eq.trim(cports(il_port)) ) then 
		ig_var_index(il_port)=il_i
		ig_vars(il_port)=il_vars(il_i)
              endif

	    enddo

!Check: Did I find the variable? If not, the user has given the wrong
!       input file.
            if(ig_var_index(il_port).lt.0) then

              write(nulprt, * ) &
                  'psmile_def_metadata: Variable name given in namcouple '&
                  ,cports(il_port) &
                  ,'could not be found in file ',cg_my_input_file(il_port) &
                  ,' port number: ',il_port
              call MPI_ABORT(mpi_comm,0,mpi_err)

            endif

	    deallocate(il_vars)

	  elseif((ig_def_state(il_port).eq.ip_output).or. &
		 (ig_def_state(il_port).eq.ip_expout).or. &
		 (ig_def_state(il_port).eq.ip_ignout))then

!Define the global axis X and Y.
!I have no informations on the units of the axis. I assume that
!the axis are indices in order to perform a mapping like lat(i,j).
	    call mpp_get_global_domain(domain(il_port) &
                                      ,xbegin=il_xbegin,xend=il_xend &
                                      ,ybegin=il_ybegin,yend=il_yend)
            call mpp_write_meta(ig_units(il_port) &
                               ,'Conventions' &
                               ,cval='CF-1.0')
#ifdef __VERBOSE
            write(nulprt,*)'psmile_def_metadata: x-axis ',il_xbegin,il_xend
#endif

            call mpp_write_meta(ig_units(il_port),x_axis(il_port) &
                               ,'X','1','global index space for x' &
                               ,domain=xdom(il_port) &
                               ,data=(/(il_i-1.,il_i=il_xbegin,il_xend)/) )

#ifdef __VERBOSE
            write(nulprt,*)'psmile_def_metadata: y-axis ',il_ybegin,il_yend
            call flush(nulprt)
#endif

            call mpp_write_meta(ig_units(il_port),y_axis(il_port) &
                               ,'Y','1','global index space for y' &
                               ,domain=ydom(il_port) &
                               ,data=(/(il_i-1.,il_i=il_ybegin,il_yend)/) )
#ifdef __VERBOSE
            write(nulprt,*)'psmile_def_metadata: c-axis '
#endif
            call mpp_write_meta(ig_units(il_port),c_axis(il_port) &
                               ,'C','1','global index space for c' &
                               ,data=(/(il_i-1.,il_i=1,nbr_corners(il_port))/) )


!Define the unlimited time axis
            call combine_with_date('second','since',ig_inidate &
                                 ,cl_varname)

            call mpp_write_meta( ig_units(il_port), t_axis(il_port) &
                               , 'time', trim(cl_varname), 'Time' )
!Define the real coordinates , here lattitudes and longitudes.
! I have no information with regard to latitude and longitude
!RV:17.04.2003 Wright this information anyway to pass the CF check.
            call mpp_write_meta( ig_units(il_port), latij(il_port) &
                               , (/x_axis(il_port),y_axis(il_port)/) &
                               , 'lat', 'degrees_north' &
                               , 'latitude')
            call mpp_write_meta( ig_units(il_port), lonij(il_port) &
                               , (/x_axis(il_port),y_axis(il_port)/) &
                               , 'lon', 'degrees_east' &
                               , 'longitude')
!rv,sgi Number of corners has to be the leading dimension according the
!rv,sgi CF convention
            call mpp_write_meta( ig_units(il_port), crnlatij(il_port) &
                      , (/c_axis(il_port),x_axis(il_port),y_axis(il_port)/) &
                      , 'bounds_lat', 'degrees_north' &
                      , 'corner latitudes')
            call mpp_write_meta( ig_units(il_port), crnlonij(il_port) &
                      , (/c_axis(il_port),x_axis(il_port),y_axis(il_port)/) &
                      , 'bounds_lon', 'degrees_east' &
                      , 'corner longitudes')
!Define that variable cports(il_port) of unit cports_units(il_port) and
!longname cports_lgname(il_port) will be written as field field(il_port)
!according to the three axis x_axis(il_port),y_axis(il_port) and
!t_axis(il_port). The precision is double (pack=1).
!RV
! Initialize cports_lgname and cports_units  according to the CF name table 
! exploiting the field label.
!
            IF(allocated(cfldlab)) then
              IF(ig_def_numlab(il_port).gt.0.and. &
                ig_def_numlab(il_port).le.PSMILE_MAX_CF_TABLE_ENTRIES) then
                 cports_lgname(il_port)=cfldlab(ig_def_numlab(il_port))
                 cports_units(il_port)=cfunitslab(ig_def_numlab(il_port))
              ENDIF
            ENDIF
!RV
            IF (ig_def_state(il_port).EQ.ip_ignout) THEN
               call mpp_write_meta( ig_units(il_port), field(il_port) &
                    , (/x_axis(il_port),y_axis(il_port) &
                    ,t_axis(il_port)/) &
                    ,trim(cg_ignout_field(il_port)) &
                    ,trim(cports_units(il_port)) &
                    , trim(cports_lgname(il_port)), pack=1 )
#ifdef __DEBUG
              write(nulprt,*)'psmile_def_metadata 1:',ig_def_numlab(il_port) &
                            ,' ',trim(cg_ignout_field(il_port)) &
                            ,' ',trim(cports_lgname(il_port)) &
                            ,' ',trim(cports_units(il_port))
              call flush(nulprt)
#endif
            ELSE
               call mpp_write_meta( ig_units(il_port), field(il_port) &
                    , (/x_axis(il_port),y_axis(il_port) &
                    ,t_axis(il_port)/) &
                    ,trim(cports(il_port)) &
                    ,trim(cports_units(il_port)) &
                    , trim(cports_lgname(il_port)), pack=1 )
#ifdef __DEBUG
              write(nulprt,*)'psmile_def_metadata 2:',ig_def_numlab(il_port) &
                            ,' ', trim(cports(il_port)) &
                            ,' ',trim(cports_lgname(il_port)) &
                            ,' ',trim(cports_units(il_port))
              call flush(nulprt)
#endif
             ENDIF

!
!            Define an additional attribute bounds for lon and lats.
!
             call mpp_write_meta(ig_units(il_port) &
                                ,mpp_get_id(latij(il_port)) &
                                , 'bounds',cval='bounds_lat')
             call mpp_write_meta(ig_units(il_port) &
                                ,mpp_get_id(lonij(il_port)) &
                                , 'bounds',cval='bounds_lon')
!Define an additional attribute that field(il_port) lives on the coordinates
!lon and lat. This is a CF requirement!

             call mpp_write_meta(ig_units(il_port) &
                                ,mpp_get_id(field(il_port)) &
                                , 'coordinates',cval='lon lat')

!Flush the  metadata information to the file connected to unit 
!ig_units(il_port).

             call mpp_write(ig_units(il_port), x_axis(il_port) )
             call mpp_write(ig_units(il_port), y_axis(il_port) )
!
!rv          Metadata should be flushed from the struct c_axis
!rv          into the NetCDF file
!
             call mpp_write(ig_units(il_port), c_axis(il_port) )
!
             IF (mpp_pe() .EQ. mpp_root_pe()) THEN
                 WRITE(nulprt,*)'mod_psmile_io - - check grids.nc file:'     
                 cg_clim_cgrdnamnc=cg_clim_cgrdnam//'.nc'                  
                 INQUIRE(FILE = cg_clim_cgrdnamnc, EXIST = lg_exist)       
                 IF (.NOT. lg_exist) THEN                                  
                     WRITE(nulprt,*) '     - no grids.nc file found'
                     WRITE(nulprt,*) 'lats and lons will not be written to output files' 
                     CALL flush(nulprt)                                    
                 ELSE                                                      
                     WRITE(nulprt,*)'mod_psmile_io - - grids.nc file found'
                     ! Read the lats and the lons
                     CALL mpp_open(nc_grdid, 'grids.nc', &
                        action=MPP_RDONLY, &
                        form=MPP_NETCDF, &
                        threading=MPP_SINGLE)
                     CALL mpp_get_info(nc_grdid, il_ndim, il_nvar, &
                        il_natt, il_ntime )
                     ALLOCATE(il_vars(1:il_nvar))
                     CALL mpp_get_fields (nc_grdid,il_vars(:))
                     !
                     ! Longitudes
                     !
                     DO il_i=1,il_nvar
                       clstrg = cga_clim_locator(il_port)//cg_clim_lonsuf
                       CALL mpp_get_atts(il_vars(il_i),name=cl_varname,siz=lens_lon)
!
!RV,sgi                       ALLOCATE(lon(1:il_xend-il_xbegin+1,1:il_yend-il_ybegin+1))
!
                       IF(TRIM(cl_varname).EQ.TRIM(clstrg) ) THEN
#ifdef __VERBOSE
                           write(nulprt,*)'psmile_def_metadata: cl_varname= ' &
                           ,trim(cl_varname)
                           call flush(nulprt)
#endif
                           IF(.NOT.ALLOCATED(lon)) &
                           ALLOCATE(lon(1:il_xend-il_xbegin+1,1:il_yend-il_ybegin+1))
                           ALLOCATE(rl_lons(lens_lon(1),lens_lon(2)))

                           !NOTE: I am using tindex = 1 in order to circumvent a
                           !peculiarity of mpp_io: grids.nc contains the lons/lats
                           !declared as lon(i,j)/lat(i,j). mpp_io interprets i and j as
                           !axes. However, the axis i and j have no associated data.
                           !Therefore, i and j are NOT taken as spatial axes but
                           !as something related to a time stamp. Until that "feature"
                           !is corrected  use the call below.
                           !
                           CALL mpp_read(nc_grdid,il_vars(il_i),rl_lons,tindex=1)
                           !
!rv,sgi<
                           !If the user has specified INVERT/REVERSE and 
                           !$CORLAT=NORSUD and/or $CORLON=ESTWST in the 
                           !preprocessing section of the field we INVERT the 
                           !ordering of longitudes such that data and grid are
                           !are matching. The assertion is that 
                           !grid informations are initialized SUDNOR and WSTEST.

                           if((ig_def_invert(il_port).gt.1).or. &
                              (ig_def_reverse(il_port).gt.1)) then
                             llxlon=ig_def_invert(il_port).eq.3  &
                                .or.ig_def_invert(il_port).eq.4  &
                                .or.ig_def_reverse(il_port).eq.3  &
                                .or.ig_def_reverse(il_port).eq.4  
                             llylat=ig_def_invert(il_port).eq.2  &
                                .or.ig_def_invert(il_port).eq.4  &
                                .or.ig_def_reverse(il_port).eq.2 &
                                .or.ig_def_reverse(il_port).eq.4  
                             call inv2d (rl_lons,lens_lon(1),lens_lon(2) &
                                         ,llxlon,llylat)
                           endif
!rv,sgi>
                           !Reshape the read array such that it matches the current
                           !axes of the output file.
                           !
                           lon=RESHAPE(rl_lons,shape=SHAPE(lon))
                           !
                           !Dump it ot the output file.
                           !
                           CALL mpp_write(ig_units(il_port),lonij(il_port),lon)
                           DEALLOCATE(rl_lons)
                       ENDIF
                     !
                     !Latitudes
                     !
                     !See comments above.
                     !
                     clstrg = cga_clim_locator(il_port)//cg_clim_latsuf
!
                     ALLOCATE(lat(1:il_xend-il_xbegin+1,1:il_yend-il_ybegin+1))
!
                       CALL mpp_get_atts(il_vars(il_i),name=cl_varname,siz=lens_lat)
                       IF(TRIM(cl_varname).EQ.TRIM(clstrg) ) THEN
#ifdef __VERBOSE
                           write(nulprt,*)'psmile_def_metadata: cl_varname= ' &
                           ,trim(cl_varname)
                           call flush(nulprt)
#endif
                           ALLOCATE(rl_lats(lens_lat(1),lens_lat(2)))

                           CALL mpp_read(nc_grdid,il_vars(il_i),rl_lats,tindex=1)
!rv,sgi<
                           if((ig_def_invert(il_port).gt.1).or. &
                              (ig_def_reverse(il_port).gt.1)) then
                             llxlon=ig_def_invert(il_port).eq.3  &
                                .or.ig_def_invert(il_port).eq.4  &
                                .or.ig_def_reverse(il_port).eq.3  &
                                .or.ig_def_reverse(il_port).eq.4  
                             llylat=ig_def_invert(il_port).eq.2  &
                                .or.ig_def_invert(il_port).eq.4  &
                                .or.ig_def_reverse(il_port).eq.2 &
                                .or.ig_def_reverse(il_port).eq.4  
                             call inv2d (rl_lats,lens_lat(1),lens_lat(2) &
                                         ,llxlon,llylat)
                           endif
!rv,sgi>
                           lat=reshape(rl_lats,shape=shape(lat))
                           call mpp_write(ig_units(il_port),latij(il_port),lat)
                           DEALLOCATE(rl_lats)
                       ENDIF
                     !
                     ! Longitude corners
                     !
                       clstrg = cga_clim_locator(il_port)//crn_clim_lonsuf

                       CALL mpp_get_atts(il_vars(il_i),name=cl_varname,siz=lens_lon)
                       IF(TRIM(cl_varname).EQ.TRIM(clstrg) ) THEN
#ifdef __VERBOSE
                           write(nulprt,*) &
                           'psmile_def_metada, lon, clstrg: ',clstrg,lens_lon 
                           call flush(nulprt)
#endif
                           nbr_corners(il_port)=lens_lon(3)
                           ALLOCATE(rl_lonscrn(lens_lon(1),lens_lon(2) &
                                              ,nbr_corners(il_port)))
                           ALLOCATE(loncrn(nbr_corners(il_port) &
                                          ,1:il_xend-il_xbegin+1 &
                                          ,1:il_yend-il_ybegin+1))
                           lens_lon(1)=il_xend-il_xbegin+1
                           lens_lon(2)=il_yend-il_ybegin+1
                           lens_lon(3)=nbr_corners(il_port)
                           !
                           CALL mpp_read(nc_grdid,il_vars(il_i),rl_lonscrn,tindex=1)
!rv,sgi<
                           if((ig_def_invert(il_port).gt.1).or. &
                              (ig_def_reverse(il_port).gt.1)) then
                             llxlon=ig_def_invert(il_port).eq.3  &
                                .or.ig_def_invert(il_port).eq.4  &
                                .or.ig_def_reverse(il_port).eq.3  &
                                .or.ig_def_reverse(il_port).eq.4  
                             llylat=ig_def_invert(il_port).eq.2  &
                                .or.ig_def_invert(il_port).eq.4  &
                                .or.ig_def_reverse(il_port).eq.2 &
                                .or.ig_def_reverse(il_port).eq.4  
                             call inv2dcrn (rl_lonscrn,lens_lon(1),lens_lon(2) &
                                           ,nbr_corners(il_port),llxlon,llylat)
                           endif
!rv,sgi>

                           !
                           !Reshape the read array such that it matches the current
                           !axes of the output file.
                           !
                           call transp_crn2cf(rl_lonscrn,loncrn,lens_lon)
!rv,sgi<
                           !
                           ! Check for cyclic boundaries !
                           ! For plot routines using a mesh-fill method
                           ! the distances of the cell centers to the corners
                           ! have to be less equal 360. degrees.
                           !
                            do il_crn=1,nbr_corners(il_port)
                              do il_jj=1,il_yend-il_ybegin+1
                              do il_ii=1,il_xend-il_xbegin+1
                                difflon=lon(il_ii,il_jj)- &
                                        loncrn(il_crn,il_ii,il_jj)
                                if(difflon.gt.300.0) then
                                  loncrn(il_crn,il_ii,il_jj)= &
                                         loncrn(il_crn,il_ii,il_jj)+threesixty
                                elseif(difflon.lt.-300.0) then
                                  loncrn(il_crn,il_ii,il_jj)= &
                                         loncrn(il_crn,il_ii,il_jj)-threesixty
                                endif
                              enddo
                              enddo
                            enddo
!rv,sgi>
                           !
                           !Dump it to the output file.
                           !
                           CALL mpp_write(ig_units(il_port),crnlonij(il_port),loncrn)
                           DEALLOCATE(rl_lonscrn)
                           DEALLOCATE(loncrn)
                       ENDIF
!rv,sgi                       DEALLOCATE(lon)
                     !
                     !Latitude corners
                     !
                     !See comments above.
                     !
                       clstrg = cga_clim_locator(il_port)//crn_clim_latsuf
                     
                       CALL mpp_get_atts(il_vars(il_i),name=cl_varname,siz=lens_lat)
                       IF(TRIM(cl_varname).EQ.TRIM(clstrg) ) THEN
#ifdef __VERBOSE
                           write(nulprt,*) &
                           'psmile_def_metada, lat, clstrg: ',clstrg,lens_lat
                           call flush(nulprt)
#endif
                           nbr_corners(il_port)=lens_lat(3)
                           ALLOCATE(rl_latscrn(lens_lat(1),lens_lat(2) &
                                   ,nbr_corners(il_port)))
                           ALLOCATE(latcrn(nbr_corners(il_port) &
                                   ,1:il_xend-il_xbegin+1 &
                                   ,1:il_yend-il_ybegin+1))
                           lens_lat(1)=il_xend-il_xbegin+1
                           lens_lat(2)=il_yend-il_ybegin+1
                           lens_lat(3)=nbr_corners(il_port)

                           CALL mpp_read(nc_grdid,il_vars(il_i),rl_latscrn,tindex=1)
!rv,sgi<
                           if((ig_def_invert(il_port).gt.1).or. &
                              (ig_def_reverse(il_port).gt.1)) then
                             llxlon=ig_def_invert(il_port).eq.3  &
                                .or.ig_def_invert(il_port).eq.4  &
                                .or.ig_def_reverse(il_port).eq.3  &
                                .or.ig_def_reverse(il_port).eq.4  
                             llylat=ig_def_invert(il_port).eq.2  &
                                .or.ig_def_invert(il_port).eq.4  &
                                .or.ig_def_reverse(il_port).eq.2 &
                                .or.ig_def_reverse(il_port).eq.4  
                             call inv2dcrn(rl_latscrn,lens_lat(1),lens_lat(2) &
                                          ,nbr_corners(il_port),llxlon,llylat)
                           endif
!rv,sgi>

                           call transp_crn2cf(rl_latscrn,latcrn,lens_lat)
                           call mpp_write(ig_units(il_port),crnlatij(il_port),latcrn)
                           DEALLOCATE(rl_latscrn)
                           DEALLOCATE(latcrn)
                       ENDIF
                       DEALLOCATE(lat)
                     ENDDO
!rv,sgi<
                     if(ALLOCATED(lon)) DEALLOCATE(lon)
!rv,sgi>
                     DEALLOCATE(il_vars)
                 ENDIF

                 CALL mpp_flush(ig_units(il_port))

             ENDIF

         ENDIF
#ifdef __VERBOSE
       write(nulprt,*)'End -- psmile_def_metada'
       call flush(nulprt)
#endif
     ENDDO
       end subroutine psmile_def_metadata

       subroutine psmile_close_files(id_error)
!------------------------------------------------------------------------------
!      This subroutine closes all opened psmile_io files 
!      Supposed to be called in prism_terminate before  psmile_io_cleanup
!      and before shutting down MPI!
!-----------------------------------------------------------------------
!
       use mod_kinds_model   
       use mod_prism_proto
       use mod_comprism_proto
       use mod_psmile_io
       implicit none

#include <mpif.h>
       integer(kind=ip_intwp_p),intent(out)::id_error
!Local variables
       integer(kind=ip_intwp_p)::il_port

       if( ig_color.ne.1000*ig_mynummod) return
#ifdef __VERBOSE
       write(nulprt,*)'psmile_close_files: Start'
#endif
       id_error=CLIM_Ok

       do il_port=1,nports
!          IF (ig_def_state(il_port) .eq. ip_ignout .or. &
!            ig_def_state(il_port) .eq. ip_expout .or. &
!            ig_def_state(il_port) .eq. ip_output .or. &
!            ig_def_state(il_port) .eq. ip_input ) THEN
!RV:22.01.2003
!Just in case that ig_def_state has been deallocated during termination.
!This way has less interference with rest of prism_terminate.
          IF (ig_units(il_port).gt.0) THEN
             IF (allocated(dg_times).and. &
                 associated(dg_times(il_port)%rl_times)) THEN

               deallocate(dg_times(il_port)%rl_times)

             ENDIF

             call mpp_close(ig_units(il_port))

          ENDIF
       enddo

!RV: 18.12.2002: Moved these lines into psmile_io_cleanup
!       call mpp_io_exit()
!       call mpp_domains_exit()
!       call mpp_exit()

#ifdef __VERBOSE
       write(nulprt,*)'psmile_close_files: End'
#endif
       
       end subroutine psmile_close_files

       subroutine psmile_io_cleanup(id_error)
!-----------------------------------------------------------------------
!      Supposed to be called in prism_terminate after psmile_close_files
!-----------------------------------------------------------------------
       use mod_kinds_model
       use mod_prism_proto
       use mod_comprism_proto
       use mod_psmile_io

       implicit none

       integer(kind=ip_intwp_p),intent(out)::id_error

       if( ig_color.ne.1000*ig_mynummod) return
!Global arrays to specify IO domains
       if(allocated(domain))deallocate(domain)
       if(allocated(xdom))deallocate(xdom)
       if(allocated(ydom))deallocate(ydom)
       if(allocated(lg_is_odd_distribution))deallocate(lg_is_odd_distribution)

!Global arrarys for opening files)
       if(allocated(ig_units))deallocate(ig_units)
       if(allocated(ig_action))deallocate(ig_action)
       if(allocated(ig_form))deallocate(ig_form)
       if(allocated(ig_threading))deallocate(ig_threading)
       if(allocated(ig_fileset))deallocate(ig_fileset)
 
!Global arrays for netcdf metadata informations)
       if(allocated(ig_var_index))deallocate(ig_var_index)
       if(allocated(ig_ntimes))deallocate(ig_ntimes)
       if(allocated(dg_times))deallocate( dg_times)
       if(allocated(ig_vars))deallocate(ig_vars)
       if(allocated(field))deallocate(field)
       if(allocated(latij))deallocate(latij)
       if(allocated(lonij))deallocate(lonij)
       if(allocated(x_axis))deallocate(x_axis)
       if(allocated(y_axis))deallocate(y_axis)
       if(allocated(t_axis))deallocate(t_axis)

!Was not allocated. Oasis sends 2D arrays. In case of bundles or 3d arrays
!we have to declare z_axis + plus one for bundles.
       if(allocated(z_axis))deallocate(z_axis)

! These arrays are supposed to be part of a different module!)
! However, no information is available in Oasis 3.0. So, let them live here)
! for a while.)
       
       if(allocated(cports_lgname)) deallocate(cports_lgname)
       if(allocated(cports_units)) deallocate(cports_units)

       call mpp_io_exit()
       call mpp_domains_exit()
       call mpp_exit()


       end subroutine psmile_io_cleanup

       subroutine psmile_read_8(id_port_id,rd_field,id_newtime)
!--------------------------------------------------------------------------
!       Called in rism_get
       use mod_kinds_model
       use mod_prism_proto
       use mod_comprism_proto
       use mod_psmile_io

       implicit none

       integer(kind=ip_intwp_p),intent(in)::id_newtime,id_port_id
       REAL(kind=ip_double_p),DIMENSION(myport(4,id_port_id)), &
                                                  intent(out) :: rd_field
!local
       real(kind=ip_double_p)::rl_time
       integer(kind=ip_intwp_p)::il_record_no
       real(kind=ip_double_p)::diff_first,diff_i
       integer(kind=ip_intwp_p)::il_i,i

#ifdef __VERBOSE
       write(nulprt,*) 'psmile_read_8: Start'
       call flush(nulprt)
#endif

!RV:22.01.2003

#ifdef __READ_BY_LOOKUP
!I try to find the record number of timestamp id_newtime by scrolling through
!the time stamps of the file.

       rl_time=id_newtime
       if(.not.associated(dg_times(id_port_id)%rl_times))then

         allocate(dg_times(id_port_id)%rl_times(1:ig_ntimes(id_port_id)))
         call mpp_get_times( ig_units(id_port_id) &
                           ,dg_times(id_port_id)%rl_times(:))
       endif
!       il_record_no=minloc(abs(dg_times(id_port_id)%rl_times-rl_time),dim=1)
       il_record_no=1
       diff_first=abs(dg_times(id_port_id)%rl_times(1)-rl_time)
       do i=1,ig_ntimes(id_port_id)
     	   diff_i=abs(dg_times(id_port_id)%rl_times(i)-rl_time)
    	   if(abs(diff_i).lt.diff_first) then
        	diff_first=diff_i
        	il_record_no=i
    	   endif
       enddo
       if(abs(dg_times(id_port_id)%rl_times(il_record_no)-rl_time).gt.1.d-8) &
       then
         write(nulprt,*)'psmile_read_8: The time stamp ',id_newtime, &
                        ' is not available in file ',cg_def_inpfile(id_port_id)
         call flush(nulprt)
         call MPI_ABORT(mpi_comm,0,mpi_err)
       endif

#ifdef __VERBOSE
       write(nulprt,*)'psmile_read_8: timestamp, record no.',id_newtime, &
	              il_record_no
       call flush(nulprt)
#endif
#else
!Alternative:
!Assuming that time stamps are starting from zero
       il_record_no=id_newtime/ig_def_freq(id_port_id) +1
#endif
       
       call mpp_read(ig_units(id_port_id),ig_vars(id_port_id) &
                    ,domain(id_port_id),rd_field,il_record_no)

#ifdef __VERBOSE
       write(nulprt,*) 'psmile_read_8: End'
       call flush(nulprt)
#endif
       end subroutine psmile_read_8

       subroutine psmile_read_4(id_port_id,rd_field,id_newtime)
!--------------------------------------------------------------------------
!       Called in prism_get
       use mod_kinds_model
       use mod_prism_proto
       use mod_comprism_proto
       use mod_psmile_io

       implicit none

       integer(kind=ip_intwp_p),intent(in)::id_newtime,id_port_id
       REAL(kind=ip_single_p), DIMENSION(myport(4,id_port_id)) :: rd_field
!local
       REAL(kind=ip_double_p), DIMENSION(myport(4,id_port_id)) :: rl_field
       real(kind=ip_double_p)::rl_time
       integer(kind=ip_intwp_p)::il_record_no,il_i

#ifdef __VERBOSE
       write(nulprt,*) 'psmile_read_4: Start'
#endif

!RV:22.01.2003
#ifdef __READ_BY_LOOKUP
!I try to find the record number of timestamp id_newtime by scrolling through
!the time stamps of the file.
       rl_time=id_newtime

       if(.not.associated(dg_times(id_port_id)%rl_times))then

         allocate(dg_times(id_port_id)%rl_times(1:ig_ntimes(id_port_id)))
         call mpp_get_times( ig_units(id_port_id), &
                           dg_times(id_port_id)%rl_times(:))

       endif

       il_record_no=minloc(abs(dg_times(id_port_id)%rl_times-rl_time),dim=1)

       if(abs(dg_times(id_port_id)%rl_times(il_record_no)-rl_time).gt.1.d-8) &
       then
         write(nulprt,*)'psmile_read_4: The time stamp ',id_newtime, &
                        ' is not available in file ',cg_def_inpfile(id_port_id)
         call flush(nulprt)
         call MPI_ABORT(mpi_comm,0,mpi_err)
       endif
#ifdef __VERBOSE
       write(nulprt,*)'psmile_read_4: timestamp, record no.',id_newtime, &
	              il_record_no
       call flush(nulprt)
#endif
#else
!Alternative:
!Assuming that time stamps are starting from zero
       il_record_no=id_newtime/ig_def_freq(id_port_id) +1
#endif
       
       call mpp_read(ig_units(id_port_id),ig_vars(id_port_id) &
                    ,domain(id_port_id),rl_field,il_record_no)
       rd_field=rl_field

#ifdef __VERBOSE
       write(nulprt,*) 'psmile_read_4: End'
       call flush(nulprt)
#endif
       end subroutine psmile_read_4

       subroutine psmile_write_4(id_port_id,rd_field,id_newtime)
!--------------------------------------------------------------------------
!       Called in prism_get or prism_put
       use mod_kinds_model
       use mod_prism_proto
       use mod_comprism_proto
       use mod_psmile_io

       implicit none
       integer(kind=ip_intwp_p),intent(in)::id_newtime,id_port_id
       REAL(kind=ip_single_p), DIMENSION(myport(4,id_port_id)) :: rd_field
!Local
       real(kind=ip_double_p)::rl_time
       real(ip_double_p),DIMENSION(myport(4,id_port_id)) ::rl_field

#ifdef __VERBOSE
       write(nulprt,*)' psmile_write_4: Start'
#endif
       rl_time=id_newtime
       rl_field=rd_field
       call mpp_write(ig_units(id_port_id),field(id_port_id) &
                              ,domain(id_port_id),rl_field,rl_time)
#ifdef __VERBOSE
       write(nulprt,*)' psmile_write_4: End'
#endif

       end subroutine psmile_write_4

       subroutine psmile_write_8(id_port_id,rd_field,id_newtime)
!--------------------------------------------------------------------------
!       Called in prism_get or prism_put
       use mod_kinds_model
       use mod_prism_proto
       use mod_comprism_proto
       use mod_psmile_io

       implicit none
       integer(kind=ip_intwp_p),intent(in)::id_newtime,id_port_id
       integer(kind=ip_intwp_p)::il_stat
       REAL(kind=ip_double_p), &
          DIMENSION(1:myport(4,id_port_id)),intent(inout) :: rd_field
!Local
       real(kind=ip_double_p)::rl_time
       integer(kind=ip_intwp_p) :: ij

!       write(70+mpi_rank+(ig_color/1000-1)*4,'(4e12.5)')rd_field
#ifdef __VERBOSE
       write(nulprt,*)' psmile_write_8: Start'
       call flush(nulprt)
#endif
       rl_time=id_newtime
#ifdef __VERBOSE
       write(nulprt,*)'rl_time ',rl_time
#endif
!       do ij =1,4608,4608/10
!          write(nulprt,*)'rd_field bef mpp_write ',rd_field(ij)
!       enddo
!       call flush(nulprt)
       call mpp_write(ig_units(id_port_id),field(id_port_id) &
                              ,domain(id_port_id),rd_field,rl_time)

#ifdef __VERBOSE
       write(nulprt,*)' psmile_write_8: End'
       call flush(nulprt)
#endif
       end subroutine psmile_write_8

       subroutine indexi(n,arr,indx)
!---------------------------------------------------------------------
! Generates a list indx which sorts arr in ascending order.
! This is called sorting by indexing.
! Code taken from 'Numerical Recipes'
!---------------------------------------------------------------------
      use mod_kinds_model
      INTEGER(KIND=IP_INTWP_P),intent(in):: n
      INTEGER(KIND=IP_INTWP_P),intent(out)::indx(n)
      integer(kind=ip_intwp_p),intent(in):: arr(n)
!Local
      integer(kind=ip_intwp_p),PARAMETER:: M=7,NSTACK=128
      INTEGER(KIND=IP_INTWP_P):: i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
      integer(kind=ip_intwp_p):: a

      do 11 j=1,n
        indx(j)=j
11    continue
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 13 j=l+1,ir
          indxt=indx(j)
          a=arr(indxt)
          do 12 i=j-1,1,-1
            if(arr(indx(i)).le.a)goto 2
            indx(i+1)=indx(i)
12        continue
          i=0
2         indx(i+1)=indxt
13      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        itemp=indx(k)
        indx(k)=indx(l+1)
        indx(l+1)=itemp
        if(arr(indx(l+1)).gt.arr(indx(ir)))then
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l)).gt.arr(indx(ir)))then
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        endif
        if(arr(indx(l+1)).gt.arr(indx(l)))then
          itemp=indx(l+1)
          indx(l+1)=indx(l)
          indx(l)=itemp
        endif
        i=l+1
        j=ir
        indxt=indx(l)
        a=arr(indxt)
3       continue
          i=i+1
        if(arr(indx(i)).lt.a)goto 3
4       continue
          j=j-1
        if(arr(indx(j)).gt.a)goto 4
        if(j.lt.i)goto 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        goto 3
5       indx(l)=indx(j)
        indx(j)=indxt
        jstack=jstack+2
        if(jstack.gt.NSTACK)pause 'NSTACK too small in indexx'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END subroutine indexi

      subroutine combine_with_date(cd_in,cd_mode,id_initial_date,cd_on)
!-------------------------------------------------------------
      use mod_kinds_model
      character(len=*),intent(in)::cd_in
      character(len=*),intent(in)::cd_mode
      character(len=*),intent(out)::cd_on
      integer(kind=ip_intwp_p),intent(in)::id_initial_date(:)
!Local
      integer(kind=ip_intwp_p),parameter::il_nsuffixes=2
      integer(kind=ip_intwp_p)::leni,leno,lens,lenb,il_i
      character(len=8)::suffixes(il_nsuffixes),suffix
      character(len=len(cd_in))::cl_basename
      character(len=4)::cl_year
      character(len=2)::cl_date(1:size(id_initial_date)-1)
     
      suffixes(1)='.nc'
      suffixes(2)='.grib'
     
      leni=len(trim(cd_in))
      leno=len(cd_on)
     
      do il_i=1,il_nsuffixes
     
        suffix=suffixes(il_i)
        lens=len(trim(suffix))
      
        if(cd_in(leni-lens+1:leni).eq.suffix(1:lens)) then
          cl_basename(1:leni-lens)=cd_in(1:leni-lens)
          lenb=leni-lens
          EXIT
        else
          cl_basename(1:leni)=cd_in(1:leni)
          lenb=leni
          suffix='.nc'
        endif
     
      enddo
     
     
      write(cl_year,'(i4.4)')id_initial_date(1)

      do il_i=2,size(id_initial_date)

        write(cl_date(il_i-1),'(i2.2)')id_initial_date(il_i)

      enddo
     
      if(trim(cd_mode) .eq. 'since' ) then
      cd_on=trim(cl_basename(1:lenb))//' '//trim(cd_mode)//' ' &
                                     //cl_year//'-'//cl_date(1)//'-' &
                                     //cl_date(2)//' ' &
                                     //cl_date(3)//':' &
                                     //cl_date(4)//':' &
                                     //cl_date(5)
      else
      cd_on=trim(cl_basename(1:lenb))//'_'//trim(cd_mode)//'.' &
                                     //cl_year//'-'//cl_date(1)//'-' &
                                     //cl_date(2)//'T' &
                                     //cl_date(3)//':' &
                                     //cl_date(4)//':' &
                                     //cl_date(5)//suffix
      endif
     
      return
      end subroutine combine_with_date

      subroutine transp_crn2cf(x,y,vshape)
        use mod_kinds_model
        implicit none
!       Input parameters
        Integer, Intent(in):: vshape(3)
        REAL(kind=ip_double_p),Intent(in)::x(vshape(1),vshape(2),vshape(3))
!       Ouput parameters
        REAL(kind=ip_double_p),Intent(out)::y(vshape(3),vshape(1),vshape(2))
!Local
        integer::il_i,il_j,il_k

        do il_k=1,vshape(3)
          do il_j=1,vshape(2)
            do il_i=1,vshape(1)
              y(il_k, il_i, il_j)=x(il_i ,il_j, il_k)
            enddo
          enddo
        enddo

      end subroutine transp_crn2cf

!rv,sgi<
      subroutine inv2d(x,ilon,ilat,lxlon,lylat)
        use mod_kinds_model
        use mod_comprism_proto
        implicit none
!
!     Input Parameters
!
        Integer,Intent(In)::ilon,ilat
        REAL(kind=ip_double_p)::x(ilon,ilat)
        Logical,Intent(In)::lxlon,lylat
!
!     Local Variables
!
        REAL(kind=ip_double_p)::xtmp
        Integer(kind=ip_double_p):: ijmed,iimed,ji,jj
#ifdef __VERBOSE
       write(nulprt,*)'inv2d start:',ilon,ilat,lxlon,lylat
       call flush(nulprt)
#endif
!
!       Reorder lattitude related values stored as the y-direction.
!
        if(lylat) then
          ijmed = ilat/2
          DO jj = 1, ijmed
            DO ji = 1, ilon
              xtmp = x(ji,ilat + 1 - jj)
              x(ji,ilat + 1 - jj) = x(ji,jj)
              x(ji,jj) = xtmp
            ENDDO
          ENDDO
        endif

!
!       Reorder longitude related values stored as the x-direction.
!
        if(lxlon) then
          iimed = ilon/2
          DO  jj = 1, ilat
            DO  ji = 1, iimed
              xtmp = x(ilon + 1 - ji,jj)
              x(ilon + 1 - ji,jj) = x(ji,jj)
              x(ji,jj) = xtmp
            ENDDO
          ENDDO
        endif

#ifdef __VERBOSE
       write(nulprt,*)'inv2d end'
       call flush(nulprt)
#endif
        
      end subroutine inv2d

      subroutine inv2dcrn(x,ilon,ilat,icrn,lxlon,lylat)
        use mod_kinds_model
        use mod_comprism_proto
        implicit none
!
!     Input Parameters
!
        Integer,Intent(In)::ilon,ilat,icrn
        REAL(kind=ip_double_p)::x(ilon,ilat,icrn)
        Logical,Intent(In)::lxlon,lylat
!
!     Local Variables
!
        REAL(kind=ip_double_p)::xtmp
        Integer(kind=ip_double_p):: ijmed,iimed,ji,jj,jc
#ifdef __VERBOSE
       write(nulprt,*)'inv2dcrn start:',ilon,ilat,icrn,lxlon,lylat
       call flush(nulprt)
#endif
!
!       Reorder corners of lattitude 
!
        if(lylat) then
          ijmed = ilat/2
          DO jc=1,icrn
            DO jj = 1, ijmed
              DO ji = 1, ilon
                xtmp = x(ji,ilat + 1 - jj,jc)
                x(ji,ilat + 1 - jj,jc) = x(ji,jj,jc)
                x(ji,jj,jc) = xtmp
              ENDDO
            ENDDO
          ENDDO
        endif

!
!       Reorder corners of longitude 
!
        if(lxlon) then
          iimed = ilon/2
          DO jc=1,icrn
            DO  jj = 1, ilat
              DO  ji = 1, iimed
                xtmp = x(ilon + 1 - ji,jj,jc)
                x(ilon + 1 - ji,jj,jc) = x(ji,jj,jc)
                x(ji,jj,jc) = xtmp
              ENDDO
            ENDDO
          ENDDO
        endif

        
#ifdef __VERBOSE
       write(nulprt,*)'inv2dcrn end'
       call flush(nulprt)
#endif
      end subroutine inv2dcrn
!rv,sgi>
#endif
