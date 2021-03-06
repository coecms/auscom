!============================================================================
  module cpl_interface
!============================================================================
! coupling interface between CICE and the oasis3_25 coupler (via MPI2) using 
! the PRISM System Model Interface (PSMILe).
!----------------------------------------------------------------------------

  !prism stuff
  use mod_kinds_model
  use mod_prism_proto
  use mod_prism_def_partition_proto
  use mod_prism_put_proto
  use mod_prism_get_proto

  !cice stuff
  use ice_kinds_mod
  use ice_communicate, only : my_task, master_task
  use ice_blocks,      only : nx_block, ny_block, nghost
  use ice_domain_size  
  use ice_gather_scatter
  use ice_constants
  use ice_domain,      only : distrb_info

  !cpl stuff
  use cpl_parameters
  use cpl_netcdf_setup
  use cpl_arrays_setup
  use cpl_forcing_handler

  implicit none

  public :: prism_init, init_cpl, coupler_termination, get_time0_sstsss, &
            from_atm, into_ocn, from_ocn, into_atm, il_commlocal

  private

  integer(kind=int_kind), dimension(jpfldout) :: il_var_id_out ! ID for fields sent 
  integer(kind=int_kind), dimension(jpfldin)  :: il_var_id_in  ! ID for fields rcvd

  character(len=6), parameter :: cp_modnam='cicexx' ! Component model name

  integer(kind=int_kind) :: il_commlocal  ! Component internal communicator 
  integer(kind=int_kind) :: ierror
  integer(kind=int_kind) :: il_comp_id    ! Component ID

  integer(kind=int_kind) :: il_bufsize
  real(kind=dbl_kind), dimension(:,:), allocatable :: rla_array
  real(kind=dbl_kind), dimension(:),   allocatable :: rla_bufsend

  contains

!======================================================================
  subroutine prism_init
!-----------------------!

  include 'mpif.h'

  integer(kind=int_kind) :: io_size, ii, integer_byte_size, integer_io_size
  integer(kind=int_kind) :: il_real, il_bufsizebyt

  logical :: mpiflag

  !-----------------------------------
  ! 'define' the model global domain: 
  !-----------------------------------
  nt_cells = nx_global * ny_global

  !allocate rla_array to be used below
  allocate (rla_array(nx_global,ny_global) )

  !-------------------
  ! Initialize PSMILe.
  !-------------------

  ! Initialise MPI
  mpiflag = .FALSE.
  call MPI_Initialized (mpiflag, ierror)

  if ( .not. mpiflag ) then
    call MPI_INIT(ierror)
  endif

  call MPI_Initialized (mpiflag, ierror)

  print *, 'CICE: (prism_init) calling prism_init_comp_proto...'

  call prism_init_comp_proto (il_comp_id, cp_modnam, ierror)

  if (ierror /= PRISM_Ok) then 
      call prism_abort_proto(il_comp_id, 'cice prism_init','STOP 1')
  else
      print *, 'CICE: (prism_init) called prism_init_comp_proto !'
  endif

  !B: the following part may not be really needed(?)
  !
  ! Let's suppose the model attaches to a MPI buffer for its own use
  !
  ! ! Sophisticated way to determine buffer size needed (without "kind")
  ! ! Here one message containing rla_array

  integer_byte_size = BIT_SIZE(ii)/8
  inquire (iolength=io_size) ii
  integer_io_size = io_size
  inquire (iolength=io_size) rla_array(1,1)
  il_real = io_size/integer_io_size*integer_byte_size
  il_bufsize = nt_cells + MPI_BSEND_OVERHEAD/il_real + 1
  allocate (rla_bufsend(il_bufsize), stat = ierror)
  il_bufsizebyt = il_bufsize * il_real
  call MPI_Buffer_Attach(rla_bufsend, il_bufsizebyt, ierror)

  if (ierror /= PRISM_Ok) then
      print *, 'CICE: (prism_init) Error in MPI_Buffer_Attach.'
      call prism_abort_proto(il_comp_id, 'cice prism_init','STOP 2')
  else
      print *, 'CICE: (prism_init) MPI_Buffer_Attach ok!'
  endif
  !
  ! PSMILe attribution of local communicator.
  ! 
  !   Either MPI_COMM_WORLD if MPI2 is used, 
  !   or a local communicator created by Oasis if MPI1 is used.
  !
  call prism_get_localcomm_proto(il_commlocal, ierror)
  !
  if (ierror /= PRISM_Ok) then
      print *, 'CICE: Error in prism_get_localcomm_proto'
      call prism_abort_proto(il_comp_id, 'cice prism_init','STOP 3')
  else
      print *, 'CICE: _get_localcomm_ OK! il_commlocal= ',il_commlocal
  endif

  print *, '* CICE: prism_init called OK!'  

  end subroutine prism_init

!=======================================================================
  subroutine init_cpl
!--------------------!

  character(len=8) :: chiceout
  character(len=2) :: chout

  integer(kind=int_kind) :: jf

  logical :: ll_comparal                   ! paralell or mono-cpl coupling
  integer(kind=int_kind) :: il_nbtotproc   ! Total number of processes
  integer(kind=int_kind) :: il_nbcplproc   ! Number of processes involved in the coupling
  integer(kind=int_kind), dimension(2) :: il_var_nodims ! see below
  integer(kind=int_kind), dimension(4) :: il_var_shape  ! see below
  integer(kind=int_kind) :: il_part_id     ! Local partition ID
  integer(kind=int_kind) :: il_length      ! Size of partial field for each process

!-------------------------------------------------------------------------
  
  !
  ! Inquire if model is parallel or not and open the process log file 
  !
  print *, '* CICE: Entering init_cpl.....'

  print *, '* CICE4 (init_cpl) calling MPI_Comm_Size ...'
  call MPI_Comm_Size(il_commlocal, il_nbtotproc, ierror)
  print *, '* CICE4 (init_cpl) calling MPI_Comm_Rank ...'
  call MPI_Comm_Rank(il_commlocal, my_task, ierror)

  print *, '* CICE4 (init_cpl) il_commlocal, il_nbtotproc, my_task = ',&
                             il_commlocal, il_nbtotproc, my_task

  !il_nbcplproc = il_nbtotproc   !multi-process coupling
  il_nbcplproc = 1               !mono process coupling
  ll_comparal = .FALSE.		 !hard-wired for mono-cpl coupling!

  ! Open the process log file
  !if (my_task == 0 .or. ll_comparal) then
    il_out = 85 + my_task
    write(chout,'(I2)')il_out
    chiceout='iceout'//chout
    open(il_out,file=chiceout,form='formatted')
  
    write(il_out,*) 'Number of processes:', il_nbtotproc
    write(il_out,*) 'Local process number:', my_task
    write(il_out,*) 'Local communicator is : ',il_commlocal
    write(il_out,*) 'Grid layout: nx_global,ny_global= ',nx_global,ny_global
    write(il_out,*) 'Grid decomposition: nx_block,ny_block,max_blocks= ',&
                     nx_block,ny_block,max_blocks
  !endif

  if (my_task == 0 ) then
    !
    ! The following steps need to be done:
    ! -> by the process if cice is monoprocess;
    ! -> only by the master process, if cice is parallel and only 
    !    master process is involved in the coupling;
    ! -> by all processes, if cice is parallel and all processes 
    ! are involved in the coupling.
    
    call decomp_def (il_part_id, il_length, nt_cells, &
         my_task, il_nbcplproc, ll_comparal, il_out)

    write(il_out,*)'(init_cpl) called decomp_def, my_task, ierror = ',my_task, ierror

    !
    ! PSMILe coupling fields declaration
    !

    il_var_nodims(1) = 2 ! rank of coupling field
    il_var_nodims(2) = 1 ! number of bundles in coupling field (always 1)
    il_var_shape(1)= 1     ! min index for the coupling field local dim
    il_var_shape(2)= nx_global ! max index for the coupling field local dim  
    il_var_shape(3)= 1   
    il_var_shape(4)= ny_global 
 
    ! ?Does this help?
    !il_var_shape(1)= 2       ! min index for the coupling field local dim
    !il_var_shape(2)= nx_global+1 ! max index for the coupling field local dim
    !il_var_shape(3)= 2  
    !il_var_shape(4)= ny_global+1

    !
    ! Define name (as in namcouple) and declare each field sent by ice 
    !

    !ice ==> atm
    cl_writ(1)='isst_ia'
    !ice ==> ocn
    cl_writ(n_i2a+1 )='strsu_io'
    cl_writ(n_i2a+2 )='strsv_io'
    cl_writ(n_i2a+3 )='rain_io'
    cl_writ(n_i2a+4 )='snow_io'
    cl_writ(n_i2a+5 )='stflx_io'
    cl_writ(n_i2a+6 )='htflx_io'
    cl_writ(n_i2a+7 )='swflx_io'
    cl_writ(n_i2a+8 )='qflux_io'
    cl_writ(n_i2a+9 )='shflx_io'
    cl_writ(n_i2a+10)='lwflx_io'
    cl_writ(n_i2a+11)='runof_io'
    cl_writ(n_i2a+12)='press_io'
    cl_writ(n_i2a+13)='aice_io'
    do jf=1, jpfldout
      call prism_def_var_proto (il_var_id_out(jf),cl_writ(jf), il_part_id, &
         il_var_nodims, PRISM_Out, il_var_shape, PRISM_Real, ierror)
    enddo 
    !
    ! Define name (as in namcouple) and declare each field received by ice
    !

    !atm ==> ice
    cl_read(1) ='swfld_i'
    cl_read(2) ='lwfld_i'
    cl_read(3) ='rain_i'
    cl_read(4) ='snow_i'
    cl_read(5) ='press_i'
    cl_read(6) ='runof_i'
    cl_read(7) ='tair_i'
    cl_read(8) ='qair_i'
    cl_read(9) ='uwnd_i'
    cl_read(10)='vwnd_i'
    !ocn ==> ice
    cl_read(n_a2i+1)='sst_i'
    cl_read(n_a2i+2)='sss_i'
    cl_read(n_a2i+3)='ssu_i'
    cl_read(n_a2i+4)='ssv_i'
    cl_read(n_a2i+5)='sslx_i'
    cl_read(n_a2i+6)='ssly_i'
    cl_read(n_a2i+7)='pfmice_i'
    !
    do jf=1, jpfldin
      call prism_def_var_proto (il_var_id_in(jf), cl_read(jf), il_part_id, &
         il_var_nodims, PRISM_In, il_var_shape, PRISM_Real, ierror)
    enddo 
    !
    ! 7- PSMILe end of declaration phase 
    !
    call prism_enddef_proto (ierror)

  endif

  !
  ! Allocate the 'coupling' fields (to be used) for EACH PROCESS:! 
  !

  ! fields in: (local domain)
  allocate ( tair0(nx_block,ny_block,max_blocks));  tair0(:,:,:) = 0
  allocate (swflx0(nx_block,ny_block,max_blocks)); swflx0(:,:,:) = 0
  allocate (lwflx0(nx_block,ny_block,max_blocks)); lwflx0(:,:,:) = 0
  allocate ( uwnd0(nx_block,ny_block,max_blocks));  uwnd0(:,:,:) = 0
  allocate ( vwnd0(nx_block,ny_block,max_blocks));  vwnd0(:,:,:) = 0
  allocate ( qair0(nx_block,ny_block,max_blocks));  qair0(:,:,:) = 0
  allocate ( rain0(nx_block,ny_block,max_blocks));  rain0(:,:,:) = 0
  allocate ( snow0(nx_block,ny_block,max_blocks));  snow0(:,:,:) = 0
  allocate ( runof0(nx_block,ny_block,max_blocks)); runof0(:,:,:) = 0
  allocate ( press0(nx_block,ny_block,max_blocks)); press0(:,:,:) = 0

  allocate ( runof(nx_block,ny_block,max_blocks)); runof(:,:,:) = 0
  allocate ( press(nx_block,ny_block,max_blocks)); press(:,:,:) = 0

  !
  allocate ( core_runoff(nx_block,ny_block,max_blocks));  core_runoff(:,:,:) = 0.
  !

  allocate (ssto(nx_block,ny_block,max_blocks));  ssto(:,:,:) = 0
  allocate (ssso(nx_block,ny_block,max_blocks));  ssso(:,:,:) = 0
  allocate (ssuo(nx_block,ny_block,max_blocks));  ssuo(:,:,:) = 0
  allocate (ssvo(nx_block,ny_block,max_blocks));  ssvo(:,:,:) = 0
  allocate (sslx(nx_block,ny_block,max_blocks));  sslx(:,:,:) = 0
  allocate (ssly(nx_block,ny_block,max_blocks));  ssly(:,:,:) = 0
  allocate (pfmice(nx_block,ny_block,max_blocks));  pfmice(:,:,:) = 0

  ! fields out: (local domain)
  allocate (  isst(nx_block,ny_block,max_blocks));   isst(:,:,:) = 0
  !
  allocate (iostrsu(nx_block,ny_block,max_blocks)); iostrsu(:,:,:) = 0
  allocate (iostrsv(nx_block,ny_block,max_blocks)); iostrsv(:,:,:) = 0
  allocate (iorain(nx_block,ny_block,max_blocks));  iorain(:,:,:) = 0
  allocate (iosnow(nx_block,ny_block,max_blocks));  iosnow(:,:,:) = 0
  allocate (iostflx(nx_block,ny_block,max_blocks)); iostflx(:,:,:) = 0
  allocate (iohtflx(nx_block,ny_block,max_blocks)); iohtflx(:,:,:) = 0
  allocate (ioswflx(nx_block,ny_block,max_blocks)); ioswflx(:,:,:) = 0
  allocate (ioqflux(nx_block,ny_block,max_blocks)); ioqflux(:,:,:) = 0
  allocate (iolwflx(nx_block,ny_block,max_blocks)); iolwflx(:,:,:) = 0
  allocate (ioshflx(nx_block,ny_block,max_blocks)); ioshflx(:,:,:) = 0
  allocate (iorunof(nx_block,ny_block,max_blocks)); iorunof(:,:,:) = 0
  allocate (iopress(nx_block,ny_block,max_blocks)); iopress(:,:,:) = 0
  allocate (ioaice (nx_block,ny_block,max_blocks)); ioaice(:,:,:) = 0

  allocate (tiostrsu(nx_block,ny_block,max_blocks)); tiostrsu(:,:,:) = 0
  allocate (tiostrsv(nx_block,ny_block,max_blocks)); tiostrsv(:,:,:) = 0
  allocate (tiorain(nx_block,ny_block,max_blocks));  tiorain(:,:,:) = 0
  allocate (tiosnow(nx_block,ny_block,max_blocks));  tiosnow(:,:,:) = 0
  allocate (tiostflx(nx_block,ny_block,max_blocks)); tiostflx(:,:,:) = 0
  allocate (tiohtflx(nx_block,ny_block,max_blocks)); tiohtflx(:,:,:) = 0
  allocate (tioswflx(nx_block,ny_block,max_blocks)); tioswflx(:,:,:) = 0
  allocate (tioqflux(nx_block,ny_block,max_blocks)); tioqflux(:,:,:) = 0
  allocate (tiolwflx(nx_block,ny_block,max_blocks)); tiolwflx(:,:,:) = 0
  allocate (tioshflx(nx_block,ny_block,max_blocks)); tioshflx(:,:,:) = 0
  allocate (tiorunof(nx_block,ny_block,max_blocks)); tiorunof(:,:,:) = 0
  allocate (tiopress(nx_block,ny_block,max_blocks)); tiopress(:,:,:) = 0
  allocate (tioaice(nx_block,ny_block,max_blocks));  tioaice(:,:,:) = 0

  allocate (vwork(nx_block,ny_block,max_blocks)); vwork(:,:,:) = 0
  allocate (gwork(nx_global,ny_global)); gwork(:,:) = 0

  !
  allocate (sicemass(nx_block,ny_block,max_blocks)); sicemass(:,:,:) = 0.
  allocate (u_star0(nx_block,ny_block,max_blocks)); u_star0(:,:,:) = 0.
  allocate (rough_mom0(nx_block,ny_block,max_blocks)); rough_mom0(:,:,:) = 0.
  allocate (rough_heat0(nx_block,ny_block,max_blocks)); rough_heat0(:,:,:) = 0.
  allocate (rough_moist0(nx_block,ny_block,max_blocks)); rough_moist0(:,:,:) = 0.
  !

  end subroutine init_cpl

!=======================================================================
  subroutine from_atm(isteps)
!-------------------------------------------!

  implicit none

  integer(kind=int_kind), intent(in) :: isteps

  integer(kind=int_kind) :: jf

  if (my_task == 0) then
    write(il_out,*) '(from_atm) receiving coupling fields at rtime= ', isteps
  endif

  do jf = 1, n_a2i       !10, not jpfldin, only 10 fields from cpl (atm) 

    if (my_task==0) then

      !jf-th field in
      write(il_out,*) '*** receiving coupling field No. ', jf, cl_read(jf)

      call prism_get_proto (il_var_id_in(jf), isteps, gwork, ierror)

      if ( ierror /= PRISM_Ok .and. ierror < PRISM_Recvd) then
        write(il_out,*) 'Err in _get_ sst at time with error: ', isteps, ierror
        call prism_abort_proto(il_comp_id, 'cice from_atm','stop 1') 
      else 
        write(il_out,*)'(from_atm) rcvd at time with err: ',cl_read(jf),isteps,ierror
      endif

    endif

    if (jf ==  1) call scatter_global(swflx0,gwork,master_task,distrb_info, &
                                      field_loc_center, field_type_scalar)
    if (jf ==  2) call scatter_global(lwflx0,gwork,master_task,distrb_info, &
                                      field_loc_center, field_type_scalar)
    if (jf ==  3) call scatter_global(rain0, gwork,master_task,distrb_info, &
                                      field_loc_center, field_type_scalar)
    if (jf ==  4) call scatter_global(snow0, gwork,master_task,distrb_info, &
                                      field_loc_center, field_type_scalar)
    if (jf ==  5) call scatter_global(press0,gwork,master_task,distrb_info, &
                                      field_loc_center, field_type_scalar)
    if (jf ==  6) call scatter_global(runof0,gwork,master_task,distrb_info, &
                                      field_loc_center, field_type_scalar)
    if (jf ==  7) call scatter_global(tair0, gwork,master_task,distrb_info, &
                                      field_loc_center, field_type_scalar)
    if (jf ==  8) call scatter_global(qair0, gwork,master_task,distrb_info, &
                                      field_loc_center, field_type_scalar)
    if (jf ==  9) call scatter_global(uwnd0, gwork,master_task,distrb_info, &
!                                      field_loc_center, field_type_scalar)
                                      field_loc_center, field_type_vector)  !20090723
    if (jf == 10) call scatter_global(vwnd0, gwork,master_task,distrb_info, &
!                                      field_loc_center, field_type_scalar)
                                      field_loc_center, field_type_vector)  !
  enddo

  ! need do t-grid to u-grid shift for vectors since all coupling occur on
  ! t-grid points: <==No! actually CICE requires the input wind on T grid! 
  ! (see comment in code ice_flux.F)
  !call t2ugrid(uwnd1)
  !call t2ugrid(vwnd1)
  ! ...and, as we use direct o-i communication and o-i share the same grid, 
  ! no need for any t2u and/or u2t shift before/after i-o coupling!

  if ( chk_a2i_fields ) then
    call check_a2i_fields('fields_a2i_in_ice.nc',isteps)
  endif

  end subroutine from_atm

!=======================================================================
  subroutine from_ocn(isteps)
!-------------------------------------------!  

  integer(kind=int_kind), intent(in) :: isteps
 
  integer(kind=int_kind) :: jf
  
  if (my_task==0) then
    write(il_out,*) '(from_ocn) receiving coupling fields at rtime: ', isteps
  endif

  do jf = n_a2i+1, jpfldin          !no 11-17 from ocn
  
    if (my_task==0) then

      !jf-th field in
      write(il_out,*) '*** receiving coupling fields No. ', jf, cl_read(jf)

      call prism_get_proto (il_var_id_in(jf), isteps, gwork, ierror)

      if ( ierror /= PRISM_Ok .and. ierror < PRISM_Recvd) then
        write(il_out,*) 'Err in _get_ sst at time with error: ', isteps, ierror
        call prism_abort_proto(il_comp_id, 'cice from_atm','stop 1')
      else
        write(il_out,*)'(from_atm) rcvd at time with err: ',cl_read(jf),isteps,ierror
      endif

    endif

    if (jf == n_a2i+1) call scatter_global(ssto,  gwork, master_task, distrb_info, &
                                      field_loc_center, field_type_scalar)
    if (jf == n_a2i+2) call scatter_global(ssso,  gwork, master_task, distrb_info, &
                                      field_loc_center, field_type_scalar)
    if (jf == n_a2i+3) call scatter_global(ssuo,  gwork, master_task, distrb_info, &
!                                      field_loc_center, field_type_scalar)
                                      field_loc_center, field_type_vector)   !20090723	 
    if (jf == n_a2i+4) call scatter_global(ssvo,  gwork, master_task, distrb_info, &
!                                      field_loc_center, field_type_scalar)  
                                      field_loc_center, field_type_vector)   !
    if (jf == n_a2i+5) call scatter_global(sslx,  gwork, master_task, distrb_info, &
!                                      field_loc_center, field_type_scalar)
                                      field_loc_center, field_type_vector)   ! 
    if (jf == n_a2i+6) call scatter_global(ssly,  gwork, master_task, distrb_info, &
!                                      field_loc_center, field_type_scalar)
                                      field_loc_center, field_type_vector)   !
    if (jf == n_a2i+7) call scatter_global(pfmice,gwork, master_task, distrb_info, &
                                      field_loc_center, field_type_scalar)

  enddo

  if (chk_o2i_fields) then
    call check_o2i_fields('fields_o2i_in_ice.nc',isteps)
  endif

  end subroutine from_ocn

!=======================================================================
  subroutine into_ocn(isteps, scale)
!
! Note dummy 'scale', if /= 1 (then must be 1/coef_ic), is used here for the very 
! first-time-step-of-exp i2o fluxes scaling-up, because routine 'tavg_i2o_fluxes' 
! has scaled-down the current step i2o fluxes (calculated in get_i2o_fluxes) by 
! * coef_ic.  
! 
  integer(kind=int_kind), intent(in) :: isteps
  real, intent(in) :: scale             !only 1 or 1/coef_ic allowed! 
  integer(kind=int_kind) :: jf

  if (my_task == 0) then
    write(il_out,*) '(into_ocn) sending coupling fields at stime= ', isteps
  endif

  do jf = n_i2a+1, jpfldout       !no 2-14 are for the ocn

    if (jf == n_i2a+1 ) vwork = scale * iostrsu
    if (jf == n_i2a+2 ) vwork = scale * iostrsv
    if (jf == n_i2a+3 ) vwork = scale * iorain
    if (jf == n_i2a+4 ) vwork = scale * iosnow
    if (jf == n_i2a+5 ) vwork = scale * iostflx
    if (jf == n_i2a+6 ) vwork = scale * iohtflx
    if (jf == n_i2a+7 ) vwork = scale * ioswflx
    if (jf == n_i2a+8 ) vwork = scale * ioqflux
    if (jf == n_i2a+9 ) vwork = scale * ioshflx
    if (jf == n_i2a+10 ) vwork = scale * iolwflx
    if (jf == n_i2a+11) then 
       if (use_core_runoff) then 
         vwork = core_runoff
       else 
         vwork = scale * iorunof
       endif 
    endif 
    if (jf == n_i2a+12) vwork = scale * iopress
    if (jf == n_i2a+13) vwork = scale * ioaice

    call gather_global(gwork, vwork, master_task, distrb_info)

    if (my_task == 0 ) then   

      write(il_out,*) '*** sending coupling field No. ', jf, cl_writ(jf)

      call prism_put_proto(il_var_id_out(jf), isteps, gwork, ierror)
      if ( ierror /= PRISM_Ok .and. ierror < PRISM_Sent) then
        write(il_out,*) '(into_ocn) Err in _put_ ', cl_writ(jf), isteps, ierror
        call prism_abort_proto(il_comp_id, 'cice into_ocn','STOP 1') 
      else
        write(il_out,*)'(into_ocn) sent: ', cl_writ(jf), isteps, ierror
      endif

    endif !my_task == 0

  enddo     !jf = 6, jpfldout

  if (chk_i2o_fields) then
    call check_i2o_fields('fields_i2o_in_ice.nc',isteps, scale)
  endif

  end subroutine into_ocn

!=======================================================================
  subroutine into_atm(isteps)
!-------------------------------------------!    

  integer(kind=int_kind), intent(in) :: isteps
  integer(kind=int_kind) :: jf

  if (my_task == 0) then
    write(il_out,*) '(into_atm) sending coupling fields at stime= ', isteps
  endif

  do jf = 1, n_i2a      !1
    
    if (jf ==  1) call gather_global(gwork,   isst, master_task, distrb_info)
    
    if (my_task == 0 ) then
  
      write(il_out,*) '*** sending coupling field No. ', jf, cl_writ(jf)

      call prism_put_inquire_proto(il_var_id_out(jf),isteps,ierror)
  
      write(il_out,*) '(into_atm) what to do with this var==> Err= ',ierror
  
      call prism_put_proto(il_var_id_out(jf), isteps, gwork, ierror)
  
      if ( ierror /= PRISM_Ok .and. ierror < PRISM_Sent) then
        write(il_out,*) '(into_atm) Err in _put_ ', cl_writ(jf), isteps, ierror
        call prism_abort_proto(il_comp_id, 'cice into_atm','STOP 1')
      else
        write(il_out,*)'(into_atm) sent: ', cl_writ(jf), isteps, ierror
      endif

    endif 

  enddo

  if (chk_i2a_fields) then
    call check_i2a_fields('fields_i2a_in_ice.nc',isteps)
  endif 

  end subroutine into_atm

!=======================================================================
  subroutine coupler_termination
!-------------------------------!
  !
  ! Detach from MPI buffer
  !
  call MPI_Buffer_Detach(rla_bufsend, il_bufsize, ierror) 
  write(il_out,*) 'il_bufsize =', il_bufsize
  deallocate (rla_bufsend)

  deallocate (tair0, swflx0, lwflx0, uwnd0, vwnd0, qair0, rain0, snow0, runof0, press0)
  deallocate (runof, press)
  deallocate (core_runoff)
  deallocate (ssto, ssso, ssuo, ssvo, sslx, ssly, pfmice)
  deallocate (isst)
  deallocate (iostrsu, iostrsv, iorain, iosnow, iostflx, iohtflx, ioswflx, &
              ioqflux, iolwflx, ioshflx, iorunof, iopress)
  deallocate (tiostrsu, tiostrsv, tiorain, tiosnow, tiostflx, tiohtflx, tioswflx, &
              tioqflux, tiolwflx, tioshflx, tiorunof, tiopress) 
  deallocate (gwork, vwork, sicemass)
  !  
  ! PSMILe termination 
  !   
  call prism_terminate_proto (ierror)
  if (ierror /= PRISM_Ok) then
    if (my_task == 0) then
      write (il_out,*) 'An error occured in prism_terminate = ', ierror
    endif
  else 
    if (my_task == 0) then
      write(il_out,*) '(main) calling prism_terminate_proto done!'
      write(il_out,*) '==================*** END ***================='
    endif
  endif
  ! 
  print *
  print *, '********** End of CICE **********'
  print *

  close(il_out)
  call MPI_Finalize (ierror)

  end subroutine coupler_termination

!=======================================================================
  subroutine decomp_def(id_part_id, id_length, id_imjm, &
   id_rank, id_nbcplproc, ld_comparal, ld_mparout)
!-------------------------------------------------------!
  !
  !use mod_prism_proto
  !use mod_prism_def_partition_proto

  implicit none

  integer(kind=int_kind), dimension(:), allocatable :: il_paral ! Decomposition for each proc
  integer(kind=int_kind) :: ig_nsegments  ! Number of segments of process decomposition 
  integer(kind=int_kind) :: ig_parsize    ! Size of array decomposition
  integer(kind=int_kind) :: id_nbcplproc  ! Number of processes involved in the coupling
  integer(kind=int_kind) :: id_part_id    ! Local partition ID
  integer(kind=int_kind) :: id_imjm       ! Total grid dimension, ib, ierror, my_task
  integer(kind=int_kind) :: id_length     ! Size of partial field for each process
  integer(kind=int_kind) :: id_rank       ! Rank of process
  integer(kind=int_kind) :: ld_mparout    ! Unit of log file
  logical :: ld_comparal
  integer(kind=int_kind) :: ib, ierror
  character(len=80), parameter :: cdec='BOX'
  !
  integer(kind=int_kind) :: ilo, ihi, jlo, jhi
  !
  !
  ! Refer to oasis/psmile/prism/modules/mod_prism_proto.F90 for integer(kind=int_kind) value
  ! of clim_xxxx parameters
  !
  if ( .not. ld_comparal .and. id_rank == 0) then
      ! Monoprocess model, or parallel model with only master process involved 
      ! in coupling: the entire field will be exchanged by the process. 
      ig_nsegments = 1
      ig_parsize = 3
      allocate(il_paral(ig_parsize))
      !
      il_paral ( clim_strategy ) = clim_serial
      il_paral ( clim_offset   ) = 0
      il_paral ( clim_length   ) = id_imjm
      id_length = id_imjm
      !
      call prism_def_partition_proto (id_part_id, il_paral, ierror)
      deallocate(il_paral)
      !
  else
      ! Parallel atm with all process involved in the coupling
      !
      if (cdec == 'APPLE') then
          ! Each process is responsible for a part of field defined by
          ! the number of grid points and the offset of the first point
          !
          write (ld_mparout,*) 'APPLE partitioning'
          ig_nsegments = 1
          ig_parsize = 3
          allocate(il_paral(ig_parsize))
          write(ld_mparout,*)'ig_parsize',ig_parsize
          !
          if (id_rank .LT. (id_nbcplproc-1)) then
              il_paral ( clim_strategy ) = clim_apple
              il_paral ( clim_length   ) = id_imjm/id_nbcplproc
              il_paral ( clim_offset   ) = id_rank*(id_imjm/id_nbcplproc)
          else
              il_paral ( clim_strategy ) = clim_apple
              il_paral ( clim_length   ) = id_imjm-(id_rank*(id_imjm/id_nbcplproc))
              il_paral ( clim_offset   ) = id_rank*(id_imjm/id_nbcplproc)
          endif
          id_length = il_paral(clim_length) 
          !
          call prism_def_partition_proto (id_part_id, il_paral, ierror)
          deallocate(il_paral)
          !
      else if (cdec == 'BOX') then
          !B: CICE uses a kind of Cartisian decomposition which actually may NOT
          !   be simply taken as "BOX" decomposition described here !!!
          !   (there is an issue associated with the 'halo' boundary for each
          !    segment and may NOT be treated as what we do below! 
          !    It needs further consideration to make this work correctly 
          !    for 'paralell coupling' if really needed in the future ...)
          !  
          ! Each process is responsible for a rectangular box 
          !
          write (ld_mparout,*) 'BOX partitioning'
          ig_parsize = 5
          allocate(il_paral(ig_parsize))
          write(ld_mparout,*)'ig_parsize',ig_parsize
          
          ilo = 1 + nghost
          ihi = nx_block - nghost
          jlo = 1 + nghost
          jhi = ny_block - nghost
          
          il_paral ( clim_strategy ) = clim_Box
          il_paral ( clim_offset   ) = nx_global * (jlo-1) + (ilo-1)
          il_paral ( clim_SizeX    ) = ihi-ilo+1
          il_paral ( clim_SizeY    ) = jhi-jlo+1
          il_paral ( clim_LdX      ) = nx_global
          
          id_length = il_paral(clim_sizeX) * il_paral(clim_sizeY)
          
          call prism_def_partition_proto (id_part_id, il_paral, ierror)
          deallocate(il_paral)
          !
      else if (cdec == 'ORANGE') then
          !B: NOT FOR COMMON USE!
          ! Each process is responsible for arbitrarily distributed
          ! pieces of the field (here two segments by process)
          !
          write (ld_mparout,*) 'ORANGE partitioning'
          ig_nsegments = 2
          ig_parsize = 2 * ig_nsegments + 2
          write(ld_mparout,*)'ig_parsize',ig_parsize
          allocate(il_paral(ig_parsize))
          !
          il_paral ( clim_strategy   ) = clim_orange
          il_paral ( clim_segments   ) = 2
          il_paral ( clim_segments+1 ) = id_rank*768
          il_paral ( clim_segments+2 ) = 768
          il_paral ( clim_segments+3 ) = (id_rank+3)*768
          il_paral ( clim_segments+4 ) = 768
          id_length = 0
          do ib=1,2*il_paral(clim_segments) 
            if (mod(ib,2).eq.0) then
                id_length = id_length + il_paral(clim_segments+ib)
            endif
          enddo
          !
          call prism_def_partition_proto (id_part_id, il_paral, ierror)
          deallocate(il_paral)
          !
      else
          write (ld_mparout,*) 'incorrect decomposition '
      endif
  endif

  end subroutine decomp_def

!============================================================================

  end module cpl_interface
