PROGRAM oce
!======================================================================
! This program simulates the oceanic part of a coupled model using
! the PRISM System Model Interface (PSMILe) to the Oasis CLIM communication 
! library for field exchange. The field dimensions are realistic, 
! corresponding to a resolution of 182*152 grid points, but beside the 
! exchanges the model is pratically "empty" (no real physics nor dynamics).
!======================================================================
  USE mod_kinds_model
  USE mod_prism_proto
  USE mod_prism_def_partition_proto
  USE mod_prism_put_proto
  USE mod_prism_get_proto
  USE mod_prism_grids_writing
  IMPLICIT NONE
#ifndef key_nonetCDF
#include <netcdf.inc>
#endif
  !
  ! Grid dimensions 
  INTEGER, PARAMETER :: il_im = 182, il_jm = 152, il_imjm = il_im*il_jm 
  !
  INTEGER, PARAMETER :: jpfldout = 2    ! Number of fields sent
  INTEGER, PARAMETER :: jpfldin = 7     ! Number of fields received 
  !
  CHARACTER(len=8), DIMENSION(jpfldout) :: cl_writ ! Symb names fields sent
  CHARACTER(len=8), DIMENSION(jpfldin) :: cl_read ! Symb names fields received
  !
  INTEGER, dimension(jpfldout) :: il_var_id_out ! ID for fields sent 
  INTEGER, dimension(jpfldin)  :: il_var_id_in ! ID for fields received
  !
  ! Fields received
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: zqsr, znsol, zemp  
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: zrunoff, ztaux, ztauy
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: ssalb 
  !
  ! Fields sent
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: sstoc, sieoc  
  !
  CHARACTER(len=6), PARAMETER :: cp_modnam='toyoce' ! Component model name
  !
  INTEGER, PARAMETER :: npas = 36 ! Number of steps for the simulation. 
  ! We hard code this numbers but later specific PSMILe routines will be 
  ! developed to access the relevant information in the SCC file.
  !
  INTEGER, PARAMETER :: il_print = 6 ! Printing interval  
  INTEGER, PARAMETER :: itimestep = 14400 ! Timestep in seconds
  !
  LOGICAL :: ll_comparal ! Logical true if component is parallel 
                         ! and if all process communicates with Oasis.
  !
  INTEGER  :: itap, itap_sec, ji, jf, ierror
  INTEGER  :: il_rank
  INTEGER  :: il_mparout
  CHARACTER(len=8) :: choceout
  CHARACTER(len=2) :: chout
  INTEGER :: il_comp_id     ! Component ID
  INTEGER :: il_commlocal   ! Component internal communicator 
  INTEGER :: il_nbtotproc   ! Total number of processes
  INTEGER :: il_nbcplproc   ! Number of processes involved in the coupling
  INTEGER :: il_part_id     ! Local partition ID
  INTEGER, DIMENSION(2) :: il_var_nodims(2), il_var_shape(2) ! see below
  !
  INTEGER, dimension(3) :: il_paral ! Definition of monoprocess partition
  !
  INTEGER :: il_flag        ! Flag for grid writing
  INTEGER :: il_status, il_fileid, il_varid
  INTEGER, DIMENSION(2) :: ist, icnt
  REAL(kind=ip_double_p),DIMENSION(il_im,il_jm) :: dla_lon, dla_lat, dla_srf
  REAL(kind=ip_realwp_p),DIMENSION(il_im,il_jm) :: rla_lon, rla_lat, rla_srf
  INTEGER,DIMENSION(il_im,il_jm) :: ila_msk 
!
!======================================================================
  !
  ! 1- PSMILe initialization
  !
  CALL prism_init_comp_proto (il_comp_id, cp_modnam, ierror)
  !
  IF (ierror .NE. PRISM_Ok) THEN
      WRITE (*,*) ' oce : pb prism_init_comp_proto'
      CALL prism_abort_proto(il_comp_id, 'oce.F90','abort1')
  ELSE
      WRITE(*,*) 'oce : prism_init_comp_proto ok '
  ENDIF
  !
  ! 2- PSMILe attribution of local communicator.
  ! 
  !   Either MPI_COMM_WORLD if MPI2 is used, 
  !   Either a local communicator created by Oasis if MPI1 is used.
  !
  CALL prism_get_localcomm_proto(il_commlocal, ierror)
  !
  IF (ierror .NE. PRISM_Ok) THEN
      WRITE (*,*) ' oce : pb prism_init_comp_proto'
      CALL prism_abort_proto(il_comp_id, 'oce.F90','abort2')
  ELSE
      WRITE(*,*) 'oce : prism_init_comp_proto ok '
  ENDIF
  !
  ! 3- Open the process log file 
  !
  CALL MPI_Comm_Size(il_commlocal, il_nbtotproc, ierror)
  CALL MPI_Comm_Rank(il_commlocal, il_rank, ierror)
  !
  il_mparout = 85 + il_rank
  WRITE(chout,'(I2)')il_mparout
  choceout='oceout'//chout
  !
  OPEN(il_mparout,file=choceout,form='formatted')
  !
  WRITE(il_mparout,*) 'Oce: Number of processes:', il_nbtotproc
  WRITE(il_mparout,*) 'Local process number:', il_rank
  WRITE(il_mparout,*) 'Local communicator is : ',il_commlocal
  !
  ! Define the grids by master proc
  !
  IF (il_rank.EQ.0) THEN
      CALL prism_start_grids_writing(il_flag)
#ifndef key_nonetCDF
      IF (il_flag .EQ. 1) THEN
          !
          ! read and write the grid
          ist(1)=1 ; ist(2)=1
          icnt(1)=il_im ; icnt(2)=il_jm
          il_status=NF_OPEN('grids_in.nc', NF_NOWRITE, il_fileid)
          il_status=NF_INQ_VARID(il_fileid, 'topa.lon' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_lon(:,:))
          il_status=NF_INQ_VARID(il_fileid, 'topa.lat' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_lat(:,:))
          rla_lon(:,:) = dla_lon(:,:)
          rla_lat(:,:) = dla_lat(:,:)
          call prism_write_grid('topa', il_im, il_jm, rla_lon, rla_lat)
          !
          il_status=NF_INQ_VARID(il_fileid, 'uopa.lon' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_lon(:,:))
          il_status=NF_INQ_VARID(il_fileid, 'uopa.lat' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_lat(:,:))
          rla_lon(:,:) = dla_lon(:,:)
          rla_lat(:,:) = dla_lat(:,:)
          call prism_write_grid('uopa', il_im, il_jm, rla_lon, rla_lat)
          !
          il_status=NF_INQ_VARID(il_fileid, 'vopa.lon' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_lon(:,:))
          il_status=NF_INQ_VARID(il_fileid, 'vopa.lat' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_lat(:,:))
          rla_lon(:,:) = dla_lon(:,:)
          rla_lat(:,:) = dla_lat(:,:)
          call prism_write_grid('vopa', il_im, il_jm, rla_lon, rla_lat)
          il_status=NF_CLOSE(il_fileid)
          !
          ! read and write the masks
          il_status=NF_OPEN('masks_in.nc', NF_NOWRITE, il_fileid)
          il_status=NF_INQ_VARID(il_fileid, 'topa.msk' , il_varid)
          il_status=NF_GET_VARA_INT (il_fileid, il_varid, ist, icnt, & 
             ila_msk(:,:))
          call prism_write_mask('topa', il_im, il_jm, ila_msk)
          il_status=NF_INQ_VARID(il_fileid, 'uopa.msk' , il_varid)
          il_status=NF_GET_VARA_INT (il_fileid, il_varid, ist, icnt, & 
             ila_msk(:,:))
          call prism_write_mask('uopa', il_im, il_jm, ila_msk) 
          il_status=NF_INQ_VARID(il_fileid, 'vopa.msk' , il_varid)
          il_status=NF_GET_VARA_INT (il_fileid, il_varid, ist, icnt, & 
             ila_msk(:,:))
          call prism_write_mask('vopa', il_im, il_jm, ila_msk)
          il_status=NF_CLOSE(il_fileid)
          !          
          ! read and write the areas
          il_status=NF_OPEN('areas_in.nc', NF_NOWRITE, il_fileid)
          il_status=NF_INQ_VARID(il_fileid, 'topa.srf' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_srf(:,:))
          rla_srf(:,:) = dla_srf(:,:)
          call prism_write_area('topa', il_im, il_jm, rla_srf)
          il_status=NF_INQ_VARID(il_fileid, 'uopa.srf' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_srf(:,:))
          rla_srf(:,:) = dla_srf(:,:)
          call prism_write_area('uopa', il_im, il_jm, rla_srf)
          il_status=NF_INQ_VARID(il_fileid, 'vopa.srf' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_srf(:,:))
          rla_srf(:,:) = dla_srf(:,:)
          call prism_write_area('vopa', il_im, il_jm, rla_srf)
          il_status=NF_CLOSE(il_fileid)
          !
          call prism_terminate_grids_writing()
      ENDIF
#endif
  ENDIF
  !
  ! 4- Define monoprocess partition (the whole field) 
  !
  ! Refer to oasis/psmile/prism/modules/mod_prism_proto.F90 for integer value
  ! of clim_xxxx parameters
  !
  il_paral ( clim_strategy ) = clim_serial
  il_paral ( clim_offset   ) = 0
  il_paral ( clim_length   ) = il_imjm
  !
  CALL prism_def_partition_proto (il_part_id, il_paral, ierror)
  !
  ALLOCATE(zqsr(il_imjm))   ; zqsr(:) = 0
  ALLOCATE(znsol(il_imjm))  ; znsol(:) = 0
  ALLOCATE(zemp(il_imjm))   ; zemp(:) = 0
  ALLOCATE(zrunoff(il_imjm)); zrunoff(:) = 0
  ALLOCATE(ztaux(il_imjm))  ; ztaux(:) = 0
  ALLOCATE(ztauy(il_imjm))  ; ztauy(:) = 0
  ALLOCATE(ssalb(il_imjm))  ; ssalb(:) = 0
  ALLOCATE(sstoc(il_imjm))  ; sstoc(:) = 0
  ALLOCATE(sieoc(il_imjm))  ; sieoc(:) = 0
  !
  ! 5- PSMILe coupling fields declaration
  !
  il_var_nodims(1) = 1 ! rank of coupling field
  il_var_nodims(2) = 1 ! number of bundles in coupling field (always 1)
  il_var_shape(1)= 1   ! min index for the coupling field local dimension
  il_var_shape(2)= il_imjm ! max index for the coupling field local dim
  !
  ! Define name (as in namcouple) and declare each field sent by oce
  !
  cl_writ(1)='SOSSTSST'
  cl_writ(2)='SOICECOV'
  !
  DO jf=1, jpfldout
    CALL prism_def_var_proto (il_var_id_out(jf),cl_writ(jf), il_part_id, &
       il_var_nodims, PRISM_Out, il_var_shape, PRISM_Real, ierror)
  END DO
  !
  ! Define name (as in namcouple) and declare each field received by oce
  !
  cl_read(1)='SONSHLDO'    
  cl_read(2)='SOSHFLDO'    
  cl_read(3)='SOWAFLDO'    
  cl_read(4)='SORUNOFF'    
  cl_read(5)='SOZOTAUX'    
  cl_read(6)='SOMETAUY'    
  cl_read(7)='SOALBEDO'
  !
  DO jf=1, jpfldin
    CALL prism_def_var_proto (il_var_id_in(jf), cl_read(jf), il_part_id, &
       il_var_nodims, PRISM_In, il_var_shape, PRISM_Real, ierror)
  END DO
  !
  ! 6- PSMILe end of declaration phase 
  !
  CALL prism_enddef_proto (ierror)
  !
  ! Component model timestepping
  !
  DO itap = 1, npas
    itap_sec = itimestep*(itap-1) ! Time in sec at beginning of timestep
    !
    !
    WRITE (il_mparout,*) 'Oce tstep (proc involved in coupling)',itap
    !
    ! 7- PSMILe prism_get_proto or prism_put_proto at each timestep
    ! 
    CALL prism_get_proto (il_var_id_in(1),itap_sec, znsol, ierror)
    IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Recvd) THEN
        WRITE(il_mparout,FMT=1001)cl_read(1), itap_sec, ierror
        WRITE(il_mparout,FMT=*)'STOP in oce.F90'
        CALL prism_abort_proto(il_comp_id, 'oce.F90','abort3')
    ENDIF
    CALL prism_get_proto (il_var_id_in(2),itap_sec, zqsr, ierror)
    IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Recvd) THEN
        WRITE(il_mparout,FMT=1001)cl_read(2), itap_sec, ierror
        WRITE(il_mparout,FMT=*)'STOP in oce.F90'
        CALL prism_abort_proto(il_comp_id, 'oce.F90','abort4')
    ENDIF
    CALL prism_get_proto (il_var_id_in(3),itap_sec, zemp, ierror)
    IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Recvd) THEN
        WRITE(il_mparout,FMT=1001)cl_read(3), itap_sec, ierror
        WRITE(il_mparout,FMT=*)'STOP in oce.F90'
        CALL prism_abort_proto(il_comp_id, 'oce.F90','abort5')
    ENDIF
    CALL prism_get_proto (il_var_id_in(4),itap_sec, zrunoff, ierror)
    IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Recvd) THEN
        WRITE(il_mparout,FMT=1001)cl_read(4), itap_sec, ierror
        WRITE(il_mparout,FMT=*)'STOP in oce.F90'
        CALL prism_abort_proto(il_comp_id, 'oce.F90','abort6')
    ENDIF
    CALL prism_get_proto (il_var_id_in(5),itap_sec, ztaux, ierror)
    IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Recvd) THEN
        WRITE(il_mparout,FMT=1001)cl_read(5), itap_sec, ierror
        WRITE(il_mparout,FMT=*)'STOP in oce.F90'
        CALL prism_abort_proto(il_comp_id, 'oce.F90','abort7')
    ENDIF
    CALL prism_get_proto (il_var_id_in(6),itap_sec, ztauy, ierror)
    IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Recvd) THEN
        WRITE(il_mparout,FMT=1001)cl_read(6), itap_sec, ierror
        WRITE(il_mparout,FMT=*)'STOP in oce.F90'
        CALL prism_abort_proto(il_comp_id, 'oce.F90','abort8')
    ENDIF
    CALL prism_get_proto (il_var_id_in(7),itap_sec, ssalb, ierror)
    IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Recvd) THEN
        WRITE(il_mparout,FMT=1001)cl_read(7), itap_sec, ierror
        WRITE(il_mparout,FMT=*)'STOP in oce.F90'
        CALL prism_abort_proto(il_comp_id, 'oce.F90','abort9')
    ENDIF
    !   
      sstoc(:) = znsol(:) + 1
      sieoc(:) = ztaux(:) + 1
!    sstoc(:)=1.0
!    sieoc(:)=float(itap) 
    !
    ! Print some values
    !
    WRITE(il_mparout, *) ssalb(1:10)
    IF (MOD(itap,il_print).EQ.1) THEN
        DO ji = 1,il_imjm,il_imjm/20  
          WRITE (il_mparout,'(i6,2f10.2)') ji, sstoc(ji), sieoc(ji)
        ENDDO
    ENDIF
    !
    CALL prism_put_proto(il_var_id_out(1),itap_sec, sstoc, ierror)
    IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Sent) THEN
        WRITE(il_mparout,FMT=1002)cl_writ(1), itap_sec, ierror
        WRITE(il_mparout,FMT=*)'STOP in oce.F90'
        CALL prism_abort_proto(il_comp_id, 'oce.F90','abort10')
    ENDIF
    CALL prism_put_proto(il_var_id_out(2), itap_sec, sieoc, ierror)
    IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Sent) THEN
        WRITE(il_mparout,FMT=1002)cl_writ(2), itap_sec, ierror
        WRITE(il_mparout,FMT=*)'STOP in oce.F90'
        CALL prism_abort_proto(il_comp_id, 'oce.F90','abort11')
    ENDIF
    !
  END DO
  !
  DEALLOCATE(zqsr)  
  DEALLOCATE(znsol) 
  DEALLOCATE(zemp)  
  DEALLOCATE(zrunoff)
  DEALLOCATE(ztaux) 
  DEALLOCATE(ztauy) 
  DEALLOCATE(ssalb) 
  DEALLOCATE(sstoc) 
  DEALLOCATE(sieoc) 
  !
  ! 8- PSMILe termination 
  !    
  CALL prism_terminate_proto (ierror)
  IF (ierror .NE. PRISM_Ok) THEN
      WRITE (il_mparout,*) 'An error occured in prism_terminate = ', ierror
  ENDIF
  ! 
  WRITE (il_mparout,*) 'End of toyoce'   
  CLOSE(il_mparout)
  !    
  !      
1001 FORMAT(/,'Pb in reading ',A8,/,'Time is ',I8,/,'Error code is ',I2)
1002 FORMAT(/,'Pb in writing ',A8,/,'Time is ',I8,/,'Error code is ',I2)
  !   
END PROGRAM oce
