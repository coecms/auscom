PROGRAM atm
!======================================================================
! This program simulates the atmospheric part of a coupled model using
! the PRISM System Model Interface (PSMILe) to the Oasis CLIM communication 
! library for field exchange. The field dimensions are realistic, 
! corresponding to a T31 reduced gaussian grid, but beside the exchanges 
! the model is pratically "empty" (no real physics nor dynamics).
!======================================================================
  USE mod_kinds_model
  USE mod_prism_proto
  USE mod_prism_put_proto
  USE mod_prism_get_proto
  USE mod_prism_grids_writing
  IMPLICIT NONE
!
#ifdef use_comm_GSIP
#include <mpif.h>
#endif
#ifndef key_nonetCDF
#include <netcdf.inc>
#endif
  !
  ! Grid dimensions 
  INTEGER, PARAMETER :: il_im = 96, il_jm = 48, il_imjm = il_im*il_jm 
  !
  INTEGER, PARAMETER :: jpfldout = 7    ! Number of fields sent
  INTEGER, PARAMETER :: jpfldin = 3     ! Number of fields received 
  !
  CHARACTER(len=8), DIMENSION(jpfldout) :: cl_writ ! Symb names fields sent
  CHARACTER(len=8), DIMENSION(jpfldin) :: cl_read ! Symb names fields received
  !
  INTEGER, dimension(jpfldout) :: il_var_id_out ! ID for fields sent 
  INTEGER, dimension(jpfldin)  :: il_var_id_in ! ID for fields received
  !
  ! Fields sent
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: fsol, fnsol, waflx  
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: runoff, taux, tauy
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: sens 
  !
  ! Fields received
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: sst  
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: glace 
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: spec1 
  !
  CHARACTER(len=6), PARAMETER :: cp_modnam='toyatm' ! Component model name
  !
  INTEGER, PARAMETER :: npas = 144 ! Number of steps for the simulation. 
  ! We hard code this numbers but later specific PSMILe routines will be 
  ! developed to access the relevant information in the SCC file.
  !
  INTEGER, PARAMETER :: il_print = 24 ! Printing interval  
  INTEGER, PARAMETER :: itimestep = 3600 ! Timestep in seconds
  !
  LOGICAL :: ll_comparal ! Logical true if component is parallel 
                         ! and if all process communicates with Oasis.
  !
  INTEGER  :: itap, itap_sec, ji, jf, ierror, ibou
  INTEGER  :: il_rank
  INTEGER  :: il_mparout
  CHARACTER(len=8) :: chatmout
  CHARACTER(len=2) :: chout
  INTEGER :: il_comp_id     ! Component ID
  INTEGER :: il_commlocal   ! Component internal communicator 
  INTEGER :: il_nbtotproc   ! Total number of processes
  INTEGER :: il_nbcplproc   ! Number of processes involved in the coupling
  INTEGER :: il_part_id     ! Local partition ID
  INTEGER :: il_length      ! Size of partial field for each process
  INTEGER, DIMENSION(2) :: il_var_nodims, il_var_shape ! see below
  !
  INTEGER :: il_flag        ! Flag for grid writing
  INTEGER :: il_status, il_fileid, il_varid
  INTEGER, DIMENSION(2) :: ist, icnt
  REAL(kind=ip_double_p),DIMENSION(il_im,il_jm) :: dla_lon, dla_lat, dla_srf
  REAL(kind=ip_double_p),DIMENSION(il_im,il_jm,4) :: dla_lonb, dla_latb
!
  REAL(kind=ip_realwp_p),DIMENSION(il_im,il_jm) :: rla_lon, rla_lat, rla_srf
  INTEGER,DIMENSION(il_im,il_jm) :: ila_msk 
  REAL(kind=ip_realwp_p),DIMENSION(il_im,il_jm,4) :: rla_lonb, rla_latb
!
  EXTERNAL decomp_def       ! defines the decomposition
!
  INTEGER      :: io_size, ii, il_bufsize, il_real, il_bufsizebyt
  INTEGER      :: integer_byte_size, integer_io_size
  REAL(kind=ip_realwp_p), DIMENSION(il_im,il_jm)  :: rla_array
  REAL(kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: rla_bufsend
!======================================================================
  !
  ! 1- PSMILe initialization
  !
  CALL MPI_Init (ierror)
  
  CALL prism_init_comp_proto (il_comp_id, cp_modnam, ierror)
  !
  ! Let's suppose the model attaches to a MPI buffer for its own use
  !
  ! ! Sophisticated way to determine buffer size needed (without "kind")
  ! ! Here one message containing rla_array

  integer_byte_size = BIT_SIZE(ii)/8
  INQUIRE (iolength=io_size) ii
  integer_io_size = io_size
  INQUIRE (iolength=io_size) rla_array(1,1)
  il_real = io_size/integer_io_size*integer_byte_size
  il_bufsize = il_imjm + MPI_BSEND_OVERHEAD/il_real + 1
  ALLOCATE (rla_bufsend(il_bufsize), stat = ierror)
  il_bufsizebyt = il_bufsize * il_real
  CALL MPI_Buffer_Attach(rla_bufsend, il_bufsizebyt, ierror)

  IF (ierror .NE. PRISM_Ok) THEN
      WRITE (*,*) ' atm : pb prism_init_comp_proto'
      CALL prism_abort_proto(il_comp_id, 'atm.F90','abort1')
  ELSE
      WRITE(*,*) 'atm : prism_init_comp_proto ok '
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
      WRITE (*,*) ' atm : pb prism_init_comp_proto'
      CALL prism_abort_proto(il_comp_id, 'atm.F90','abort2')
  ELSE
      WRITE(*,*) 'atm : prism_init_comp_proto ok '
  ENDIF
  !
  ! 3- Inquire if atm is parallel or not and open the process log file 
  !
  CALL MPI_Comm_Size(il_commlocal, il_nbtotproc, ierror)
  CALL MPI_Comm_Rank(il_commlocal, il_rank, ierror)
  !
  il_mparout = 85 + il_rank
  WRITE(chout,'(I2)')il_mparout
  chatmout='atmout'//chout
  !
  OPEN(il_mparout,file=chatmout,form='formatted')
  !
  WRITE(il_mparout,*) 'Atm: Number of processes:', il_nbtotproc
  WRITE(il_mparout,*) 'Local process number:', il_rank
  WRITE(il_mparout,*) 'Local communicator is : ',il_commlocal
  !
  ! 4- Compare the total number of processes and the number of processes
  ! involved in the coupling.
  !
  !  3 cases are illustrated here:
  !  . A monoprocess atm which process is involved in the coupling
  !   (ll_comparal = .FALSE.); put il_nbcplproc = 1 here after.
  !  . A parallel atm with only the master process involved in the coupling
  !   (ll_comparal = .FALSE.); put il_nbcplproc = 1 here after.
  !  . A parallel atm with all processes involved in the coupling
  !   (ll_comparal = .TRUE.);  put il_nbcplproc = 3 here after.
  !   Here, we hard code these numbers but later specific PSMILe routines
  !   will be developed to access this information directly in the SCC file.
  !   The case in which n processes are involved in the coupling among
  !   a total number m of processes should work but has not been tested.
  !
  il_nbcplproc = 3
  !
  IF (il_nbcplproc .EQ. il_nbtotproc .and. il_nbtotproc .ne. 1) THEN
      ll_comparal = .TRUE.
  ELSE
      ll_comparal = .FALSE.
  ENDIF
  WRITE(il_mparout,*)'ll_comparal',ll_comparal
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
          il_status=NF_INQ_VARID(il_fileid, 'at31.lon' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_lon(:,:))
          il_status=NF_INQ_VARID(il_fileid, 'at31.lat' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_lat(:,:))
          il_status=NF_CLOSE(il_fileid)
          DO ibou = 1,4
            ! Not exact definition, just for test
            dla_lonb(:,:, ibou) = dla_lon(:,:)
            dla_latb(:,:, ibou) = dla_lat(:,:)
          ENDDO
          rla_lon(:,:) = dla_lon(:,:)
          rla_lat(:,:) = dla_lat(:,:)
          rla_lonb(:,:,:) = dla_lonb(:,:,:)
          rla_latb(:,:,:) = dla_latb(:,:,:)
          call prism_write_grid('at31', il_im, il_jm, rla_lon, rla_lat)
          call prism_write_corner('at31', il_im, il_jm, 4, rla_lonb, rla_latb)
          !
          il_status=NF_OPEN('masks_in.nc', NF_NOWRITE, il_fileid)
          il_status=NF_INQ_VARID(il_fileid, 'at31.msk' , il_varid)
          il_status=NF_GET_VARA_INT (il_fileid, il_varid, ist, icnt, & 
             ila_msk(:,:))
          il_status=NF_CLOSE(il_fileid)
          call prism_write_mask('at31', il_im, il_jm, ila_msk)
          !
          il_status=NF_OPEN('areas_in.nc', NF_NOWRITE, il_fileid)
          il_status=NF_INQ_VARID(il_fileid, 'at31.srf' , il_varid)
          il_status=NF_GET_VARA_DOUBLE (il_fileid, il_varid, ist, icnt, & 
             dla_srf(:,:))
          il_status=NF_CLOSE(il_fileid)
          rla_srf(:,:) = dla_srf(:,:)
          call prism_write_area('at31', il_im, il_jm, rla_srf)
          !
          call prism_terminate_grids_writing()
      ENDIF
#endif
  ENDIF
  IF (il_rank.EQ.0 .OR. ll_comparal) THEN
      !
      ! The following steps need to be done:
      ! -> by the process if atm is monoprocess;
      ! -> only by the master process, if atm is parallel and only 
      !    master process is involved in the coupling;
      ! -> by all processes, if atm is parallel and all processes 
      ! are involved in the coupling.
      !
      ! 5- Define parallel partitions 
      !    (prism_def_partition_proto is called in decomp_def)
      !    and allocate coupling fields accordingly
      !
      CALL decomp_def (il_part_id, il_length, il_imjm, &
         il_rank, il_nbcplproc, ll_comparal, il_mparout)
      ALLOCATE(sst(il_length))   ; sst(:) = 0
      ALLOCATE(glace(il_length)) ; glace(:) = 0
      ALLOCATE(spec1(il_length)) ; spec1(:) = 0
      ALLOCATE(fsol(il_length))  ; fsol(:) = 0
      ALLOCATE(fnsol(il_length)) ; fnsol(:) = 0
      ALLOCATE(waflx(il_length)) ; waflx(:) = 0
      ALLOCATE(runoff(il_length)); runoff(:) = 0
      ALLOCATE(taux(il_length))  ; taux(:) = 0
      ALLOCATE(tauy(il_length))  ; tauy(:) = 0
      ALLOCATE(sens(il_length))  ; sens(:) = 0
      !
      ! 6- PSMILe coupling fields declaration
      !
      il_var_nodims(1) = 1 ! rank of coupling field
      il_var_nodims(2) = 1 ! number of bundles in coupling field (always 1)
      il_var_shape(1)= 1   ! min index for the coupling field local dimension
      il_var_shape(2)= il_length ! max index for the coupling field local dim
      !
      ! Define name (as in namcouple) and declare each field sent by atm
      !
      cl_writ(1)='CONSFTOT'
      cl_writ(2)='COSHFTOT'
      cl_writ(3)='COWATFLU'
      cl_writ(4)='CORUNOFF'
      cl_writ(5)='COZOTAUX'
      cl_writ(6)='COMETAUY'
      cl_writ(7)='COSENHFL'
      !
      DO jf=1, jpfldout
        CALL prism_def_var_proto (il_var_id_out(jf),cl_writ(jf), il_part_id, &
           il_var_nodims, PRISM_Out, il_var_shape, PRISM_Real, ierror)
      END DO 
      !
      ! Define name (as in namcouple) and declare each field received by atm
      !
      cl_read(1)='SISUTESU'
      cl_read(2)='SIICECOV'
      cl_read(3)='COTHSHSU'
      !
      DO jf=1, jpfldin
        CALL prism_def_var_proto (il_var_id_in(jf), cl_read(jf), il_part_id, &
           il_var_nodims, PRISM_In, il_var_shape, PRISM_Real, ierror)
      END DO 
      !
      ! 7- PSMILe end of declaration phase 
      !
      CALL prism_enddef_proto (ierror)
      !
  ENDIF
  !
  ! Component model timestepping
  !
  itap = 0
  IF (il_rank.EQ.0 .OR. ll_comparal) THEN
  !
      DO itap = 1, npas

        itap_sec = itimestep*(itap-1) ! Time in sec at beginning of timestep
        !
        WRITE (il_mparout,*) 'Atm tstep (proc involved in coupling)',itap
        !
        ! 8- PSMILe prism_get_proto or prism_put_proto at each timestep
        !   
        CALL prism_get_proto (il_var_id_in(1),itap_sec, sst, ierror)
        WRITE(il_mparout,FMT=*)'itap_sec, ierrorsst=',itap_sec, ierror
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Recvd) THEN
            WRITE(il_mparout,FMT=1001)cl_read(1), itap_sec, ierror
            CALL prism_abort_proto(il_comp_id, 'atm.F90','abort3') 
        ENDIF
        CALL prism_get_proto (il_var_id_in(2),itap_sec, glace, ierror)
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Recvd) THEN
            WRITE(il_mparout,FMT=1001)cl_read(2), itap_sec, ierror
            CALL prism_abort_proto(il_comp_id, 'atm.F90','abort4') 
        ENDIF
        CALL prism_get_proto (il_var_id_in(3),itap_sec, spec1, ierror)
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Recvd) THEN
            WRITE(il_mparout,FMT=1001)cl_read(3), itap_sec, ierror
            CALL prism_abort_proto(il_comp_id, 'atm.F90','abort5') 
        ENDIF
        !   
        ! NB: For a real model in which only the master process would receive
        ! the coupling fields, the master process would have to redistribute
        ! the fields to the other processes. Here we did not code this.
        !
        DO ji = 1,il_length
          fsol  (ji) = sst (ji) + 1
          fnsol (ji) = sst (ji) + 1
          waflx (ji) = sst (ji) + 1
          runoff(ji) = glace (ji) + 1
          taux  (ji) = glace (ji) + 1
          tauy  (ji) = glace (ji) + 1
          sens  (ji) = spec1 (ji) + 1
        END DO
!       DO ji = 1,il_length
!          fsol  (ji) = 1.0
!          fnsol (ji) = 2.0
!          waflx (ji) = 3.0
!          runoff(ji) = 4.0
!          taux  (ji) = 5.0
!          tauy  (ji) = 6.0
!          sens  (ji) = 7.0
!        END DO
!        !
!        ! Print some values
!        !
        IF (MOD(itap,il_print).EQ.1) THEN
            DO ji = 1,il_length,il_length/10  
              WRITE (il_mparout,'(i6,3f10.2)') ji, fsol(ji), runoff(ji), sens(ji)
            ENDDO
        ENDIF
        !
        CALL prism_put_proto(il_var_id_out(1),itap_sec, fnsol, ierror)
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Sent) THEN
            WRITE(il_mparout,FMT=1002)cl_writ(1), itap_sec, ierror
            CALL prism_abort_proto(il_comp_id, 'atm.F90','abort6') 
        ENDIF
        CALL prism_put_inquire_proto(il_var_id_out(2), itap_sec, ierror)
        WRITE(il_mparout,FMT=*)'itap_sec, ierror1=',itap_sec, ierror
        CALL prism_put_proto(il_var_id_out(2), itap_sec, fsol, ierror)
        WRITE(il_mparout,FMT=*)'itap_sec, ierror2=',itap_sec, ierror
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Sent) THEN
            WRITE(il_mparout,FMT=1002)cl_writ(2), itap_sec, ierror
            CALL prism_abort_proto(il_comp_id, 'atm.F90','abort7')
        ENDIF
        CALL prism_put_proto(il_var_id_out(3), itap_sec, waflx, ierror)
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Sent) THEN
            WRITE(il_mparout,FMT=1002)cl_writ(3), itap_sec, ierror
            CALL prism_abort_proto(il_comp_id, 'atm.F90','abort8')
        ENDIF
        CALL prism_put_proto(il_var_id_out(4), itap_sec, runoff, ierror)
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Sent) THEN
            WRITE(il_mparout,FMT=1002)cl_writ(4), itap_sec, ierror
            CALL prism_abort_proto(il_comp_id, 'atm.F90','abort9')
        ENDIF
        CALL prism_put_proto(il_var_id_out(5), itap_sec, taux, ierror)
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Sent) THEN
            WRITE(il_mparout,FMT=1002)cl_writ(5), itap_sec, ierror
            CALL prism_abort_proto(il_comp_id, 'atm.F90','abort10')
        ENDIF
        CALL prism_put_proto(il_var_id_out(6), itap_sec, tauy, ierror)
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Sent) THEN
            WRITE(il_mparout,FMT=1002)cl_writ(6), itap_sec, ierror
            CALL prism_abort_proto(il_comp_id, 'atm.F90','abort11')
        ENDIF
        CALL prism_put_proto(il_var_id_out(7), itap_sec, sens, ierror)
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Sent) THEN
            WRITE(il_mparout,FMT=1002)cl_writ(7), itap_sec, ierror
            CALL prism_abort_proto(il_comp_id, 'atm.F90','abort12')
        ENDIF
        !
        IF (itap .EQ. 12) THEN
            CALL prism_put_restart_proto(il_var_id_out(1), itap_sec, ierror)
            WRITE(il_mparout,*)'After prism_put_restart, return code is: ',&
               ierror
!            CALL prism_put_restart_proto(il_var_id_out(2), itap_sec, ierror)
!            CALL prism_put_restart_proto(il_var_id_out(3), itap_sec, ierror)
!            CALL prism_put_restart_proto(il_var_id_out(4), itap_sec, ierror)
!            CALL prism_put_restart_proto(il_var_id_out(5), itap_sec, ierror)
!            CALL prism_put_restart_proto(il_var_id_out(6), itap_sec, ierror)
!            CALL prism_put_restart_proto(il_var_id_out(7), itap_sec, ierror)
        ENDIF
      END DO
      !
      DEALLOCATE(sst)
      DEALLOCATE(glace)
      DEALLOCATE(spec1)
      DEALLOCATE(fsol)
      DEALLOCATE(fnsol)
      DEALLOCATE(waflx)
      DEALLOCATE(runoff)
      DEALLOCATE(taux)
      DEALLOCATE(tauy)
      DEALLOCATE(sens)
      !
  ELSE
      WRITE (il_mparout,*) 'Atm tstep (proc not involved in coupling)',itap
  ENDIF
  !
  ! Do not detach from MPI buffer as it is done in prism_terminate_proto
  !
  DEALLOCATE (rla_bufsend)
  !  
  ! 9- PSMILe termination 
  !   
  CALL prism_terminate_proto (ierror)
  IF (ierror .NE. PRISM_Ok) THEN
      WRITE (il_mparout,*) 'An error occured in prism_terminate = ', ierror
  ENDIF
  ! 
  WRITE (il_mparout,*) 'End of toyatm'
  CLOSE(il_mparout)
  call MPI_Finalize (ierror)
  !    
  !      
1001 FORMAT(/,'Pb in reading ',A8,/,'Time is ',I8,/,'Error code is ',I2)
1002 FORMAT(/,'Pb in writing ',A8,/,'Time is ',I8,/,'Error code is ',I2)
  !   
END PROGRAM atm
