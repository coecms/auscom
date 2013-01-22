PROGRAM che
!======================================================================
! This program simulates the atmospheric chemistry of a coupled model using
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
#ifndef key_nonetCDF
#include <netcdf.inc>
#endif
  !
  ! Grid dimensions 
  INTEGER, PARAMETER :: il_im = 96, il_jm = 48, il_imjm = il_im*il_jm 
  !
  INTEGER, PARAMETER :: jpfldout = 1    ! Number of fields sent
  INTEGER, PARAMETER :: jpfldin = 1     ! Number of fields received 
  !
  CHARACTER(len=8), DIMENSION(jpfldout) :: cl_writ ! Symb names fields sent
  CHARACTER(len=8), DIMENSION(jpfldin) :: cl_read ! Symb names fields received
  !
  INTEGER, dimension(jpfldout) :: il_var_id_out ! ID for fields sent 
  INTEGER, dimension(jpfldin)  :: il_var_id_in ! ID for fields received
  !
  ! Field sent
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: spec1 
  !
  ! Field received
  REAL(kind=ip_realwp_p),DIMENSION(:),ALLOCATABLE :: sens 
  !
  CHARACTER(len=6), PARAMETER :: cp_modnam='toyche' ! Component model name
  !
  INTEGER, PARAMETER :: npas = 72 ! Number of steps for the simulation. 
  ! We hard code this numbers but later specific PSMILe routines will be 
  ! developed to access the relevant information in the SCC file.
  !
  INTEGER, PARAMETER :: il_print = 12 ! Printing interval 
  INTEGER, PARAMETER :: itimestep = 7200 ! Timestep in seconds
  !
  LOGICAL :: ll_comparal ! Logical true if component is parallel 
                         ! and if all process communicates with Oasis.
  !
  INTEGER  :: itap, itap_sec, ji, jf, ierror
  INTEGER  :: il_rank
  INTEGER  :: il_mparout
  CHARACTER(len=8) :: chcheout
  CHARACTER(len=2) :: chout
  INTEGER :: il_comp_id     ! Component ID
  INTEGER :: il_commlocal   ! Component internal communicator 
  INTEGER :: il_nbtotproc   ! Total number of processes
  INTEGER :: il_nbcplproc   ! Number of processes involved in the coupling
  INTEGER :: il_part_id     ! Local partition ID
  INTEGER :: il_length      ! Size of partial field for each process
  INTEGER, DIMENSION(2) :: il_var_nodims(2), il_var_shape(2) ! see below
  !
  INTEGER :: il_flag        ! Flag for grid writing
  INTEGER :: il_status, il_fileid, il_varid
  INTEGER, DIMENSION(2) :: ist, icnt
  REAL(kind=ip_double_p),DIMENSION(il_im,il_jm) :: dla_lon, dla_lat, dla_srf
  REAL(kind=ip_realwp_p),DIMENSION(il_im,il_jm) :: rla_lon, rla_lat, rla_srf
  INTEGER,DIMENSION(il_im,il_jm) :: ila_msk 
  EXTERNAL decomp_def       ! defines the decomposition
!
!======================================================================
  !
  ! 1- PSMILe initialization
  !
  CALL MPI_Init (ierror)
  CALL prism_init_comp_proto (il_comp_id, cp_modnam, ierror)
  !
  IF (ierror .NE. PRISM_Ok) THEN
      WRITE (*,*) ' che : pb prism_init_comp_proto'
      CALL prism_abort_proto(il_comp_id, 'che.F90','abort1')
  ELSE
      WRITE(*,*) 'che : prism_init_comp_proto ok '
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
      WRITE (*,*) ' che : pb prism_init_comp_proto'
      CALL prism_abort_proto(il_comp_id, 'che.F90','abort2')
  ELSE
      WRITE(*,*) 'che : prism_init_comp_proto ok '
  ENDIF
  !
  ! 3- Inquire if che is parallel or not and open the process log file 
  !
  CALL MPI_Comm_Size(il_commlocal, il_nbtotproc, ierror)
  CALL MPI_Comm_Rank(il_commlocal, il_rank, ierror)
  !
  il_mparout = 85 + il_rank
  WRITE(chout,'(I2)')il_mparout
  chcheout='cheout'//chout
  !
  OPEN(il_mparout,file=chcheout,form='formatted')
  !
  WRITE(il_mparout,*) 'Che: Number of processes:', il_nbtotproc
  WRITE(il_mparout,*) 'Local process number:', il_rank
  WRITE(il_mparout,*) 'Local communicator is : ',il_commlocal
  !
  ! 4- Compare the total number of processes and the number of processes
  ! involved in the coupling.
  !
  !  3 cases are illustrated here:
  !  . A monoprocess che which process is involved in the coupling
  !   (ll_comparal = .FALSE.); put il_nbcplproc = 1 here after.
  !  . A parallel che with only the master process involved in the coupling
  !   (ll_comparal = .FALSE.); put il_nbcplproc = 1 here after.
  !  . A parallel che with all processes involved in the coupling
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
          rla_lat(:,:)=dla_lat(:,:)
          rla_lon(:,:)=dla_lon(:,:)
          call prism_write_grid('at31', il_im, il_jm, rla_lon, rla_lat)
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
          rla_srf(:,:)=dla_srf(:,:)
          call prism_write_area('at31', il_im, il_jm, rla_srf)
          !
          call prism_terminate_grids_writing()
      ENDIF
#endif
  ENDIF
  IF (il_rank.EQ.0 .OR. ll_comparal) THEN
      !
      ! The following steps need to be done:
      ! -> by the process if che is monoprocess;
      ! -> only by the master process, if che is parallel and only 
      !    master process is involved in the coupling;
      ! -> by all processes, if che is parallel and all processes 
      ! are involved in the coupling.
      !
      ! 5- Define parallel partitions 
      !    (prism_def_partition_proto is called in decomp_def)
      !    and allocate coupling fields accordingly
      !
      CALL decomp_def (il_part_id, il_length, il_imjm, &
         il_rank, il_nbcplproc, ll_comparal, il_mparout)
      ALLOCATE(spec1(il_length)) ; spec1(:) = 0
      ALLOCATE(sens(il_length))  ; sens(:) = 0
      !
      ! 6- PSMILe coupling fields declaration
      !
      il_var_nodims(1) = 1 ! rank of coupling field
      il_var_nodims(2) = 1 ! number of bundles in coupling field (always 1)
      il_var_shape(1)= 1   ! min index for the coupling field local dimension
      il_var_shape(2)= il_length ! max index for the coupling field local dim
      !
      ! Define name (as in namcouple) and declare each field sent by che
      !
      cl_writ(1)='SOTHSHSU'
      !
      DO jf=1, jpfldout
        CALL prism_def_var_proto (il_var_id_out(jf),cl_writ(jf), il_part_id, &
           il_var_nodims, PRISM_Out, il_var_shape, PRISM_Real, ierror)
      END DO 
      !
      ! Define name (as in namcouple) and declare each field received by che
      !
      cl_read(1)='SOSENHFL'
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
  ! Component model timestepping (only if involved in the coupling)
  !
  itap = 0
  IF (il_rank.EQ.0 .OR. ll_comparal) THEN

      DO itap = 1, npas

        itap_sec = itimestep*(itap-1) ! Time in sec at beginning of timestep
        !
        WRITE (il_mparout,*) 'Che tstep (proc involved in coupling)',itap
        !
        ! 8- PSMILe prism_get_proto or prism_put_proto at each timestep
        !   
        CALL prism_get_proto (il_var_id_in(1),itap_sec, sens, ierror)
        WRITE(il_mparout,FMT=*)'itap_sec, ierrorsens=',itap_sec, ierror
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Recvd) THEN
            WRITE(il_mparout,FMT=1001)cl_read(1), itap_sec, ierror
            WRITE(il_mparout,*)'STOP in che.F90'
            CALL prism_abort_proto(il_comp_id, 'che.F90','abort3')
        ENDIF
        !   
        ! NB: For a real model in which only the master process would receive
        ! the coupling fields, the master process would have to redistribute
        ! the fields to the other processes. Here we did not code this.
        !
        DO ji = 1,il_length
          spec1(ji) = sens(ji) + 1
        END DO
        !
        ! Print some values
        !
        IF (MOD(itap,il_print).EQ.1) THEN
            DO ji = 1,il_length,il_length/10  
              WRITE (il_mparout,'(i6,1f10.2)') ji, spec1(ji)
            ENDDO
        ENDIF
        !
        CALL prism_put_proto(il_var_id_out(1), itap_sec, spec1, ierror)
        IF ( ierror .NE. PRISM_Ok .and. ierror .LT. PRISM_Sent) THEN
            WRITE(il_mparout,FMT=1002)cl_writ(1), itap_sec, ierror
            WRITE(il_mparout,*)'STOP in che.F90'
            CALL prism_abort_proto(il_comp_id, 'che.F90','abort4')
        ENDIF
    !
  END DO
  !
      DEALLOCATE(spec1)
      DEALLOCATE(sens)
  !
  ELSE
      WRITE (il_mparout,*) 'Che tstep (proc not involved in coupling)',itap
  ENDIF
  !
  ! 9- PSMILe termination 
  !    
  CALL prism_terminate_proto (ierror)
  IF (ierror .NE. PRISM_Ok) THEN
      WRITE (il_mparout,*) 'An error occured in prism_terminate = ', ierror
  ENDIF
  !
  WRITE (il_mparout,*) 'End of toyche'    
  CLOSE(il_mparout)
  !
  call MPI_Finalize (ierror)    
  !      
1001 FORMAT(/,'Pb in reading ',A8,/,'Time is ',I8,/,'Error code is ',I2)
1002 FORMAT(/,'Pb in writing ',A8,/,'Time is ',I8,/,'Error code is ',I2)
  !   
END PROGRAM che
