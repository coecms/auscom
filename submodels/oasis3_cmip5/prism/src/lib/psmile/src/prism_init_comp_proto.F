       SUBROUTINE prism_init_comp_proto(il_mynummod, cdnam, kinfo)
c
c*    *** PRISM_init_comp ***   PRISM 1.0
c
c     purpose:
c     --------
c        start models
c
c     interface:
c     ----------
c        cdnam  : name of the calling model
c        il_mynummod : model number
c        kinfo  : exit status
c
c     lib mp:
c     -------
c        MPI-1 or MPI-2 or GSIP
c
c     author:
c     -------
c       Sophie Valcke  - CERFACS (08/09/00 -created from CLIM_Init)
c       Jean Latour - F.S.E. - Version MPMD launch with mpi-1
c                   - implies the use of mpiexec server process on VPPs     
c                     or the command mpirun on most platforms
c       Arnaud Caubel - FECIT (08/02 - created from CLIM_Init - removed 
c                              some arguments and added dynamic allocation)
c       S. Legutke - MPI M&D - cg_clim/def_rstfile initialized
c       S. Valcke - Added GSIP
c     ----------------------------------------------------------------
c
      USE mod_kinds_model
      USE mod_prism_proto
      USE mod_comprism_proto
#if !defined key_noIO
      USE mod_psmile_io_interfaces
#endif
#if defined use_comm_GSIP 
      USE mod_gsip_model
#endif
      IMPLICIT NONE
#if defined use_libMPI || defined use_comm_MPI1 || defined use_comm_MPI2
#include <mpif.h>
      INTEGER (kind=ip_intwp_p)     mpi_status(MPI_STATUS_SIZE)
#endif
c     ----------------------------------------------------------------
      CHARACTER*(*) cdnam
      INTEGER (kind=ip_intwp_p)	    kinfo
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)     il_mynummod     
      INTEGER (kind=ip_intwp_p)     imodst, iost, ip, iprcou, iprmod
      INTEGER (kind=ip_intwp_p)     il_err, il_rank, il_maxcplproc
      INTEGER (kind=ip_intwp_p)     il_CLIM_Maxport, il_CLIM_MaxLink
      INTEGER (kind=ip_intwp_p)     ji, jj, jl, ir, iremsize,ibuff, 
     $    itagcol, il_size
      INTEGER (kind=ip_intwp_p)     icolor, ikey, iposbuf, info
      INTEGER (kind=ip_intwp_p)     il_start, il_end, il_logrank
      INTEGER (kind=ip_intwp_p)     impi_newcomm2, impi_intercomp1,
     $    jlocal
      INTEGER (kind=ip_intwp_p)     imaxmodel, iarrb(2)
      INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: 
     $    impi_intercomp 
      INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: 
     $    impi_newcomm
      LOGICAL       ll_file
      PARAMETER     (itagcol=9876)
      REAL (kind=ip_realwp_p) rl_testvar
#if defined use_libMPI || defined use_comm_MPI1 || defined use_comm_MPI2
      REAL (kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: rl_work
#endif
      CHARACTER(len=6) :: cl_comm, cl_argv
      CHARACTER*(CLIM_Clength), DIMENSION(:), ALLOCATABLE :: cunames
      INTEGER(kind=ip_intwp_p), DIMENSION(1024) :: iarr_err
      INTEGER(kind=ip_intwp_p) :: integer_byte_size, ii, io_size, 
     $    integer_io_size, il_ibyt, il_int, il_char, il_log, il_rl_work
      LOGICAL :: ll_log
      CHARACTER*1 :: cl_char 
c     GSIP specific declarations
#if defined use_comm_GSIP
      INTEGER :: il_mil, il_cen, il_diz, il_uni, il_errgsip
      INTEGER :: il_st, il_en
      INTEGER :: mgi_init, mgi_open, mgi_read
      INTEGER (kind=ip_intwp_p) :: il_cwk_size, il_iwk_size
      INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  ila_work
      CHARACTER*8, DIMENSION(:), ALLOCATABLE :: cla_work
      CHARACTER(LEN=1) :: cl_mil, cl_cen, cl_diz, cl_uni
      CHARACTER(LEN=40) :: cl_channam
#endif
c
c     ----------------------------------------------------------------
c
      rl_testvar = 0.0_ip_realwp_p
c
c*    0.0 Initializations
c     -------------------
c
      kinfo = CLIM_Ok
      cmynam=' '
      cmynam=cdnam
      nports = 0
      nlinks = 0
      ig_nbpart = 0
      nbsend = 0
      nbrecv = 0
      ig_local_comm = 0
#if defined use_libMPI || defined use_comm_MPI1 || defined use_comm_MPI2  
      mpi_status(:) = 0
#endif
      iarrb(:)=0
      ibuff = 0
c     By default, the number of corners is 4. It will be changed if the
c     model calls prism_write_corner.
      ig_noc = 4
c
c*    1.0 Start global MPI environment
c     --------------------------------
c
#if defined use_comm_MPI1     
      WRITE (0, FMT='(A)')   'Init - CLIM / MPI-1'
#elif defined use_comm_MPI2
      WRITE (0, FMT='(A)')   'Init - CLIM / MPI-2'
#elif defined use_comm_GSIP
      WRITE (0, FMT='(A)')   'Init - CLIM / GSIP'
#endif
c
#if defined use_libMPI || defined use_comm_MPI1 || defined use_comm_MPI2
      lg_mpiflag = .FALSE.
      CALL MPI_Initialized ( lg_mpiflag, mpi_err )
      IF ( .NOT. lg_mpiflag ) THEN
          CALL MPI_INIT ( mpi_err )
          WRITE (0,FMT='(A)') 'Calling MPI_Init in prism_init_comp...'
      ELSE
          WRITE (0,FMT='(A)') 'No call of MPI_Init in prism_init_comp.'
      ENDIF
c
      CALL MPI_Comm_Size(MPI_COMM_WORLD,mpi_size,mpi_err)
      CALL MPI_Comm_Rank(MPI_COMM_WORLD,mpi_rank,mpi_err)
c
      WRITE (0, FMT='(A,A)') 'Init - name of the model: ', cdnam
      WRITE(0,*)'Init - -  rank = ',mpi_rank,' in MPI_COMM_WORLD'
      WRITE(0,*)'Init - -  size = ',mpi_size,' of MPI_COMM_WORLD'

#else
      mpi_size = 1
      mpi_rank = 0
#endif
#if defined use_libMPI && defined use_comm_GSIP
      ig_local_comm = MPI_COMM_WORLD
#endif
c
c     For GSIP, mpi_rank is the rank of the local process in the number of
c     processes for the model
c
c*    1.1 Case MPI-1 : Split global communicator into disjoint communicators
c*                     local for each model
c*    ----------------------------------------------------------------------
c
#ifdef use_comm_MPI1
c     
c*    MPI_COMM_WORLD is the global communicator for all processes
c*    it includes Oasis and all model processes
c*    It is duplicated in "mpi_comm" for compatibility with the
c*    MPI-2 start option that implies multiple MPI_Comm_Spawn.
c     
         CALL MPI_COMM_DUP(MPI_COMM_WORLD,mpi_comm,mpi_err)
         WRITE(0,*)'Init - - comm_dup done= ',mpi_comm

c     
c*       1.1.1 generates a "color" from the model name 
c     
         ALLOCATE (cunames(mpi_size), stat = il_err)
         IF (il_err.NE.0) WRITE (0,*) 
     $        'Init - Error in cunames allocation '
         call MPI_Allgather(cmynam,CLIM_Clength,MPI_CHARACTER,
     &        cunames,CLIM_Clength,MPI_CHARACTER,
     &        mpi_comm,mpi_err)
         icolor=1
         do while ((trim(cmynam).ne.trim(cunames(icolor))).and.
     &        (icolor.le.mpi_size))
            icolor=icolor+1
         enddo
         IF (icolor.le.mpi_size) THEN
            icolor=icolor*100
         ELSE 
          WRITE (0,*) 
     $    'Init - - Could not find myself in the model namespace!'
          WRITE (0,*) 'Init - - Check namcouple and '
          WRITE (0,*) 'model name tags for consistency !'
          call MPI_ABORT (mpi_comm, 0, mpi_err)
         ENDIF
         
         DEALLOCATE (cunames)
c     
c*       1.1.2 split MPI_COMM_WORLD in local, disjoints, communicators
c     
         ikey = 1
         call MPI_COMM_SPLIT(MPI_COMM_WORLD, icolor, ikey,
     $       ig_local_comm, mpi_err)
         IF(mpi_err .NE. MPI_SUCCESS) GOTO 215
c     
c*       1.1.3 get the model number from Oasis (proc 0 in global comm)
c     
         CALL MPI_Send(icolor,1,MPI_INTEGER,0,itagcol,mpi_comm, mpi_err)
c     
         CALL MPI_Recv(ibuff,1,MPI_INTEGER,0,itagcol,mpi_comm,
     $       mpi_status, mpi_err)
c
         il_mynummod = ibuff
         ig_mynummod = il_mynummod
         WRITE(0,*)'Init - model number : mynummod = ',il_mynummod
c
c*       1.2 Case MPI-2 
c*       --------------
c 
#elif defined use_comm_MPI2
c      
         CALL MPI_Comm_get_parent(impi_intercomp1,mpi_err)
         WRITE(0,*)'impi_intercomp1 ',impi_intercomp1
         IF ( mpi_err .NE. MPI_SUCCESS ) THEN
            WRITE(0,*)'Init_comp - - Error on Intercomm '
            kinfo = CLIM_Mpi
            GO TO 1010
         ENDIF
c     
         CALL MPI_Comm_remote_size(impi_intercomp1,iremsize,mpi_err)
c     
         CALL MPI_Intercomm_merge(impi_intercomp1, .true., 
     &        impi_newcomm2, mpi_err)
         CALL MPI_Comm_size(impi_newcomm2, mpi_size, mpi_err)
         CALL MPI_Comm_rank(impi_newcomm2, mpi_rank, mpi_err)
c     
         jlocal=2
c     
         STARTMOD : DO
c     
         IF (jlocal.eq.2) THEN
            CALL MPI_BCAST(iarrb,2,MPI_INTEGER,0,
     &           impi_newcomm2,ir)
            jl=iarrb(1)
            imaxmodel=iarrb(2)
            ALLOCATE(impi_intercomp(imaxmodel))
            ALLOCATE(impi_newcomm(imaxmodel + 1))
            impi_intercomp(:)=0
            impi_newcomm(:)=0
            impi_intercomp(1) = impi_intercomp1 
            impi_newcomm(2) = impi_newcomm2
            if (jl .gt. imaxmodel ) EXIT STARTMOD 
            cl_comm = 'obione'
            cl_argv='      '
            CALL MPI_COMM_SPAWN(cl_comm,cl_argv,1,MPI_INFO_NULL, 0,
     &           impi_newcomm(jlocal), impi_intercomp(jlocal),
     &           iarr_err, mpi_err)
c     
            CALL MPI_Intercomm_merge(impi_intercomp(jlocal), .false.,
     &           impi_newcomm(jlocal+1), mpi_err)
            jlocal=jlocal+1
            CALL MPI_Comm_size(impi_newcomm(jlocal), mpi_size, mpi_err)
            CALL MPI_Comm_rank(impi_newcomm(jlocal), mpi_rank, mpi_err)
         ELSE
            
            CALL MPI_BCAST(iarrb,2,MPI_INTEGER,0,
     &           impi_newcomm(jlocal),ir)
            jl=iarrb(1)
            imaxmodel=iarrb(2)
            
            if (jl .gt. imaxmodel ) EXIT STARTMOD
c     
            cl_comm = 'obione'
            cl_argv='      '
            CALL MPI_COMM_SPAWN(cl_comm,cl_argv,1,MPI_INFO_NULL, 0,
     &           impi_newcomm(jlocal), impi_intercomp(jlocal), 
     &           iarr_err, mpi_err)
c     
            CALL MPI_Intercomm_merge(impi_intercomp(jlocal), .false., 
     &           impi_newcomm(jlocal+1), mpi_err)
            jlocal=jlocal+1
            CALL MPI_Comm_size(impi_newcomm(jlocal), mpi_size, mpi_err)
            CALL MPI_Comm_rank(impi_newcomm(jlocal), mpi_rank, mpi_err)
            WRITE(0,*)'Init_comp - -  rank = ',
     $           mpi_rank,' in new comm'
            WRITE(0,*)'Init_comp - -  size = ',
     $           mpi_size,' of new comm'
c     
         ENDIF
         END DO    STARTMOD
c     
         CALL MPI_COMM_DUP(impi_newcomm(jlocal),mpi_comm,mpi_err)
         DO jl=2,jlocal
            call MPI_COMM_FREE(impi_newcomm(jl),mpi_err)
         ENDDO
         ig_local_comm = MPI_COMM_WORLD
         il_mynummod = imaxmodel - jlocal + 2
         ig_mynummod = il_mynummod
         WRITE(0,*)'Init - model number : mynummod = ',il_mynummod
         WRITE(0,*)'Init_comp - - Intercomm with Oasis = ',mpi_comm
         DEALLOCATE(impi_intercomp)
         DEALLOCATE(impi_newcomm)
c
#elif defined use_comm_GSIP
c
c     2.0 Init/open two GSIP channels for this process of this model
c     --------------------------------------------------------------
c
      ig_numproc = mpi_rank + 1
      il_mil = ig_numproc/1000
      cl_mil=ACHAR(il_mil + 48)
      il_cen = (ig_numproc - il_mil*1000)/100
      cl_cen=ACHAR(il_cen + 48)
      il_diz = (ig_numproc - il_mil*1000 - il_cen*100)/10
      cl_diz=ACHAR(il_diz + 48)
      il_uni = ig_numproc - il_mil*1000 -il_cen*100 - il_diz*10
      cl_uni=ACHAR(il_uni + 48)
c
c     Build and init writing channel name
c
      cl_channam = 
     $    trim(adjustl(cdnam))//'R'//cl_mil//cl_cen//cl_diz//cl_uni
      ig_gsipw = mgi_init (cl_channam)
      IF (ig_gsipw .ge. 0) THEN
          WRITE(*,*)'GSIP Writing Channel Init OK:', 
     $        cl_channam, ig_gsipw
      ELSE
          WRITE(*,*) 'GSIP Error is: ', ig_gsipw
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'GSIP Writing Channel Init NOT OK:')
      ENDIF
c
c     Build and init reading channel name
c
      cl_channam = 
     $    trim(adjustl(cdnam))//'W'//cl_mil//cl_cen//cl_diz//cl_uni
      ig_gsipr = mgi_init (cl_channam)
      IF (ig_gsipr .ge. 0) THEN
          WRITE(*,*)'GSIP Reading Channel Init OK:', 
     $            cl_channam, ig_gsipr
      ELSE
          WRITE(*,*) 'GSIP Error is: ', ig_gsipr
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'GSIP Reading Channel Init NOT OK:')
      ENDIF
c
c     Open writing channel name
c
      il_errgsip = mgi_open (ig_gsipw, 'W')
      IF (il_errgsip .ge. 0) THEN
          WRITE(*,*)
     $        'prism_init_comp - GSIP Writing Channel Open OK:', 
     $        ig_gsipw
      ELSE
          WRITE(*,*) 'GSIP Error is: ', il_errgsip
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'GSIP Writing Channel Open NOT OK:')
      ENDIF
c
c     Open reading channel name
c
      il_errgsip = mgi_open (ig_gsipr, 'R')
      IF (il_errgsip .ge. 0) then
          WRITE(*,*) 
     $        'prism_init_comp - GSIP Reading Channel Open OK:', 
     $        ig_gsipr
      ELSE
          WRITE(*,*) 'GSIP Error is: ', il_errgsip
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'GSIP Reading Channel Open NOT OK:')
      ENDIF
c
c     Initial exchange of information
c
c     Read number of models in channel
c
      il_errgsip = mgi_read (ig_gsipr, knmods, 1, 'I')
      IF (il_errgsip .ge. 0) then
          WRITE(*,*) 'prism_init_comp - GSIP knmods OK:', knmods
      ELSE
          WRITE(*,*) 'GSIP Error is: ', il_errgsip
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'GSIP knmods Open NOT OK:')
      ENDIF 
c
c     Read model number in channel
c
      il_errgsip = mgi_read (ig_gsipr, ig_mynummod, 1, 'I')
      IF (il_errgsip .ge. 0) then
          WRITE(*,*) 'prism_init_comp - GSIP model number OK:', 
     $        ig_mynummod
      ELSE
          WRITE(*,*) 'GSIP Error is: ', il_errgsip
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'GSIP model number NOT OK:')
      ENDIF
      il_mynummod = ig_mynummod
c
c     Read number of fields in channel
c
      il_errgsip = mgi_read (ig_gsipr, ig_clim_nfield, 1, 'I')
      IF (il_errgsip .ge. 0) THEN
          WRITE(*,*)
     $        'prism_init_comp - GSIP  ig_clim_nfield OK: ', 
     $        ig_clim_nfield
      ELSE
          WRITE(*,*) 'GSIP Error is: ', il_errgsip
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'GSIP number of fields NOT OK:')
      ENDIF
c
#endif
c
c*    2.0 Receive information from Oasis, allocate global arrays,
c         initialize global variables and open logfile
c      ------------------------------------------------------------------
#if defined use_comm_MPI1 || defined use_comm_MPI2
      CALL MPI_Recv(knmods, 1, MPI_INTEGER, 0, itagcol,
     $    mpi_comm, mpi_status, mpi_err)
      IF (mpi_ERR.ne.MPI_SUCCESS) THEN
          WRITE(UNIT = 0,FMT = *)
     $    'Init - Problem with reception of information from Oasis !'
          WRITE(UNIT = 0,FMT = *)'STOP in PRISM_init_comp'
          call flush(0)
          call MPI_ABORT (mpi_comm, 0, mpi_err)
      ENDIF
      CALL MPI_Recv(ig_clim_nfield, 1, MPI_INTEGER, 0, itagcol+1,
     $    mpi_comm, mpi_status, mpi_err)
      IF (mpi_ERR.ne.MPI_SUCCESS) THEN
          WRITE(UNIT = 0,FMT = *)
     $    'Init - Problem with reception of information from Oasis !'
          WRITE(UNIT = 0,FMT = *)'STOP in PRISM_init_comp'
          call flush(0)
          call MPI_ABORT (mpi_comm, 0, mpi_err)
      ENDIF
#endif
c
      ALLOCATE(cg_modnam(knmods))
      ALLOCATE(kbcplproc(knmods))
      ALLOCATE(kbtotproc(knmods))
      ALLOCATE(iga_unitmod(knmods))
c
      integer_byte_size = BIT_SIZE(ii)/8
      INQUIRE (iolength=io_size) ii
      integer_io_size = io_size
      il_int = io_size/integer_io_size*integer_byte_size
      INQUIRE (iolength=io_size) ll_log
      il_log = io_size/integer_io_size*integer_byte_size
#if defined use_comm_MPI1 || defined use_comm_MPI2
      INQUIRE (iolength=io_size) rl_testvar
      il_ibyt = io_size/integer_io_size*integer_byte_size
      INQUIRE (iolength=io_size) cl_char
      il_char = io_size/integer_io_size*integer_byte_size
      il_rl_work = (29 + 64*ig_clim_nfield) * (il_char/il_ibyt + 1) + 
     $    (9 + 3*knmods + 9*ig_clim_nfield) * (il_int/il_ibyt + 1) +
     $    3 * (il_log/il_ibyt + 1)
      ALLOCATE (rl_work(il_rl_work), stat=il_err)
      rl_work(:)=0
      il_size = il_rl_work * il_ibyt
#elif defined use_comm_GSIP
      IF (il_log .gt. il_int) THEN
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'ila_work not be big enough for a logical')
      ENDIF
#endif
c
      kbcplproc(:)=0
      kbtotproc(:)=0
      il_CLIM_Maxport = ig_clim_nfield *2
c
      ALLOCATE (cg_cnaminp(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $    'Error in cg_cnaminp allocation in PRISM_init_comp routine!'
      cg_cnaminp(:)=' '
      ALLOCATE (cg_cnamout(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $    'Error in cg_cnamout allocation in PRISM_init_comp routine!'
      cg_cnamout(:)=' '
      ALLOCATE (ig_clim_lag(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $    'Error in ig_clim_lag allocation in PRISM_init_comp routine!'
      ig_clim_lag(:)=0
      ALLOCATE (ig_clim_reverse(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $   'Error ig_clim_reverse allocation in PRISM_init_comp routine!'
      ig_clim_reverse(:)=0
      ALLOCATE (ig_clim_invert(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $    'Error ig_clim_invert allocation in PRISM_init_comp routine!'
      ig_clim_invert(:)=0
      ALLOCATE (ig_def_lag(il_CLIM_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $    'Error in ig_def_lag allocation in PRISM_init_comp routine!'
      ig_def_lag(:)=0
      ALLOCATE (ig_def_reverse(il_CLIM_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $    'Error ig_def_reverse allocation in PRISM_init_comp routine!'
      ig_def_reverse(:)=0
      ALLOCATE (ig_def_invert(il_CLIM_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $    'Error ig_def_invert allocation in PRISM_init_comp routine!'
      ig_def_invert(:)=0
      ALLOCATE (ig_clim_freq(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $   'Error in ig_clim_freq allocation in PRISM_init_comp routine!'
      ig_clim_freq(:)=0
      ALLOCATE (ig_def_freq(il_CLIM_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $    'Error in ig_def_freq allocation in PRISM_init_comp routine!'
      ig_def_freq (:) = 0
      ALLOCATE (ig_clim_seq(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $    'Error in ig_clim_seq allocation in PRISM_init_comp routine!'
      ig_clim_seq(:)=0
      ALLOCATE (ig_def_seq(il_CLIM_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $    'Error in ig_def_seq allocation in PRISM_init_comp routine!'
      ig_def_seq(:)=0
      ALLOCATE (cg_clim_rstfile(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $'Error in cg_clim_rstfile allocation in PRISM_init_comp routine!'
      cg_clim_rstfile(:)=' '
      ALLOCATE (cg_def_rstfile(il_CLIM_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in cg_def_rstfile allocation in PRISM_init_comp routine!'
      cg_def_rstfile(:)=' '
      ALLOCATE (ig_clim_norstfile(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in ig_clim_norstfile alloc in PRISM_init_comp routine!'
      ig_clim_norstfile(:)=0
      ALLOCATE (ig_def_norstfile(il_CLIM_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in ig_def_norstfile alloc in PRISM_init_comp routine!'
      ig_def_norstfile(:)=0
      ALLOCATE (ig_clim_state(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in ig_clim_state allocation in PRISM_init_comp routine!'
      ig_clim_state(:)=0
      ALLOCATE (ig_def_state(il_CLIM_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in ig_def_state allocation in PRISM_init_comp routine!'
      ig_def_state(:)=0
      ALLOCATE (ig_clim_trans(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in ig_clim_trans allocation in PRISM_init_comp routine!'
      ig_clim_trans(:)=0
      ALLOCATE (ig_clim_numlab(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in ig_clim_numlab allocation in PRISM_init_comp routine!'
      ig_clim_numlab(:)=0
      ALLOCATE (ig_def_trans(il_CLIM_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in ig_def_trans allocation in PRISM_init_comp routine!'
      ig_def_trans(:)=0
      ALLOCATE (cg_clim_inpfile(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in ig_clim_inpfile allocation in PRISM_init_comp routine!'
      cg_clim_inpfile(:)=' '
      ALLOCATE (cg_def_inpfile(il_CLIM_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in ig_def_inpfile allocation in PRISM_init_comp routine!'
      cg_def_inpfile(:)=' '
      ALLOCATE (cg_ignout_field(il_CLIM_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in cd_ignout_field allocation in PRISM_init_comp routine!'
      cg_ignout_field(:)=' ' 
      ALLOCATE (ig_def_numlab(il_clim_Maxport), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in ig_def_numlab allocation in PRISM_init_comp routine!'
      ig_def_numlab(:)=0
      ALLOCATE (cga_clim_locatorbf(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in cga_clim_locatorbf allocation in PRISM_init_comp!'
      cga_clim_locatorbf(:)='    '
      ALLOCATE (cga_clim_locatoraf(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in cga_clim_locatoraf allocation in PRISM_init_comp!'
      cga_clim_locatoraf(:)='    '
      ALLOCATE (cga_clim_locator(ig_clim_nfield), stat=il_err)
      IF (il_err.ne.0) WRITE(0,*)
     $ 'Error in cga_clim_locator allocation in PRISM_init_comp!'
      cga_clim_locator(:)='    '
c
#if defined use_comm_MPI1 || defined use_comm_MPI2 
      CALL MPI_Recv ( rl_work, il_size, MPI_PACKED, 0,
     $     itagcol+2, mpi_comm, mpi_status, mpi_err )
c
      iposbuf = 0
      call MPI_Unpack (rl_work, il_size, iposbuf, cgroup, 8,
     $    MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_ntime, 1,
     $    MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf,kbcplproc, knmods,
     $    MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, kbtotproc, knmods,
     $    MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, iga_unitmod, knmods,
     $    MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_frqmin, 1,
     $    MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, cg_cnaminp, 
     $    8*ig_clim_nfield,MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, cg_cnamout, 
     $    8*ig_clim_nfield,MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_clim_lag, 
     $    ig_clim_nfield,MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_clim_reverse, 
     $    ig_clim_nfield,MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_clim_invert, 
     $    ig_clim_nfield,MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_clim_freq, 
     $    ig_clim_nfield,MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_clim_seq, 
     $    ig_clim_nfield,MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, cg_clim_rstfile, 
     $    8*ig_clim_nfield,MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_clim_norstfile, 
     $    ig_clim_nfield,MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_nbr_rstfile, 1, 
     $     MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, lg_ncdfrst, 1, 
     $     MPI_LOGICAL, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, lg_oasis_field, 1, 
     $     MPI_LOGICAL, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_clim_state, 
     $     ig_clim_nfield, MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_clim_trans, 
     $     ig_clim_nfield, MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, cg_clim_inpfile, 
     $     32*ig_clim_nfield, MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_inidate, 
     $     6, MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, ig_clim_numlab, 
     $     ig_clim_nfield, MPI_INTEGER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, cg_clim_cgrdnam, 
     $    5, MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, cg_clim_lonsuf, 
     $    4, MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, cg_clim_latsuf, 
     $    4, MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, crn_clim_lonsuf, 
     $    4, MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, crn_clim_latsuf, 
     $    4, MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, cga_clim_locatorbf, 
     $    4*ig_clim_nfield, MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, cga_clim_locatoraf, 
     $    4*ig_clim_nfield, MPI_CHARACTER, mpi_comm, info)
      call MPI_Unpack (rl_work, il_size, iposbuf, lg_clim_bsend, 1, 
     $     MPI_LOGICAL, mpi_comm, info)
#elif defined use_comm_GSIP
c
c*    Read input file names in channel
c
      il_errgsip = mgi_read 
     $    (ig_gsipr, cg_clim_inpfile, 32*ig_clim_nfield, 'C')
      IF (il_errgsip .ge. 0) then
          WRITE(*,*)
     $        'prism_init_comp - GSIP cg_clim_inpfile OK:', il_errgsip 
      ELSE
          WRITE(*,*) 'GSIP Error is: ', il_errgsip
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'GSIP cg_clim_inpfile NOT OK')
      ENDIF
c
c     Allocate INTEGER array used to receive initial information
c
      il_iwk_size = 9 + 3*knmods + 9*ig_clim_nfield + 3
      ALLOCATE (ila_work(il_iwk_size), stat=il_err)
      IF (il_err.ne.0) WRITE(*,*)'Error in ila_work allocation'
      ila_work(:) = 0
c
c     Read INTEGER array 
c
      il_errgsip = mgi_read (ig_gsipr, ila_work, il_iwk_size, 'I')
      IF (il_errgsip .ge. 0) then
c          WRITE(*,*) 'GSIP- ila_work received OK:', ila_work(:) 
          WRITE(*,*) 'GSIP- ila_work received OK:', il_errgsip 
      ELSE
          WRITE(*,*) 'GSIP Error is: ', il_errgsip
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'GSIP - ila_work NOT received OK')
      ENDIF
c
c     Put INTEGER array content in variables
c
      il_st = 1
      il_en = il_st
      ig_ntime = ila_work(il_st)
      il_st = il_en + 1
      il_en = il_st + knmods - 1
      kbcplproc(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + knmods - 1
      kbtotproc(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + knmods - 1
      iga_unitmod(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st 
      ig_frqmin = ila_work(il_st)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ig_clim_lag(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ig_clim_reverse(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ig_clim_invert(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ig_clim_freq(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ig_clim_seq(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ig_clim_norstfile(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st
      ig_nbr_rstfile = ila_work(il_st)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ig_clim_state(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ig_clim_trans(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + 6 - 1
      ig_inidate(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ig_clim_numlab(:) = ila_work(il_st:il_en)
      il_st = il_en + 1
      lg_ncdfrst = .false.
      IF (ila_work(il_st) .eq. 1) lg_ncdfrst = .true.
      il_st = il_st + 1
      lg_oasis_field = .false.
      IF (ila_work(il_st) .eq. 1) lg_oasis_field = .true.
      il_st = il_st + 1
      lg_clim_bsend = .false.
      IF (ila_work(il_st) .eq. 1) lg_clim_bsend = .true.
      WRITE(*,*)'GSIP il_iwk_size il_st =', il_iwk_size, il_st
c
c     Allocate CHARACTER array used to receive initial information
c     
      il_cwk_size = 6 + 5*ig_clim_nfield 
      ALLOCATE (cla_work(il_cwk_size), stat=il_err)
      IF (il_err.ne.0) WRITE(*,*)'Error in cla_work allocation'
      cla_work(:) = ' '   
c
c     Read CHARACTER array 
c
      il_errgsip = mgi_read (ig_gsipr, cla_work, 8*il_cwk_size, 'C')
      IF (il_errgsip .ge. 0) THEN
c          WRITE(*,*) 'GSIP- cla_work received OK:', cla_work
          WRITE(*,*) 'GSIP- cla_work received OK:', il_errgsip
      ELSE
          WRITE(*,*) 'GSIP Error is: ', il_errgsip
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'GSIP - cla_work NOT received OK')
      ENDIF
c
c     Put CHARACTER array content in variables
      il_st = 1
      il_en = il_st
      cgroup = cla_work(il_st)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      cg_cnaminp(:) = cla_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      cg_cnamout(:) = cla_work(il_st:il_en)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      cg_clim_rstfile(:) = cla_work(il_st:il_en)
      il_st = il_en + 1
      cg_clim_cgrdnam = trim(adjustl(cla_work(il_st)))
      il_st = il_st + 1
      cg_clim_lonsuf = trim(adjustl(cla_work(il_st)))
      il_st = il_st + 1
      cg_clim_latsuf = trim(adjustl(cla_work(il_st)))
      il_st = il_st + 1
      crn_clim_lonsuf = trim(adjustl(cla_work(il_st)))
      il_st = il_st + 1
      crn_clim_latsuf = trim(adjustl(cla_work(il_st)))
      il_st = il_st + 1
      il_en = il_st + ig_clim_nfield - 1
      DO ji = 1, ig_clim_nfield 
        cga_clim_locatorbf(ji) = trim(adjustl(cla_work(il_st+ji-1)))
      END DO
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      DO ji = 1, ig_clim_nfield 
        cga_clim_locatoraf(ji) = trim(adjustl(cla_work(il_st+ji-1)))
      END DO
      WRITE(*,*)'GSIP il_cwk_size il_en =', il_cwk_size, il_en
c
      DEALLOCATE (cla_work)
c
#endif
      il_maxcplproc = 1
      DO ji = 1, knmods
        IF (il_maxcplproc.lt.kbcplproc(ji)) 
     $      il_maxcplproc = kbcplproc(ji)
      END DO
c
      ig_CLIMmax = 3 + CLIM_Clength + il_CLIM_Maxport * 
     $    (CLIM_Clength + 5 + CLIM_ParSize)
c     
c*    2.1 open trace file 
c     -------------------
c     
      iost = 0
      nulprt = iga_unitmod(il_mynummod)
      INQUIRE (nulprt, OPENED = ll_file)
      DO WHILE (ll_file)
         nulprt = nulprt + 1 
         INQUIRE (nulprt, OPENED = ll_file)
      END DO
#if defined use_libMPI || defined use_comm_MPI1 || defined use_comm_MPI2
      CALL MPI_Comm_Rank(ig_local_comm, il_logrank, mpi_err)
#else
      il_logrank = 0
#endif
      IF(il_logrank .le. 9) THEN
         WRITE(cnaprt, FMT='(A,''.prt'',I1)') cdnam, il_logrank 
      ELSE IF (il_logrank .le. 99) THEN
         WRITE(cnaprt, FMT='(A,''.prt'',I2)') cdnam, il_logrank 
      ELSE IF (il_logrank .le. 999) THEN
         WRITE(cnaprt, FMT='(A,''.prt'',I3)') cdnam, il_logrank 
      ELSE IF (il_logrank .le. 9999) THEN
         WRITE(cnaprt, FMT='(A,''.prt'',I4)') cdnam, il_logrank
      ELSE IF (il_logrank .gt. 99999) THEN
         WRITE(0, *)'Cannot create the name of the trace file'
         WRITE(0, *)'if more than 99999 processes for the model'
         CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $       'ABORT - Cannot create the name of the trace file ')
      ENDIF
      OPEN (UNIT=nulprt, FILE=cnaprt, STATUS='UNKNOWN',
     *     FORM='FORMATTED', ERR=110, IOSTAT=iost)
c     
 110  CONTINUE
      IF (iost.ne.0) THEN
          WRITE(*,*) ' iost= ', iost, ' nulprt = ', nulprt, 
     $        ' cnaprt = ', cnaprt
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'ABORT in Init - unable to open trace file')
      ENDIF
C
C*    2.2 Define ncplprocs, the total number of processes involved
C*    in the coupling, initialize il_CLIM_MaxLink
C     ------------------------------------------------------------
c
c     For oasis monoprocessor and involved in the coupling : ncplprocs = 1
      IF (lg_oasis_field) THEN
         ncplprocs=1
      ELSE
         ncplprocs=0
      ENDIF
      DO 3 ji = 1, knmods
        ncplprocs = ncplprocs + kbcplproc(ji)
 3    CONTINUE 
      WRITE(nulprt,*)'Init - - ncplprocs = ', ncplprocs
      IF (lg_oasis_field) THEN
         il_start = 0
         il_end = ncplprocs-1
      ELSE
         il_start = 1
         il_end = ncplprocs
      ENDIF
      il_CLIM_MaxLink = ncplprocs * il_CLIM_Maxport
c
C*    2.3 Allocate and define modtid, the vector giving, 
C*    for each process involved in the coupling, its number in mpi_com 
C*    (i.e in all model processes involved OR NOT in the coupling) 
C*    For MPI1/MPI2 only
C     -----------------------------------------------------------
c
#if defined use_comm_MPI1 || defined use_comm_MPI2
      ALLOCATE (modtid(il_start:il_end), stat=il_err)
      IF (il_err.ne.0) WRITE(nulprt,*)
     $    'Error in modtid allocation in PRISM_init_comp routine!'
      modtid(:)=0

      DO 10 ip = il_start, il_end
        modtid(ip) = -1
 10   CONTINUE
c
c     For coupler
      iprcou = 0
      imodst = 0
      IF (lg_oasis_field) modtid(0) = 0
c     For models
      DO 5 ji = 1, knmods
        IF (ji .eq. 1) THEN
           imodst = 1
        ELSE
           imodst = imodst + kbtotproc(ji-1)
        ENDIF
        iprmod = 0
        DO 7 jj = 1, kbcplproc(ji)
          iprcou = iprcou + 1
          iprmod = iprmod + 1
          modtid(iprcou) = imodst + iprmod - 1
 7      CONTINUE
 5    CONTINUE
#endif
c
C     2.4 Allocate and initialize arrays defined in mod_comprism module
c     --------------------------------------------------------------
c
#if defined use_comm_MPI1 || defined use_comm_MPI2
      CALL MPI_Comm_Rank(mpi_comm,il_rank,mpi_err)
c
      DO ji = 1, il_end
        IF (il_rank.eq.modtid(ji)) THEN
            ALLOCATE (cnames(il_start:il_end), stat=il_err)
            IF (il_err.ne.0) WRITE(nulprt,*)
     $         'Error in cnames allocation in PRISM_init_comp routine!'
            cnames(il_start:il_end)=' '
#endif 
            ALLOCATE (myport(5+il_maxcplproc,il_CLIM_Maxport), 
     $          stat = il_err)
            IF (il_err.ne.CLIM_Ok) WRITE(nulprt,*)
     $          ' Problem in myport allocation in PRISM_init_comp!'
            myport(:,:)=0
            ALLOCATE (mydist(CLIM_ParSize, il_CLIM_Maxport), 
     $          stat = il_err)
            IF (il_err.ne.CLIM_Ok) WRITE(nulprt,*)
     $          ' Problem in mydist allocation in PRISM_init_comp!'
            mydist(:,:)=0
            ALLOCATE (cports(il_CLIM_Maxport), stat = il_err)
            IF (il_err.ne.CLIM_Ok) WRITE(nulprt,*)
     $          ' Problem in cports allocation in PRISM_init_comp!'
            cports(:)=' '
            ALLOCATE(clrport(il_CLIM_Maxport), stat = il_err)
            IF (il_err.ne.CLIM_Ok) WRITE(nulprt,*)
     $          'Error in clrport allocation in PRISM_init_comp!'
            clrport(:)=' '
            ALLOCATE(irdist(CLIM_ParSize, il_CLIM_Maxport), 
     $          stat = il_err)
            IF (il_err.ne.0) WRITE(nulprt,*)
     $          'Error in irdist allocation in PRISM_init_comp'
            irdist(:,:)=0
            ALLOCATE(irport(5,il_CLIM_Maxport ), stat = il_err)
            IF (il_err.ne.0) WRITE(nulprt,*)
     $          'Error in irport allocation in PRISM_init_comp'
            irport(:,:)=0
            ALLOCATE (mylink(4+CLIM_ParSize, il_CLIM_MaxLink), 
     $          stat = il_err)
            IF (il_err.ne.CLIM_Ok) WRITE(nulprt,*)
     $          ' Problem in mylink allocation in PRISM_init_comp!'
            mylink(:,:)=0
#if defined use_comm_MPI1 || defined use_comm_MPI2
            ALLOCATE (pkwork(ig_CLIMmax), stat = il_err)
            IF (il_err.ne.0) WRITE(nulprt,*)
     $          'Error in pkwork allocation in PRISM_init_comp'
            pkwork(:)=0
#endif
            ALLOCATE (ig_def_part(CLIM_ParSize, ig_clim_nfield), 
     $          stat = il_err)
            IF (il_err.ne.CLIM_Ok) WRITE(nulprt,*)
     $        ' Problem in ig_def_part allocation in PRISM_init_comp!'
            ig_def_part(:,:)=0
            ALLOCATE (ig_length_part(ig_clim_nfield), stat = il_err)
            IF (il_err.ne.CLIM_Ok) WRITE(nulprt,*)
     $         ' Problem in ig_def_part allocation in PRISM_init_comp!'
            ig_length_part(:)=0
c
#if defined use_comm_MPI1 || defined use_comm_MPI2
        ENDIF
      END DO
#endif
c
c
C*    2.5 Define mynum, the number of the actual processor 
c     in all processors involved in the coupling 
c     If actual process is NOT involved in the coupling
c     it will NOT call CLIM_Start (for MPI1/MPI2 only)
C     -----------------------------------------------------------
c
#if defined use_comm_MPI1 || defined use_comm_MPI2
      CALL MPI_Comm_Size(ig_local_comm, mpi_size, mpi_err)
      CALL MPI_Comm_Rank(ig_local_comm, mpi_rank, mpi_err)
c
c     For model with il_mynummod=1, imodst=1
      imodst = 1
      IF (il_mynummod .gt. 1) then
          DO 8 ji = 2, il_mynummod
            imodst = imodst +  kbcplproc(ji-1)
            WRITE(nulprt,*)'imodst :',imodst
 8        CONTINUE
      ENDIF
c
      mynum = imodst + mpi_rank      
C
      WRITE(nulprt,*)'Init - - mynum = ', mynum
      WRITE(nulprt,*)'Init - - modtid() = ', modtid
      DEALLOCATE(rl_work)
#endif
      WRITE(nulprt,*)'Init - - mynummod = ', il_mynummod
      cg_modnam(il_mynummod) = cdnam
c
#if !defined key_noIO
      call psmile_io_init_comp(il_err)
#endif
c
C*    2.6 Initialization of grids writing
C     -----------------------------------
c
c-- Receive flag 'grids_start' stating whether or not grids writing is needed
c
      WRITE (nulprt,*) 'Recv - grids_start'
#if defined use_comm_MPI1 || defined use_comm_MPI2 
      CALL MPI_Recv (grids_start, 1, MPI_INTEGER, 0, itagcol+3,
     $     mpi_comm, mpi_status, mpi_err)
      IF (mpi_err == MPI_SUCCESS) THEN 
         WRITE(nulprt,*) 'Recv - <from:0> <comm:',mpi_comm,'> <len:1>
     $        <type:',MPI_INTEGER,'> <tag:',itagcol,'+3> ::  ',
     $        grids_start
      ELSE
         WRITE (nulprt,*) ' '
         WRITE (nulprt,*) 'prism_init_comp: an error occured'
         WRITE (nulprt,*) 'prism_init_comp: err= ', mpi_err 
         WRITE (nulprt,*) 'prism_init_comp: STOP'
         call MPI_ABORT (mpi_comm, 0, mpi_err)
      ENDIF
#elif defined use_comm_GSIP
      il_errgsip = mgi_read (ig_gsipr, grids_start, 1, 'I')
      IF (il_errgsip .ge. 0) THEN
          WRITE(nulprt,*) 'GSIP- grids_start received OK:', grids_start
      ELSE
          WRITE(nulprt,*) 'GSIP- grids_start NOT received OK:', 
     $        il_errgsip
          CALL prism_abort_proto (ig_mynummod, 'prism_init_comp_proto',
     $        'GSIP grids_start not read OK from channel')
      ENDIF
#endif
c
c     3. Normal EXIT
c
 1010 CONTINUE
      
      WRITE (nulprt,FMT='(A)') '-- END prism_init_comp_proto -- '
      CALL FLUSH(nulprt)
      RETURN
c
c     4. Error STOP
c
#if defined use_comm_MPI1 || defined use_comm_MPI2 
 215  CONTINUE
      WRITE (UNIT = nulprt,FMT = *) '        ***WARNING***'
      WRITE (UNIT = nulprt,FMT = *)
     $    ' Problem with MPI_Comm_Split function !!! '
      WRITE (UNIT = nulprt,FMT = *) ' Mpi error code = ',mpi_err
      WRITE (UNIT = nulprt,FMT = *) ' '
      WRITE (UNIT = nulprt,FMT = *) ' '
      WRITE (UNIT = nulprt,FMT = *) 'STOP in PRISM_init_comp'
      call MPI_ABORT (mpi_comm, 0, mpi_err)
#endif
C
      RETURN
      END


