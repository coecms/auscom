       SUBROUTINE CLIM_Init_Oasis(cdexp,cdnam,knmods,
     &                     cdmodnam, kbcplproc, kbtotproc,cdmpiarg, 
     &                     ida_unitmod, ktrout,kinfo)
c
c     *** Init ***   CLIM/GSIP
c
c     purpose:
c     --------
c        initialises GSIP communication
c
c     interface:
c     ----------
c        cdexp  : experience descriptor
c        cdnam  : name of the calling model
c	 knmods	: number of models to be launched
c        cdmodnam : names of models to be launched 
c        kbcplproc : number of processes implied in the coupling
c                    for each model to be launched
c        kbtotproc : total number of procs for each model to be launched
c        cdmpiarg: nothing significant for MPI1 and launching argument 
c                  for each model to be launched for MPI2
c        ida_unitmod: minimum logfile unit number for each model
c        ktrout : unit number for trace file
c	 kinfo	: output status
c
c     lib:
c     ----
c        GSIP
c
c     author:
c     -------
c       Sophie Valcke  (22/10/04 -created from lib/clim/src/CLIM_Init_Oasis)
c
c     ----------------------------------------------------------------
c
#if defined use_comm_GSIP
      USE mod_kinds_oasis
      USE mod_clim
      USE mod_comclim
      USE mod_parameter
      USE mod_timestep
      USE mod_string
      USE mod_unitncdf
      USE mod_hardware
      USE mod_calendar
      USE mod_label
      USE mod_gsip
      IMPLICIT NONE
#ifdef use_netCDF
#include <netcdf.inc>
#endif
c     ----------------------------------------------------------------
      CHARACTER*(*) cdexp, cdnam
      INTEGER (kind=ip_intwp_p)	    knmods, ktrout, kinfo
      CHARACTER*6   cdmodnam(knmods)
      INTEGER (kind=ip_intwp_p)     kbtotproc(knmods), 
     $    kbcplproc(knmods), ida_unitmod(knmods)
      CHARACTER*80  cdmpiarg(knmods)
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)     iost, ip, mynummod, il_maxcplproc
      INTEGER (kind=ip_intwp_p)     il_CLIM_Maxport, il_CLIM_MaxLink
      INTEGER (kind=ip_intwp_p)     ji, jl, jn, il_err
      INTEGER (kind=ip_intwp_p)     il_cwk_size, il_iwk_size
      INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  ila_work
      CHARACTER*8, DIMENSION(:), ALLOCATABLE :: cla_work
      INTEGER(kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: iarr_err
      INTEGER(kind=ip_intwp_p) :: integer_byte_size, ii, io_size, 
     $    integer_io_size, il_int, il_log
      LOGICAL :: ll_log
      INTEGER (kind=ip_intwp_p)  grids_start, grids_done
      INTEGER (kind=ip_intwp_p)  ilen 
      CHARACTER*8                cgrdnamnc 
      LOGICAL                    existent
c     GSIP specific declarations
      INTEGER :: il_mil, il_cen, il_diz, il_uni, il_errgsip, il_loc
      INTEGER :: il_st, il_en, jch
      CHARACTER(LEN=1) :: cl_mil, cl_cen, cl_diz, cl_uni
      CHARACTER(LEN=40) :: cl_channam
      INTEGER :: mgi_init, mgi_open, mgi_write, mgi_read
      CHARACTER*4 :: cgrid                      ! grid acronym
      CHARACTER*8 :: clon                       !locator string 
      CHARACTER*8 :: clat                       !locator string
      CHARACTER*8 :: crnlon                     !locator string 
      CHARACTER*8 :: crnlat                     !locator string 
      CHARACTER*8 :: cmsk                       !locator string
      CHARACTER*8 :: csrf                       !locator string 
      INTEGER(kind=ip_intwp_p) :: nx, ny, nc    ! number of lon lat corners
      REAL(kind=ip_realwp_p), DIMENSION(:,:),  ALLOCATABLE :: lon, lat
      REAL(kind=ip_realwp_p), DIMENSION(:,:,:),ALLOCATABLE :: crnrlon
      REAL(kind=ip_realwp_p), DIMENSION(:,:,:),ALLOCATABLE :: crnrlat
      INTEGER(kind=ip_intwp_p), DIMENSION(:,:),ALLOCATABLE :: mask
      REAL(kind=ip_realwp_p), DIMENSION(:,:),ALLOCATABLE :: area
      INTEGER(kind=ip_intwp_p) :: nulgrd ! logical unit of grids file
      INTEGER(kind=ip_intwp_p) :: stat ! netcdf status
      INTEGER(kind=ip_intwp_p) :: ncid ! netcdf file id
      INTEGER(kind=ip_intwp_p) :: idx, idy ! netcdf dimension ids
      INTEGER(kind=ip_intwp_p) :: idlon, idlat ! netcdf variable ids
      INTEGER(kind=ip_intwp_p) :: dims(2) ! netcdf variable dimensions
      INTEGER(kind=ip_intwp_p) :: crndims(3) ! corner netcdf dimensions
      INTEGER(kind=ip_intwp_p) :: il_nftype ! netcdf TYPE
      INTEGER(kind=ip_intwp_p) :: idmsk, idsrf ! netcdf variable id
      INTEGER(kind=ip_intwp_p) :: idclon, idclat, idc ! netcdf variable ids
c     --------------------------------------------------------------
c
c*    0.0 Initializations
c     -----------------------------------------
c
      kinfo  = CLIM_Ok
      cgroup=' '
      cgroup=cdexp
      cmynam=' '
      cmynam=cdnam
c
      nports = 0
      nlinks = 0
c
c     1.0 open trace file 
c     -------------------
c
      iost = 0
      nulprt = ktrout
      WRITE (cnaprt,FMT='(A,''.prt'')') cdnam
      OPEN (UNIT=nulprt, FILE=cnaprt, STATUS='UNKNOWN', 
     $     FORM='FORMATTED', ERR=110, IOSTAT=iost)
c
  110 CONTINUE
      IF (iost.ne.0) THEN
         WRITE(6,*) 'Abort in Init_Oasis - unable to open trace file '
         WRITE(6,*) iost, ' ', nulprt, ' ', cnaprt
         STOP
      ENDIF
c
      WRITE (nulprt, FMT='(A)')   'Init_Oasis - CLIM/GSIP'
      WRITE (nulprt, FMT='(A,A)') 'Init_Oasis - Exp name: ', cgroup
      WRITE (nulprt, FMT='(A,A)') 'Init_Oasis - Model name: ', cdnam
      CALL flush(nulprt)
c
      ig_nmods = knmods
      ALLOCATE (iga_totproc(ig_nmods),  stat=il_err)
      IF (il_err.ne.0) THEN
          WRITE(nulprt,*) 'Error in iga_nbtotproc allocation'
          CALL FLUSH(nulprt)
          STOP
      ENDIF
c
      iga_totproc(:) = kbtotproc(:)
c
c     1.1 Prepare initial information to be send to PSMILe 
c     ----------------------------------------------------
c
      ig_clim_nfield = ig_total_nfield
c
c     Array of integers
c
c     Check if a INTEGER can contain a LOGICAL as ila_work
c     will contain logicals
c
      integer_byte_size = BIT_SIZE(ii)/8
      INQUIRE (iolength=io_size) ii
      integer_io_size = io_size
      il_int = io_size/integer_io_size*integer_byte_size
      INQUIRE (iolength=io_size) ll_log
      il_log = io_size/integer_io_size*integer_byte_size
      IF (il_log .gt. il_int) THEN
          WRITE(UNIT = nulprt,FMT = *)
     $        'Init_Oasis - ila_work not be big enough for a logical'
          CALL FLUSH(nulprt)
          STOP
      ENDIF
c
      il_iwk_size = 9 + 3*knmods + 9*ig_clim_nfield + 3
      ALLOCATE (ila_work(il_iwk_size), stat=il_err)
      IF (il_err.ne.0) THEN
          WRITE(nulprt,*)'Error in ila_work allocation'
          CALL FLUSH(nulprt)
          STOP
      ENDIF
      ila_work(:) = 0
c
      il_st = 1
      il_en = il_st
      ila_work(il_st:il_en) = ntime
      il_st = il_en + 1
      il_en = il_st + knmods - 1
      ila_work(il_st:il_en) = kbcplproc(:)
      il_st = il_en + 1
      il_en = il_st + knmods - 1
      ila_work(il_st:il_en) = kbtotproc(:)      
      il_st = il_en + 1
      il_en = il_st + knmods - 1
      ila_work(il_st:il_en) = ida_unitmod(:)
      il_st = il_en + 1
      il_en = il_st 
      ila_work(il_st:il_en) = ig_total_frqmin
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ila_work(il_st:il_en) = ig_lag(:)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ila_work(il_st:il_en) = ig_reverse(:)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ila_work(il_st:il_en) = ig_invert(:)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ila_work(il_st:il_en) = ig_freq(:) 
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ila_work(il_st:il_en) = ig_total_nseqn(:)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ila_work(il_st:il_en) = ig_no_rstfile(:)   
      il_st = il_en + 1
      il_en = il_st 
      ila_work(il_st:il_en) = ig_nbr_rstfile
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ila_work(il_st:il_en) = ig_total_state(:)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ila_work(il_st:il_en) = ig_local_trans(:)
      il_st = il_en + 1
      il_en = il_st + 6 - 1
      ila_work(il_st:il_en) = ig_date(:)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      ila_work(il_st:il_en) = ig_numlab(:)
      il_st = il_en + 1
      ila_work(il_st) = 0
      IF (lncdfrst) ila_work(il_st) = 1
      il_st = il_st + 1
      ila_work(il_st) = 0
      IF (lg_oasis_field) ila_work(il_st) = 1
      il_st = il_st + 1
      ila_work(il_st) = 0
      IF(lg_bsend) ila_work(il_st) = 1
      WRITE(nulprt,*)'GSIP il_iwk_size il_st =', il_iwk_size, il_st
c
c     Array of characters
c     
      il_cwk_size = 6 + 5*ig_clim_nfield 
      ALLOCATE (cla_work(il_cwk_size), stat=il_err)
      IF (il_err.ne.0) THEN
          WRITE(nulprt,*)'Error in cla_work allocation'
          CALL FLUSH(nulprt)
          STOP
      ENDIF
      cla_work(:) = ' '
c
      il_st = 1
      il_en = il_st
      cla_work(il_st:il_en) = cgroup
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      cla_work(il_st:il_en) = cg_input_field(:)  
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      cla_work(il_st:il_en) = cg_output_field(:)     
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      cla_work(il_st:il_en) = cg_restart_file(:)
      il_st = il_en + 1
      il_en = il_st
      cla_work(il_st:il_en) = cgrdnam
      il_st = il_en + 1
      il_en = il_st
      cla_work(il_st:il_en) = cglonsuf
      il_st = il_en + 1
      il_en = il_st
      cla_work(il_st:il_en) = cglatsuf
      il_st = il_en + 1
      il_en = il_st
      cla_work(il_st:il_en) = crnlonsuf
      il_st = il_en + 1
      il_en = il_st
      cla_work(il_st:il_en) = crnlatsuf
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      cla_work(il_st:il_en) = cga_locatorbf(:)
      il_st = il_en + 1
      il_en = il_st + ig_clim_nfield - 1
      cla_work(il_st:il_en) = cga_locatoraf(:)
c
c
c     2.0 Init/open two GSIP channels for each process of each model
c     --------------------------------------------------------------
c
      jl = 0
      DO ji = 1, knmods
        DO jn =  1, kbtotproc(ji)
          jl = jl + 1
          il_mil = jn/1000
          cl_mil=ACHAR(il_mil + 48)
          il_cen = (jn - il_mil*1000)/100
          cl_cen=ACHAR(il_cen + 48)
          il_diz = (jn - il_mil*1000 - il_cen*100)/10
          cl_diz=ACHAR(il_diz + 48)
          il_uni = jn - il_mil*1000 -il_cen*100 - il_diz*10
          cl_uni=ACHAR(il_uni + 48)
c
c         Build and init writing channel name
c
          cl_channam = trim(adjustl(
     $        cdmodnam(ji)))//'W'//cl_mil//cl_cen//cl_diz//cl_uni
          iga_gsipw(jl) = mgi_init (cl_channam)
          IF (iga_gsipw(jl) .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)'GSIP Channel Init OK:',
     $            cl_channam, iga_gsipw(jl)
          ELSE
              WRITE(UNIT = nulprt,FMT = *)'GSIP Channel Init NOT OK:', 
     $            cl_channam, iga_gsipw(jl)
              CALL FLUSH(nulprt)
              STOP
          ENDIF
c
c         Build and init reading channel name
c
          cl_channam = trim(adjustl(
     $        cdmodnam(ji)))//'R'//cl_mil//cl_cen//cl_diz//cl_uni
          iga_gsipr(jl) = mgi_init (cl_channam)
          IF (iga_gsipr(jl) .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)'GSIP Channel Init OK:', 
     $            cl_channam, iga_gsipr(jl)
          ELSE
              WRITE(UNIT = nulprt,FMT = *)'GSIP Channel Init NOT OK:', 
     $            cl_channam, iga_gsipr(jl)
              CALL FLUSH(nulprt)
              STOP
          ENDIF
c
c         Open writing channel name
c
          il_errgsip = mgi_open (iga_gsipw(jl),'W')
          IF (il_errgsip .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP Channel Open OK:', iga_gsipw(jl)
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP Channel Open NOT OK:',iga_gsipw(jl)
              CALL FLUSH(nulprt)
              STOP
          ENDIF
c
c         Open reading channel name
c
          il_errgsip = mgi_open (iga_gsipr(jl),'R')
          IF (il_errgsip .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP Channel Open OK:', iga_gsipr(jl)
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP Channel Open NOT OK:',iga_gsipr(jl)
              CALL FLUSH(nulprt)
              STOP
          ENDIF
c
c         Initial exchange of information
c
c         Write number of models in channel
c
          il_errgsip = mgi_write (iga_gsipw(jl), knmods, 1, 'I')
          IF (il_errgsip .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP number of models OK:', knmods
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP number of models NOT OK:', knmods
             WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP error code is:', il_errgsip
              CALL FLUSH(nulprt)
              STOP
          ENDIF 
c
c         Write model number in channel
c
          il_errgsip = mgi_write (iga_gsipw(jl), ji, 1, 'I')
          IF (il_errgsip .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP model number OK:', ji
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP model number NOT OK:', ji
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP error code is:', il_errgsip
             CALL FLUSH(nulprt)
             STOP
          ENDIF
c 
c         Write number of fields in channel
c
          il_errgsip = mgi_write (iga_gsipw(jl),ig_clim_nfield,1,'I')
          IF (il_errgsip .ge. 0) THEN
              WRITE(UNIT = nulprt,FMT = *)'Init_Oasis - ',
     $            'GSIP number of models OK: ', ig_clim_nfield
          ELSE
              WRITE(UNIT = nulprt,FMT = *)'Init_Oasis - ',
     $            'GSIP number of models NOT OK:', ig_clim_nfield
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP error code is:', il_errgsip
             CALL FLUSH(nulprt)
             STOP
          ENDIF
c
c         Write input file names in channel
c
          il_errgsip = mgi_write 
     $       (iga_gsipw(jl), cg_input_file, 32*ig_clim_nfield, 'C')
          IF (il_errgsip .ge. 0) THEN
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP input file names OK:', il_errgsip
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP input file names NOT OK:'
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP error code is:', il_errgsip
              CALL FLUSH(nulprt)
              STOP
          ENDIF
c
c         Write ila_work in channel
c
          il_errgsip = mgi_write 
     $       (iga_gsipw(jl), ila_work, il_iwk_size, 'I')
          IF (il_errgsip .ge. 0) THEN
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP ila_work OK:', il_errgsip
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP ila_work NOT OK:'
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP error code is:', il_errgsip
              CALL FLUSH(nulprt)
              STOP
          ENDIF
c
c         Write cla_work in channel
c
          il_errgsip = mgi_write 
     $       (iga_gsipw(jl), cla_work, 8*il_cwk_size, 'C')
          IF (il_errgsip .ge. 0) THEN
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP cla_work OK:', il_errgsip
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP cla_work NOT OK:'
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP error code is:', il_errgsip
              CALL FLUSH(nulprt)
              STOP
          ENDIF
c
        END DO
      END DO
c
      DEALLOCATE (cla_work)
c
c     Maximum number of ports per process
      il_CLIM_Maxport = 2 * ig_clim_nfield
c     Minimum coupling period (sec)
      ig_frqmin = ig_total_frqmin
c     Total run time (sec)
      ig_ntime = ntime
      il_maxcplproc = 1
      DO ji = 1, knmods
        IF (il_maxcplproc.lt.kbcplproc(ji)) il_maxcplproc=kbcplproc(ji)
      END DO
c      
C*    4.1 Define ncplprocs, the total number of processes involved
C*    in the coupling, including Oasis and initialize il_CLIM_MaxLink
C     ------------------------------------------------------------
c
      ncplprocs=1
      DO 3 ji = 1, knmods
        ncplprocs = ncplprocs + kbcplproc(ji)
 3    CONTINUE
c
      il_CLIM_MaxLink = ncplprocs * il_CLIM_Maxport

C     4.2 Allocate and initialize arrays defined in comclim MODULE 
c     ------------------------------------------------------------
c
      ALLOCATE (ncode(0:ncplprocs-1), stat=il_err)
      IF (il_ERR.ne.0) WRITE(nulprt,*)'Error in "ncode" allocation in 
     $    CLIM_Init_Oasis routine ! '
      ncode(:)=0
      ALLOCATE (cnames(0:ncplprocs-1), stat=il_err)
      IF (il_ERR.ne.0) WRITE(nulprt,*)'Error in "cnames" allocation in 
     $    CLIM_Init_Oasis routine ! '
      cnames(:)=' '
c
      ALLOCATE (myport(5+il_maxcplproc,il_CLIM_Maxport), 
     $    stat = il_ERR)
      IF (il_ERR.ne.CLIM_Ok) WRITE(UNIT = nulprt,FMT = *)' Problem 
     $    in myport allocation in CLIM_Init_Oasis !'
      myport(:,:)=0
      ALLOCATE (mydist(CLIM_ParSize, il_CLIM_Maxport), 
     $    stat = il_ERR)
      IF (il_ERR.ne.CLIM_Ok) WRITE(UNIT = nulprt,FMT = *)' Problem 
     $    in mydist allocation in CLIM_Init_Oasis !'
      mydist(:,:)=0
      ALLOCATE (cports(il_CLIM_Maxport), stat = il_ERR)
      IF (il_ERR.ne.CLIM_Ok) WRITE(UNIT = nulprt,FMT = *)' Problem 
     $    in mydist allocation in CLIM_Init_Oasis !'
      cports(:)=' '
      ALLOCATE(clrport(il_CLIM_Maxport), stat = il_ERR)
      IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)'Error in clrport 
     $    allocation in CLIM_Init_Oasis !'
      clrport(:)=' '
      ALLOCATE(irdist(CLIM_ParSize, il_CLIM_Maxport), stat = il_ERR)
      IF (il_ERR.ne.0) WRITE(nulprt,*)'Error in irdist allocation 
     $    in CLIM_Init_Oasis'
      irdist(:,:)=0
      ALLOCATE(irport(5,il_CLIM_Maxport ), stat = il_ERR)
      IF (il_ERR.ne.0) WRITE(nulprt,*)'Error in irport allocation 
     $    in CLIM_Init_Oasis'
      irport(:,:)=0
      ALLOCATE (mylink(4+CLIM_ParSize,il_CLIM_MaxLink), stat = il_ERR)
      IF (il_ERR.ne.CLIM_Ok) WRITE(UNIT = nulprt,FMT = *)' Problem 
     $    in mylink allocation in CLIM_Init_Oasis !'
      mylink(:,:)=0
      DO 10 ip = 0, ncplprocs-1
        cnames(ip) = ' '
 10   CONTINUE
c
C*    4.3 Define mynum, the number of the actual processor 
c     in all processors involved in the coupling 
C     ----------------------------------------------------
c
c*    For the coupler (if mono-processor)
      mynummod = 0
      WRITE(nulprt,*)'Init_Oasis - - Oasis mynummod = ', mynummod
c
C*    4.4 Initialize writing of the grids file
C     ----------------------------------------
c
      grids_start=0
      WRITE(nulprt,*)'Init_Oasis - - check grids file:'
      INQUIRE(FILE = cgrdnam, EXIST = existent)
      IF (.NOT. existent) THEN
          cgrdnamnc=cgrdnam//'.nc'
          INQUIRE(FILE = cgrdnamnc, EXIST = existent)
          IF (.NOT. existent) THEN
              grids_start=1
              WRITE(nulprt,*) '             - no grids file found'
              lncdfgrd = .true.
          ELSE
              WRITE(nulprt,*)
     $            'Init_Oasis - - grids file "', cgrdnamnc, '" found'
              lncdfgrd = .true.
          ENDIF
      ELSE
          WRITE(nulprt,*)
     $        'Init_Oasis - - grids file "', cgrdnam, '" found'
          lncdfgrd = .false.
      ENDIF
c      
      jl = 1
      jch = 1
      DO ji = 1, knmods
c
        DO jn =  1, kbtotproc(ji)
c
c       Write starting flag (grids_start) in channel
c
          il_errgsip = mgi_write (iga_gsipw(jl), grids_start, 1, 'I')
          IF (il_errgsip .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP grids_start OK:', grids_start
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP grids_start NOT OK:', grids_start
              WRITE(UNIT = nulprt,FMT = *)
     $            'Init_Oasis - GSIP error code is:', il_errgsip
              CALL FLUSH(nulprt)
              STOP
          ENDIF
          jl = jl + 1 
        END DO
c
        IF (.NOT. existent) THEN
c
            il_loc = 0
            DO WHILE (il_loc .ne. 1)
c           Read flag indicating what will be sent by model
              il_errgsip = mgi_read (iga_gsipr(jch), il_loc, 1, 'I')
              IF (il_errgsip .ge. 0) then
                  WRITE(UNIT = nulprt, FMT = *)
     $                'Init_Oasis - Grid writing OK, il_loc =', il_loc
              ELSE
                  WRITE(UNIT = nulprt,FMT = *)
     $                'Init_Oasis - Grid writing NOT OK:', il_errgsip
                  CALL FLUSH(nulprt)
                  STOP
              ENDIF 
              IF (il_loc .eq. 1) THEN
                  WRITE(nulprt,*)
     $        'prism_terminate_grids_writing was called by model ', jch
c
              ELSE IF (il_loc .eq. 100) THEN
c                 Read grid name, dimensions, longitudes, and latitudes
c
                  il_errgsip = mgi_read (iga_gsipr(jch), cgrid, 4, 'C')
                  il_errgsip = mgi_read (iga_gsipr(jch), nx, 1, 'I')
                  il_errgsip = mgi_read (iga_gsipr(jch), ny, 1, 'I')
                  allocate (lon(nx,ny))
                  allocate (lat(nx,ny))
                  il_errgsip = mgi_read (iga_gsipr(jch),lon,nx*ny,'R')
                  il_errgsip = mgi_read (iga_gsipr(jch),lat,nx*ny,'R')
                  WRITE(nulprt,*)'Read grid from model ', jch
                  WRITE(nulprt,*)'cgrid, nx, ny= ', cgrid, nx, ny
#ifdef use_netCDF
                  il_nftype = NF_DOUBLE
                  IF (ll_single) il_nftype = NF_REAL
c                 open grids file
                  stat = NF_OPEN(cgrdnam//'.nc', NF_WRITE, ncid)
                  IF (stat /= NF_NOERR) THEN
                      stat=NF_CREATE(cgrdnam//'.nc',NF_CLOBBER,ncid)
                      IF (stat /= NF_NOERR) THEN
                          WRITE (nulprt,*) 
     $                     'CLIM_Init_Oasis: error opening grids file.'
                          WRITE (nulprt,*) 'CLIM_Init_Oasis: STOP'
                          STOP
                      ENDIF
                  ENDIF
c                 define dimensions
                  stat = NF_REDEF(ncid)
                  stat = NF_INQ_DIMID(ncid, 'x_'//cgrid, idx)
                  IF (stat .NE. NF_NOERR) THEN
                      stat = NF_DEF_DIM(ncid, 'x_'//cgrid, nx, idx)
                      IF (stat /= NF_NOERR) THEN
                          WRITE (nulprt,*)  
     $                        'ERROR defining dimension x_',cgrid
                      ENDIF
                  ELSE
                      WRITE(nulprt,*)'Grid already defined: ', cgrid
                      RETURN
                  ENDIF
                  stat = NF_DEF_DIM(ncid, 'y_'//cgrid, ny, idy)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR defining dimension y_',cgrid
                  ENDIF
                  dims(1) = idx
                  dims(2) = idy
c                 define longitudes
                  clon=cgrid//cglonsuf
                  stat = NF_DEF_VAR 
     $                (ncid, clon , il_nftype, 2, dims, idlon)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR defining variable ', clon
                  ENDIF
                  stat = NF_PUT_ATT_TEXT
     $              (ncid,idlon,'long_name',18,'Longitudes of '//cgrid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                  'ERROR creating att. for longitudes of ', cgrid
                  ENDIF
                  stat = NF_PUT_ATT_TEXT
     $                (ncid, idlon, 'units', 8, 'degree_E')
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*)
     $                  'ERROR creating att. for longitudes of ', cgrid
                  ENDIF
c                 define latitudes
                  clat=cgrid//cglatsuf
                  stat = NF_DEF_VAR 
     $                (ncid, clat , il_nftype, 2, dims, idlat)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR defining variable ', clat
                  ENDIF
                  stat = NF_PUT_ATT_TEXT
     $              (ncid, idlat,'long_name',17,'Latitudes of '//cgrid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                  'ERROR creating att. for latitudes of ', cgrid
                  ENDIF
                  stat = NF_PUT_ATT_TEXT
     $                (ncid, idlat, 'units', 8, 'degree_N')
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                   'ERROR creating att. for latitudes of ', cgrid
                  ENDIF
c                 switch to data mode
                  stat = NF_ENDDEF(ncid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR: file ', ncid, ' still in define mode'
                  ENDIF
c                 write longitudes
                  IF (ll_single) THEN
                      stat = NF_PUT_VAR_REAL (ncid, idlon, lon(:,:))
                  ELSE
                      stat = NF_PUT_VAR_DOUBLE (ncid, idlon, lon(:,:))
                  ENDIF
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR writing lonitudes of ', cgrid
                  ENDIF
c                 write latitudes
                  IF (ll_single) THEN
                      stat = NF_PUT_VAR_REAL (ncid, idlat, lat(:,:))
                  ELSE
                      stat = NF_PUT_VAR_DOUBLE (ncid, idlat, lat(:,:))
                  ENDIF
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR writing latitudes of ', cgrid
                  ENDIF
c                 close grids file
                  stat = NF_CLOSE(ncid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR closing file', cgrdnam
                  ENDIF
#else
c                 binary format
c                 -------------
c
c                 open grids file
                  INQUIRE(FILE=cgrdnam, EXIST=existent, OPENED=grdopen)
                  IF (existent .AND. grdopen) THEN
                      WRITE (nulprt,*) ' '
                      WRITE (nulprt,*) 
     $                    'CLIM_Init_Oasis: grids file already opened'
                      WRITE (nulprt,*) 'CLIM_Init_Oasis: STOP'
                      STOP     
                  ENDIF
                  iost = 0
                  nulgrd = 7
                  INQUIRE (nulgrd,OPENED = grdopen)
                  DO WHILE (grdopen)
                    nulgrd = nulgrd + 1 
                    INQUIRE (nulgrd,OPENED = grdopen)
                  END DO
                    
                  OPEN (UNIT=nulgrd, FILE=cgrdnam, STATUS='UNKNOWN',
     $                ACCESS='SEQUENTIAL', FORM='UNFORMATTED', 
     $                POSITION='APPEND',
     $                ACTION='WRITE', IOSTAT=iost, ERR=110)

 110              CONTINUE
                  IF (iost /= 0) THEN
                      WRITE (nulprt,*) ' '
                      WRITE (nulprt,*) 
     $                    'CLIM_Init_Oasis: Error opening grids file'
                      WRITE (nulprt,*) 'CLIM_Init_Oasis: STOP'
                      STOP
                  ENDIF
c                 write longitudes
                  clon=cgrid//cglonsuf
                  WRITE (UNIT = nulgrd) clon
                  WRITE (UNIT = nulgrd) lon(:,:)
c                 write latitudes     
                  clat=cgrid//cglatsuf
                  WRITE (UNIT = nulgrd) clat
                  WRITE (UNIT = nulgrd) lat(:,:)
c
c                 close grids file
                  CLOSE (nulgrd)
#endif
                  deallocate (lon)
                  deallocate (lat)
c
              ELSE IF (il_loc .eq. 300) THEN
c                 Read mask name, dimensions, and values
c
                  il_errgsip = mgi_read (iga_gsipr(jch), cgrid, 4, 'C')
                  il_errgsip = mgi_read (iga_gsipr(jch), nx, 1, 'I')
                  il_errgsip = mgi_read (iga_gsipr(jch), ny, 1, 'I')
                  allocate (mask(nx,ny))
                  il_errgsip = mgi_read (iga_gsipr(jch),mask,nx*ny,'I')
                  WRITE(nulprt,*)'Read mask from model ', jch
                  WRITE(nulprt,*)'cgrid, nx, ny= ', cgrid, nx, ny
#ifdef use_netCDF
                  il_nftype = NF_DOUBLE
                  IF (ll_single) il_nftype = NF_REAL
c                 open masks file
                  stat = NF_OPEN(cmsknam//'.nc', NF_WRITE, ncid)
                  IF (stat /= NF_NOERR) THEN
                      stat = NF_CREATE(cmsknam//'.nc',NF_CLOBBER, ncid)
                      IF (stat /= NF_NOERR) THEN
                          WRITE (nulprt,*) 
     $                    'prism_write_mask: error opening masks file.'
                          WRITE (nulprt,*) 
     $                        'prism_write_mask: STOP'
                          STOP
                      ENDIF
                  ENDIF
c                 define dimensions
                  stat = NF_REDEF(ncid)
                  stat = NF_INQ_DIMID(ncid, 'x_'//cgrid, idx)
                  IF (stat .NE. NF_NOERR) THEN
                      stat = NF_DEF_DIM(ncid, 'x_'//cgrid, nx, idx)
                      IF (stat /= NF_NOERR) THEN
                          WRITE (nulprt,*) 
     $                        'ERROR defining dimension x_',cgrid
                      ENDIF
                  ELSE
                      WRITE(nulprt,*)'Mask already defined: ', cgrid
                      RETURN
                  ENDIF
                  stat = NF_DEF_DIM(ncid, 'y_'//cgrid, ny, idy)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR defining dimension y_',cgrid
                  ENDIF

                  dims(1) = idx
                  dims(2) = idy
c                 define mask
                  cmsk = cgrid//cmsksuf
                  stat = NF_DEF_VAR(ncid, cmsk, NF_INT, 2, dims, idmsk)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR defining variable ', cmsk
                  ENDIF
                  stat = NF_PUT_ATT_TEXT
     $                (ncid,idmsk,'long_name',12,'Mask of '//cgrid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR creating att. for mask of ', cgrid
                  ENDIF
                  stat = NF_PUT_ATT_TEXT(ncid, idmsk, 'units', 1, '1')
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR creating att. for mask of ', cgrid
                  ENDIF
c                 switch to data mode
                  stat = NF_ENDDEF(ncid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR: file ', ncid, ' still in define mode'
                  ENDIF
c                 write mask
                  stat = NF_PUT_VAR_INT (ncid, idmsk, mask(:,:))
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR writing mask of ', cgrid
                  ENDIF
c                 close masks file
                  stat = NF_CLOSE(ncid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR closing file', cmsknam
                  ENDIF
#else
c                 binary format
c                 -------------
c
c                 open masks file
                  INQUIRE(FILE=cmsknam, EXIST=existent, OPENED=mskopen)
                  IF (existent .AND. mskopen) THEN
                      WRITE (nulprt,*) ' '
                      WRITE (nulprt,*) 
     $                    'prism_write_mask: masks file already opened'
                      WRITE (nulprt,*) 'prism_write_mask: STOP'
                      STOP     
                  ENDIF
                  iost = 0
                  nulmsk = 7
                  INQUIRE (nulmsk,OPENED = mskopen)
                  DO WHILE (mskopen)
                    nulmsk = nulmsk + 1 
                    INQUIRE (nulmsk,OPENED = mskopen)
                  END DO

                  OPEN (UNIT=nulmsk, FILE=cmsknam, STATUS='UNKNOWN',
     $                ACCESS='SEQUENTIAL', FORM='UNFORMATTED', 
     $                POSITION='APPEND',  ACTION='WRITE', 
     $                IOSTAT=iost, ERR=110)
 110              CONTINUE
                  IF (iost /= 0) THEN
                      WRITE (nulprt,*) ' '
                      WRITE (nulprt,*) 
     $                    'prism_write_mask: Error opening masks file'
                      WRITE (nulprt,*) 'prism_write_mask: STOP'
                      STOP
                  ENDIF
c                 write mask
                  cmsk=cgrid//cmsksuf
                  WRITE (UNIT = nulmsk) cmsk
                  WRITE (UNIT = nulmsk) mask(:,:)
c                 close mask file
                  CLOSE (nulmsk)
#endif
                  deallocate (mask)

              ELSE IF (il_loc .eq. 200) THEN
c                 Read grid name, dimensions, corner lons and lats
c
                  il_errgsip = mgi_read (iga_gsipr(jch), cgrid, 4, 'C')
                  il_errgsip = mgi_read (iga_gsipr(jch), nx, 1, 'I')
                  il_errgsip = mgi_read (iga_gsipr(jch), ny, 1, 'I')
                  il_errgsip = mgi_read (iga_gsipr(jch), nc, 1, 'I')
                  allocate (crnrlon(nx,ny,nc))
                  allocate (crnrlat(nx,ny,nc))
                  il_errgsip =
     $                mgi_read (iga_gsipr(jch),crnrlon,nx*ny*nc,'R')
                  il_errgsip =
     $                mgi_read (iga_gsipr(jch),crnrlat,nx*ny*nc,'R')
                  WRITE(nulprt,*)'Read corners from model ', jch
                  WRITE(nulprt,*)'cgrid, nx, ny= ', cgrid, nx, ny
#ifdef use_netCDF
                  il_nftype = NF_DOUBLE
                  IF (ll_single) il_nftype = NF_REAL
c                 open grids file
                  stat = NF_OPEN(cgrdnam//'.nc', NF_WRITE, ncid)
                  IF (stat /= NF_NOERR) THEN
                      stat=NF_CREATE(cgrdnam//'.nc',NF_CLOBBER,ncid)
                      IF (stat /= NF_NOERR) THEN
                          WRITE (nulprt,*) 
     $                     'CLIM_Init_Oasis: error opening grids file.'
                          WRITE (nulprt,*) 'CLIM_Init_Oasis: STOP'
                          STOP
                      ENDIF
                  ENDIF
c                 define corner dimensions
                  stat = NF_REDEF(ncid)
                  stat = NF_INQ_DIMID(ncid, 'x_'//cgrid, idx)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR finding out dimension id of x_',cgrid
                  ENDIF
                  stat = NF_INQ_DIMID(ncid, 'y_'//cgrid, idy)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR finding out dimension id of y_',cgrid
                  ENDIF
                  stat = NF_DEF_DIM(ncid, 'crn_'//cgrid, nc, idc)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR defining dimension crn_',cgrid
                  ENDIF

                  crndims(1) = idx
                  crndims(2) = idy
                  crndims(3) = idc
c                 define corner longitudes
                  crnlon=cgrid//crnlonsuf
                  stat = NF_INQ_VARID (ncid, crnlon, idclon)
                  IF (stat .NE. NF_NOERR) THEN
                      stat = NF_DEF_VAR 
     $                    (ncid, crnlon , il_nftype, 3, crndims, idclon)
                      IF (stat /= NF_NOERR) THEN
                          WRITE (nulprt,*) 
     $                        'ERROR defining variable ', crnlon
                      ENDIF
                      stat = NF_PUT_ATT_TEXT 
     $                    (ncid,idclon,'long_name',39,
     $                    'Longitudes of grid cell corners of '//cgrid)
                      IF (stat /= NF_NOERR) THEN
                          WRITE (nulprt,*) 
     $           'ERROR creating att. for corner longitudes of ', cgrid
                      ENDIF
                      stat = NF_PUT_ATT_TEXT
     $                    (ncid, idclon, 'units', 8, 'degree_E')
                      IF (stat /= NF_NOERR) THEN
                          WRITE (nulprt,*) 
     $           'ERROR creating att. for corner longitudes of ', cgrid
                      ENDIF
                  ELSE
                      WRITE(nulprt,*)'Corners already defined: ', cgrid
                      RETURN
                  ENDIF
C                 define corner latitudes
                  crnlat=cgrid//crnlatsuf
                  stat = NF_DEF_VAR 
     $                (ncid, crnlat , il_nftype, 3, crndims, idclat)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR defining variable ', crnlat
                  ENDIF
                  stat = NF_PUT_ATT_TEXT
     $                (ncid, idclat,'long_name',38,
     $                'Latitudes of grid cell corners of '//cgrid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) '
     $            ERROR creating att. for corner latitudes of ', cgrid
                  ENDIF
                  stat = NF_PUT_ATT_TEXT
     $                (ncid, idclat, 'units', 8, 'degree_N')
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $            'ERROR creating att. for corner latitudes of ', cgrid
                  ENDIF
c                 switch to data mode
                  stat = NF_ENDDEF(ncid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR: file ', ncid, 
     $                    ' still in define mode'
                  ENDIF
c                 write corner longitudes
                  IF (ll_single) THEN
                      stat = NF_PUT_VAR_REAL 
     $                    (ncid, idclon, crnrlon(:,:,:)) 
                  ELSE
                      stat = NF_PUT_VAR_DOUBLE 
     $                    (ncid, idclon, crnrlon(:,:,:))
                  ENDIF
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR writing corner lonitudes of ', cgrid
                  ENDIF
c                 write corner latitudes
                  IF (ll_single) THEN
                      stat = NF_PUT_VAR_REAL 
     $                    (ncid, idclat, crnrlat(:,:,:))
                  ELSE
                      stat = NF_PUT_VAR_DOUBLE 
     $                    (ncid, idclat, crnrlat(:,:,:))
                  ENDIF
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR writing corner latitudes of ', cgrid
                  ENDIF
c                 close grids file
                  stat = NF_CLOSE(ncid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR closing file', cgrdnam
                  ENDIF
#else
c                 binary format
c                 -------------
                  WRITE (nulprt,*) ' '
                  WRITE (nulprt,*) 'prisn_write_corner: WARNING: '
                  WRITE (nulprt,*) 
     $                'prism_write_corner: Binary format not supported'
                  WRITE (nulprt,*) 
     $                'prism_write_corner:   No corners added'
#endif
                  deallocate (crnrlon)
                  deallocate (crnrlat)

              ELSE IF (il_loc .eq. 400) THEN
c                 Read grid name, dimensions, and areas
c
                  il_errgsip = mgi_read (iga_gsipr(jch), cgrid, 4, 'C')
                  il_errgsip = mgi_read (iga_gsipr(jch), nx, 1, 'I')
                  il_errgsip = mgi_read (iga_gsipr(jch), ny, 1, 'I')
                  allocate (area(nx,ny))
                  il_errgsip=mgi_read (iga_gsipr(jch),area,nx*ny,'R')
                  WRITE(nulprt,*)'Read areas from model ', jch
                  WRITE(nulprt,*)'cgrid, nx, ny= ', cgrid, nx, ny
#ifdef use_netCDF
                  il_nftype = NF_DOUBLE
                  IF (ll_single) il_nftype = NF_REAL
c                 open areas file
                  stat = NF_OPEN(csurnam//'.nc', NF_WRITE, ncid)
                  IF (stat /= NF_NOERR) THEN
                      stat = NF_CREATE(csurnam//'.nc', NF_CLOBBER, ncid)
                      IF (stat /= NF_NOERR) THEN
                          WRITE (nulprt,*) 
     $                    'prism_write_area: error opening areas file.'
                          WRITE (nulprt,*) 'prism_write_area: STOP'
                          STOP
                      ENDIF
                  ENDIF
c                 define dimensions
                  stat = NF_REDEF(ncid)
                  stat = NF_INQ_DIMID(ncid, 'x_'//cgrid, idx)
                  IF (stat .NE. NF_NOERR) THEN
                      stat = NF_DEF_DIM(ncid, 'x_'//cgrid, nx, idx)
                      IF (stat /= NF_NOERR) THEN
                          WRITE (nulprt,*) 
     $                        'ERROR defining dimension x_',cgrid
                      ENDIF
                  ELSE
                      WRITE(nulprt,*)'Areas already defined: ', cgrid
                      RETURN
                  ENDIF
                  stat = NF_DEF_DIM(ncid, 'y_'//cgrid, ny, idy)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR defining dimension y_',cgrid
                  ENDIF
                  dims(1) = idx
                  dims(2) = idy
c                 define areas
                  csrf=cgrid//csursuf
                  stat = NF_DEF_VAR (ncid,csrf,il_nftype,2,dims,idsrf)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR defining variable ', csrf
                  ENDIF
                  stat = 
     $                NF_PUT_ATT_TEXT(ncid,idsrf,'long_name',13,
     $                'Areas of '//cgrid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR creating att. for areas of ', cgrid
                  ENDIF
                  stat = NF_PUT_ATT_TEXT(ncid, idsrf, 'units', 2, 'm2')
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 
     $                    'ERROR creating att. for areas of ', cgrid
                  ENDIF
c                 switch to data mode
                  stat = NF_ENDDEF(ncid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR: file ', ncid, 
     $                    ' still in define mode'
                  ENDIF
c                 write areas
                  IF (ll_single) THEN
                      stat = NF_PUT_VAR_REAL (ncid, idsrf, area(:,:))
                  ELSE
                      stat = NF_PUT_VAR_DOUBLE (ncid, idsrf, area(:,:))
                  ENDIF
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR writing area of ', cgrid
                  ENDIF
c                 close areas file
                  stat = NF_CLOSE(ncid)
                  IF (stat /= NF_NOERR) THEN
                      WRITE (nulprt,*) 'ERROR closing file', csurnam
                  ENDIF
#else
c                 binary format
c                 -------------
c                 open areas file
                  INQUIRE (FILE = csurnam, EXIST = existent, 
     $                     OPENED = srfopen)
                  IF (existent .AND. srfopen) THEN
                       WRITE (nulprt,*) ' '
                       WRITE (nulprt,*) 
     $                     'prism_write_area: areas file already opened'
                       WRITE (nulprt,*) 'prism_write_area: STOP'
                       STOP     
                   ENDIF
                   iost = 0
                   nulsrf = 7
                   INQUIRE (nulsrf,OPENED = srfopen)
                   DO WHILE (srfopen)
                     nulsrf = nulsrf + 1 
                     INQUIRE (nulsrf,OPENED = srfopen)
                   END DO
                   
                   OPEN (UNIT=nulsrf, FILE=csurnam, STATUS='UNKNOWN',
     $                 ACCESS='SEQUENTIAL', FORM='UNFORMATTED', 
     $                 POSITION='APPEND',
     $                 ACTION='WRITE', IOSTAT=iost, ERR=110)
 110               CONTINUE
                   IF (iost /= 0) THEN
                       WRITE (nulprt,*) ' '
                       WRITE (nulprt,*) 
     $                     'prism_write_area: Error opening areas file'
                       WRITE (nulprt,*) 'prism_write_area: STOP'
                       STOP
                   ENDIF
c                  write areas
                   csrf=cgrid//csursuf
                   WRITE (UNIT = nulsrf) csrf
                   WRITE (UNIT = nulsrf) area(:,:)
c                  close areas file
                   CLOSE (nulsrf)
#endif
                   deallocate (area)  
               ENDIF
            ENDDO
        ENDIF
        jch = jch + kbtotproc(ji)
      ENDDO
c 
C*    5. Normal EXIT
C     --------------
C
      WRITE (nulprt,FMT='(A)') 'Init_Oasis - -'
      CALL FLUSH(nulprt)
      RETURN
#endif
      END
c

