      SUBROUTINE prism_enddef_proto(kinfo)
c
c*    *** Enddef ***   PRISM 1.0
c
c     purpose:
c     --------
c        beginning of the coupled run
c
c     interface:
c     ----------
c	 kinfo	: output status
c
c     lib mp:
c     -------
c        mpi-1 or mpi-2 or gsip
c
c     author:
c     -------
c        Eric Sevault   - METEO FRANCE
c        Laurent Terray - CERFACS
c        Jean Latour    - F.S.E.   (mpi-2)
c        Arnaud Caubel  - FECIT - removed kmxtag as argument
c        Sophie Valcke  - Added GOSSIP
c
c     ----------------------------------------------------------------
      USE mod_kinds_model
      USE mod_prism_proto
      USE mod_comprism_proto
#if !defined key_noIO
      USE mod_psmile_io_interfaces
#endif
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
#endif
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p) kinfo, il_err, il_max
#if defined use_comm_MPI1 || defined use_comm_MPI2 
      REAL(kind=ip_double_p) rl_testvar
      INTEGER (kind=ip_intwp_p) il_bufsendsize,
     $    il_bufsizebyt, integer_byte_size, io_size, 
     $    integer_io_size, il_modbufsizebyt, il_modbufsize,
     $    il_bufsize, il_errhandler
      INTEGER (kind=ip_intwp_p) :: ii, il_bytesize, il_iosize
      INTEGER (kind=ip_intwp_p) :: il_int_iosize, il_rbyt
#endif
c     ----------------------------------------------------------------
c
c*    0. First Check
c     --------------
c
      kinfo  = CLIM_Ok
      il_max = maxval(myport(4,:))
c
#if defined use_comm_MPI1 || defined use_comm_MPI2
      rl_testvar = 0.0_ip_double_p
c     Determination of size of real element without kind
      il_bytesize = BIT_SIZE(ii)/8
      INQUIRE (iolength=il_iosize) ii
      il_int_iosize = il_iosize
      INQUIRE (iolength=il_iosize) rl_testvar
      il_rbyt = il_iosize/il_int_iosize*il_bytesize
c       
      ig_maxtype = ig_CLIMmax*il_rbyt
      ig_maxtype_field = il_max*il_rbyt
c
      IF (lg_clim_bsend) THEN
c         Determination of size of dg_bufsend element without kind
c
c         MPI_BSEND_OVERHEAD/il_rbyt+1 is the number of dg_bufsend
c         elements needed to store MPI_BSEND_OVERHEAD bytes; there
c         may be MPI_BSEND_OVERHEAD bytes per message (nports) 
          il_bufsendsize = (il_max + MPI_BSEND_OVERHEAD/il_rbyt+1)
     $        * nports
          WRITE(nulprt,*) 'il_bufsendsize = ', il_bufsendsize
C 
C         Test if model has already attached to mpi buffer
          CALL MPI_Errhandler_set(MPI_COMM_WORLD,
     $         MPI_ERRORS_RETURN, mpi_err)

          CALL MPI_Buffer_Detach(rl_testvar,il_modbufsizebyt,mpi_err)

          IF (mpi_err .ne. MPI_SUCCESS) THEN
              il_bufsize = il_bufsendsize
          ELSE
              il_modbufsize = il_modbufsizebyt/il_rbyt
              il_bufsize = il_bufsendsize + il_modbufsize
              WRITE(nulprt,*) 'il_bufsize = ', il_bufsize
          ENDIF

          CALL MPI_Errhandler_set(MPI_COMM_WORLD,
     $         MPI_ERRORS_ARE_FATAL, mpi_err)

          ALLOCATE (dg_bufsend(il_bufsize),  stat = il_err)
          il_bufsizebyt = il_bufsize * il_rbyt
          WRITE(nulprt,*) 'il_bufsizebyt = ', il_bufsizebyt
C 
C*   Attach a buffer able to contain the number of coupling fields 
c    (nports) with maximum possible size (il_max) supposed to be declared 
c    as DOUBLE.  
          CALL MPI_Buffer_Attach(dg_bufsend, il_bufsizebyt, mpi_err)
          WRITE(nulprt,*)'Attached buffer of size=',il_bufsizebyt
c
      ENDIF
      ALLOCATE (pkwork_field(il_max), stat = il_err)
      IF (il_err.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "pkwork_field" allocation in PRISM_enddef !'
      pkwork_field(:)=0
#endif
c
      CALL PRISM_Start_MPI(kinfo)
c
c*    1. Allocation of arrays used for exchange of fields and local 
c*    transformations
c     -----------------------------------------------------
      IF (ip_realwp_p == ip_single_p) THEN
          ALLOCATE (rg_field_trans(il_max,nports),stat = il_err)
          IF (il_err.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "rg_field_trans" allocation in PRISM_enddef !'
          rg_field_trans(:,:)=0
      ELSE IF (ip_realwp_p == ip_double_p) THEN
          ALLOCATE (dg_field_trans(il_max,nports),stat = il_err)
          IF (il_err.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "dg_field_trans" allocation in PRISM_enddef !'
          dg_field_trans(:,:)=0
      ENDIF
      ALLOCATE (ig_number(nports),stat = il_err)
      IF (il_err.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "ig_number" allocation in PRISM_enddef !'
      ig_number(:) = 0
c
#if !defined key_noIO
      call psmile_def_domains(il_err)
      call psmile_def_files(il_err)
      call psmile_def_metadata(il_err)
#endif
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      WRITE (nulprt,FMT='(A)') 'Returning from Enddef -- '
      CALL FLUSH(nulprt)
      RETURN
      END
      SUBROUTINE PRISM_Start_MPI(kinfo)
c
c*    *** Start-mpi ***   CLIM 3.0
c
c     purpose:
c     --------
c        beginning of the coupled run (MPI-2 only)
c
c     interface:
c     ----------
c	 kinfo	: output status
c
c     method:
c     -------
c        With MPI2, use the new intra-communicator created by the
c                   MPI_Intercomm_merge in CLIM_Init (models)
c                   or in inicmc via CLIM_Init (coupler Oasis)
c
c     lib mp:
c     -------
c        mpi-1 (or mpi-2)
c
c     author:
c     -------
c        Eric Sevault   - METEO FRANCE
c        Laurent Terray - CERFACS
c        Jean Latour    - F.S.E.   (mpi-2)
c        Sophie Valcke  - CERFACS (08/09/00 -modified)
c        Arnaud Caubel  - FECIT (08/02 - removed kmxtag as argument)
c        Sophie Valcke  - CERFACS (25/10/04 -added GSIP)
c     ----------------------------------------------------------------
      USE mod_kinds_model
      USE mod_prism_proto
      USE mod_comprism_proto
#if defined use_comm_GSIP
      USE mod_gsip_model
#endif
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
#endif
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p) kinfo
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)    ip, info1, info, ilgdt, ipos, is,
     *             itag1, isdmod, incp, ipotag,nsegid
      INTEGER (kind=ip_intwp_p)    irempo, il_maxtag, il_start, il_end
c
      PARAMETER    (ibuflen=3)
      INTEGER (kind=ip_intwp_p)    iposbuf, ibuff_mpi(ibuflen) 
      LOGICAL      ll_flag
#if defined use_comm_MPI1 || defined use_comm_MPI2
      INTEGER (kind=ip_intwp_p)    istatus(MPI_STATUS_SIZE), imaxbyt
      INTEGER (kind=ip_intwp_p), allocatable :: ireq(:)
      INTEGER (kind=ip_intwp_p)              :: ireqrecv
      INTEGER (kind=ip_intwp_p)              :: irc
cLK      INTEGER (kind=ip_intwp_p)              :: rpkwork(ig_CLIMmax)
      REAL(kind=ip_DOUBLE_p)                 :: rpkwork(ig_CLIMmax)
#elif defined use_comm_GSIP
      INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  ila_work
      CHARACTER*32, DIMENSION(:), ALLOCATABLE :: cla_work
      INTEGER (kind=ip_intwp_p) :: il_iwk_size, il_cwk_size
      INTEGER (kind=ip_intwp_p) :: il_ist, il_ien, il_cst
      INTEGER (kind=ip_intwp_p) :: isdmodnumber, isdprocnumber
      CHARACTER*32              :: csdname
#endif
c     ----------------------------------------------------------------
c
c     0. define some variables 
c     ------------------------
c
      WRITE(nulprt,*)'Start - -  '
c           
      ibuff_mpi(:)=0
#if defined use_comm_MPI1 || defined use_comm_MPI2
      istatus(:)=0
      call MPI_Attr_get(MPI_COMM_WORLD, MPI_TAG_UB, CLIM_MaxTag, 
     $    ll_flag,info)
      if(info.ne.MPI_SUCCESS) then
        write(nulprt, * )'CLIM_Start_MPI -- MPI_Attr_get failed !',info
        call MPI_ABORT (mpi_comm, 0, info)
      endif
      if ( .not. ll_flag ) then
         write(nulprt, * ) 'Warning:  MPI_Attr_get did not return '
         write(nulprt, * ) ' a valid CLIM_MaxTag!'
         write(nulprt, * ) ' CLIM_MaxTag is set to 32767!'
         CLIM_MaxTag = 32767
      else
         write(nulprt, * ) 'CLIM_MaxTag is now ', CLIM_MaxTag
!
!     limit CLIM_MaxTag to 2^^30 - 1 (J. Latour)
!
         if ( CLIM_MaxTag .gt. 1073741823 ) CLIM_MaxTag =  1073741823
         write(nulprt, * ) 'CLIM_MaxTag is now ', CLIM_MaxTag
!
      endif
      itag1 = CLIM_MaxTag
#elif defined use_comm_GSIP
       CLIM_MaxTag =  1073741823     
#endif
      il_maxtag = CLIM_MaxTag - 1
      ilgdt = CLIM_ParSize
C
c*    1. broadcast usefull informations
c     ---------------------------------
c
#if defined use_comm_MPI1 || defined use_comm_MPI2
      ibuff_mpi(1) = mynum 
      ibuff_mpi(2) = modtid(mynum) 
#elif defined use_comm_GSIP
      ibuff_mpi(1) = ig_mynummod
      ibuff_mpi(2) = ig_numproc
#endif
      ibuff_mpi(3) = nports
c
      WRITE(nulprt,FMT='(A,A)') 'Start- send- MODEL ',cmynam
#if defined use_comm_MPI1 || defined use_comm_MPI2
      WRITE(nulprt,FMT='(A,I9)')'Start- send-  num  :',mynum
      WRITE(nulprt,FMT='(A,I9)')'Start- send-  tid  :',modtid(mynum)
#elif defined use_comm_GSIP
      WRITE(nulprt,FMT='(A,I9)')'Start- send- model number :', 
     $    ig_mynummod
      WRITE(nulprt,FMT='(A,I9)')'Start- send- process local no  :',  
     $    ig_numproc
#endif
      WRITE(nulprt,FMT='(A,I9)')'Start- send-  nport:',nports
c
#if defined use_comm_MPI1 || defined use_comm_MPI2
      iposbuf = 0
      call MPI_Pack ( ibuff_mpi, ibuflen, MPI_INTEGER, pkwork,
     *                ig_maxtype, iposbuf, mpi_comm, info )
      call MPI_Pack ( cmynam, CLIM_Clength, MPI_CHARACTER, pkwork,
     *                ig_maxtype, iposbuf, mpi_comm, info )
      DO 210 ji=1,nports
         call MPI_Pack ( cports(ji), CLIM_Clength, MPI_CHARACTER,
     *           pkwork, ig_maxtype, iposbuf, mpi_comm, info )
         call MPI_Pack ( myport(1,ji), 5, MPI_INTEGER, pkwork,
     *                   ig_maxtype, iposbuf, mpi_comm, info )
         call MPI_Pack ( mydist(1,ji), ilgdt, MPI_INTEGER, pkwork,
     *                   ig_maxtype, iposbuf, mpi_comm, info )
 210   CONTINUE 
#elif defined use_comm_GSIP
      il_iwk_size = ibuflen + 2*ig_clim_nfield * (5 + ilgdt)
      ALLOCATE (ila_work(il_iwk_size))
      ila_work(:) = 0
      il_cwk_size = 1 + 2*ig_clim_nfield
      ALLOCATE (cla_work(il_cwk_size))
      cla_work(:) = ' '
      ila_work(1:ibuflen) = ibuff_mpi(1:ibuflen)
      cla_work(1) = cmynam
      il_ien = ibuflen
      il_cst = 1       
      DO 210 ji=1,nports
        il_ist = il_ien + 1
        il_ien = il_ist + 5 - 1
        ila_work(il_ist:il_ien) = myport(1:5, ji)
        il_ist = il_ien + 1
        il_ien = il_ist + ilgdt - 1
        ila_work(il_ist:il_ien) = mydist(1:ilgdt,ji)
        il_cst = il_cst + 1
        cla_work(il_cst) = cports(ji)
 210   CONTINUE   
#endif
c
#if defined use_comm_MPI1 || defined use_comm_MPI2
c*     In the direct case, Oasis process is not a process involved in the 
c*     coupling
c
       IF (lg_oasis_field) THEN
          il_start = 0
          il_end = ncplprocs-1
       ELSE
          il_start = 1
          il_end = ncplprocs
       ENDIF
       ALLOCATE(ireq(0:ncplprocs))
       irc = 0
      DO 220 ji = il_start, il_end
c
c       Send to all processors involved in the coupling except itself
        IF ( ji .NE. mynum ) THEN
            CALL MPI_Isend ( pkwork, iposbuf, MPI_PACKED, modtid(ji),
     *                      itag1, mpi_comm, ireq(irc), info )
            irc = irc+1
c
        ENDIF 
 220  CONTINUE
#elif defined use_comm_GSIP
c
c     Write information in channel to Oasis (no direct communication)
c
      il_errgsip = mgi_write (ig_gsipw, ila_work, il_iwk_size , 'I')
      IF (il_errgsip .ge. 0) then
          WRITE(UNIT = nulprt,FMT = *)
     $        'prism_enddef_proto - ila_work write OK:', il_errgsip
      ELSE
          WRITE(UNIT = nulprt,FMT = *)
     $        'prism_enddef_proto - error :', il_errgsip
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'STOP in prism_enddef_proto (ila_work write)')
      ENDIF
      il_errgsip = mgi_write (ig_gsipw, cla_work, 32*il_cwk_size , 'C')
      IF (il_errgsip .ge. 0) then
          WRITE(UNIT = nulprt,FMT = *)
     $        'prism_enddef_proto - cla_work write OK:', il_errgsip
      ELSE
          WRITE(UNIT = nulprt,FMT = *)
     $        'prism_enddef_proto - error :', il_errgsip
          CALL prism_abort_proto (0, 'prism_init_comp_proto',
     $        'STOP in prism_enddef_proto (cla_work write)')
      ENDIF

#endif
c
#if defined use_comm_MPI1 || defined use_comm_MPI2
      WRITE (nulprt,FMT='(A,I3,A,I8,A)') 
     * 'Start - broadcast from mynum = ',mynum,' <MPI ',info,'>'
c
c*    3. get these infos from other models and check ports
c     ----------------------------------------------------
c
      imaxbyt=0
      ireqrecv = 0
      DO 380 ip=1, ncplprocs-1
c
        CALL MPI_IRecv(rpkwork, ig_maxtype, MPI_PACKED, MPI_ANY_SOURCE,
     *                 itag1, mpi_comm, ireqrecv, info1 )
        CALL MPI_Wait ( ireqrecv, istatus, info )
        CALL MPI_Get_count ( istatus, MPI_PACKED, imaxbyt, info )
        IF ( info .NE. 0 ) THEN
            kinfo = CLIM_Unpack
            WRITE(nulprt,FMT='(A,I3,A)')
     *                 'Import - pb unpack <mpi ',info,'>'
            GO TO 1010
        ENDIF
c end of receive
c
        iposbuf = 0
        CALL MPI_Unpack ( rpkwork, ig_maxtype, iposbuf, ibuff_mpi,
     *                      ibuflen, MPI_INTEGER, mpi_comm, info )
        IF ( info .NE. 0 ) THEN
            kinfo = CLIM_Unpack
            WRITE(nulprt,FMT='(A,I3,A)')
     *                 'Import - pb unpack <mpi ',info,'>'
            GO TO 1010
        ENDIF
c
        isdmod         = ibuff_mpi(1)
        modtid(isdmod) = ibuff_mpi(2)
        irempo         = ibuff_mpi(3)
c
        call MPI_Unpack ( rpkwork, ig_maxtype, iposbuf, cnames(isdmod),
     *                   CLIM_Clength, MPI_CHARACTER, mpi_comm, info )
        DO 310 ji=1,irempo
          call MPI_Unpack ( rpkwork, ig_maxtype, iposbuf, clrport(ji),
     *                   CLIM_Clength, MPI_CHARACTER, mpi_comm, info )
          call MPI_Unpack( rpkwork, ig_maxtype, iposbuf, irport(1,ji), 
     *                           5, MPI_INTEGER, mpi_comm, info )
          call MPI_Unpack( rpkwork, ig_maxtype, iposbuf, irdist(1,ji),
     *                           ilgdt, MPI_INTEGER, mpi_comm, info )
 310    CONTINUE

#elif defined use_comm_GSIP
c
c       Read ig_CLIMmax in channel from Oasis 
c
        il_errgsip = mgi_read (ig_gsipr, ig_CLIMmax, 1, 'I')
        IF (il_errgsip .ge. 0) then
            WRITE(UNIT = nulprt,FMT = *)
     $          'prism_enddef_proto - ig_CLIMmax read OK:', ig_CLIMmax
        ELSE
            WRITE(UNIT = nulprt,FMT = *)
     $          'prism_enddef_proto - error :', il_errgsip
            CALL FLUSH(nulprt)
            CALL prism_abort_proto (0, 'prism_enddef_proto',
     $        'STOP in prism_enddef_proto ( ig_CLIMmax read)')
        ENDIF 
        ALLOCATE (pkworkps(ig_CLIMmax), stat = il_err)
        IF (il_err.ne.0) WRITE(nulprt,*)
     $      'Error in pkworkps allocation in prism_enddef_proto'
        pkworkps(:)=0        
c
c       Read information in channel from Oasis (no DIRECT communication)
c
        il_errgsip = mgi_read (ig_gsipr, ila_work, il_iwk_size , 'I')
        IF (il_errgsip .ge. 0) then
            WRITE(UNIT = nulprt,FMT = *)
     $          'prism_enddef_proto - ila_work read OK:', il_errgsip
        ELSE
            WRITE(UNIT = nulprt,FMT = *)
     $          'prism_enddef_proto - error :', il_errgsip
          CALL prism_abort_proto (0, 'prism_enddef_proto',
     $        'STOP in prism_enddef_proto (ila_work read)')
        ENDIF
        il_errgsip = mgi_read (ig_gsipr, cla_work, 32*il_cwk_size , 'C')
        IF (il_errgsip .ge. 0) then
            WRITE(UNIT = nulprt,FMT = *)
     $          'prism_enddef_proto  - cla_work read OK:', il_errgsip
        ELSE
            WRITE(UNIT = nulprt,FMT = *)
     $          'prism_enddef_proto  - error :', il_errgsip
            CALL FLUSH(nulprt)
            CALL prism_abort_proto (0, 'prism_enddef_proto',
     $        'STOP in prism_enddef_proto (cla_work read)')
        ENDIF
        ibuff_mpi(1:ibuflen) = ila_work(1:ibuflen)
c       isdmonnumber is distant model number -> mylink(1,:)
        isdmodnumber   = ibuff_mpi(1)
c       isdprocnumber is distant process number -> mylink(2,:)
        isdprocnumber  = ibuff_mpi(2)
        irempo         = ibuff_mpi(3)
        csdname        = cla_work(1)
        il_ien = ibuflen
        il_cst = 1 
        DO 310 ji = 1, irempo
          il_cst = il_cst + 1
          clrport(ji) = cla_work(il_cst)
          il_ist = il_ien + 1
          il_ien = il_ist + 5 - 1
          irport(1:5,ji) = ila_work(il_ist:il_ien)
          il_ist = il_ien + 1
          il_ien = il_ist + ilgdt - 1
          irdist(1:ilgdt,ji) =  ila_work(il_ist:il_ien)
 310    CONTINUE 
#endif
c
#if defined use_comm_MPI1 || defined use_comm_MPI2 
        WRITE (nulprt,FMT='(A,A)') 'Start -recv MODEL ',cnames(isdmod)
        WRITE (nulprt,FMT='(A,I9)') 'Start -   num  :',isdmod
        WRITE (nulprt,FMT='(A,I9)') 'Start -   tid  :',modtid(isdmod)
#elif defined use_comm_GSIP
        WRITE (nulprt,FMT='(A,A)')  'Start - MODEL ', csdname
        WRITE (nulprt,FMT='(A,I9)') 'Start -recv model number :',
     $      isdmodnumber
        WRITE (nulprt,FMT='(A,I9)') 'Start -recv process local no :',
     $      isdprocnumber 
#endif
        WRITE (nulprt,FMT='(A,I9)') 'Start -   nport:',irempo
c
        DO 350 ji=1,nports
          DO 340 jj=1,irempo
            IF (cports(ji).eq.clrport(jj).and.
     *          myport(1,ji)+irport(1,jj).eq.1) THEN
                IF ((mydist(CLIM_Strategy,ji).eq.CLIM_Serial.and.
     *              myport(4,ji).lt.irport(4,jj)).or.
     *              (irdist(CLIM_Strategy,jj).eq.CLIM_Serial.and.
     *              irport(4,jj).lt.myport(4,ji))) THEN
                    kinfo = CLIM_IncSize
                    WRITE(nulprt,FMT='(A,A,A,I2,A,I2,A,I8,I8)')
     *                  'Start - WARNING Incompatible sizes - field',
     *                  cports(ji),'model ',mynum,' and model ',
     *                  isdmod,': ',myport(4,ji),irport(4,jj)
                ELSEIF (myport(2,ji).ne.irport(2,jj)) THEN
                    kinfo = CLIM_BadType
                    WRITE(nulprt,FMT='(A,A,A,I2,A,I2,A,I2,I2)')
     *                  'Start - WARNING Incompatible types - field',
     *                  cports(ji),'model ',mynum,' and model ',
     *                  isdmod,': ',myport(2,ji),irport(2,jj)
                ELSE
                    IF (myport(1,ji).eq.CLIM_Out) THEN
                        incp   = ig_ntime / ig_frqmin + 1
                        ipotag = CLIM_MaxTag - (ji-1)*incp - 1
                    ELSE
                        incp   = ig_ntime / ig_frqmin + 1
                        ipotag = CLIM_MaxTag - (jj-1)*incp - 1
                    ENDIF
                    IF ((ipotag-incp).lt.il_maxtag) THEN
                        il_maxtag = ipotag - incp
                    ENDIF
                    IF (mydist(CLIM_Strategy,ji).ne.CLIM_Serial.and
     *               .irdist(CLIM_Strategy,jj).ne.CLIM_Serial) THEN
                        IF (mydist(CLIM_Strategy,ji).ne.
     *                      irdist(CLIM_Strategy,jj)) THEN
                           WRITE (nulprt,FMT=*)
     $                          'STOP 1 in prism_enddef_proto'
                           CALL prism_abort_proto (ig_mynummod, 
     $                         'prism_enddef_proto',
     $                         'STOP 1 in prism_enddef_proto')
                        ELSE
                            IF (mydist(CLIM_Segments,ji).eq.
     *                          irdist(CLIM_Segments,jj)) THEN
                                nsegid = 0                            
                                DO  is=1,2*mydist(CLIM_Segments,ji)
                                  IF (mydist(CLIM_Segments+is,ji).eq.
     *                               irdist(CLIM_Segments+is,jj)) THEN 
                                      nsegid = nsegid + 1
                                  ENDIF
                                END DO
                                IF (nsegid.eq.
     *                              (2*mydist(CLIM_Segments,ji))) THEN
                                    nlinks = nlinks + 1
                                    ipos   = 5
                                    myport(ipos,ji)=myport(ipos,ji)+ 1
                                    myport(ipos+myport(ipos,ji),ji)
     *                                     = nlinks
#if defined use_comm_MPI1 || defined use_comm_MPI2
                                    mylink(1,nlinks) = isdmod
                                    mylink(2,nlinks) = modtid(isdmod)
#elif defined use_comm_GSIP
                                    mylink(1,nlinks) = isdmodnumber
                                    mylink(2,nlinks) = isdprocnumber
#endif
                                    mylink(3,nlinks) = ipotag
                                    mylink(4,nlinks) = 1
                                    mylink(5,nlinks) = 0
                                    mylink(6,nlinks) = myport(4,ji)
                                    WRITE(nulprt,FMT='(A,A)')
     *                                  'Start - LINK ',cports(ji)
                                    WRITE(nulprt,FMT='(A,I2,A,I1,A,I2,
     *                                 A,I1,A,I10,A,I10)')
     *                                  'Start - [model ',
     *                                  mynum,'/io ',myport(1,ji),'] - 
     *                                    [model ',
     *                                  isdmod,'/io ',irport(1,jj),'] 
     *                                   using tags ',
     *                                  ipotag,' to ',ipotag-incp+1
                                ENDIF    
                            ENDIF
                        ENDIF
                    ELSE
                        nlinks = nlinks + 1
                        ipos   = 5
                        myport(ipos,ji) = myport(ipos,ji) + 1
                        myport(ipos+myport(ipos,ji),ji) = nlinks
#if defined use_comm_MPI1 || defined use_comm_MPI2
                        mylink(1,nlinks) = isdmod
                        mylink(2,nlinks) = modtid(isdmod)
#elif defined use_comm_GSIP
                        mylink(1,nlinks) = isdmodnumber
                        mylink(2,nlinks) = isdprocnumber
#endif
                        mylink(3,nlinks) = ipotag
                        mylink(4,nlinks) = 1
                        mylink(5,nlinks) = 0
                        mylink(6,nlinks) = myport(4,ji)
                        IF (mydist(CLIM_Strategy,ji).eq.CLIM_Serial.and
     *                   .irdist(CLIM_Strategy,jj).ne.CLIM_Serial) THEN
                            mylink(4,nlinks) = irdist(CLIM_Segments,jj)
                            DO 330 is=1,2*irdist(CLIM_Segments,jj)
                              mylink(4+is,nlinks) = irdist
     *                                      (CLIM_Segments+is,jj)
 330                        CONTINUE
                        ENDIF
                        WRITE(nulprt,FMT='(A,A)')
     *                      'Start - LINK ',cports(ji)
                        WRITE(nulprt,FMT='(A,I2,A,I1,A,I2,A,
     *                       I1,A,I10,A,I10)')
     *                      'Start - [model ',
     *                      mynum,'/io ',myport(1,ji),'] - [model ',
     *                      isdmod,'/io ',irport(1,jj),'] using tags ',
     *                      ipotag,' to ',ipotag-incp+1
                    ENDIF                    
                ENDIF
            ENDIF
 340      CONTINUE
 350    CONTINUE
c
        WRITE (nulprt,FMT='(A)') 'Start - -'
#if defined use_comm_MPI1 || defined use_comm_MPI2 
 380  CONTINUE
c
c     MPI_wait on the above MPI_Isend so to not change the content of pkwork
c     before the sending is complete.
      irc = 0
      DO 420 ji = il_start, il_end
        IF ( ji .NE. mynum ) THEN
            CALL MPI_wait ( ireq(irc), istatus, info )
            WRITE (nulprt, FMT='(A)') 'After MPI_wait'
            irc = irc+1
        ENDIF 
 420  CONTINUE
      DEALLOCATE(ireq)
 1010 CONTINUE
#elif defined use_comm_GSIP
      DEALLOCATE (ila_work)
      DEALLOCATE (cla_work)
#endif
c
c     ----------------------------------------------------------------
c
      WRITE (nulprt,FMT='(A)') 'Returning from prism_enddef_proto  -- '
      CALL FLUSH(nulprt)
c
      RETURN
      END
