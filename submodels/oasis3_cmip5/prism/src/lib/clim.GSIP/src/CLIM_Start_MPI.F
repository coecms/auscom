      SUBROUTINE CLIM_Start_MPI(kinfo)
c
c*    *** Start-mpi ***  
c
c     purpose:
c     --------
c        beginning of the coupled run 
c
c     interface:
c     ----------
c	 kinfo	: output status
c
c     lib:
c     -------
c        GSIP
c
c     author:
c     -------
c       Sophie Valcke  
c       (22/10/04 -created from lib/clim/src/CLIM_Start)
c
c     ----------------------------------------------------------------
#if defined use_comm_GSIP
      USE mod_kinds_oasis
      USE mod_clim 
      USE mod_comclim
      USE mod_gsip
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p) kinfo
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p) ilgdt, ipos, is, isdmod, incp 
      INTEGER (kind=ip_intwp_p) irempo, il_maxtag, ipotag,nsegid
c
      INTEGER (kind=ip_intwp_p), PARAMETER ::    ibuflen=3
      INTEGER (kind=ip_intwp_p)  ibuff_mpi(ibuflen)
      REAL (kind=ip_realwp_p) rl_testvar
      INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  ila_workw
      CHARACTER*32, DIMENSION(:), ALLOCATABLE :: cla_workw
      INTEGER (kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE ::  ila_workr
      CHARACTER*32, DIMENSION(:), ALLOCATABLE :: cla_workr
      INTEGER (kind=ip_intwp_p) :: il_iwk_size, il_cwk_size
      INTEGER (kind=ip_intwp_p) :: il_ist, il_ien, il_cst
      INTEGER (kind=ip_intwp_p) :: isdmodnumber, isdprocnumber
      INTEGER (kind=ip_intwp_p) :: ii, il_bytesize, il_iosize
      INTEGER (kind=ip_intwp_p) :: il_int_iosize, il_rbyt
c     ----------------------------------------------------------------
c
c     0. define some variables 
c     ------------------------
c
      WRITE(nulprt,*)'Start - -  '
      CALL FLUSH(nulprt)
      ibuff_mpi(:)=0
      rl_testvar=0.0_ip_realwp_p
c
!
!     limit CLIM_MaxTag to 2^^30 - 1 (J. Latour)
!
      CLIM_MaxTag =  1073741823
      il_maxtag = CLIM_MaxTag - 1
      ilgdt = CLIM_ParSize
C
      il_bytesize = BIT_SIZE(ii)/8
      INQUIRE (iolength=il_iosize) ii
      il_int_iosize = il_iosize
      INQUIRE (iolength=il_iosize) rl_testvar
      il_rbyt = il_iosize/il_int_iosize*il_bytesize
      ig_maxtype = ig_CLIMmax*il_rbyt
C
c*    1. broadcast usefull informations
c     ---------------------------------
c
c     ibuff_mpi(1) is the model number (0 for Oasis)
      ibuff_mpi(1) = 0 
c     ibuff_mpi(2) is the process local number (in all its processes)
      ibuff_mpi(2) = 1
      ibuff_mpi(3) = nports
c
      WRITE(nulprt,FMT='(A,A)') 'Start- send- MODEL ',cmynam
      WRITE(nulprt,FMT='(A,I9)')'Start- send- model number :', 0
      WRITE(nulprt,FMT='(A,I9)')'Start- send- process local no  :', 1
      WRITE(nulprt,FMT='(A,I9)')'Start- send- nport:',nports
c
      il_iwk_size = ibuflen + 2*ig_clim_nfield * (5 + ilgdt)
      ALLOCATE (ila_workw(il_iwk_size))
      ALLOCATE (ila_workr(il_iwk_size))
      ila_workw(:) = 0
      ila_workr(:) = 0
      il_cwk_size = 1 + 2*ig_clim_nfield
      ALLOCATE (cla_workw(il_cwk_size))
      ALLOCATE (cla_workr(il_cwk_size))
      cla_workw(:) = ' '
      cla_workr(:) = ' '
      ila_workw(1:ibuflen) = ibuff_mpi(1:ibuflen)
      cla_workw(1) = cmynam
      il_ien = ibuflen
      il_cst = 1       
      DO 210 ji=1,nports
        il_ist = il_ien + 1
        il_ien = il_ist + 5 - 1
        ila_workw(il_ist:il_ien) = myport(1:5, ji)
        il_ist = il_ien + 1
        il_ien = il_ist + ilgdt - 1
        ila_workw(il_ist:il_ien) = mydist(1:ilgdt,ji)
        il_cst = il_cst + 1
        cla_workw(il_cst) = cports(ji)
 210   CONTINUE       
c
      jl = 0
      DO 370 jin = 1, ig_nmods
        DO 360 jn =  1, iga_totproc(jin)
          jl = jl + 1
c
c         Write ig_CLIMmax in channel
c
          il_errgsip = 
     $        mgi_write (iga_gsipw(jl), ig_CLIMmax, 1 , 'I')
          IF (il_errgsip .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)
     $            'CLIM_Start_MPI - ig_CLIMmax write OK:', il_errgsip
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'CLIM_Start_MPI - error ig_maxgrd write:', il_errgsip
              CALL FLUSH(nulprt)
              CALL abort
          ENDIF 
c
c         Write communication information in channel
c
          il_errgsip = 
     $        mgi_write (iga_gsipw(jl), ila_workw, il_iwk_size , 'I')
          IF (il_errgsip .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)
     $            'CLIM_Start_MPI - ila_workw write OK:', il_errgsip
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'CLIM_Start_MPI - error ila_workw write:', il_errgsip
              CALL FLUSH(nulprt)
              CALL abort
          ENDIF 
          il_errgsip = 
     $        mgi_write (iga_gsipw(jl), cla_workw, 32*il_cwk_size , 'C')
          IF (il_errgsip .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)
     $            'CLIM_Start_MPI - cla_workw write OK:', il_errgsip
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'CLIM_Start_MPI -error cla_workw write :', il_errgsip
              CALL FLUSH(nulprt)
              CALL abort
          ENDIF
c
c         Read communication information in channel
c
          il_errgsip = 
     $        mgi_read (iga_gsipr(jl), ila_workr, il_iwk_size , 'I')
          IF (il_errgsip .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)
     $            'CLIM_Start_MPI - ila_workr read OK:', il_errgsip
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'CLIM_Start_MPI - ila_workr read error :', il_errgsip
              CALL FLUSH(nulprt)
              CALL abort
          ENDIF 
          il_errgsip = 
     $        mgi_read (iga_gsipr(jl), cla_workr, 32*il_cwk_size , 'C')
          IF (il_errgsip .ge. 0) then
              WRITE(UNIT = nulprt,FMT = *)
     $            'CLIM_Start_MPI - cla_workr read OK:', il_errgsip
          ELSE
              WRITE(UNIT = nulprt,FMT = *)
     $            'CLIM_Start_MPI - error cla_workr read:', il_errgsip
              CALL FLUSH(nulprt)
              CALL abort
          ENDIF
c
c         Check matching ports
c
        ibuff_mpi(1:ibuflen) = ila_workr(1:ibuflen)
c       isdmonnumber is distant model number -> mylink(1,:)
        isdmodnumber   = ibuff_mpi(1)
c       isdprocnumber is distant process number -> mylink(2,:)
        isdprocnumber  = ibuff_mpi(2)
        irempo         = ibuff_mpi(3)
        cnames(jl)      = cla_workr(1)
        il_ien = ibuflen
        il_cst = 1       
        DO ji = 1, irempo
          il_cst = il_cst + 1
          clrport(ji) = cla_workr(il_cst)
          il_ist = il_ien + 1
          il_ien = il_ist + 5 - 1
          irport(1:5,ji) = ila_workr(il_ist:il_ien)
          il_ist = il_ien + 1
          il_ien = il_ist + ilgdt - 1
          irdist(1:ilgdt,ji) =  ila_workr(il_ist:il_ien)
        END DO
        WRITE (nulprt,FMT='(A,I9)') 'Start -recv model number :',
     $      isdmodnumber
        WRITE (nulprt,FMT='(A,I9)') 'Start -recv process local no :',
     $      isdprocnumber 
        WRITE (nulprt,FMT='(A,A)')  'Start - MODEL ',cnames(jl)
        WRITE (nulprt,FMT='(A,I9)') 'Start -   nport:',irempo
c
        DO 350 ji=1,nports
          DO 340 jj=1,irempo
            IF (cports(ji).eq.clrport(jj).and.
     *          myport(1,ji)+irport(1,jj).eq. 1) THEN
                IF ((mydist(1,ji).eq.irdist(1,jj).and.
     *              myport(4,ji).ne.irport(4,jj)).or.
     *              (mydist(CLIM_Strategy,ji).eq.CLIM_Serial.and.
     *              myport(4,ji).lt.irport(4,jj)).or.
     *              (irdist(CLIM_Strategy,jj).eq.CLIM_Serial.and.
     *              irport(4,jj).lt.myport(4,ji))) THEN
                    kinfo = CLIM_IncSize
                    WRITE(nulprt,FMT='(A,A,A,I2,A,I2,A,I2,I2)')
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
     *                   .irdist(CLIM_Strategy,jj).ne.CLIM_Serial) THEN
                         IF (mydist(CLIM_Strategy,ji).ne.
     *                       irdist(CLIM_Strategy,jj)) THEN
                             WRITE(nulprt,*)'STOP 1 in Clim_Start_MPI'
                             CALL FLUSH(nulprt)
                             CALL abort
                         ELSE
                             IF (mydist(CLIM_Segments,ji).eq.
     *                           irdist(CLIM_Segments,jj)) THEN
                                 nsegid = 0                            
                                 DO  is=1,2*mydist(CLIM_Segments,ji)
                                   IF (mydist(CLIM_Segments+is,ji).eq.
     *                                 irdist(CLIM_Segments+is,jj)) THEN 
                                       nsegid = nsegid + 1
                                   ENDIF
                                 END DO
                                 IF (nsegid.eq.
     *                               (2*mydist(CLIM_Segments,ji))) THEN
                                     nlinks = nlinks + 1
                                     ipos   = 5
                                     myport(ipos,ji)=myport(ipos,ji)+ 1
                                     myport(ipos+myport(ipos,ji),ji)
     *                                   = nlinks
                                     mylink(1,nlinks) = isdmodnumber
                                     mylink(2,nlinks) = isdprocnumber
                                     mylink(3,nlinks) = ipotag
                                     mylink(4,nlinks) = 1
                                     mylink(5,nlinks) = 0
                                     mylink(6,nlinks) = myport(4,ji)
                                     WRITE(nulprt,FMT='(A,A)')
     *                                  'Start - LINK ',cports(ji)
                                     WRITE(nulprt,FMT='(A,I2,A,I1,A,I2,
     *                                   A,I1,A,I10,A,I10)')
     *                                   'Start - [model ',
     *                                   mynum,'/io ',myport(1,ji),'] - 
     *                                   [model ',
     *                                   isdmod,'/io ',irport(1,jj),'] 
     *                                   using tags ',
     *                                   ipotag,' to ',ipotag-incp+1
                                 ENDIF    
                             ENDIF
                         ENDIF
                     ELSE                    
                         nlinks = nlinks + 1
                         ipos   = 5
                         myport(ipos,ji) = myport(ipos,ji) + 1
                         myport(ipos+myport(ipos,ji),ji) = nlinks
                         mylink(1,nlinks) = isdmodnumber
                         mylink(2,nlinks) = isdprocnumber
                         mylink(3,nlinks) = ipotag
                         mylink(4,nlinks) = 1
                         mylink(5,nlinks) = 0
                         mylink(6,nlinks) = myport(4,ji)
                         IF (mydist(CLIM_Strategy,ji).eq.CLIM_SERIAL
     $                       .and. irdist(CLIM_Strategy,jj) .ne.
     $                       CLIM_Serial) THEN
                             mylink(4,nlinks) = irdist(CLIM_Segments,jj)
                             DO 330 is=1,2*irdist(CLIM_Segments,jj)
                               mylink(4+is,nlinks) = irdist
     $                             (CLIM_Segments+is,jj)
 330                         CONTINUE
                         ENDIF
                         WRITE(nulprt,FMT='(A,A)')
     *                       'Start - LINK ',cports(ji)
                         WRITE(nulprt,FMT='(A,I2,A,I1,A,I2,A,
     *                       I1,A,I10,A,I10)')
     *                       'Start - [model ',
     *                       mynum,'/io ',myport(1,ji),'] - [model ',
     *                       isdmod,'/io ',irport(1,jj),'] using tags ',
     *                       ipotag,' to ',ipotag-incp+1
                     ENDIF                    
                 ENDIF
             ENDIF
 340       CONTINUE
 350     CONTINUE
 360   CONTINUE
 370  CONTINUE
      DEALLOCATE (ila_workw)
      DEALLOCATE (ila_workr)
      DEALLOCATE (cla_workw)
      DEALLOCATE (cla_workr)
c
      WRITE (nulprt,FMT='(A)') 'END of CLIM_Start_MPI  -- '
      CALL FLUSH(nulprt)
#endif
      RETURN
      END
