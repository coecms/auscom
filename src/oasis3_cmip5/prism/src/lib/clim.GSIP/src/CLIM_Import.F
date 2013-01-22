      SUBROUTINE CLIM_Import(id_port_id,kstep,pfield,kinfo)
c
c*    *** Import ***   
c
c     purpose:
c     --------
c        recv pfield from models connected to port id_port_id
c
c     interface:
c     ----------
c        id_port_id : port number of the field
c	 kstep	: current time in seconds
c	 pfield	: buffer of reals
c	 kinfo	: output status
c
c     lib mp:
c     -------
c        GSIP
c
c     author:
c     -------
c        Sophie Valcke, CERFACS (02/12/2004 - adapted from CLIM_IMport for MPI)
c
c     ----------------------------------------------------------------
#if defined use_comm_GSIP
      USE mod_kinds_oasis
      USE mod_clim
      USE mod_comclim
      USE mod_gsip
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)       kstep, kinfo
      REAL (kind=ip_realwp_p), DIMENSION(*) :: pfield    
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)	    irecv, imod, ilk, iseg, is, ilgb,
     $    itid, itag, ilen, ioff, ityp, ibyt, jl, ji
cSV      CHARACTER*32 :: clprint
c     ----------------------------------------------------------------
c
c*    0. Entering
c     --------------
c
      kinfo = CLIM_Ok
c
c*    1. check for this port in my list
c     ---------------------------------
c
      irecv = 0
      iport = -1
c
      IF (myport(1,id_port_id).eq.CLIM_In) iport=id_port_id
      IF (iport.lt.0) THEN
         kinfo = CLIM_BadPort
         WRITE(nulprt,FMT='(A,A)')
     $       'Import - WARNING Invalid port out: ', cports(id_port_id)
         CALL FLUSH(nulprt)
         CALL abort
      ENDIF
c
c*    2. check for connected ports (in)
c     ---------------------------------
c
      WRITE(nulprt,FMT='(A,A)') 'Import - ', cports(iport)
c
      ityp = myport(2,iport)
      ibyt = myport(3,iport)
c
c     myport(5,iport) is the number of links for this port/field
c     mylink is a table containing the description of all links of all ports
      DO 290 ip=1,myport(5,iport)
         ilk  = myport(5+ip,iport)
c        myport(6,iport) is the link number for the first link of iport
c        myport(7,iport) is the link number for the second link of iport
         imod = mylink(1,ilk)
         itid = mylink(2,ilk)
         itag = mylink(3,ilk) - kstep / ig_frqmin
         iseg = mylink(4,ilk)
         jl = itid
         IF (imod .gt. 1) THEN
             DO 250  ji = 1, imod - 1
               jl = jl + iga_totproc(ji)
 250         CONTINUE
         ENDIF
         IF ( ip_realwp_p .eq. ip_single_p ) THEN 
c
cSV             IF (jl .eq. 1) then
cSV                 il_errgsip = 
cSV     $               mgi_read (iga_gsipr(jl), clprint, 32, 'C')
cSV             ENDIF
c
             il_errgsip = 
     $           mgi_read (iga_gsipr(jl), pkwork, ig_CLIMmax, 'R')
c
         ELSE IF ( ip_realwp_p .eq. ip_double_p ) THEN
c
cSV            IF (jl .eq. 1) then
cSV             il_errgsip = 
cSV     $           mgi_read (iga_gsipr(jl), clprint, 32, 'C')
cSV             ENDIF
c
             il_errgsip = 
     $           mgi_read (iga_gsipr(jl), pkwork, ig_CLIMmax, 'D')
         ELSE
             WRITE(nulprt,*)
     $           'Problem with the coupling field type'
             WRITE(nulprt,*) 'ABORT in CLIM_Import 2'
             CALL FLUSH(nulprt)
             CALL abort
         ENDIF
c
         IF (il_errgsip .ge. 0) THEN 
             ilgb = 0
             il_rst = 0
             il_ren = 0
             DO 260 is=1,iseg
               ioff = mylink(4+2*is-1,ilk) + 1
               ilen = mylink(4+2*is,ilk)
               il_rst = il_ren + 1
               il_ren = il_rst + ilen - 1
               pfield(ioff:ioff+ilen-1) = pkwork(il_rst:il_ren)
               ilgb = ilgb + ilen
 260           CONTINUE

               IF (ilgb .le. ig_CLIMmax) THEN
                   irecv  = irecv + 1
                   WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)')
     *                 'Import - <from:',imod,
     *                 '> <step:',kstep,
     *                 '> <len:',ilgb,
     *                 '> <type:',ibyt,
     *                 '> <noproc:',itid,'>'
               ELSE
                   WRITE(nulprt,*)
     $          'The buffer pkwork not big enough to contain the field'
                   WRITE(nulprt,*) 'ABORT in CLIM_Export 1'
                   CALL abort
               ENDIF
           ELSE
               WRITE(UNIT = nulprt,FMT = *)
     $           'CLIM_Import - NOT OK:', il_errgsip
               WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)')
     $           'Import - <dest:',imod, '> <step:',kstep,'> <len:',
     $           ilgb, '> <type:',ibyt, '> <noproc:',itid,'>'
               WRITE(nulprt,*) 'ABORT in CLIM_Import 3'
               CALL FLUSH(nulprt)
               CALL abort
           ENDIF
c
 290     CONTINUE
c
      WRITE(nulprt,FMT='(A,I3,A)') 'Import - ',irecv,' fields imported'
      CALL FLUSH(nulprt)
c     ----------------------------------------------------------------
c
#endif
      RETURN
      END
