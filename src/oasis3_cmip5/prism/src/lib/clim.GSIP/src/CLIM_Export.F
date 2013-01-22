      SUBROUTINE CLIM_Export(id_port_id,kstep,pfield,kinfo)
c
c*    *** Export ***   
c
c     purpose:
c     --------
c        give pfield to models connected to port id_port_id at time kstep
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
c        Sophie Valcke, CERFACS (02/12/2004 - adapted from CLIM_Export for MPI)
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
      INTEGER (kind=ip_intwp_p)	   isend, ip, iport, ilk, iseg, is, 
     $    ilgb, imod, itid, itag, ilen, ioff, ityp, ibyt
      INTEGER :: il_errgsipil_rst, il_ren, jl, ji
      INTEGER :: mgi_write
c     ----------------------------------------------------------------
c
c*    0. Entering
c     -----------
c
      WRITE(nulprt, *)'entering CLIM_Export'
      kinfo = CLIM_Ok
c
c*    1. check for this port in my list
c     ---------------------------------
c
      isend = 0
      iport = -1
c
      IF (myport(1,id_port_id).eq.CLIM_Out) iport=id_port_id

      IF (iport.lt.0) THEN
         kinfo = CLIM_BadPort
         WRITE(nulprt,FMT='(A,A)')
     $       'Export - WARNING Invalid port out: ', cports(id_port_id)
         WRITE(nulprt,*) 'ABORT in CLIM_Export 0'
         CALL FLUSH(nulprt)
         CALL abort
      ENDIF
c
c*    2. check for connected ports (in)
c     ---------------------------------
c
      WRITE(nulprt,FMT='(A,A)') 'Export - ', cports(iport)
c
      ityp = myport(2,iport)
      ibyt = myport(3,iport)
c
      DO 290 ip=1,myport(5,iport)
c
         ilk  = myport(5+ip,iport)
c        imod is the distant model number
         imod = mylink(1,ilk)
c        itid is the distant local process number
         itid = mylink(2,ilk)
         itag = mylink(3,ilk) - kstep / ig_frqmin
         iseg = mylink(4,ilk)
c
         ilgb = 0
         il_rst = 0
         il_ren = 0
         DO 240 is=1,iseg
           ioff = mylink(4+2*is-1,ilk) + 1
           ilen = mylink(4+2*is,ilk)
           il_rst = il_ren + 1
           il_ren = il_rst + ilen - 1
           pkwork(il_rst:il_ren) = pfield(ioff:ioff+ilen-1)
           ilgb = ilgb + ilen
 240     CONTINUE
         IF (ilgb*ibyt .gt. ig_maxtype) THEN
             WRITE(nulprt,*)
     $           'The buffer pkwork not big enough to contain the field'
             WRITE(nulprt,*) 'ABORT in CLIM_Export 1'
             CALL abort
         ENDIF
c
         jl = itid
         IF (imod .gt. 1) THEN
             DO 250  ji = 1, imod - 1
               jl = jl + iga_totproc(ji)
 250         CONTINUE
         ENDIF
 
         IF ( ip_realwp_p .eq. ip_single_p ) THEN 
cSV             IF (jl .eq. 1) THEN
cSV                 WRITE(88, *) 'cport =', cports(iport)
cSV                 WRITE(88, *)
cSV                 WRITE(88, *) 'pkwork = ', pkwork(1:100)
cSV                 CALL FLUSH(88)
cSV             ENDIF
cSV            IF (jl .eq. 1) then
cSV             il_errgsip = 
cSV     $           mgi_write (iga_gsipw(jl), cports(iport), 32, 'C')
cSV             ENDIF
C 
             il_errgsip = 
     $           mgi_write (iga_gsipw(jl), pkwork, ig_CLIMmax, 'R')
         ELSE IF ( ip_realwp_p .eq. ip_double_p ) THEN
cSV             IF (jl .eq. 1) THEN
cSV                 pkwork(:) = 1.0
cSV                 WRITE(88, *) 'cport =', cports(iport)
cSV                 WRITE(88, *)
cSV                 WRITE(88, *) 'pkwork = ', pkwork(1:100)
cSV                 CALL FLUSH(88)
cSV             ENDIF
cSV            IF (jl .eq. 1) then
cSV             il_errgsip = 
cSV     $           mgi_write (iga_gsipw(jl), cports(iport), 32, 'C')
cSV             ENDIF
             il_errgsip = 
     $           mgi_write (iga_gsipw(jl), pkwork, ig_CLIMmax, 'D')
         ELSE
             WRITE(nulprt,*)
     $           'Problem with the coupling field type'
             WRITE(nulprt,*) 'ABORT in CLIM_Export 2'
             CALL FLUSH(nulprt)
             CALL abort
         ENDIF
         IF (il_errgsip .ge. 0) THEN
             WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)')
     $           'Export - <dest:',imod, '> <step:',kstep,'> <len:',
     $           ilgb, '> <type:',ibyt, '> <noproc:',itid,'>'
             isend = isend + 1
         ELSE
             WRITE(UNIT = nulprt,FMT = *)
     $           'CLIM_Export - NOT OK:', il_errgsip
             WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)')
     $           'Export - <dest:',imod, '> <step:',kstep,'> <len:',
     $           ilgb, '> <type:',ibyt, '> <noproc:',itid,'>'
             WRITE(nulprt,*) 'ABORT in CLIM_Export 3'
             CALL FLUSH(nulprt)
             CALL abort
         ENDIF
c
c
 290   CONTINUE
c
       WRITE(nulprt,FMT='(A,I3,A)') 
     $     'Export - ',isend,' fields exported'
       CALL FLUSH(nulprt)
c     ----------------------------------------------------------------
c
#endif
      RETURN
      END
