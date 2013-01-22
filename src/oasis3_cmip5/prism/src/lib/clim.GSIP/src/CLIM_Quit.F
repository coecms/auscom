      SUBROUTINE CLIM_Quit(kinfo)
c
c*    *** Quit ***   CLIM 3.0
c
c     purpose:
c     --------
c        leave mpi
c
c     interface:
c     ----------
c	 kinfo	: output status
c
c     lib mp:
c     -------
c        GSIP
c
c     author:
c     -------
c        Sophie Valcke  
c        (14/01/05 -created from lib/clim/src/CLIM_Quit)
c     ----------------------------------------------------------------
#if defined use_comm_GSIP
      USE mod_kinds_oasis
      USE mod_clim
      USE mod_comclim
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)	kinfo
      INTEGER :: mgi_term
c
c*    0. Entering
c     --------------
c
      WRITE(nulprt,*)'entering CLIM_Quit'
      kinfo = CLIM_Ok
c
      DEALLOCATE (ncode, stat=il_err)
      IF (il_ERR.ne.0) WRITE(nulprt,*)
     $    'Error in "ncode" deallocation in CLIM_Quit routine ! '
      DEALLOCATE (cnames, stat=il_err)
      IF (il_ERR.ne.0) WRITE(nulprt,*)
     $    'Error in "cnames" deallocation in CLIM_Quit routine ! '
      DEALLOCATE (myport, stat = il_ERR)
      IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "myport" deallocation in CLIM_Quit !'
      DEALLOCATE (mydist,stat = il_ERR)
      IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "mydist" deallocation in CLIM_Quit !'
      DEALLOCATE (cports, stat = il_ERR)
      IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "cports" allocation in CLIM_Quit !'
      DEALLOCATE(clrport, stat = il_ERR)
      IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $    'Error in clrport deallocation in CLIM_Quit !'
      DEALLOCATE(irdist, stat = il_ERR)
      IF (il_ERR.ne.0) WRITE(nulprt,*)
     $    'Error in "irdist" deallocation in CLIM_Quit'
      DEALLOCATE(irport, stat = il_ERR)
      IF (il_ERR.ne.0) WRITE(nulprt,*)
     $    'Error in "irport" deallocation in CLIM_Quit'
      DEALLOCATE (mylink, stat = il_ERR)
      IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "mylink" allocation in CLIM_Quit !'
      DEALLOCATE (pkwork, stat = il_ERR)
      IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "pkwork" allocation in CLIM_Quit !'
c
      kinfo = mgi_term()
c
      WRITE(nulprt,FMT='(A,I3,A)')
     *    'Quit - mgi_term status <mpi ',kinfo,'>'
c     ----------------------------------------------------------------
c
      CLOSE (nulprt)
#endif
      RETURN
      END
