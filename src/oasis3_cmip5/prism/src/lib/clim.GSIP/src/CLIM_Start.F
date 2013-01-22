      SUBROUTINE CLIM_Start(kinfo)
c
c*    *** Start ***   CLIM 3.0
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
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p) kinfo
c     ----------------------------------------------------------------
c
      kinfo  = CLIM_Ok
      CALL CLIM_Start_MPI(kinfo)
      WRITE (nulprt,FMT='(A)') 'Returning from Start -- '
      CALL FLUSH(nulprt)
#endif
      RETURN
      END
