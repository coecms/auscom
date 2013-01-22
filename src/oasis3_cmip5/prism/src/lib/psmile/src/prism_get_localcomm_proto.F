       SUBROUTINE prism_get_localcomm_proto(il_local_comm, kinfo)
c
c*    *** Get_localcomm ***   PRISM 1.0
c
c     purpose:
c     --------
c        Each component get his local communicator.
c
c     interface:
c     ----------
c        ig_local_comm : local communicator
c        kinfo  : exit status
c
c     lib mp:
c     -------
c        mpi-1 or mpi-2 or GSIP
c
c     author:
c     -------
c        Arnaud Caubel - Fecit (08/02 - created)
c
c     ----------------------------------------------------------------
c
      USE mod_kinds_model
      USE mod_prism_proto 
      USE mod_comprism_proto
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)	    il_local_comm, kinfo
c     ----------------------------------------------------------------

      il_local_comm = ig_local_comm
      kinfo = CLIM_Ok

      END
