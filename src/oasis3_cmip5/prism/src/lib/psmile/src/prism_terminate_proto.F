      SUBROUTINE prism_terminate_proto(kinfo)
c
c*    *** PRISM_Terminate ***   PRISM 1.0
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
c        MPI-1 or MPI-2 or GSIP
c
c     author:
c     -------
c         Arnaud Caubel  - Fecit (08/02 - created from CLIM_Quit)
c         S. Legutke     - MPI M&D, MPI_Finalize info added
c         S. Valcke      - CERFACS: added GSIP
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
#if defined use_comm_GSIP
      INTEGER :: mgi_term
#endif
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)	kinfo
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p) info, il_rank, ji, il_start
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)       il_bufsizebyt
      REAL(kind=ip_double_p)  dl_bufaddr
c
c*    0. Entering
c     -----------
c
      WRITE(nulprt,*)'entering PRISM_terminate'
      kinfo = CLIM_Ok
c
      DEALLOCATE(kbtotproc)
      DEALLOCATE(kbcplproc)
      DEALLOCATE(iga_unitmod)
      DEALLOCATE(cg_modnam)
#if defined use_comm_MPI1 || defined use_comm_MPI2
      CALL MPI_Comm_Rank(mpi_comm,il_rank,mpi_err)
c
      IF (lg_oasis_field) THEN
         il_start = 0
      ELSE
         il_start = 1
      ENDIF

      DO ji = il_start, ncplprocs-1 
        IF (il_rank.eq.modtid(ji)) THEN
#endif
#if !defined key_noIO
           call psmile_close_files(il_err)
           call psmile_io_cleanup(il_err)
#endif
#if defined use_comm_MPI1 || defined use_comm_MPI2
            DEALLOCATE (cnames, stat=il_err)
            IF (il_ERR.ne.0) WRITE(nulprt,*)
     $          'Error in cnames deallocation in prism_terminate'
#endif
            DEALLOCATE (myport, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $          ' Problem in "myport" deallocation in prism_terminate'
            DEALLOCATE (mydist,stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $          ' Problem in "mydist" deallocation in prism_terminate'
            DEALLOCATE (cports, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $          ' Problem in "cports" deallocation in prism_terminate'
            DEALLOCATE(clrport, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $          'Error in clrport deallocation in prism_terminate'
            DEALLOCATE(irdist, stat = il_ERR)
            IF (il_ERR.ne.0) WRITE(nulprt,*)
     $          'Error in irdist deallocation in PRISM_terminate'
            DEALLOCATE(irport, stat = il_ERR)
            IF (il_ERR.ne.0) WRITE(nulprt,*)
     $          'Error in irport deallocation in PRISM_terminate'
            DEALLOCATE (mylink, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $          ' Problem in "mylink" deallocation in prism_terminate'
#if defined use_comm_MPI1 || defined use_comm_MPI2
            DEALLOCATE (pkwork, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $          ' Problem in "pkwork" deallocation in prism_terminate'
#elif defined use_comm_GSIP
            IF (ALLOCATED(pkworkps)) DEALLOCATE (pkworkps,stat=il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $          ' Problem in "pkworkps" deallocation in prism_terminate'
#endif
            DEALLOCATE (ig_def_part, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $      ' Problem in "ig_def_part" deallocation in prism_terminate'
            DEALLOCATE (ig_length_part, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $   ' Problem in "ig_length_part" deallocation in prism_terminate'
#if defined use_comm_MPI1 || defined use_comm_MPI2
             IF (ALLOCATED(pkwork_field))
     $          DEALLOCATE (pkwork_field, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "pkwork_field" deallocation in prism_terminate'
#endif
            DEALLOCATE (cg_cnaminp, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $      ' Problem in "cg_cnaminp" deallocation in prism_terminate'
            DEALLOCATE (cg_cnamout, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $       ' Problem in "cg_cnamout" deallocation in prism_terminate'
            DEALLOCATE (ig_clim_lag, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $      ' Problem in "ig_clim_lag" deallocation in prism_terminate'
            DEALLOCATE (ig_clim_reverse, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $      ' Problem ig_clim_reverse deallocation in prism_terminate'
            DEALLOCATE (ig_clim_invert, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $      ' Problem ig_clim_invert deallocation in prism_terminate'
            DEALLOCATE (ig_def_lag, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $       ' Problem in "ig_def_lag" deallocation in prism_terminate'
            DEALLOCATE (ig_def_reverse, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $       ' Problem ig_def_reverse deallocation in prism_terminate'
            DEALLOCATE (ig_def_invert, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $       ' Problem ig_def_invert deallocation in prism_terminate'
            DEALLOCATE (ig_clim_freq, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $     ' Problem in "ig_clim_freq" deallocation in prism_terminate'
            DEALLOCATE (ig_def_freq, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $      ' Problem in "ig_def_freq" deallocation in prism_terminate'
            DEALLOCATE (ig_clim_seq, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $      ' Problem in "ig_clim_seq" deallocation in prism_terminate'
            DEALLOCATE (ig_def_seq, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $      ' Problem in "ig_def_seq" deallocation in prism_terminate'
            DEALLOCATE (cg_clim_rstfile, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $  ' Problem in "cg_clim_rstfile" deallocation in prism_terminate'
            DEALLOCATE (cg_def_rstfile, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $   ' Problem in "cg_def_rstfile" deallocation in prism_terminate'
            DEALLOCATE (ig_clim_norstfile, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $' Problem in "ig_clim_norstfile" deallocation in prism_terminate'
            DEALLOCATE (ig_def_norstfile, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $' Problem in "ig_def_norstfile" deallocation in prism_terminate'
            DEALLOCATE (ig_clim_state, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "ig_clim_state" deallocation in prism_terminate'
            DEALLOCATE (ig_def_state, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $    ' Problem in "ig_def_state" deallocation in prism_terminate'
             DEALLOCATE (ig_clim_trans, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $   ' Problem in "ig_clim_trans" deallocation in prism_terminate'
             DEALLOCATE (ig_clim_numlab, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $   ' Problem in "ig_clim_numlab" deallocation in prism_terminate'
            DEALLOCATE (ig_def_trans, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $     ' Problem in "ig_def_trans" deallocation in prism_terminate'
            IF (ip_realwp_p == ip_single_p) THEN
                IF (ALLOCATED(rg_field_trans)) 
     $              DEALLOCATE (rg_field_trans, stat = il_ERR)
                IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $      'Problem in rg_field_trans deallocation in prism_terminate'
            ELSE IF (ip_realwp_p == ip_double_p) THEN
                IF (ALLOCATED(dg_field_trans)) 
     $              DEALLOCATE (dg_field_trans, stat = il_ERR)
                IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $      'Problem in dg_field_trans deallocation in prism_terminate'
            ENDIF
            IF (ALLOCATED(ig_number))
     $          DEALLOCATE (ig_number, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $        ' Problem in "ig_number" deallocation in prism_terminate'
            DEALLOCATE (cg_clim_inpfile, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $  ' Problem in "cg_clim_inpfile" deallocation in prism_terminate'
            DEALLOCATE (cg_def_inpfile, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $  ' Problem in "cg_def_inpfile" deallocation in prism_terminate'
            DEALLOCATE (cg_ignout_field, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $  ' Problem in "cg_ignout_field" deallocation in prism_terminate'
            DEALLOCATE (ig_def_numlab, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $  ' Problem in "ig_def_numlab" deallocation in prism_terminate'
            DEALLOCATE (cga_clim_locatorbf, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $  ' Problem cga_clim_locatorbf deallocation in prism_terminate'
            DEALLOCATE (cga_clim_locatoraf, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $  ' Problem cga_clim_locatoraf deallocation in prism_terminate'
            DEALLOCATE (cga_clim_locator, stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $  ' Problem cga_clim_locator deallocation in prism_terminate'
            IF (allocated(ig_aux)) DEALLOCATE(ig_aux,stat = il_ERR)
            IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $  ' Problem in "ig_aux" deallocation in prism_terminate'
#if defined use_comm_MPI1 || defined use_comm_MPI2 
            IF (lg_clim_bsend) THEN
                CALL MPI_Buffer_Detach(dl_bufaddr, il_bufsizebyt, 
     $              mpi_err)
                IF (ALLOCATED(dg_bufsend))
     $              DEALLOCATE (dg_bufsend, stat = il_ERR)
                IF (il_ERR.ne.CLIM_Ok) WRITE(nulprt,*)
     $  ' Problem in "dg_bufsend" deallocation in prism_terminate'
            ENDIF
        ENDIF
      END DO
      DEALLOCATE (modtid, stat=il_err)
      IF (il_ERR.ne.0) WRITE(nulprt,*)
     $    'Error in modtid deallocation in prism_terminate'
c
      WRITE(nulprt, *) 'lg_mpiflag=', lg_mpiflag
c
      info = 0.0    
      IF (.NOT. lg_mpiflag ) THEN
          WRITE(nulprt,FMT='(A)') 
     $    'Calling MPI_Finalize in prism_terminate ...'

          CALL MPI_Finalize ( info )
      ELSE
          WRITE(nulprt,FMT='(A)') 
     $    'No call of MPI_Finalize in prism_terminate.'
      ENDIF      
c
#elif defined use_comm_GSIP
      info = mgi_term()
      IF (info .EQ. 0) THEN
          WRITE(UNIT = nulprt,FMT = *) 
     $        'prism_terminate_proto -  mgi terminated OK:', info
      ELSE IF (info .EQ. -1) THEN
          WRITE(UNIT = nulprt,FMT = *) 
     $        'prism_terminate_proto info :', info
          WRITE(UNIT = nulprt,FMT = *) 
     $        'All channels probably already closed by Oasis'
      ELSE
          WRITE(UNIT = nulprt,FMT = *) 
     $        'prism_terminate_proto error :', info
          CALL prism_abort_proto (0, 'prism_terminate_proto', 
     $        'STOP - mgi not terminated OK)')
      ENDIF
#endif
      WRITE(nulprt,FMT='(A,I3,A)')
     $    'Quit - exit status <mpi ',info,'>'
c
c     ----------------------------------------------------------------
c
      CLOSE(nulprt)
      RETURN
      END
