  SUBROUTINE prism_put_restart_proto (id_port_id, kstep, kinfo)
!
!*    *** PRISM_put ***   PRISM 1.0
!
!     purpose:
!     --------
!        write buffered field corresponding to port id_port_id in its 
!        restart coupling file
!
!     interface:
!     ----------
!        id_port_id : port number of the field
!	 kstep	: current time in seconds
!	 kinfo	: output status
!
!     lib mp:
!     -------
!        none
!
!     author:
!     -------
!         Sophie Valcke (07/03 - created from prism_put_proto)
!     ----------------------------------------------------------------
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
    IMPLICIT NONE
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
#endif
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p)      kstep, kinfo, id_port_id
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p)	   iport
!     ----------------------------------------------------------------
!
!*    0. Entering
!     -----------
!
    kinfo = PRISM_Ok
!
!*    1. check for this port in my list
!     ---------------------------------
!
    iport = -1
!
!   Test if the field is defined in the namcouple and if its coupling period
!   is not greater than the time of the simulation.
    IF (ig_def_freq(id_port_id) .eq. 0.or. &
         ig_def_freq(id_port_id) .gt. ig_ntime) THEN 
       GOTO 1010
    ENDIF
!
    IF (myport(1,id_port_id).eq.CLIM_Out) THEN
       iport=id_port_id
    ENDIF
    IF (iport.lt.0) THEN
       kinfo = CLIM_BadPort
       WRITE(nulprt,FMT='(A,A)') &
            'Put - WARNING - Invalid port out: ', &
            cports(id_port_id)
       GO TO 1010
    ENDIF
!
!*  Field is written to restart file 
    IF (lg_dgfield) THEN
        IF (mydist(CLIM_Strategy,iport) .EQ. CLIM_Serial) THEN
            CALL write_filer8(dg_field_trans(:,iport),cports(iport),iport)
        ELSE
            CALL write_file_parar8(dg_field_trans(:,iport),cports(iport),iport)
        ENDIF
    ELSE
        IF (mydist(CLIM_Strategy,iport) .EQ. CLIM_Serial) THEN
            CALL write_filer8(rg_field_trans(:,iport),cports(iport),iport)
        ELSE
            CALL write_file_parar8(rg_field_trans(:,iport),cports(iport),iport)
        ENDIF
    ENDIF
    kinfo = PRISM_ToRest
!
!     ----------------------------------------------------------------
!
1010 CONTINUE
    CALL FLUSH(nulprt)
!
  END SUBROUTINE prism_put_restart_proto

