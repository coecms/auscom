  SUBROUTINE prism_put_inquire_proto(id_port_id,kstep,kinfo)
!
!*    *** PRISM_put_inquire ***   PRISM 1.0
!
!     purpose:
!     --------
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
!         Sophie Valcke (08/03 - created from prism_put_proto)
!     ----------------------------------------------------------------
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
    IMPLICIT NONE
#if defined use_comm_MPI1 || defined use_comm_MPI2
#include <mpif.h>
#endif
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p)       kstep, kinfo, id_port_id
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p)    il_newtime
    INTEGER (kind=ip_intwp_p)	 iport
!     ----------------------------------------------------------------
!
!*    0. Entering
!     --------------
!
    kinfo = PRISM_Ok
!
!*    1. check for this port in my list
!     ---------------------------------
!
    iport = -1
!   Test if the field is defined in the namcouple and if its coupling period
!   is not greater than the time of the simulation.
    IF (ig_def_freq(id_port_id) .eq. 0.or. &
         ig_def_freq(id_port_id) .gt. ig_ntime) THEN 
       GOTO 1010
    ENDIF
!
!
!     if a field exported at a certain time should match an import
!     at a time+lag, add the lag here; the lag is given by the user
!     in the namcouple at the end of the field 2nd line.
    IF (myport(1,id_port_id).eq.CLIM_Out) THEN
       iport=id_port_id
       il_newtime = kstep + ig_def_lag(iport)
    ENDIF
    IF (iport.lt.0) THEN
       WRITE(nulprt,FMT='(A,A)') &
            'Put - WARNING - Invalid port out: ', &
            cports(id_port_id)
       GO TO 1010
    ENDIF
!
!   If the user indicated in the namcouple that the field must be
!   accumulated or averaged (keyword 'AVERAGE' or 'ACCUMUL' at the
!   end of the field 2nd line), do the local transformations.
!
    IF (ig_def_trans(iport) .eq. ip_average .or. &
       ig_def_trans(iport) .EQ. ip_accumul .OR. &
       ig_def_trans(iport) .EQ. ip_min .OR. &
       ig_def_trans(iport) .EQ. ip_max) THEN
        kinfo = PRISM_LocTrans
    ENDIF
!
!*    Test if field must be written to restart file i.e.
!*    - current time is time at the end of simulation +
!*    - lag of current field is greater than 0
!
    IF (il_newtime.EQ.ig_ntime.AND.ig_def_lag(iport).GT.0) THEN
!ac
       IF (ig_def_state(iport) .NE. ip_output) THEN
!ac
!
!*       Note: A model can have several restart files but same restart 
!*       file can't be used by different models
!
       kinfo = PRISM_ToRest
!ac
   ENDIF
!ac
!    Test if the current time is a coupling (or I/O) time  
       IF (MOD(il_newtime,ig_def_freq(iport)).EQ.0) THEN
#if !defined key_noIO
!*   If the user indicated in the namcouple that the field is
!*   a field output-to-file (keyword 'OUTPUT', 'IGNOUT' or 'EXPOUT'
!*   at the end of the field 1st line), do the writing to file here, e.g.:
!ac
          IF (ig_def_state(iport) .EQ. ip_output ) THEN
              	kinfo = PRISM_Output
          ELSE IF (ig_def_state(iport) .EQ. ip_expout .OR. ig_def_state(iport) .EQ. ip_ignout) THEN
              kinfo = PRISM_ToRestOut
          ENDIF	
!ac
#endif
      ENDIF
  ELSE
!    Test if the current time is a coupling (or I/O) time
       IF (MOD(il_newtime,ig_def_freq(iport)).EQ.0) THEN
!
#if !defined key_noIO
!*   If the user indicated in the namcouple that the field is
!*   a field output-to-file (keyword 'OUTPUT', 'IGNOUT' or 'EXPOUT'
!*   at the end of the field 1st line), do the writing to file here, e.g.:
          IF (ig_def_state(iport) .EQ. ip_output .OR. &
             ig_def_state(iport) .EQ. ip_expout .OR. &
             ig_def_state(iport) .EQ. ip_ignout) THEN
              kinfo = PRISM_Output
          ENDIF
#endif
!*
!*   If the user indicated in the namcouple that the field is
!*   a coupling field (keyword EXPORTED','EXPOUT','IGNORED', 'IGNOUT' 
!*   or 'AUXILARY' at the end of the field 1st line),do the export here.
!*
          IF (ig_def_state(iport) .EQ. ip_expout .OR. &
               ig_def_state(iport) .eq. ip_exported .or. &
               ig_def_state(iport) .eq. ip_ignored .or. &
               ig_def_state(iport) .eq. ip_ignout .or. &
               ig_def_state(iport) .eq. ip_auxilary) THEN

              IF (kinfo .EQ. PRISM_Output) THEN
                  kinfo = PRISM_SentOut
              ELSE
                  kinfo = PRISM_Sent
              ENDIF

          ENDIF
      ENDIF
  ENDIF	
!
!     ----------------------------------------------------------------
!
1010 CONTINUE
    CALL FLUSH(nulprt)
    RETURN
  END SUBROUTINE prism_put_inquire_proto
