module mod_prism_get_proto
#include "psmile_os.h"

  interface prism_get_proto
     
#ifndef __NO_4BYTE_REALS
     module procedure prism_get_proto_r14
     module procedure prism_get_proto_r24
#endif
     module procedure prism_get_proto_r18, &
                      prism_get_proto_r28
     
  end interface

contains

  SUBROUTINE prism_get_proto_r14(id_port_id,kstep,rd_field,kinfo)
!
!*    *** PRISM_get ***   PRISM 1.0
!
!     purpose:
!     --------
!        recv pfield from oasis or models connected to port id_port_id
!
!     interface:
!     ----------
!        id_port_id : port number of the field
!	 kstep	: current time in seconds
!	 pfield	: buffer of reals
!	 kinfo	: output status
!
!     lib mp:
!     -------
!        mpi-1 or mpi-2 or gsip
!
!     author:
!     -------
!        Arnaud Caubel  - Fecit (08/02 - created from CLIM_Import)
!
!     modified:
!     ---------
!        Reiner Vogelsang, SGI,  27 April 2003
!        - Screening of 4 byte real interfaces in case a of dbl4 compilation.
!        File has to be preprocessed with -D__SXdbl4.
!        S. Legutke,       MPI M&D  (05/03 - kinfo = PRISM_Recvd added)
!        S. Valcke, CERFACS, 24/10/2004: Added GSIP
!
!---------------------------------------------------------------------
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
#if defined use_comm_GSIP 
      USE mod_gsip_model
#endif
    IMPLICIT none
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
    INTEGER(kind=ip_intwp_p)     istatus(MPI_STATUS_SIZE)
#endif
!     ----------------------------------------------------------------
    INTEGER(kind=ip_intwp_p), intent(in) :: id_port_id,kstep
    INTEGER(kind=ip_intwp_p), intent(out) :: kinfo
    REAL(kind=ip_single_p), DIMENSION(myport(4,id_port_id)), intent(inout) :: rd_field
!     ----------------------------------------------------------------
    INTEGER(kind=ip_intwp_p)     info, ip, iport 
    INTEGER(kind=ip_intwp_p)	  irecv, imod, ilk, iseg, is, ilgb
    INTEGER(kind=ip_intwp_p)     itid, itag, il_len, ioff, ityp, ibyt
    INTEGER(kind=ip_intwp_p)     iposbuf, imaxbyt
#if defined __DEBUG
    INTEGER(kind=ip_intwp_p)     icount
    INTEGER(kind=ip_intwp_p), parameter :: icountmax=600
    LOGICAL ::                   iflag
#endif
#ifdef use_comm_GSIP
     INTEGER(kind=ip_intwp_p)     ::  il_rst, il_ren, il_errgsip 
     INTEGER :: mgi_read
#endif
!     ----------------------------------------------------------------
!
#if defined use_comm_MPI1 || defined use_comm_MPI2 
    istatus(:)=0
#endif

!*    0. Entering
!     -----------
!
    kinfo = PRISM_Ok
!
!*    1. check for this port in my list
!     ---------------------------------
!
    irecv = 0
    iport = -1
!
!   Test if the field is defined in the namcouple and if its coupling period
!   is not greater than the time of the simulation.
    IF (ig_def_freq(id_port_id) .eq. 0 .or. &
         ig_def_freq(id_port_id) .gt. ig_ntime .or. &
         ig_def_state(id_port_id) .eq. ip_auxilary) THEN 
       GOTO 1010
    ENDIF
    IF (myport(1,id_port_id).eq.CLIM_In) iport=id_port_id
    IF (iport.lt.0) THEN
       kinfo = CLIM_BadPort
       WRITE(nulprt,FMT='(A,A)')'Get - WARNING - Invalid port in: ', &
            cports(id_port_id)
       GO TO 1010
    ENDIF
!
!*    Test if the current time is a coupling (or I/O) time
! 
    IF (mod(kstep,ig_def_freq(iport)).eq.0) THEN
!
!*    If the user indicated in the namcouple that the field is
!*    a field input-from-file (keyword 'INPUT' at the end of the
!*    field 1st line), do the reading from file here, e.g.:
!
#if !defined key_noIO
       IF (ig_def_state(iport) .EQ. ip_input) THEN
           CALL psmile_read_4(iport,rd_field,kstep)
           kinfo = PRISM_Input
       ENDIF
#endif
!
!* Define return code (direct or via Oasis does not matter)
!
       IF (kstep.EQ.0 .AND. ig_def_lag(iport) .GT. 0) THEN
           kinfo = PRISM_FromRest
#if !defined key_noIO
           IF (ig_def_state(iport) .EQ. ip_ignout .OR. &
              ig_def_state(iport) .EQ. ip_expout) THEN
               kinfo = PRISM_FromRestOut
           ENDIF
#endif  
       ELSE
           IF (ig_def_state(iport) .NE. ip_input) THEN
               kinfo = PRISM_Recvd
!
#if !defined key_noIO
               IF (ig_def_state(iport) .EQ. ip_expout .OR. &
                  ig_def_state(iport) .EQ. ip_ignout) THEN
                   kinfo = PRISM_RecvOut
               ENDIF
#endif
           ENDIF
       ENDIF
!
       IF (kstep.eq.0 .and. ig_def_lag(iport) .gt. 0 .and. &
            (ig_def_state(iport) .eq. ip_ignored .or. &
            ig_def_state(iport) .eq. ip_ignout)) THEN
!
!*       Note: A model can have several restart files but same restart 
!*       file can't be used by different models
!*       Test if model is serial or parallel and if variables are real
!        or double precision
          IF (mydist(CLIM_Strategy,iport) .eq. CLIM_Serial) THEN
             call read_filer4(rd_field, cports(iport),iport)
#if defined use_libMPI || defined use_comm_MPI1 || defined use_comm_MPI2
          ELSE
             call read_file_parar4(rd_field, cports(iport),iport)
#endif
          ENDIF
#if !defined key_noIO
          IF (ig_def_state(iport) .EQ. ip_ignout) &
             CALL psmile_write_4(iport,rd_field,kstep)
#endif           
       ELSE
!*
!*    If the user indicated in the namcouple that the field is
!*    a coupling field then do the import :
          IF (ig_def_state(iport) .ne. ip_output .and. &
               ig_def_state(iport) .ne. ip_input) THEN
!
!*       Check for connected ports (in)
!        ------------------------------
!
             WRITE(nulprt,FMT='(A,A)') 'Get - ', cports(iport)
!
             ityp = myport(2,iport)
             ibyt = myport(3,iport)
!     
             DO ip=1,myport(5,iport)
!     
                ilk  = myport(5+ip,iport)
                imod = mylink(1,ilk)
                itid = mylink(2,ilk)
                itag = mylink(3,ilk) - kstep / ig_frqmin
                iseg = mylink(4,ilk)
!
!*   Implementation with "blocking" receives : the program will wait
!*   indefinitely until a message is received (this may generate a
!*   deadlock if all models are waiting on a receive).
!*   However this method will be more efficient in most cases than the
!*   receives with a time-out loop. 
!
#if defined use_comm_MPI1 || use_comm_MPI2   
#ifdef __DEBUG
!jl
!jl add a nonblocking syntax, in order to avoid deadlocks, when NO mailbox
!jl exist in the network  (2004-04-28)
!jl Also, allows to check the timing of the receives of messages 
!jl For completion, the same syntax should be added in oasis "Getfld"
!jl
                CALL MPI_Iprobe ( itid, itag, mpi_comm, iflag, istatus, info )
                WRITE(nulprt,*) 'probing for tid = ',itid,' tag = ',itag, &
                ' comm = ',mpi_comm,' result is : ',iflag
                call flush(nulprt)

                IF (.NOT.iflag) THEN
                   icount = 0
   WAITLOOP:       DO
                   CALL  MPI_Iprobe ( itid, itag, mpi_comm, iflag, istatus, info )
                   icount = icount + 1
                   IF ( iflag ) EXIT WAITLOOP
                   IF ( icount .GE. icountmax ) THEN
                      WRITE(nulprt,*) 'probing for tid = ',itid,' tag = ',itag, &
                      ' still negative after ',icountmax,' seconds : Abort the job'
                      call flush(nulprt)
                      CALL MPI_ABORT (mpi_comm, 0, mpi_err)
                   ENDIF
                   call sleep(1)
                   END DO WAITLOOP
                   WRITE(nulprt,*) 'probing for tid = ',itid,'icount = ', icount
                   call flush(nulprt)
                ENDIF
#endif
!jl
                CALL MPI_Recv ( pkwork_field, ig_maxtype_field, MPI_PACKED, &
                     itid, itag, mpi_comm, istatus, info )
                CALL MPI_Get_count ( istatus, MPI_PACKED, imaxbyt, &
                     info )
!     
                IF ( info .EQ. CLIM_ok  .AND.  imaxbyt .GT. 0) THEN
                   ilgb = 0
                   iposbuf = 0
                   DO is=1,iseg
                      ioff = mylink(4+2*is-1,ilk) * 2 + 1
                      il_len = mylink(4+2*is,ilk)
!     
                      IF ( ityp .EQ. PRISM_Real ) THEN
                         CALL MPI_Unpack ( pkwork_field, ig_maxtype_field, &
                              iposbuf, rd_field(ioff), il_len, &
                              MPI_REAL, mpi_comm, info)
                      ELSE
                         WRITE(nulprt,*)'Get - pb type incorrect ',ityp
                         kinfo = CLIM_BadType
                         GO TO 1010
                      ENDIF
                      ilgb = ilgb + il_len
                   ENDDO
                   IF (ilgb*ibyt .le. imaxbyt) THEN
                      irecv  = irecv + 1
                      nbrecv = nbrecv + ilgb * ibyt
                      WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                           'Get - <from:',imod, &
                           '> <step:',kstep, &
                           '> <len:',ilgb, &
                           '> <type:',ibyt, &
                           '> <tag:',itag,'>' 
                   ELSE
                      kinfo = CLIM_Unpack
                      WRITE(nulprt,FMT='(A,I3,A)')'Get - pb unpack <mpi ', &
                           info,'>'
                      GO TO 1010
                   ENDIF
                ELSE
                   kinfo = CLIM_TimeOut
                   WRITE(nulprt,FMT='(A,I3,A)') &
                        'Get - abnormal exit from trecv <mpi ',info,'>'
                   GO TO 1010
                ENDIF
!
#elif defined use_comm_GSIP
                if (myport(5,iport) .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- only one reception from Oasis,  myport(5,iport) should be 1') 
                if (imod .ne. 0) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if received from Oasis, imod should be 0')                   
                if (itid .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if received from Oasis, itid should be 1')  
                 
!
!               Read info in channel from Oasis (no DIRECT communication)
!
                il_errgsip = mgi_read (ig_gsipr, pkworkps, ig_CLIMmax , 'R')
                IF (il_errgsip .GE. 0) THEN
                    WRITE(UNIT = nulprt,FMT = *) &
                       'prism_get_proto - pkworkps read OK:', il_errgsip
                    call flush(nulprt)
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '1- prism_get_proto - error :', il_errgsip
                    call flush(nulprt)
                    CALL prism_abort_proto (0, 'prism_get_proto', &
                       'STOP - pkworkps not read OK)')
                ENDIF
!
!               Fill rd_field with segments of pkworkps
                ilgb = 0
                il_rst = 0
                il_ren = 0
                DO is=1,iseg
                  ioff = mylink(4+2*is-1,ilk) + 1
                  il_len = mylink(4+2*is,ilk)
                  il_rst = il_ren + 1
                  il_ren = il_rst + il_len - 1
                  rd_field(ioff:ioff+il_len-1) = pkworkps(il_rst:il_ren)
!     
                  ilgb = ilgb + il_len
                ENDDO
                IF (ilgb .LE. ig_CLIMmax) THEN
                    irecv  = irecv + 1
                    WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                       'Get - <from:',imod, '> <step:',kstep, &
                       '> <len:',ilgb, '> <type:',ibyt, '> <tag:',itid,'>' 
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '2- prism_get_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_get_proto', &
                       'STOP - sum of segments greater than pkworkps size')
                ENDIF
#endif  
             ENDDO
!     
             WRITE(nulprt,FMT='(A,I3,A)')'Get - ',irecv,' fields imported'
!
#if !defined key_noIO
             IF (ig_def_state(iport) .eq. ip_expout .or. &
                  ig_def_state(iport) .eq. ip_ignout) &
!
!*    If the user indicated in the namcouple that the field must be written 
!*     to file, do the writing here :
!
                  CALL psmile_write_4(iport,rd_field,kstep)
#endif
          ENDIF
       ENDIF
    ENDIF
!
!     ----------------------------------------------------------------
!
1010 CONTINUE
    CALL FLUSH(nulprt)
    RETURN
  END SUBROUTINE prism_get_proto_r14

  SUBROUTINE prism_get_proto_r18(id_port_id,kstep,rd_field,kinfo)
!
!*    *** PRISM_get ***   PRISM 1.0
!
!     purpose:
!     --------
!        recv pfield from oasis or models connected to port id_port_id
!
!     interface:
!     ----------
!        id_port_id : port number of the field
!	 kstep	: current time in seconds
!	 rd_field	: buffer of reals
!	 kinfo	: output status
!
!     lib mp:
!     -------
!        mpi-1 or mpi-2 or gsip
!
!     author:
!     -------
!        Arnaud Caubel  - Fecit (08/02 - created from CLIM_Import)
!        S. Legutke,    - MPI M&D  (05/03 - kinfo = PRISM_Recvd added)
!        S. Valcke, CERFACS, 24/10/2004: Added GSIP
!
!     ----------------------------------------------------------------
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
#if defined use_comm_GSIP 
      USE mod_gsip_model
#endif
    IMPLICIT none
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
    INTEGER (kind=ip_intwp_p)     istatus(MPI_STATUS_SIZE)
#endif
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p), intent(in) :: id_port_id,kstep
    INTEGER (kind=ip_intwp_p), intent(out) :: kinfo
    REAL(kind=ip_double_p), DIMENSION(myport(4,id_port_id)), intent(inout) :: rd_field
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p)     info, ip, iport 
    INTEGER (kind=ip_intwp_p)	    irecv, imod, ilk, iseg, is, ilgb
    INTEGER (kind=ip_intwp_p)     itid, itag, il_len, ioff, ityp, ibyt
    INTEGER (kind=ip_intwp_p)     iposbuf, imaxbyt
#if defined __DEBUG
    INTEGER(kind=ip_intwp_p)     icount
    INTEGER(kind=ip_intwp_p), parameter :: icountmax=600
    LOGICAL ::                   iflag
#endif
#ifdef use_comm_GSIP
     INTEGER(kind=ip_intwp_p)     ::  il_rst, il_ren, il_errgsip 
     INTEGER :: mgi_read
#endif
!     ----------------------------------------------------------------
!
#if defined use_comm_MPI1 || defined use_comm_MPI2 
    istatus(:)=0
#endif
!
!*    0. Entering
!     --------------
!
    kinfo = PRISM_Ok
!
!*    1. check for this port in my list
!     ---------------------------------
!
    irecv = 0
    iport = -1
!
!   Test if the field is defined in the namcouple and if its coupling period
!   is not greater than the time of the simulation.
    IF (ig_def_freq(id_port_id) .eq. 0 .or. &
         ig_def_freq(id_port_id) .gt. ig_ntime .or. &
         ig_def_state(id_port_id) .eq. ip_auxilary) THEN 
       GOTO 1010
    ENDIF
    IF (myport(1,id_port_id).eq.CLIM_In) iport=id_port_id
    IF (iport.lt.0) THEN
       kinfo = CLIM_BadPort
       WRITE(nulprt,FMT='(A,A)')'Get - WARNING - Invalid port in: ', &
            cports(id_port_id)
       GO TO 1010
    ENDIF
!
!*    Test if the current time is a coupling (or I/O) time
! 
    IF (mod(kstep,ig_def_freq(iport)).eq.0) THEN
!
!*    If the user indicated in the namcouple that the field is
!*    a field input-from-file (keyword 'INPUT' at the end of the
!*    field 1st line), do the reading from file here, e.g.:
!
#if !defined key_noIO
       IF (ig_def_state(iport) .eq. ip_input) then
           WRITE(nulprt,*) 'Get - Input field'
           CALL psmile_read_8(iport,rd_field,kstep)
           kinfo = PRISM_Input
       ENDIF
#endif
!
!* Define return code (direct or via Oasis does not matter)
!
       IF (kstep.EQ.0 .AND. ig_def_lag(iport) .GT. 0) THEN
           kinfo = PRISM_FromRest
#if !defined key_noIO
           IF (ig_def_state(iport) .EQ. ip_ignout .OR. &
              ig_def_state(iport) .EQ. ip_expout) THEN
               kinfo = PRISM_FromRestOut
           ENDIF
#endif  
       ELSE
           IF (ig_def_state(iport) .NE. ip_input) THEN
               kinfo = PRISM_Recvd
!
#if !defined key_noIO
               IF (ig_def_state(iport) .EQ. ip_expout .OR. &
                  ig_def_state(iport) .EQ. ip_ignout) THEN
                   kinfo = PRISM_RecvOut
               ENDIF
#endif
           ENDIF
       ENDIF
!
!*     Test if first import and if the user indicated in the 
!*     namcouple that the field is exchanged directly 
!*     between the models and not treated by Oasis
!*     (keyword 'IGNORED' or 'IGNOUT' at the end of the field 1st line),
!*     do the reading from restart file (not implemented).
! 
       IF (kstep.eq.0 .and. ig_def_lag(iport) .gt. 0 .and. &
            (ig_def_state(iport) .eq. ip_ignored .or. &
          ig_def_state(iport) .eq. ip_ignout)) THEN
!
!*       Note: A model can have several restart files but same restart 
!*       file can't be used by different models
!*       Test if model is serial or parallel and if variables are real
!        or double precision
          IF (mydist(CLIM_Strategy,iport) .eq. CLIM_Serial) THEN
             call read_filer8(rd_field, cports(iport),iport)
#if defined use_libMPI || defined use_comm_MPI1 || defined use_comm_MPI2
          ELSE
             call read_file_parar8(rd_field, cports(iport),iport)
#endif
          ENDIF
#if !defined key_noIO
          IF (ig_def_state(iport) .EQ. ip_ignout) &
              CALL psmile_write_8(iport,rd_field,kstep)
#endif
       ELSE
!*
!*    If the user indicated in the namcouple that the field is
!*    a coupling field then do the import :
!
          IF (ig_def_state(iport) .ne. ip_output .and. &
               ig_def_state(iport) .ne. ip_input) THEN
!
!*       Check for connected ports (in)
!        ------------------------------
!
             WRITE(nulprt,FMT='(A,A)') 'Get - ', cports(iport)
!
             ityp = myport(2,iport)
             ibyt = myport(3,iport)
!  
             DO ip=1,myport(5,iport)
!     
                ilk  = myport(5+ip,iport)
                imod = mylink(1,ilk)
                itid = mylink(2,ilk)
                itag = mylink(3,ilk) - kstep / ig_frqmin
                iseg = mylink(4,ilk)
!     
!*   Implementation with "blocking" receives : the program will wait
!*   indefinitely until a message is received (this may generate a
!*   deadlock if all models are waiting on a receive).
!*   However this method will be more efficient in most cases than the
!*   receives with a time-out loop. 
!     
!
#if defined use_comm_MPI1 || defined use_comm_MPI2
#if defined __DEBUG 
!jl
!jl add a nonblocking syntax, in order to avoid deadlocks, when NO mailbox
!jl exist in the network  (2004-04-28)
!jl
                CALL MPI_Iprobe ( itid, itag, mpi_comm, iflag, istatus, info )
                WRITE(nulprt,*) 'probing for tid = ',itid,' tag = ',itag, &
                ' comm = ',mpi_comm,' result is : ',iflag
                call flush(nulprt)

                IF (.NOT.iflag) THEN
                   icount = 0
   WAITLOOP:       DO
                   CALL  MPI_Iprobe ( itid, itag, mpi_comm, iflag, istatus, info )
                   icount = icount + 1
                   IF ( iflag ) EXIT WAITLOOP
                   IF ( icount .GE. icountmax ) THEN
                      WRITE(nulprt,*) 'probing for tid = ',itid,' tag = ',itag, &
                      ' still negative after ',icountmax,' seconds : Abort the job'
                      call flush(nulprt)
                      CALL MPI_ABORT (mpi_comm, 0, mpi_err)
                   ENDIF
                   call sleep(1)
                   END DO WAITLOOP
                   WRITE(nulprt,*) 'probing for tid = ',itid,'icount = ', icount
                   call flush(nulprt)
                ENDIF
#endif
!jl
                CALL MPI_Recv ( pkwork_field, ig_maxtype_field, MPI_PACKED, &
                     itid, itag, mpi_comm, istatus, info )
                CALL MPI_Get_count ( istatus, MPI_PACKED, imaxbyt, &
                     info )
!     
                IF ( info .EQ. CLIM_ok  .AND.  imaxbyt .GT. 0) THEN
                   ilgb = 0
                   iposbuf = 0
                   DO is=1,iseg
                      ioff = mylink(4+2*is-1,ilk) + 1
                      il_len = mylink(4+2*is,ilk)
!     
                      IF ( ityp .EQ. PRISM_Real ) THEN
                         CALL MPI_Unpack ( pkwork_field, ig_maxtype_field, &
                              iposbuf, rd_field(ioff), il_len, &
                              MPI_DOUBLE_PRECISION, mpi_comm, info)
                      ELSE
                         WRITE(nulprt,*)'Get - pb type incorrect ',ityp
                         kinfo = CLIM_BadType
                         GO TO 1010
                      ENDIF
                      ilgb = ilgb + il_len
                   ENDDO
                   IF (ilgb*ibyt .le. imaxbyt) THEN
                      irecv  = irecv + 1
                      nbrecv = nbrecv + ilgb * ibyt
                      WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                           'Get - <from:',imod, &
                           '> <step:',kstep, &
                           '> <len:',ilgb, &
                           '> <type:',ibyt, &
                           '> <tag:',itag,'>' 
                   ELSE
                      kinfo = CLIM_Unpack
                      WRITE(nulprt,FMT='(A,I3,A)')'Get - pb unpack <mpi ', &
                           info,'>'
                      GO TO 1010
                   ENDIF
                ELSE
                   kinfo = CLIM_TimeOut
                   WRITE(nulprt,FMT='(A,I3,A)') &
                        'Get - abnormal exit from trecv <mpi ',info,'>'
                   GO TO 1010
                ENDIF
!
#elif defined use_comm_GSIP
                if (myport(5,iport) .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- only one reception from Oasis,  myport(5,iport) should be 1') 
                if (imod .ne. 0) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if received from Oasis, imod should be 0')                   
                if (itid .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if received from Oasis, itid should be 1')  
                 
!
!               Read info in channel from Oasis (no DIRECT communication)
!
                il_errgsip = mgi_read (ig_gsipr, pkworkps, ig_CLIMmax , 'D')
                IF (il_errgsip .GE. 0) THEN
                    WRITE(UNIT = nulprt,FMT = *) &
                       'prism_get_proto - pkworkps read OK:', il_errgsip
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '1- prism_get_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_get_proto', &
                       'STOP - pkworkps not read OK)')
                ENDIF
!
!               Fill rd_field with segments of pkworkps
                ilgb = 0
                il_rst = 0
                il_ren = 0
                DO is=1,iseg
                  ioff = mylink(4+2*is-1,ilk) + 1
                  il_len = mylink(4+2*is,ilk)
                  il_rst = il_ren + 1
                  il_ren = il_rst + il_len - 1
                  rd_field(ioff:ioff+il_len-1) = pkworkps(il_rst:il_ren)
!     
                  ilgb = ilgb + il_len
                ENDDO
                IF (ilgb .LE. ig_CLIMmax) THEN
                    irecv  = irecv + 1
                    WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                       'Get - <from:',imod, '> <step:',kstep, &
                       '> <len:',ilgb, '> <type:',ibyt, '> <tag:',itid,'>' 
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '2- prism_get_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_get_proto', &
                       'STOP - sum of segments greater than pkworkps size')
                ENDIF
#endif 
              ENDDO
!     
             WRITE(nulprt,FMT='(A,I3,A)')'Get - ',irecv,' fields imported'
!
#if !defined key_noIO
             IF (ig_def_state(iport) .eq. ip_expout .or. &
                  ig_def_state(iport) .eq. ip_ignout) &
!
!*    If the user indicated in the namcouple that the field must be written 
!*     to file, do the writing here :
!
                  CALL psmile_write_8(iport,rd_field,kstep) 
#endif
          ENDIF
       ENDIF
    ENDIF
!
!     ----------------------------------------------------------------
!
1010 CONTINUE
    CALL FLUSH(nulprt)
    RETURN
  END SUBROUTINE prism_get_proto_r18

  SUBROUTINE prism_get_proto_r24(id_port_id,kstep,rd_field_2d,kinfo)
!
!*    *** PRISM_get ***   PRISM 1.0
!
!     purpose:
!     --------
!        recv pfield from oasis or models connected to port id_port_id
!
!     interface:
!     ----------
!        id_port_id : port number of the field
!	 kstep	: current time in seconds
!	 rd_field_2d : buffer of reals
!	 kinfo	: output status
!
!     lib mp:
!     -------
!        mpi-1 or mpi-2 or gsip
!
!     author:
!     -------
!        Arnaud Caubel  - Fecit (08/02 - created from CLIM_Import)
!        S. Legutke,    - MPI M&D  (05/03 - kinfo = PRISM_Recvd added)
!        S. Valcke, CERFACS, 24/10/2004: Added GSIP
!     ----------------------------------------------------------------
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
#if defined use_comm_GSIP 
      USE mod_gsip_model
#endif
    IMPLICIT none
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
    INTEGER (kind=ip_intwp_p)     istatus(MPI_STATUS_SIZE)
#endif
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p), intent(in) :: id_port_id, kstep
    INTEGER (kind=ip_intwp_p), intent(out) :: kinfo
    REAL(kind=ip_single_p), DIMENSION(:,:), intent(inout) :: rd_field_2d
!     ----------------------------------------------------------------    
    REAL(kind=ip_single_p), DIMENSION(myport(4,id_port_id)) :: rd_field
    INTEGER (kind=ip_intwp_p)     info, ip, iport 
    INTEGER (kind=ip_intwp_p)	  irecv, imod, ilk, iseg, is, ilgb
    INTEGER (kind=ip_intwp_p)     itid, itag, il_len, ioff, ityp, ibyt
    INTEGER (kind=ip_intwp_p)     iposbuf, imaxbyt
!jl
#ifdef __DEBUG
    INTEGER(kind=ip_intwp_p)     icount
    INTEGER(kind=ip_intwp_p), parameter :: icountmax=600
    LOGICAL ::                   iflag
#endif
#ifdef use_comm_GSIP
     INTEGER(kind=ip_intwp_p)     ::  il_rst, il_ren, il_errgsip 
     INTEGER :: mgi_read
#endif
!     ----------------------------------------------------------------
!
    rd_field(:)=0
#if defined use_comm_MPI1 || defined use_comm_MPI2 
    istatus(:)=0
#endif 
!
!*    0. Entering
!     --------------
!
    kinfo = PRISM_Ok
!
!*    1. check for this port in my list
!     ---------------------------------
!
    irecv = 0
    iport = -1
!
!   Test if the field is defined in the namcouple and if its coupling period
!   is not greater than the time of the simulation.
    IF (ig_def_freq(id_port_id) .eq. 0 .or. &
         ig_def_freq(id_port_id) .gt. ig_ntime .or. &
         ig_def_state(id_port_id) .eq. ip_auxilary) THEN 
       GOTO 1010
    ENDIF
    IF (myport(1,id_port_id).eq.CLIM_In) iport=id_port_id
    IF (iport.lt.0) THEN
       kinfo = CLIM_BadPort
       WRITE(nulprt,FMT='(A,A)')'Get - WARNING - Invalid port in: ', &
            cports(id_port_id)
       GO TO 1010
    ENDIF
!
!
!*    Test if the current time is a coupling (or I/O) time
! 
    IF (mod(kstep,ig_def_freq(iport)).eq.0) THEN
!
!*    If the user indicated in the namcouple that the field is
!*    a field input-from-file (keyword 'INPUT' at the end of the
!*    field 1st line), do the reading from file here, e.g.:
!
#if !defined key_noIO
        IF (ig_def_state(iport) .EQ. ip_input) THEN
            CALL psmile_read_4(iport,rd_field,kstep)
            kinfo = PRISM_Input
        ENDIF
#endif
!
!* Define return code (direct or via Oasis does not matter)
!
       IF (kstep.EQ.0 .AND. ig_def_lag(iport) .GT. 0) THEN
           kinfo = PRISM_FromRest
#if !defined key_noIO
           IF (ig_def_state(iport) .EQ. ip_ignout .OR. &
              ig_def_state(iport) .EQ. ip_expout) THEN
               kinfo = PRISM_FromRestOut
           ENDIF
#endif  
       ELSE
           IF (ig_def_state(iport) .NE. ip_input) THEN
               kinfo = PRISM_Recvd
!
#if !defined key_noIO
               IF (ig_def_state(iport) .EQ. ip_expout .OR. &
                  ig_def_state(iport) .EQ. ip_ignout) THEN
                   kinfo = PRISM_RecvOut
               ENDIF
#endif
           ENDIF
       ENDIF
!
!*     Test if first import and if the user indicated in the 
!*     namcouple that the field is
!*     exchanged directly between the models and not treated by
!*     Oasis (keyword 'IGNORED' or 'IGNOUT' at the end of the field 1st line),
!*     do the reading from restart file (not implemented).
! 
       IF (kstep.eq.0 .and. ig_def_lag(iport) .gt. 0 .and. &
            (ig_def_state(iport) .eq. ip_ignored .or. &
            ig_def_state(iport) .eq. ip_ignout)) THEN
!
!*       Note: A model can have several restart files but same restart 
!*       file can't be used by different models
!*       Test if model is serial or parallel and if variables are real
!        or double precision
          IF (mydist(CLIM_Strategy,iport) .eq. CLIM_Serial) THEN
             call read_filer4(rd_field, cports(iport),iport)
#if defined use_libMPI || defined use_comm_MPI1 || defined use_comm_MPI2
          ELSE
             call read_file_parar4(rd_field, cports(iport),iport)
#endif
          ENDIF
#if !defined key_noIO
          IF (ig_def_state(iport) .EQ. ip_ignout) &
               call psmile_write_4(iport,rd_field,kstep)
#endif
       ELSE
!
!*    If the user indicated in the namcouple that the field is
!*    a coupling field then do the import :
          IF (ig_def_state(iport) .NE. ip_output .AND. &
               ig_def_state(iport) .ne. ip_input) THEN
!
!*       Check for connected ports (in)
!        ------------------------------
!
             WRITE(nulprt,FMT='(A,A)') 'Get - ', cports(iport)
!
             ityp = myport(2,iport)
             ibyt = myport(3,iport)
!     
             DO ip=1,myport(5,iport)
!     
                ilk  = myport(5+ip,iport)
                imod = mylink(1,ilk)
                itid = mylink(2,ilk)
                itag = mylink(3,ilk) - kstep / ig_frqmin
                iseg = mylink(4,ilk)
!     
!*   Implementation with "blocking" receives : the program will wait
!*   indefinitely until a message is received (this may generate a
!*   deadlock if all models are waiting on a receive).
!*   However this method will be more efficient in most cases than the
!*   receives with a time-out loop. 
! 
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#if defined __DEBUG
!
!jl
!jl add a nonblocking syntax, in order to avoid deadlocks, when NO mailbox
!jl exist in the network  (2004-04-28)
!jl
                CALL MPI_Iprobe ( itid, itag, mpi_comm, iflag, istatus, info )
                WRITE(nulprt,*) 'probing for tid = ',itid,' tag = ',itag, &
                ' comm = ',mpi_comm,' result is : ',iflag
                call flush(nulprt)

                IF (.NOT.iflag) THEN
                   icount = 0
   WAITLOOP:       DO
                   CALL  MPI_Iprobe ( itid, itag, mpi_comm, iflag, istatus, info )
                   icount = icount + 1
                   IF ( iflag ) EXIT WAITLOOP
                   IF ( icount .GE. icountmax ) THEN
                      WRITE(nulprt,*) 'probing for tid = ',itid,' tag = ',itag, &
                      ' still negative after ',icountmax,' seconds : Abort the job'
                      call flush(nulprt)
                      CALL MPI_ABORT (mpi_comm, 0, mpi_err)
                   ENDIF
                   call sleep(1)
                   END DO WAITLOOP
                   WRITE(nulprt,*) 'probing for tid = ',itid,'icount = ', icount
                   call flush(nulprt)
                ENDIF
#endif
!jl
                CALL MPI_Recv ( pkwork_field, ig_maxtype_field, MPI_PACKED, &
                     itid, itag, mpi_comm, istatus, info )
                CALL MPI_Get_count ( istatus, MPI_PACKED, imaxbyt, &
                     info )
!     
                IF ( info .EQ. CLIM_ok  .AND.  imaxbyt .GT. 0) THEN
                   ilgb = 0
                   iposbuf = 0
                   DO is=1,iseg
                      ioff = mylink(4+2*is-1,ilk) * 2 + 1
                      il_len = mylink(4+2*is,ilk)
!     
                      IF ( ityp .EQ. PRISM_Real ) THEN
                         CALL MPI_Unpack ( pkwork_field, ig_maxtype_field, &
                              iposbuf, rd_field(ioff), il_len, &
                              MPI_REAL, mpi_comm, info)
                      ELSE
                         WRITE(nulprt,*)'Get - pb type incorrect ',ityp
                         kinfo = CLIM_BadType
                         GO TO 1010
                      ENDIF
                      ilgb = ilgb + il_len
                   ENDDO
                   IF (ilgb*ibyt .le. imaxbyt) THEN
                      irecv  = irecv + 1
                      nbrecv = nbrecv + ilgb * ibyt
                      WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                           'Get - <from:',imod, &
                           '> <step:',kstep, &
                           '> <len:',ilgb, &
                           '> <type:',ibyt, &
                           '> <tag:',itag,'>' 
                   ELSE
                      kinfo = CLIM_Unpack
                      WRITE(nulprt,FMT='(A,I3,A)')'Get - pb unpack <mpi ', &
                           info,'>'
                   ENDIF
                ELSE
                   kinfo = CLIM_TimeOut
                   WRITE(nulprt,FMT='(A,I3,A)') &
                        'Get - abnormal exit from trecv <mpi ',info,'>'
                ENDIF
!
#elif defined use_comm_GSIP
                if (myport(5,iport) .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- only one reception from Oasis,  myport(5,iport) should be 1') 
                if (imod .ne. 0) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if received from Oasis, imod should be 0')                   
                if (itid .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if received from Oasis, itid should be 1')  
                 
!
!               Read info in channel from Oasis (no DIRECT communication)
!
                il_errgsip = mgi_read (ig_gsipr, pkworkps, ig_CLIMmax , 'R')
                IF (il_errgsip .GE. 0) THEN
                    WRITE(UNIT = nulprt,FMT = *) &
                       'prism_get_proto - pkworkps read OK:', il_errgsip
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '1- prism_get_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_get_proto', &
                       'STOP - pkworkps not read OK)')
                ENDIF
!
!               Fill rd_field with segments of pkworkps
                ilgb = 0
                il_rst = 0
                il_ren = 0
                DO is=1,iseg
                  ioff = mylink(4+2*is-1,ilk) + 1
                  il_len = mylink(4+2*is,ilk)
                  il_rst = il_ren + 1
                  il_ren = il_rst + il_len - 1
                  rd_field(ioff:ioff+il_len-1) = pkworkps(il_rst:il_ren)
!     
                  ilgb = ilgb + il_len
                ENDDO
                IF (ilgb .LE. ig_CLIMmax) THEN
                    irecv  = irecv + 1
                    WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                       'Get - <from:',imod, '> <step:',kstep, &
                       '> <len:',ilgb, '> <type:',ibyt, '> <tag:',itid,'>' 
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '2- prism_get_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_get_proto', &
                       'STOP - sum of segments greater than pkworkps size')
                ENDIF
#endif 
             ENDDO
!     
             WRITE(nulprt,FMT='(A,I3,A)')'Get - ',irecv,' fields imported'
!
#if !defined key_noIO
             IF (ig_def_state(iport) .eq. ip_expout .or. &
                  ig_def_state(iport) .eq. ip_ignout) &
!
!*    If the user indicated in the namcouple that the field must be written 
!*     to file, do the writing here :
!
                call psmile_write_4(iport,rd_field,kstep)
#endif
         ENDIF
          rd_field_2d(:,:) = RESHAPE (rd_field(:),(/size(rd_field_2d,1), &
               size(rd_field_2d,2)/)) 
       ENDIF
    ENDIF
!
!     ----------------------------------------------------------------
!
1010 CONTINUE
    CALL FLUSH(nulprt)
    RETURN
  END SUBROUTINE prism_get_proto_r24

  SUBROUTINE prism_get_proto_r28(id_port_id,kstep,rd_field_2d,kinfo)
!
!*    *** PRISM_get ***   PRISM 1.0
!
!     purpose:
!     --------
!        recv pfield from oasis or models connected to port id_port_id
!
!     interface:
!     ----------
!        id_port_id : port number of the field
!	 kstep	: current time in seconds
!	 rd_field_2d : buffer of reals
!	 kinfo	: output status
!
!     lib mp:
!     -------
!        mpi-1 or mpi-2 or gsip
!
!     author:
!     -------
!        Arnaud Caubel  - Fecit    (08/02 - created from CLIM_Import)
!        S. Legutke     - MPI M&D  (05/03 - kinfo = PRISM_Recvd added)
!        S. Valcke, CERFACS, 24/10/2004: Added GSIP
!     ----------------------------------------------------------------
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
#if defined use_comm_GSIP 
      USE mod_gsip_model
#endif
    IMPLICIT none
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
    INTEGER(kind=ip_intwp_p)     istatus(MPI_STATUS_SIZE)
#endif
!     ----------------------------------------------------------------
    INTEGER(kind=ip_intwp_p), intent(in) :: id_port_id, kstep
    INTEGER(kind=ip_intwp_p), intent(out) :: kinfo
    REAL(kind=ip_double_p), DIMENSION(:,:), intent(inout) :: rd_field_2d
!     ----------------------------------------------------------------    
    REAL(kind=ip_double_p), DIMENSION(myport(4,id_port_id)) :: rd_field
    INTEGER(kind=ip_intwp_p)     info, ip, iport 
    INTEGER(kind=ip_intwp_p)	  irecv, imod, ilk, iseg, is, ilgb
    INTEGER(kind=ip_intwp_p)     itid, itag, il_len, ioff, ityp, ibyt
    INTEGER(kind=ip_intwp_p)     iposbuf, imaxbyt
#ifdef __DEBUG
    INTEGER(kind=ip_intwp_p)     icount
    INTEGER(kind=ip_intwp_p), parameter :: icountmax=600
    LOGICAL ::                   iflag
#endif
#ifdef use_comm_GSIP
     INTEGER(kind=ip_intwp_p)     ::  il_rst, il_ren, il_errgsip 
     INTEGER :: mgi_read
#endif
!     ----------------------------------------------------------------
    rd_field(:)=0
#if defined use_comm_MPI1 || defined use_comm_MPI2 
    istatus(:)=0
#endif  
!
!*    0. Entering
!     --------------
!
    kinfo = PRISM_Ok
!
!*    1. check for this port in my list
!     ---------------------------------
!
    irecv = 0
    iport = -1
!
!   Test if the field is defined in the namcouple and if its coupling period
!   is not greater than the time of the simulation.
   IF (ig_def_freq(id_port_id) .eq. 0 .or. &
         ig_def_freq(id_port_id) .gt. ig_ntime .or. &
         ig_def_state(id_port_id) .eq. ip_auxilary) THEN 
       GOTO 1010
    ENDIF 
    IF (myport(1,id_port_id).eq.CLIM_In) iport=id_port_id
    IF (iport.lt.0) THEN
       kinfo = CLIM_BadPort
       WRITE(nulprt,FMT='(A,A)')'Get - WARNING - Invalid port in: ', &
            cports(id_port_id)
       GO TO 1010
    ENDIF
!
!*    Test if the current time is a coupling (or I/O) time
! 
    IF (mod(kstep,ig_def_freq(iport)).eq.0) THEN
!
!*    If the user indicated in the namcouple that the field is
!*    a field input-from-file (keyword 'INPUT' at the end of the
!*    field 1st line), do the reading from file here, e.g.:
!
#if !defined key_noIO
        IF (ig_def_state(iport) .EQ. ip_input) THEN
           CALL psmile_read_8(iport,rd_field,kstep)
           kinfo = PRISM_Input
       ENDIF
#endif
!
!* Define return code (direct or via Oasis does not matter)
!
       IF (kstep.EQ.0 .AND. ig_def_lag(iport) .GT. 0) THEN
           kinfo = PRISM_FromRest
#if !defined key_noIO
           IF (ig_def_state(iport) .EQ. ip_ignout .OR. &
              ig_def_state(iport) .EQ. ip_expout) THEN
               kinfo = PRISM_FromRestOut
           ENDIF
#endif  
       ELSE
           IF (ig_def_state(iport) .NE. ip_input) THEN
               kinfo = PRISM_Recvd
!
#if !defined key_noIO
               IF (ig_def_state(iport) .EQ. ip_expout .OR. &
                  ig_def_state(iport) .EQ. ip_ignout) THEN
                   kinfo = PRISM_RecvOut
               ENDIF
#endif
           ENDIF
       ENDIF
!
!*     Test if first import and if the user indicated in the 
!*     namcouple that the field is
!*     exchanged directly between the models and not treated by
!*     Oasis (keyword 'IGNORED' or 'IGNOUT' at the end of the field 1st line),
!*     do the reading from restart file (not implemented).
! 
       IF (kstep.eq.0 .and. ig_def_lag(iport) .gt. 0 .and. &
            (ig_def_state(iport) .eq. ip_ignored .or. &
            ig_def_state(iport) .eq. ip_ignout)) THEN
!
!*       Note: A model can have several restart files but same restart 
!*       file can't be used by different models
!*       Test if model is serial or parallel and if variables are real
!        or double precision
          IF (mydist(CLIM_Strategy,iport) .eq. CLIM_Serial) THEN
             call read_filer8(rd_field, cports(iport),iport)
#if defined use_libMPI || defined use_comm_MPI1 || defined use_comm_MPI2
          ELSE
             call read_file_parar8(rd_field, cports(iport),iport)
#endif
          ENDIF
#if !defined key_noIO
          IF (ig_def_state(iport) .EQ. ip_ignout) &
              CALL psmile_write_8(iport,rd_field,kstep)
#endif
       ELSE
!*
!*    If the user indicated in the namcouple that the field is
!*    a coupling field then do the import :
          IF (ig_def_state(iport) .NE. ip_output .AND. &
               ig_def_state(iport) .ne. ip_input) THEN
!
!*       Check for connected ports (in)
!        ------------------------------
!
             WRITE(nulprt,FMT='(A,A)') 'Get - ', cports(iport)
!
             ityp = myport(2,iport)
             ibyt = myport(3,iport)
!     
             DO ip=1,myport(5,iport)
!     
                ilk  = myport(5+ip,iport)
                imod = mylink(1,ilk)
                itid = mylink(2,ilk)
                itag = mylink(3,ilk) - kstep / ig_frqmin
                iseg = mylink(4,ilk)
!     
!*   Implementation with "blocking" receives : the program will wait
!*   indefinitely until a message is received (this may generate a
!*   deadlock if all models are waiting on a receive).
!*   However this method will be more efficient in most cases than the
!*   receives with a time-out loop. 
! 
#if defined use_comm_MPI1 || defined use_comm_MPI2
#if defined __DEBUG
!
!jl
!jl add a nonblocking syntax, in order to avoid deadlocks, when NO mailbox
!jl exist in the network  (2004-04-28)
!jl
                CALL MPI_Iprobe ( itid, itag, mpi_comm, iflag, istatus, info )
                WRITE(nulprt,*) 'probing for tid = ',itid,' tag = ',itag, &
                ' comm = ',mpi_comm,' result is : ',iflag
                call flush(nulprt)

                IF (.NOT.iflag) THEN
                   icount = 0
   WAITLOOP:       DO
                   CALL  MPI_Iprobe ( itid, itag, mpi_comm, iflag, istatus, info )
                   icount = icount + 1
                   IF ( iflag ) EXIT WAITLOOP
                   IF ( icount .GE. icountmax ) THEN
                      WRITE(nulprt,*) 'probing for tid = ',itid,' tag = ',itag, &
                      ' still negative after ',icountmax,' seconds : Abort the job'
                      call flush(nulprt)
                      CALL MPI_ABORT (mpi_comm, 0, mpi_err)
                   ENDIF
                   call sleep(1)
                   END DO WAITLOOP
                   WRITE(nulprt,*) 'probing for tid = ',itid,'icount = ', icount
                   call flush(nulprt)
                ENDIF
#endif
!jl
                CALL MPI_Recv ( pkwork_field, ig_maxtype_field, MPI_PACKED, &
                     itid, itag, mpi_comm, istatus, info )
                CALL MPI_Get_count ( istatus, MPI_PACKED, imaxbyt, &
                     info )
!     
                IF ( info .EQ. CLIM_ok  .AND.  imaxbyt .GT. 0) THEN
                   ilgb = 0
                   iposbuf = 0
                   DO is=1,iseg
                      ioff = mylink(4+2*is-1,ilk) + 1
                      il_len = mylink(4+2*is,ilk)
!     
                      IF ( ityp .EQ. PRISM_Real ) THEN
                         CALL MPI_Unpack ( pkwork_field, ig_maxtype_field, &
                              iposbuf, rd_field(ioff), il_len, &
                              MPI_DOUBLE_PRECISION, mpi_comm, info)
                      ELSE
                         WRITE(nulprt,*)'Get - pb type incorrect ',ityp
                         kinfo = CLIM_BadType
                         GO TO 1010
                      ENDIF
                      ilgb = ilgb + il_len
                   ENDDO
                   IF (ilgb*ibyt .le. imaxbyt) THEN
                      irecv  = irecv + 1
                      nbrecv = nbrecv + ilgb * ibyt
                      WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                           'Get - <from:',imod, &
                           '> <step:',kstep, &
                           '> <len:',ilgb, &
                           '> <type:',ibyt, &
                           '> <tag:',itag,'>' 
                   ELSE
                      kinfo = CLIM_Unpack
                      WRITE(nulprt,FMT='(A,I3,A)')'Get - pb unpack <mpi ', &
                           info,'>'
                      GO TO 1010
                   ENDIF
                ELSE
                   kinfo = CLIM_TimeOut
                   WRITE(nulprt,FMT='(A,I3,A)') &
                        'Get - abnormal exit from trecv <mpi ',info,'>'
                   GO TO 1010
                ENDIF
!
#elif defined use_comm_GSIP
                if (myport(5,iport) .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- only one reception from Oasis,  myport(5,iport) should be 1') 
                if (imod .ne. 0) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if received from Oasis, imod should be 0')                   
                if (itid .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if received from Oasis, itid should be 1')  
                 
!
!               Read info in channel from Oasis (no DIRECT communication)
!
                il_errgsip = mgi_read (ig_gsipr, pkworkps, ig_CLIMmax , 'D')
                IF (il_errgsip .GE. 0) THEN
                    WRITE(UNIT = nulprt,FMT = *) &
                       'prism_get_proto - pkworkps read OK:', il_errgsip
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '1- prism_get_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_get_proto', &
                       'STOP - pkworkps not read OK)')
                ENDIF
!
!               Fill rd_field with segments of pkworkps
                ilgb = 0
                il_rst = 0
                il_ren = 0
                DO is=1,iseg
                  ioff = mylink(4+2*is-1,ilk) + 1
                  il_len = mylink(4+2*is,ilk)
                  il_rst = il_ren + 1
                  il_ren = il_rst + il_len - 1
                  rd_field(ioff:ioff+il_len-1) = pkworkps(il_rst:il_ren)
!     
                  ilgb = ilgb + il_len
                ENDDO
                IF (ilgb .LE. ig_CLIMmax) THEN
                    irecv  = irecv + 1
                    WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                       'Get - <from:',imod, '> <step:',kstep, &
                       '> <len:',ilgb, '> <type:',ibyt, '> <tag:',itid,'>' 
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '2- prism_get_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_get_proto', &
                       'STOP - sum of segments greater than pkworkps size')
                ENDIF
#endif
              ENDDO
!     
             WRITE(nulprt,FMT='(A,I3,A)')'Get - ',irecv,' fields imported'
!
#if !defined key_noIO
             IF (ig_def_state(iport) .eq. ip_expout .or. &
                  ig_def_state(iport) .eq. ip_ignout) &
!
!*    If the user indicated in the namcouple that the field must be written 
!*     to file, do the writing here :
!
                call psmile_write_8(iport,rd_field,kstep) 
#endif
         ENDIF
          rd_field_2d(:,:) = RESHAPE (rd_field(:),(/size(rd_field_2d,1), &
               size(rd_field_2d,2)/)) 
      ENDIF
    ENDIF
!
!     ----------------------------------------------------------------
!
1010 CONTINUE
    CALL FLUSH(nulprt)
    RETURN
  END SUBROUTINE prism_get_proto_r28

! ********************************************************************
! ********************************************************************
! ********************************************************************
!
!*    *** READ_FILE ***   PRISM 1.0
!
!     purpose:
!     --------
!        At first time step, reads input fields from binary files or 
!        netcdf files.
!
!     interface:
!     ----------
!        inp_fld : field to be read from the restart file
!        cd_port : symbolic name of the field
!        id_port : port number of the field
!
!     lib mp:
!     -------
!        mpi-1
!
!     author:
!     -------
!        Eric Sevault   - METEO FRANCE
!        Laurent Terray - CERFACS
!        Jean Latour    - F.S.E.     (mpi-2)
!        Arnaud Caubel  - Adaptation to PRISM interface
!     ----------------------------------------------------------------
  SUBROUTINE read_filer4(inp_fld, cd_port,id_port)
!     ----------------------------------------------------------------
    USE mod_kinds_model     
    USE mod_prism_proto
    USE mod_comprism_proto
    IMPLICIT NONE
#ifdef use_netCDF
#include <netcdf.inc>
#endif
!     ----------------------------------------------------------------
    INTEGER(kind=ip_intwp_p) :: id_port
    REAL(kind=ip_single_p) :: inp_fld(myport(4,id_port)) 
    CHARACTER(len=8) :: cd_port
!     ----------------------------------------------------------------
    INTEGER(kind=ip_intwp_p) il_unit
    INTEGER(kind=ip_intwp_p) ierror,info, il_varid, il_ncid, istatus
    LOGICAL ll_file
!     ----------------------------------------------------------------
    WRITE(nulprt,*)'Entering Read_File '
!
!*    Test if restart file is in NETCDF format or not
!
#ifdef use_netCDF
    IF (lg_ncdfrst) THEN
!
!* Case NETCDF format
!
       istatus = NF_OPEN(cg_def_rstfile(id_port),NF_NOWRITE,il_ncid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_get routine '
          STOP
       ENDIF
       istatus = NF_INQ_VARID(il_ncid, cd_port, il_varid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_get routine '
          STOP
       ENDIF
       istatus = NF_GET_VAR_REAL (il_ncid, il_varid, inp_fld)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_get routine '
          STOP
       ENDIF
       istatus = NF_CLOSE(il_ncid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_get routine '
          STOP
       ENDIF
    ELSE
#endif
!
!* Case binary format
!
       il_unit = nulprt + 1 
       INQUIRE (il_unit,OPENED = ll_file)
       DO WHILE (ll_file)
          il_unit = il_unit + 1 
          INQUIRE (il_unit,OPENED = ll_file)
       END DO
       OPEN (il_unit, FILE=cg_def_rstfile(id_port),FORM='UNFORMATTED')
       CALL locreadr4(cd_port,inp_fld,myport(4,id_port),il_unit, &
            ierror, nulprt)
       CLOSE (il_unit)
#ifdef use_netCDF
    ENDIF
#endif
  END SUBROUTINE read_filer4

! ********************************************************************
! ********************************************************************
! ********************************************************************
!
!*    *** READ_FILE ***   PRISM 1.0
!
!     purpose:
!     --------
!        At first time step, reads input fields from binary files or 
!        netcdf files.
!
!     interface:
!     ----------
!        inp_fld : field to be read from the restart file
!        cd_port : symbolic name of the field
!        id_port : port number of the field
!
!     lib mp:
!     -------
!        mpi-1
!
!     author:
!     -------
!        Eric Sevault   - METEO FRANCE
!        Laurent Terray - CERFACS
!        Jean Latour    - F.S.E.     (mpi-2)
!        Arnaud Caubel  - Adaptation to PRISM interface
!     ----------------------------------------------------------------
  SUBROUTINE read_filer8(inp_fld, cd_port,id_port)
!     ---------------------------------------------------------------- 
    USE mod_kinds_model    
    USE mod_prism_proto
    USE mod_comprism_proto
    IMPLICIT NONE
#ifdef use_netCDF
#include <netcdf.inc>
#endif
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p) :: id_port
    REAL(kind=ip_double_p) :: inp_fld(myport(4,id_port)) 
    CHARACTER(len=8) :: cd_port
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p) il_unit
    INTEGER (kind=ip_intwp_p) ierror,info, il_varid, il_ncid, istatus
    LOGICAL ll_file
!     ----------------------------------------------------------------
    WRITE(nulprt,*)'Entering Read_File '
!
!*    Test if restart file is in NETCDF format or not
!
#ifdef use_netCDF
    IF (lg_ncdfrst) THEN
!
!* Case NETCDF format
!
       istatus = NF_OPEN(cg_def_rstfile(id_port),NF_NOWRITE,il_ncid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_get routine '
          STOP
       ENDIF
       istatus = NF_INQ_VARID(il_ncid, cd_port, il_varid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_get routine '
          STOP
       ENDIF
       istatus = NF_GET_VAR_DOUBLE (il_ncid, il_varid, inp_fld)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_get routine '
          STOP
       ENDIF
       istatus = NF_CLOSE(il_ncid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_get routine '
          STOP
       ENDIF
    ELSE
#endif
!
!* Case binary format
!
       il_unit = nulprt + 1 
       INQUIRE (il_unit,OPENED = ll_file)
       DO WHILE (ll_file)
          il_unit = il_unit + 1 
          INQUIRE (il_unit,OPENED = ll_file)
       END DO
       OPEN (il_unit, FILE=cg_def_rstfile(id_port),FORM='UNFORMATTED')
       CALL locreadr8(cd_port,inp_fld,myport(4,id_port),il_unit, &
            ierror, nulprt)
       CLOSE (il_unit)
#ifdef use_netCDF
    ENDIF
#endif
  END SUBROUTINE read_filer8

! ********************************************************************
! ********************************************************************
! ********************************************************************
!
!*    *** READ_FILE_PARA ***   PRISM 1.0
!
!     purpose:
!     --------
!        At first time step, reads input fields from binary files or 
!        netcdf files.
!
!     interface:
!     ----------
!        inp_fld : field to be read from the restart file
!        cd_port : symbolic name of the field
!        id_port : port number of the field
!
!     lib mp:
!     -------
!        mpi-1
!
!     author:
!     -------
!        Eric Sevault   - METEO FRANCE
!        Laurent Terray - CERFACS
!        Jean Latour    - F.S.E.     (mpi-2)
!        Arnaud Caubel  - Adaptation to PRISM interface
!     ----------------------------------------------------------------
  SUBROUTINE read_file_parar4(inp_fld, cd_port, id_port)
!     ----------------------------------------------------------------
#if defined use_libMPI || defined use_comm_MPI1 || defined use_comm_MPI2  
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
    IMPLICIT NONE
#ifdef use_netCDF
#include <netcdf.inc>
#endif
#include <mpif.h>
      INTEGER (kind=ip_intwp_p), DIMENSION(MPI_STATUS_SIZE) :: istatus
!     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p) :: id_port
      CHARACTER(len=8) :: cd_port
      REAL(kind=ip_single_p) :: inp_fld(myport(4,id_port))
!     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p),PARAMETER :: ip_tag=100
      INTEGER (kind=ip_intwp_p) il_unit
      INTEGER (kind=ip_intwp_p) ierror, info, il_varid, il_ncid, il_status_ncdf, il_aux
      INTEGER (kind=ip_intwp_p) il_maxgrd, il_maxbyte, ib, ib_aux, iposbuf, il_off, il_len
      INTEGER (kind=ip_intwp_p), DIMENSION(:,:), ALLOCATABLE :: il_paral_mast
      REAL(kind=ip_single_p), DIMENSION(:), ALLOCATABLE :: rl_start, rl_work
      REAL(kind=ip_single_p), DIMENSION(:), ALLOCATABLE :: rl_work_mast
      LOGICAL ll_file
      INTEGER(kind=ip_intwp_p)::il_sndreq
!     ----------------------------------------------------------------
      WRITE(nulprt,*)'Entering Read_File_Para '
      istatus(:)=0
!
!* Each process of local communicator sends his decomposition to master proc
!
      IF (mpi_rank.NE.0) THEN  
          CALL MPI_Send (mydist(:,id_port), CLIM_Parsize, MPI_INTEGER, 0, &
             ip_tag, ig_local_comm, ierror )
          CALL MPI_Send (myport(4,id_port), 1, MPI_INTEGER, 0, &
             ip_tag+1, ig_local_comm, ierror )
      ENDIF
!
!* Master proc receives each process decomposition
!
      IF (mpi_rank.eq.0) THEN
         il_maxgrd = 0
         ALLOCATE(il_paral_mast(CLIM_Parsize,kbcplproc(ig_mynummod)))
         il_paral_mast(:,:)=0
         il_paral_mast(:,1)=mydist(:,id_port)
         il_aux = myport(4,id_port)
         il_maxgrd = il_maxgrd + il_aux
         DO ib = 1, kbcplproc(ig_mynummod)-1
           CALL MPI_Recv (il_paral_mast(:,ib+1), &
              CLIM_Parsize, MPI_INTEGER, ib, &
              ip_tag, ig_local_comm, istatus, ierror )
           CALL MPI_Recv (il_aux, 1, MPI_INTEGER, ib, &
              ip_tag+1, ig_local_comm, istatus, ierror )
           il_maxgrd = il_maxgrd + il_aux
         END DO
         il_maxbyte = il_maxgrd * 4
      ENDIF
      ALLOCATE(rl_work(myport(4,id_port)))
      IF (mpi_rank.eq.0) THEN
         ALLOCATE (rl_start(il_maxgrd))
         ALLOCATE(rl_work_mast(il_maxgrd))
         rl_start(:)=0
         rl_work_mast(:)=0
!
!* Test if restart file is in NETCDF format or not
!
#ifdef use_netCDF
         IF (lg_ncdfrst) THEN
!
!* Case NETCDF format
!
            il_status_ncdf = NF_OPEN(cg_def_rstfile(id_port),NF_NOWRITE, &
                 il_ncid)
            IF (il_status_ncdf.ne.NF_NOERR) THEN
               WRITE(nulprt,*) NF_STRERROR(il_status_ncdf)
               WRITE(nulprt,*)' stop in PRISM_get routine '
               STOP
            ENDIF
            il_status_ncdf = NF_INQ_VARID(il_ncid, cd_port, il_varid)
            IF (il_status_ncdf.ne.NF_NOERR) THEN
               WRITE(nulprt,*) NF_STRERROR(il_status_ncdf)
               WRITE(nulprt,*)' stop in PRISM_get routine '
               STOP
            ENDIF
            il_status_ncdf = NF_GET_VAR_REAL (il_ncid, il_varid, rl_start)
            IF (il_status_ncdf.ne.NF_NOERR) THEN
               WRITE(nulprt,*) NF_STRERROR(il_status_ncdf)
               WRITE(nulprt,*)' stop in PRISM_get routine '
               STOP
            ENDIF
            il_status_ncdf = NF_CLOSE(il_ncid)
            IF (il_status_ncdf.ne.NF_NOERR) THEN
               WRITE(nulprt,*) NF_STRERROR(il_status_ncdf)
               WRITE(nulprt,*)' stop in PRISM_get routine '
               STOP
            ENDIF
         ELSE
#endif
!
!* Case binary format
!
            il_unit = nulprt + 1
            INQUIRE (il_unit,OPENED = ll_file)
            DO WHILE (ll_file)
               il_unit = il_unit + 1 
               INQUIRE (il_unit,OPENED = ll_file)
            END DO
            OPEN (il_unit, FILE=cg_def_rstfile(id_port),FORM='UNFORMATTED')
            CALL locreadr4(cd_port,rl_start,il_maxgrd,il_unit, &
                 ierror, nulprt) 
!     
            CLOSE (il_unit)
#ifdef use_netCDF
         ENDIF
#endif
!
!* Master proc sends to each proc his part of the field
!
         DO ib = kbcplproc(ig_mynummod)- 1,0,-1
            iposbuf=0
            DO ib_aux=1,il_paral_mast(clim_segments,ib+1)
               il_off=il_paral_mast(clim_segments+2*ib_aux-1,ib+1)+1
               il_len=il_paral_mast(clim_segments+2*ib_aux,ib+1)            
               call MPI_Pack(rl_start(il_off:il_off+il_len-1), &
                    il_len,MPI_REAL,rl_work_mast,il_maxbyte, &
                    iposbuf, ig_local_comm, ierror)
            END DO
            IF(ib.GT.0) THEN
            CALL MPI_Send ( rl_work_mast, iposbuf, MPI_PACKED, ib, &
                 ip_tag+2, ig_local_comm, ierror )
            ELSE
            CALL MPI_ISend ( rl_work_mast, iposbuf, MPI_PACKED, ib, &
                 ip_tag+2, ig_local_comm,il_sndreq, ierror )
            ENDIF
         ENDDO
         DEALLOCATE (il_paral_mast)
         DEALLOCATE (rl_work_mast)
         DEALLOCATE (rl_start)
      ENDIF
!
!* Each proc receives his part of the field
!      
      call MPI_Recv ( rl_work, myport(4,id_port)*4, MPI_PACKED, 0, &
           ip_tag+2, ig_local_comm, istatus, ierror )
      iposbuf=0
      il_off=1
      il_len=myport(4,id_port) 
      CALL MPI_Unpack(rl_work, myport(4,id_port)*4,iposbuf, &
           inp_fld(il_off:il_off+il_len-1), &
           il_len, MPI_REAL, ig_local_comm, ierror)
      IF (mpi_rank.eq.0) THEN
         call MPI_Wait(il_sndreq,istatus,ierror)
      ENDIF
      DEALLOCATE(rl_work)
#endif
    END SUBROUTINE read_file_parar4

! ********************************************************************
! ********************************************************************
! ********************************************************************
!
!*    *** READ_FILE_PARA ***   PRISM 1.0
!
!     purpose:
!     --------
!        At first time step, reads input fields from binary files or 
!        netcdf files.
!
!     interface:
!     ----------
!        inp_fld : field to be read from the restart file
!        cd_port : symbolic name of the field
!        id_port : port number of the field
!
!     lib mp:
!     -------
!        mpi-1
!
!     author:
!     -------
!        Eric Sevault   - METEO FRANCE
!        Laurent Terray - CERFACS
!        Jean Latour    - F.S.E.     (mpi-2)
!        Arnaud Caubel  - Adaptation to PRISM interface
!     ----------------------------------------------------------------
  SUBROUTINE read_file_parar8(inp_fld, cd_port, id_port)
!     ----------------------------------------------------------------
#if defined use_libMPI || defined use_comm_MPI1 || defined use_comm_MPI2
    USE mod_kinds_model     
    USE mod_prism_proto
    USE mod_comprism_proto
    IMPLICIT NONE
#ifdef use_netCDF
#include <netcdf.inc>
#endif
#include <mpif.h>
      INTEGER(kind=ip_intwp_p), DIMENSION(MPI_STATUS_SIZE) :: istatus
!     ----------------------------------------------------------------
      CHARACTER(len=8) :: cd_port
      INTEGER(kind=ip_intwp_p) :: id_port
      REAL(kind=ip_double_p) :: inp_fld(myport(4,id_port))
!     ----------------------------------------------------------------
      INTEGER(kind=ip_intwp_p),PARAMETER :: ip_tag=100
      INTEGER(kind=ip_intwp_p) il_unit, il_aux
      INTEGER(kind=ip_intwp_p) ierror, info, il_varid, il_ncid, il_status_ncdf
      INTEGER(kind=ip_intwp_p) il_maxgrd, il_maxbyte, ib, ib_aux, iposbuf, il_off, il_len
      INTEGER(kind=ip_intwp_p), DIMENSION(:,:), ALLOCATABLE :: il_paral_mast
      REAL(kind=ip_double_p), DIMENSION(:), ALLOCATABLE :: rl_start, rl_work
      REAL(kind=ip_double_p), DIMENSION(:), ALLOCATABLE :: rl_work_mast
      LOGICAL ll_file
      INTEGER(kind=ip_intwp_p)::il_sndreq
!     ----------------------------------------------------------------
      WRITE(nulprt,*)'Entering Read_File_Para '
      istatus(:)=0
!
!* Each process of local communicator sends his decomposition to master proc
!
      IF (mpi_rank.ne.0) THEN  
          CALL MPI_Send (mydist(:,id_port), CLIM_Parsize, MPI_INTEGER, 0, &
             ip_tag, ig_local_comm, ierror )
          CALL MPI_Send (myport(4,id_port), 1, MPI_INTEGER, 0, &
             ip_tag+1, ig_local_comm, ierror )
      ENDIF
!
!* Master proc receives each process decomposition
!
      IF (mpi_rank.eq.0) THEN
         il_maxgrd = 0
         ALLOCATE(il_paral_mast(CLIM_Parsize,kbcplproc(ig_mynummod)))
         il_paral_mast(:,:)=0
         il_paral_mast(:,1)=mydist(:,id_port)
         il_aux = myport(4,id_port)
         il_maxgrd = il_maxgrd + il_aux
         DO ib = 1, kbcplproc(ig_mynummod)-1
            CALL MPI_Recv (il_paral_mast(:,ib+1), &
                 CLIM_Parsize, MPI_INTEGER, ib, &
                 ip_tag, ig_local_comm, istatus, ierror )
            CALL MPI_Recv (il_aux, 1, MPI_INTEGER, ib, &
                 ip_tag+1, ig_local_comm, istatus, ierror )
            il_maxgrd = il_maxgrd + il_aux
         END DO
         il_maxbyte = il_maxgrd * 8
      ENDIF
      ALLOCATE(rl_work(myport(4,id_port)))
      rl_work(:)=0
      IF (mpi_rank.eq.0) THEN
         ALLOCATE (rl_start(il_maxgrd))
         ALLOCATE(rl_work_mast(il_maxgrd))
         rl_start(:)=0
         rl_work_mast(:)=0
!
!* Test if restart file is in NETCDF format or not
!
#ifdef use_netCDF
         IF (lg_ncdfrst) THEN
!
!* Case NETCDF format
!
            il_status_ncdf = NF_OPEN(cg_def_rstfile(id_port),NF_NOWRITE, &
                 il_ncid)
            IF (il_status_ncdf.ne.NF_NOERR) THEN
               WRITE(nulprt,*) NF_STRERROR(il_status_ncdf)
               WRITE(nulprt,*)' stop in PRISM_get routine '
               STOP
            ENDIF
            il_status_ncdf = NF_INQ_VARID(il_ncid, cd_port, il_varid)
            IF (il_status_ncdf.ne.NF_NOERR) THEN
               WRITE(nulprt,*) NF_STRERROR(il_status_ncdf)
               WRITE(nulprt,*)' stop in PRISM_get routine '
               STOP
            ENDIF
            il_status_ncdf = NF_GET_VAR_DOUBLE (il_ncid, il_varid, rl_start)
            IF (il_status_ncdf.ne.NF_NOERR) THEN
               WRITE(nulprt,*) NF_STRERROR(il_status_ncdf)
               WRITE(nulprt,*)' stop in PRISM_put routine '
               STOP
            ENDIF
            il_status_ncdf = NF_CLOSE(il_ncid)
            IF (il_status_ncdf.ne.NF_NOERR) THEN
               WRITE(nulprt,*) NF_STRERROR(il_status_ncdf)
               WRITE(nulprt,*)' stop in PRISM_get routine '
               STOP
            ENDIF
         ELSE
#endif
!
!* Case binary format
!
            il_unit = nulprt + 1
            INQUIRE (il_unit,OPENED = ll_file)
            DO WHILE (ll_file)
               il_unit = il_unit + 1 
               INQUIRE (il_unit,OPENED = ll_file)
            END DO
            OPEN (il_unit, FILE=cg_def_rstfile(id_port),FORM='UNFORMATTED')
            CALL locreadr8(cd_port,rl_start,il_maxgrd,il_unit, &
                 ierror, nulprt) 
!     
            CLOSE (il_unit)
#ifdef use_netCDF
         ENDIF
#endif
!
!* Master proc sends to each proc his part of the field
!
         DO ib = kbcplproc(ig_mynummod)- 1,0,-1
            iposbuf=0
            DO ib_aux=1,il_paral_mast(clim_segments,ib+1)
               il_off=il_paral_mast(clim_segments+2*ib_aux-1,ib+1)+1
               il_len=il_paral_mast(clim_segments+2*ib_aux,ib+1)            
               call MPI_Pack(rl_start(il_off:il_off+il_len-1), &
                    il_len,MPI_DOUBLE_PRECISION,rl_work_mast,il_maxbyte, &
                    iposbuf, ig_local_comm, ierror)
            END DO
            IF(ib.GT.0) THEN
            CALL MPI_Send ( rl_work_mast, iposbuf, MPI_PACKED, ib, &
                 ip_tag+2, ig_local_comm, ierror )
            ELSE
            CALL MPI_ISend ( rl_work_mast, iposbuf, MPI_PACKED, ib, &
                 ip_tag+2, ig_local_comm,il_sndreq, ierror )
            ENDIF
         ENDDO
         DEALLOCATE (il_paral_mast)
         DEALLOCATE (rl_work_mast)
         DEALLOCATE (rl_start)
      ENDIF
!
!* Each proc receives his part of the field
!      
      call MPI_Recv ( rl_work, myport(4,id_port)*8, MPI_PACKED, 0, &
           ip_tag+2, ig_local_comm, istatus, ierror )
      iposbuf=0
      il_off=1
      il_len=myport(4,id_port) 
      CALL MPI_Unpack(rl_work, myport(4,id_port)*8,iposbuf, &
           inp_fld(il_off:il_off+il_len-1), &
           il_len, MPI_DOUBLE_PRECISION, ig_local_comm, ierror)
      IF (mpi_rank.eq.0) THEN
         call MPI_Wait(il_sndreq,istatus,ierror)
      ENDIF
      DEALLOCATE(rl_work)
#endif
    END SUBROUTINE read_file_parar8

!
! ********************************************************************
! ********************************************************************
! ********************************************************************
!
    SUBROUTINE locreadr4 ( cdfldn, pfield, kdimax, knulre, kflgre, kout)
!
!**** *locread*  - Read binary field on unit knulre
!
!     Purpose:
!     -------
!     Find string cdfldn on unit knulre and read array pfield
!
!**   Interface:
!     ---------
!       *CALL*  *locread (cdfldn, pfield, kdimax, knulre, kflgre, kout)*
!
!     Input:
!     -----
!                cdfldn : character string locator
!                kdimax : dimension of field to be read 
!                knulre : logical unit to be read 
!                kout   : logical unit to write messages
!
!     Output:
!     ------
!                pfield : field array (real 1D)
!                kflgre : error status flag
!
!     Reference:
!     ---------
!     See OASIS manual (1995) 
!
!     History:
!     -------
!       Version   Programmer     Date      Description
!       -------   ----------     ----      -----------  
!       2.0       L. Terray      95/09/01  created
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!
!* ---------------------------- Argument declarations -------------------
!
      USE mod_kinds_model
      INTEGER (kind=ip_intwp_p) kdimax, knulre, kflgre, kout
      REAL(kind=ip_single_p) ::  pfield(kdimax)
      CHARACTER*8 cdfldn
!
!* ---------------------------- Local declarations ----------------------
!
      CHARACTER*8 clecfl
!
!* ---------------------------- Poema verses ----------------------------
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!*    1. Initialization
!        --------------
!
      WRITE (UNIT = kout,FMT = 1001) knulre
!
!* Formats
!
1001  FORMAT('Locread : Read binary file connected to unit = ',I4)
!
!     2. Find field in file
!        ------------------
!
      REWIND knulre
200   CONTINUE
!* Find string
      READ (UNIT = knulre, ERR = 200, END = 210) clecfl
      IF (clecfl .NE. cdfldn) GO TO  200
!* Read associated field
      READ (UNIT = knulre, ERR = 210, END = 210) pfield
!* Reading done and ok
      kflgre = 0
      GO TO 220
!* Problem in reading
210   kflgre = 1
220   CONTINUE
!
!
!*    3. End of routine
!        --------------
!
      WRITE (UNIT = kout,FMT = *) 'Locread : done'
      CALL FLUSH (kout)
      RETURN
    END SUBROUTINE locreadr4

!
! ********************************************************************
! ********************************************************************
! ********************************************************************
!
    SUBROUTINE locreadr8 ( cdfldn, pfield, kdimax, knulre, kflgre, kout)
!
!**** *locread*  - Read binary field on unit knulre
!
!     Purpose:
!     -------
!     Find string cdfldn on unit knulre and read array pfield
!
!**   Interface:
!     ---------
!       *CALL*  *locread (cdfldn, pfield, kdimax, knulre, kflgre, kout)*
!
!     Input:
!     -----
!                cdfldn : character string locator
!                kdimax : dimension of field to be read 
!                knulre : logical unit to be read 
!                kout   : logical unit to write messages
!
!     Output:
!     ------
!                pfield : field array (real 1D)
!                kflgre : error status flag
!
!     Reference:
!     ---------
!     See OASIS manual (1995) 
!
!     History:
!     -------
!       Version   Programmer     Date      Description
!       -------   ----------     ----      -----------  
!       2.0       L. Terray      95/09/01  created
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!
!* ---------------------------- Argument declarations -------------------
!
      USE mod_kinds_model
      INTEGER(kind=ip_intwp_p) kdimax, knulre, kflgre, kout
      REAL(kind=ip_double_p) ::  pfield(kdimax)
      CHARACTER*8 cdfldn
!
!* ---------------------------- Local declarations ----------------------
!
      CHARACTER*8 clecfl
!
!* ---------------------------- Poema verses ----------------------------
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!*    1. Initialization
!        --------------
!
      WRITE (UNIT = kout,FMT = 1001) knulre
!
!* Formats
!
1001  FORMAT('Locread : Read binary file connected to unit = ',I4)
!
!     2. Find field in file
!        ------------------
!
      REWIND knulre
200   CONTINUE
!* Find string
      READ (UNIT = knulre, ERR = 200, END = 210) clecfl
      IF (clecfl .NE. cdfldn) GO TO  200
!* Read associated field
      READ (UNIT = knulre, ERR = 210, END = 210) pfield
!* Reading done and ok
      kflgre = 0
      GO TO 220
!* Problem in reading
210   kflgre = 1
220   CONTINUE
!
!
!*    3. End of routine
!        --------------
!
      WRITE (UNIT = kout,FMT = *) 'Locread : done'
      CALL FLUSH (kout)
      RETURN
    END SUBROUTINE locreadr8

  END module mod_prism_get_proto
