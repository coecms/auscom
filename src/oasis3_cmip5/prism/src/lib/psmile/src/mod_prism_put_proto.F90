module mod_prism_put_proto
#include "psmile_os.h"

  interface prism_put_proto
     
#ifndef __NO_4BYTE_REALS
     module procedure prism_put_proto_r14
     module procedure prism_put_proto_r24
#endif
     module procedure prism_put_proto_r18, &
                      prism_put_proto_r28
     
  end interface

contains
#ifndef __NO_4BYTE_REALS
  SUBROUTINE prism_put_proto_r14(id_port_id,kstep,rd_field,kinfo)
!
!*    *** PRISM_put ***   PRISM 1.0
!
!     purpose:
!     --------
!        give pfield to Oasis or models connected to port id_port_id at the 
!        time kstep
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
!        mpi-1
!
!     author:
!     -------
!         Arnaud Caubel  - Fecit (08/02 - created from CLIM_Export)
!
!     modified:
!     ---------
!        Reiner Vogelsang, SGI,  27 April 2003
!        - Screening of 4 byte real interfaces in case a of dbl4 compilation.
!          File has to be preprocessed with -D__SXdbl4.
!        S. Legutke, MPI-HH M&D,  13 May 2003
!        - return PRISM_Sent if a field was received
!        S. Valcke, CERFACS, 24/10/2004: Added GSIP
!     ----------------------------------------------------------------
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
    USE mathelp_psmile
#if defined use_comm_GSIP 
      USE mod_gsip_model
#endif
    IMPLICIT NONE
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
#endif
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p)       kstep, kinfo, id_port_id
    REAL(kind=ip_single_p), DIMENSION(myport(4,id_port_id)) :: rd_field
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p)    il_newtime
    INTEGER (kind=ip_intwp_p)    info, ib
    INTEGER (kind=ip_intwp_p)	   isend, ip, iport, ilk, iseg, is, ilgb
    INTEGER (kind=ip_intwp_p)     imod, itid, itag, il_len, ioff, ityp, ibyt
    INTEGER (kind=ip_intwp_p)    iposbuf
    INTEGER (kind=ip_intwp_p) :: il_nbopp, il_nbin, il_nbout
    INTEGER (kind=ip_intwp_p), PARAMETER :: ip_nbopp_max = 10
    INTEGER (kind=ip_intwp_p), PARAMETER :: ip_nbindex=1
    INTEGER (kind=ip_intwp_p) :: il_nindex(ip_nbindex)
    REAL(kind=ip_single_p) :: rl_tmp_scal(ip_nbopp_max)
    REAL(kind=ip_single_p),PARAMETER :: ip_missing_val=1.e20
    REAL(kind=ip_single_p), DIMENSION(myport(4,id_port_id)) :: rl_field_aux
    CHARACTER(len=80) :: cl_topps, cl_str
    CHARACTER(len=7) :: cl_tmp_topp, cl_tmp_sopp(ip_nbopp_max)
#ifdef use_comm_GSIP
     INTEGER(kind=ip_intwp_p)     ::  il_rst, il_ren, il_errgsip 
     INTEGER :: mgi_write
#endif
!     ----------------------------------------------------------------
    rl_field_aux(:)=0
    cl_tmp_sopp(:)=' '
    il_nindex(:)=0
    rl_tmp_scal(:)=0
!
!*    0. Entering
!     -----------
!
    kinfo = PRISM_Ok
    IF (ip_realwp_p == ip_double_p) CALL prism_abort_proto (0,'prism_put_proto', &
       'STOP -- PSMILe compiled with double precision REAL; prism_put_proto_r14 should not be called')
    lg_dgfield = .false.
!
!*    1. check for this port in my list
!     ---------------------------------
!
    isend = 0
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
        kinfo = CLIM_BadPort
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
    IF (ig_def_trans(iport) .EQ. ip_instant) THEN 
        cl_str = 'inst(ident(X))'
        rg_field_trans(:,iport) = rd_field (:)
    ELSEIF (ig_def_trans(iport) .eq. ip_average .or. &
       ig_def_trans(iport) .EQ. ip_accumul .OR. &
       ig_def_trans(iport) .EQ. ip_min .OR. &
       ig_def_trans(iport) .EQ. ip_max) THEN
        IF (ig_number(iport) .EQ. 0) rg_field_trans(:,iport) = 0
        il_nbin = myport(4,iport)
        il_nbout = il_nbin
        cl_topps = 'ave, inst, t_sum, t_min, t_max'
        IF (ig_def_trans(iport) .EQ. ip_average) THEN
            cl_str = 'ave(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_accumul) THEN 
            cl_str = 't_sum(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_min) THEN
            cl_str = 't_min(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_max) THEN
            cl_str = 't_max(ident(X))'
        ENDIF
        CALL buildop (cl_str,cl_topps,cl_tmp_topp,ip_nbopp_max, &
           ip_missing_val,cl_tmp_sopp,rl_tmp_scal,il_nbopp)
      
        CALL mathop (cl_tmp_sopp(1), il_nbin, rd_field, ip_missing_val, &
           ip_nbindex, il_nindex, rl_tmp_scal(1), il_nbout, &
           rl_field_aux)

        IF ((cl_tmp_topp(:LEN_TRIM(cl_tmp_topp)) .NE. 'inst')) &
           CALL moycum(cl_tmp_topp, il_nbin, rg_field_trans(:,iport), &
           rl_field_aux, ig_number(iport))
        ig_number(iport) = ig_number(iport) + 1
        IF (MOD(il_newtime,ig_def_freq(iport)) .EQ. 0) THEN
            ig_number(iport) = 0
            DO ib = 1, myport(4,iport)
              rd_field(ib) = rg_field_trans(ib,iport)
            ENDDO
        ENDIF
        kinfo = PRISM_LocTrans
    ENDIF
!
!*    Test if field must be written to restart file i.e.
!*    - current time is time at the end of simulation +
!*    - lag of current field is greater 0
!
    IF (il_newtime.EQ.ig_ntime.AND.ig_def_lag(iport).GT.0) THEN
!ac
       IF (ig_def_state(iport) .ne. ip_output) THEN
!ac
!
!*       Note: A model can have several restart files but same restart 
!*       file can't be used by different models
!
       IF (mydist(CLIM_Strategy,iport) .eq. CLIM_Serial) THEN
          CALL write_filer4(rd_field,cports(iport),iport)
       ELSE
          CALL write_file_parar4(rd_field,cports(iport),iport)
       ENDIF
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
		CALL psmile_write_4(iport,rd_field,il_newtime)
              	kinfo = PRISM_Output
	  ELSEIF (ig_def_state(iport) .eq. ip_expout .or. &
        	       ig_def_state(iport) .EQ. ip_ignout) THEN
		CALL psmile_write_4(iport,rd_field,il_newtime)
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
              CALL psmile_write_4(iport,rd_field,il_newtime)
              kinfo = PRISM_Output
          ENDIF
#endif
!*
!*   If the user indicated in the namcouple that the field is
!*   a coupling field (keyword EXPORTED','EXPOUT','IGNORED' or 'IGNOUT' 
!*   at the end of the field 1st line),do the export here.
!*
          IF (ig_def_state(iport) .EQ. ip_expout .OR. &
               ig_def_state(iport) .eq. ip_exported .or. &
               ig_def_state(iport) .eq. ip_ignored .or. &
               ig_def_state(iport) .eq. ip_ignout .or. &
               ig_def_state(iport) .eq. ip_auxilary) THEN

!
!*       check for connected ports (in)
!        ------------------------------
!
             WRITE(nulprt,FMT='(A,A)') 'Put - ', cports(iport)
!
             ityp = myport(2,iport)
             ibyt = myport(3,iport)
!
             DO ip=1,myport(5,iport)
!
                ilk  = myport(5+ip,iport)
                imod = mylink(1,ilk)
                itid = mylink(2,ilk)
                itag = mylink(3,ilk) - il_newtime / ig_frqmin
                iseg = mylink(4,ilk)
!
#if defined use_comm_MPI1 || defined use_comm_MPI2      
                ilgb = 0
                iposbuf = 0
                DO is=1,iseg
                   ioff = mylink(4+2*is-1,ilk) * 2 + 1
                   il_len = mylink(4+2*is,ilk)
! 
                   IF ( ityp .EQ. PRISM_Real ) THEN
                      CALL MPI_Pack ( rd_field(ioff), il_len, &
                           MPI_REAL,pkwork_field, ig_maxtype_field, iposbuf, &
                           mpi_comm, info )
                   ELSE
                      WRITE(nulprt,*)'Put - pb type incorrect ', ityp
                      kinfo = CLIM_BadType
                      GO TO 1010
                   ENDIF
                   ilgb = ilgb + il_len
                ENDDO
                IF (info.ne.0 .or. ilgb*ibyt .gt. ig_maxtype_field) THEN
                   kinfo = CLIM_Pack
                   WRITE(nulprt,FMT='(A,I3,I8,A)') &
                        'Put - pb pack<mpi ',info,ilgb*ibyt,'>'
                ELSE
!*
                    IF (lg_clim_bsend) THEN
!*   Buffered send
!*   -> if fields are not sent and received in the same order, and
!*   and on architectures on which MPI_Send is not implemented with a 
!*   mailbox (e.g. NEC SX5)
!*
                        CALL MPI_BSend ( pkwork_field, iposbuf, &
                        MPI_PACKED, itid, itag, mpi_comm, info )
                    ELSE
!*
!*   Standard blocking send: To be used
!*   -> if fields are necessarily sent and received in the same order, 
!*   -> or on architectures on which MPI_Send is implemented with a 
!*      mailbox (e.g. VPPs); in this case, make sure that your mailbox
!*      size is large enough.
!
                        CALL MPI_Send ( pkwork_field, iposbuf, &
                           MPI_PACKED, itid, itag, mpi_comm, info )
!
                    ENDIF
!
                   IF (info.eq.CLIM_ok) THEN
                      isend = isend + 1
                      nbsend = nbsend + ilgb * ibyt
                   WRITE(nulprt,FMT='(A,I2,A,I6,A,I7,A,I2,A,I10,A,I6,A)') &
                           'Put - <dest:',imod, &
                           '> <step:',il_newtime, &
                           '> <len:',ilgb, &
                           '> <type:',ibyt, &
                           '> <tag:',itag, &
                           '> <comm:',mpi_comm,'>'
                   ELSE
                      kinfo = CLIM_Pvm
                      WRITE(nulprt,FMT='(A,I3,A)') &
                           'Put - pb send <mpi ',info,'>'
                   ENDIF
                ENDIF
#elif defined use_comm_GSIP
                if (myport(5,iport) .ne. 1) CALL prism_abort_proto &
                   (0,'prism_put_proto', 'STOP -- only one send to Oasis,  myport(5,iport) should be 1') 
                if (imod .ne. 0) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if sent to Oasis, imod should be 0')                   
                if (itid .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if sent to Oasis, itid should be 1') 
!
!               Fill pkworkps with segments of rd_field
                ilgb = 0
                il_rst = 0
                il_ren = 0
                DO is=1,iseg
                  ioff = mylink(4+2*is-1,ilk) + 1
                  il_len = mylink(4+2*is,ilk)
                  il_rst = il_ren + 1
                  il_ren = il_rst + il_len - 1
                  pkworkps(il_rst:il_ren) = rd_field(ioff:ioff+il_len-1) 
!     
                  ilgb = ilgb + il_len
                ENDDO
                IF (ilgb .GT. ig_CLIMmax) THEN
                   WRITE(UNIT = nulprt,FMT = *) &
                       '1- prism_put_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_put_proto', &
                       'STOP - sum of segments greater than pkworkps size')
                ENDIF
!
!               Write the field in channel to Oasis (no DIRECT communication)
                il_errgsip = mgi_write (ig_gsipw, pkworkps, ig_CLIMmax, 'R')
                IF (il_errgsip .GE. 0) THEN
                    WRITE(UNIT = nulprt,FMT = *) &
                       'prism_put_proto - pkworkps written OK:', il_errgsip
                    WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                       'Put - <dest:',imod, '> <step:',kstep,'> <len:', &
                       ilgb, '> <type:',ibyt, '> <noproc:',itid,'>'
                    isend = isend + 1
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '2- prism_put_proto - error :', il_errgsip
                    CALL flush(nulprt)
                    CALL prism_abort_proto (0, 'prism_put_proto', &
                       'STOP - pkworkps not written OK)')
                ENDIF
#endif
!     
             ENDDO
!
              IF (kinfo .EQ. PRISM_Output) THEN
                  kinfo = PRISM_SentOut
              ELSE
                  kinfo = PRISM_Sent
              ENDIF

             WRITE(nulprt,FMT='(A,I3,A)') & 
                  'Put r14- ',isend,' fields exported'
         ENDIF
     ENDIF
 ENDIF
!
!     ----------------------------------------------------------------
!
1010 CONTINUE
    CALL FLUSH(nulprt)
    RETURN
  END SUBROUTINE prism_put_proto_r14
#endif
  SUBROUTINE prism_put_proto_r18(id_port_id,kstep,rd_field,kinfo)
!
!*    *** PRISM_put ***   PRISM 1.0
!
!     purpose:
!     --------
!        give rd_field to Oasis or models connected to port id_port_id at the 
!        time kstep
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
!        Arnaud Caubel  - Fecit (08/02 - created from CLIM_Export)
!        S. Valcke, CERFACS, 24/10/2004: Added GSIP
!     ----------------------------------------------------------------
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
    USE mathelp_psmile
#if defined use_comm_GSIP 
      USE mod_gsip_model
#endif
    IMPLICIT NONE
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
#endif
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p)       kstep, kinfo, id_port_id
    REAL(kind=ip_double_p), DIMENSION(myport(4,id_port_id)) :: rd_field
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p)    il_newtime
    INTEGER (kind=ip_intwp_p)    info
    INTEGER (kind=ip_intwp_p)	   isend, ip, iport, ilk, iseg, is, ilgb, &
         imod, itid, itag, il_len, ioff, ityp, ibyt
    INTEGER (kind=ip_intwp_p)    iposbuf
    INTEGER (kind=ip_intwp_p) :: ib, il_nbopp, il_nbin, il_nbout
    INTEGER (kind=ip_intwp_p), PARAMETER :: ip_nbopp_max = 10
    INTEGER (kind=ip_intwp_p), PARAMETER :: ip_nbindex=1
    INTEGER (kind=ip_intwp_p) :: il_nindex(ip_nbindex)
    REAL(kind=ip_double_p) :: rl_tmp_scal(ip_nbopp_max)
    REAL(kind=ip_double_p),PARAMETER :: ip_missing_val=1.e20
    REAL(kind=ip_double_p), DIMENSION(myport(4,id_port_id)) :: rl_field_aux
    CHARACTER(len=80) :: cl_topps, cl_str
    CHARACTER(len=7) :: cl_tmp_topp, cl_tmp_sopp(ip_nbopp_max)
#ifdef use_comm_GSIP
     INTEGER(kind=ip_intwp_p)     ::  il_rst, il_ren, il_errgsip 
     INTEGER :: mgi_write
#endif
!     ----------------------------------------------------------------
    rl_field_aux(:)=0
    cl_tmp_sopp(:)=' '
    il_nindex(:)=0
    rl_tmp_scal(:)=0
!
!*    0. Entering
!     -----------
!
    kinfo = PRISM_Ok
    IF (ip_realwp_p == ip_single_p) CALL prism_abort_proto (0,'prism_put_proto', &
       'STOP -- PSMILe compiled with single precision REAL; prism_put_proto_r18 should not be called')
    lg_dgfield = .true.
!
!*    1. check for this port in my list
!     ---------------------------------
!
    isend = 0
    iport = -1
!
!   Test if the field is defined in the namcouple and if its coupling period
!   is not greater than the time of the simulation.
    IF (ig_def_freq(id_port_id) .eq. 0.or. &
         ig_def_freq(id_port_id) .gt. ig_ntime) THEN 
       GOTO 1010
    ENDIF
!
!     if a field exported at a certain time should match an import
!     at a time+lag, add the lag here; the lag is given by the user
!     in the namcouple at the end of the field 2nd line.
    IF (myport(1,id_port_id).eq.CLIM_Out) THEN
       iport=id_port_id
       il_newtime = kstep + ig_def_lag(iport)
    ENDIF

    IF (iport.LT.0) THEN
        kinfo = CLIM_BadPort
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
    IF (ig_def_trans(iport) .EQ. ip_instant) THEN 
        cl_str = 'inst(ident(X))'
        dg_field_trans(:,iport) = rd_field (:)
    ELSEIF (ig_def_trans(iport) .eq. ip_average .or. &
       ig_def_trans(iport) .EQ. ip_accumul .OR. &
       ig_def_trans(iport) .EQ. ip_min .OR. &
       ig_def_trans(iport) .EQ. ip_max) THEN
        IF (ig_number(iport) .EQ. 0) dg_field_trans(:,iport) = 0
        il_nbin = myport(4,iport)
        il_nbout = il_nbin
        cl_topps = 'ave, inst, t_sum, t_min, t_max'
        IF (ig_def_trans(iport) .EQ. ip_average) THEN
            cl_str = 'ave(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_accumul) THEN 
            cl_str = 't_sum(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_min) THEN
            cl_str = 't_min(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_max) THEN
            cl_str = 't_max(ident(X))'
        ENDIF
        CALL buildop (cl_str,cl_topps,cl_tmp_topp,ip_nbopp_max, &
           ip_missing_val,cl_tmp_sopp,rl_tmp_scal,il_nbopp)
      
        CALL mathop (cl_tmp_sopp(1), il_nbin, rd_field, ip_missing_val, &
           ip_nbindex, il_nindex, rl_tmp_scal(1), il_nbout, &
           rl_field_aux)

        IF ((cl_tmp_topp(:LEN_TRIM(cl_tmp_topp)) .NE. 'inst')) &
           CALL moycum(cl_tmp_topp, il_nbin, dg_field_trans(:,iport), &
           rl_field_aux, ig_number(iport))
        ig_number(iport) = ig_number(iport) + 1
        IF (MOD(il_newtime,ig_def_freq(iport)) .EQ. 0) THEN
            ig_number(iport) = 0
            DO ib = 1, myport(4,iport)
              rd_field(ib) = dg_field_trans(ib,iport)
            ENDDO
        ENDIF
        kinfo = PRISM_LocTrans
    ENDIF
!
!*    Test if field must be written to restart file i.e.
!*    - current time is time at the end of simulation +
!*    - lag of current field is greater 0
!
    IF (il_newtime.eq.ig_ntime.and.ig_def_lag(iport).gt.0) THEN
!ac
       IF (ig_def_state(iport) .ne. ip_output) THEN
!ac
!
!*       Note: A model can have several restart files but same restart 
!*       file can't be used by different models
!
        IF (mydist(CLIM_Strategy,iport) .eq. CLIM_Serial) THEN
          CALL write_filer8(rd_field,cports(iport),iport)
        ELSE
          CALL write_file_parar8(rd_field,cports(iport),iport)
        ENDIF
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
		CALL psmile_write_8(iport,rd_field,il_newtime)
              	kinfo = PRISM_Output
	  ELSEIF (ig_def_state(iport) .eq. ip_expout .or. &
        	       ig_def_state(iport) .EQ. ip_ignout) THEN
		CALL psmile_write_8(iport,rd_field,il_newtime)
                kinfo = PRISM_ToRestOut
          ENDIF	
!ac
#endif
      ENDIF
    ELSE

!    Test if the current time is a coupling (or I/O) time  
       IF (mod(il_newtime,ig_def_freq(iport)).eq.0) THEN
!
#if !defined key_noIO
!*   If the user indicated in the namcouple that the field is
!*   a field output-to-file (keyword 'OUTPUT', 'IGNOUT' or 'EXPOUT'
!*   at the end of the field 1st line), do the writing to file here, e.g.:
          IF (ig_def_state(iport) .eq. ip_output .or. &
               ig_def_state(iport) .eq. ip_expout .or. &
               ig_def_state(iport) .eq. ip_ignout) THEN
             call psmile_write_8(iport,rd_field,il_newtime)
              kinfo = PRISM_Output
          ENDIF
#endif
!*
!*   If the user indicated in the namcouple that the field is
!*   a coupling field (keyword EXPORTED','EXPOUT','IGNORED' or 'IGNOUT' 
!*   at the end of the field 1st line),do the export here.
!*
          IF (ig_def_state(iport) .eq. ip_expout .or. &
               ig_def_state(iport) .eq. ip_exported .or. &
               ig_def_state(iport) .eq. ip_ignored .or. &
               ig_def_state(iport) .eq. ip_ignout .or. &
               ig_def_state(iport) .eq. ip_auxilary) THEN

!
!*       check for connected ports (in)
!        ------------------------------
!
             WRITE(nulprt,FMT='(A,A)') 'Put - ', cports(iport)
!
             ityp = myport(2,iport)
             ibyt = myport(3,iport)
!
             DO ip=1,myport(5,iport)
!
                ilk  = myport(5+ip,iport)
!               imod is the distant model number
                imod = mylink(1,ilk)
!               itid is the distant local process number
                itid = mylink(2,ilk)
                itag = mylink(3,ilk) - il_newtime / ig_frqmin
                iseg = mylink(4,ilk)
! 
#if defined use_comm_MPI1 || defined use_comm_MPI2     
                ilgb = 0
                iposbuf = 0
                DO is=1,iseg
                   ioff = mylink(4+2*is-1,ilk) + 1
                   il_len = mylink(4+2*is,ilk)
!     
                   IF ( ityp .EQ. PRISM_Real ) THEN
                      CALL MPI_Pack ( rd_field(ioff), il_len, &
                           MPI_DOUBLE_PRECISION, pkwork_field, &
                           ig_maxtype_field, iposbuf, &
                           mpi_comm, info )
                   ELSE
                      WRITE(nulprt,*)'Put - pb type incorrect ', ityp
                      kinfo = CLIM_BadType
                      GO TO 1010
                   ENDIF
                   ilgb = ilgb + il_len
                ENDDO
                IF (info.ne.0 .or. ilgb*ibyt .gt. ig_maxtype_field) THEN
                   kinfo = CLIM_Pack
                   WRITE(nulprt,FMT='(A,I3,I8,A)') &
                        'Put - pb pack<mpi ',info,ilgb*ibyt,'>'
                ELSE
                    IF (lg_clim_bsend) THEN
!*
!*   Buffered send
!*   -> if fields are not sent and received in the same order, and
!*   and on architectures on which MPI_Send is not implemented with a 
!*   mailbox (e.g. NEC SX5)
!*
                        CALL MPI_BSend ( pkwork_field, iposbuf, &
                           MPI_PACKED, itid, itag, mpi_comm, info )
                    ELSE
!*
!*   Standard blocking send: To be used
!*   -> if fields are necessarily sent and received in the same order, 
!*   -> or on architectures on which MPI_Send is implemented with a 
!*      mailbox (e.g. VPPs); in this case, make sure that your mailbox
!*      size is large enough.
!
                        CALL MPI_Send ( pkwork_field, iposbuf, &
                           MPI_PACKED, itid, itag, mpi_comm, info )
!
                    ENDIF
!
                   IF (info.eq.CLIM_ok) THEN
                      isend = isend + 1
                      nbsend = nbsend + ilgb * ibyt
                      WRITE(nulprt,FMT='(A,I2,A,I6,A,I7,A,I2,A,I10,A)') &
                           'Put - <dest:',imod, &
                           '> <step:',il_newtime, &
                           '> <len:',ilgb, &
                           '> <type:',ibyt, &
                        '> <tag:',itag,'>'
                   ELSE
                      kinfo = CLIM_Pvm
                      WRITE(nulprt,FMT='(A,I3,A)') &
                           'Put - pb send <mpi ',info,'>'
                   ENDIF
                ENDIF
#elif defined use_comm_GSIP
                if (myport(5,iport) .ne. 1) CALL prism_abort_proto &
                   (0,'prism_put_proto', 'STOP -- only one send to Oasis,  myport(5,iport) should be 1') 
                if (imod .ne. 0) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if sent to Oasis, imod should be 0')                   
                if (itid .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if sent to Oasis, itid should be 1') 
!
!               Fill pkworkps with segments of rd_field
                ilgb = 0
                il_rst = 0
                il_ren = 0
                DO is=1,iseg
                  ioff = mylink(4+2*is-1,ilk) + 1
                  il_len = mylink(4+2*is,ilk)
                  il_rst = il_ren + 1
                  il_ren = il_rst + il_len - 1
                  pkworkps(il_rst:il_ren) = rd_field(ioff:ioff+il_len-1) 
!     
                  ilgb = ilgb + il_len
                ENDDO
                IF (ilgb .GT. ig_CLIMmax) THEN
                   WRITE(UNIT = nulprt,FMT = *) &
                       '1- prism_put_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_put_proto', &
                       'STOP - sum of segments greater than pkworkps size')
                ENDIF
!
!               Write the field in channel to Oasis (no DIRECT communication)
                il_errgsip = mgi_write (ig_gsipw, pkworkps, ig_CLIMmax, 'D')
                IF (il_errgsip .GE. 0) THEN
                    WRITE(UNIT = nulprt,FMT = *) &
                       'prism_put_proto - pkworkps written OK:', il_errgsip
                    WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                       'Put - <dest:',imod, '> <step:',kstep,'> <len:', &
                       ilgb, '> <type:',ibyt, '> <noproc:',itid,'>'
                    isend = isend + 1
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '2- prism_put_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_put_proto', &
                       'STOP - pkworkps not written OK)')
                ENDIF
#endif    
              ENDDO
!     
              IF (kinfo .EQ. PRISM_Output) THEN
                  kinfo = PRISM_SentOut
              ELSE
                  kinfo = PRISM_Sent
              ENDIF

             WRITE(nulprt,FMT='(A,I3,A)') & 
                  'Put r18- ',isend,' fields exported'
          ENDIF
       ENDIF
    ENDIF
!
!     ----------------------------------------------------------------
!
1010 CONTINUE
    CALL FLUSH(nulprt)
    RETURN
  END SUBROUTINE prism_put_proto_r18
#ifndef __NO_4BYTE_REALS
  SUBROUTINE prism_put_proto_r24(id_port_id,kstep,rd_field_2d,kinfo)
!
!*    *** PRISM_put ***   PRISM 1.0
!
!     purpose:
!     --------
!        give pfield to Oasis or models connected to port id_port_id at the 
!        time kstep
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
!        mpi-1
!
!     author:
!     -------
!         Arnaud Caubel  - Fecit (08/02 - created from CLIM_Export)
!         S. Valcke, CERFACS, 24/10/2004: Added GSIP
!     ----------------------------------------------------------------
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
    USE mathelp_psmile
#if defined use_comm_GSIP 
      USE mod_gsip_model
#endif
    IMPLICIT NONE
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
#endif
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p)       kstep, kinfo, id_port_id
    REAL(kind=ip_single_p), DIMENSION(:,:) :: rd_field_2d
!     ----------------------------------------------------------------
    REAL(kind=ip_single_p), DIMENSION(myport(4,id_port_id)) :: rd_field
    
    INTEGER (kind=ip_intwp_p)    il_newtime
    INTEGER (kind=ip_intwp_p)    info, ib
    INTEGER (kind=ip_intwp_p)	 isend, ip, iport, ilk, iseg, is, ilgb
    INTEGER (kind=ip_intwp_p)    imod, itid, itag, il_len, ioff, ityp, ibyt
    INTEGER (kind=ip_intwp_p)    iposbuf
    INTEGER (kind=ip_intwp_p) :: il_nbopp, il_nbin, il_nbout
    INTEGER (kind=ip_intwp_p), PARAMETER :: ip_nbopp_max = 10
    INTEGER (kind=ip_intwp_p), PARAMETER :: ip_nbindex=1
    INTEGER (kind=ip_intwp_p) :: il_nindex(ip_nbindex)
    REAL(kind=ip_single_p) :: rl_tmp_scal(ip_nbopp_max)
    REAL(kind=ip_single_p),PARAMETER :: ip_missing_val=1.e20
    REAL(kind=ip_single_p), DIMENSION(myport(4,id_port_id)) :: rl_field_aux
    CHARACTER(len=80) :: cl_topps, cl_str
    CHARACTER(len=7) :: cl_tmp_topp, cl_tmp_sopp(ip_nbopp_max)
#ifdef use_comm_GSIP
     INTEGER(kind=ip_intwp_p)     ::  il_rst, il_ren, il_errgsip 
     INTEGER :: mgi_write
#endif
!     ----------------------------------------------------------------
!
    rl_field_aux(:)=0
    cl_tmp_sopp(:)=' '
    il_nindex(:)=0
    rl_tmp_scal(:)=0
!
!*    0. Entering
!     -----------
!
    kinfo = PRISM_Ok
    IF (ip_realwp_p == ip_double_p) CALL prism_abort_proto (0,'prism_put_proto', &
       'STOP -- PSMILe compiled with double precision REAL; prism_put_proto_r24 should not be called')
    lg_dgfield = .false.
!
!*    1. check for this port in my list
!     ---------------------------------
!
    isend = 0
    iport = -1
!
!
!   Test if the field is defined in the namcouple and if its coupling period
!   is not greater than the time of the simulation.
    IF (ig_def_freq(id_port_id) .eq. 0 .or. &
         ig_def_freq(id_port_id) .gt. ig_ntime) THEN 
       GOTO 1010
    ENDIF
!     if a field exported at a certain time should match an import
!     at a time+lag, add the lag here; the lag is given by the user
!     in the namcouple at the end of the field 2nd line.
    IF (myport(1,id_port_id).eq.CLIM_Out) THEN
       iport=id_port_id
       il_newtime = kstep + ig_def_lag(iport)
    ENDIF

    IF (iport.lt.0) THEN
       kinfo = CLIM_BadPort
       WRITE(nulprt,FMT='(A,A)') &
            'Put - WARNING - Invalid port out: ', &
            cports(id_port_id)
       GO TO 1010
    ENDIF
!
!   Reshape 2d field
!
    rd_field(1:myport(4,id_port_id)) = RESHAPE (rd_field_2d(:,:), &
         (/myport(4,id_port_id)/))
!
!   If the user indicated in the namcouple that the field must be
!   accumulated or averaged (keyword 'AVERAGE' or 'ACCUMUL' at the
!   end of the field 2nd line), do the local transformations.
!    
    IF (ig_def_trans(iport) .EQ. ip_instant) THEN 
        cl_str = 'inst(ident(X))'
        rg_field_trans(:,iport) = rd_field (:)
    ELSEIF (ig_def_trans(iport) .eq. ip_average .or. &
       ig_def_trans(iport) .EQ. ip_accumul .OR. &
       ig_def_trans(iport) .EQ. ip_min .OR. &
       ig_def_trans(iport) .EQ. ip_max) THEN
        IF (ig_number(iport) .EQ. 0) rg_field_trans(:,iport) = 0
        il_nbin = myport(4,iport)
        il_nbout = il_nbin
        cl_topps = 'ave, inst, t_sum, t_min, t_max'
        IF (ig_def_trans(iport) .EQ. ip_average) THEN
            cl_str = 'ave(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_accumul) THEN 
            cl_str = 't_sum(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_min) THEN
            cl_str = 't_min(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_max) THEN
            cl_str = 't_max(ident(X))'
        ENDIF
        CALL buildop (cl_str,cl_topps,cl_tmp_topp,ip_nbopp_max, &
           ip_missing_val,cl_tmp_sopp,rl_tmp_scal,il_nbopp)
     
        CALL mathop (cl_tmp_sopp(1), il_nbin, rd_field, ip_missing_val, &
           ip_nbindex, il_nindex, rl_tmp_scal(1), il_nbout, &
           rl_field_aux)

        IF ((cl_tmp_topp(:LEN_TRIM(cl_tmp_topp)) .NE. 'inst')) &
           CALL moycum(cl_tmp_topp, il_nbin, rg_field_trans(:,iport), &
           rl_field_aux, ig_number(iport))
        ig_number(iport) = ig_number(iport) + 1
        IF (MOD(il_newtime,ig_def_freq(iport)) .EQ. 0) THEN
            ig_number(iport) = 0
            DO ib = 1, myport(4,iport)
              rd_field(ib) = rg_field_trans(ib,iport)
            ENDDO
        ENDIF
        kinfo = PRISM_LocTrans
    ENDIF
!
!*    Test if field must be written to restart file i.e.
!*    - current time is time at the end of simulation +
!*    - lag of current field is greater 0
!
    IF (il_newtime.eq.ig_ntime.and.ig_def_lag(iport).gt.0) THEN
!ac
       IF (ig_def_state(iport) .ne. ip_output) THEN
!ac
!
!*       Note: A model can have several restart files but same restart 
!*       file can't be used by different models
!
       IF (mydist(CLIM_Strategy,iport) .eq. CLIM_Serial) THEN
          CALL write_filer4(rd_field,cports(iport),iport)
       ELSE
          CALL write_file_parar4(rd_field,cports(iport),iport)
       ENDIF
       kinfo = PRISM_ToRest
!ac
       ENDIF
!ac
!    Test if the current time is a coupling (or I/O) time  
       IF (mod(il_newtime,ig_def_freq(iport)).eq.0) THEN

#if !defined key_noIO
!*   If the user indicated in the namcouple that the field is
!*   a field output-to-file (keyword 'OUTPUT', 'IGNOUT' or 'EXPOUT'
!*   at the end of the field 1st line), do the writing to file here, e.g.:
!ac
          IF (ig_def_state(iport) .EQ. ip_output ) THEN
		CALL psmile_write_4(iport,rd_field,il_newtime)
              	kinfo = PRISM_Output
	  ELSEIF (ig_def_state(iport) .eq. ip_expout .or. &
        	       ig_def_state(iport) .EQ. ip_ignout) THEN
		CALL psmile_write_4(iport,rd_field,il_newtime)
                kinfo = PRISM_ToRestOut
          ENDIF	
!ac
#endif
       ENDIF
    ELSE
!    Test if the current time is a coupling (or I/O) time  
       IF (mod(il_newtime,ig_def_freq(iport)).eq.0) THEN
!
#if !defined key_noIO
!*   If the user indicated in the namcouple that the field is
!*   a field output-to-file (keyword 'OUTPUT', 'IGNOUT' or 'EXPOUT'
!*   at the end of the field 1st line), do the writing to file here, e.g.:
          IF (ig_def_state(iport) .EQ. ip_output .OR. &
               ig_def_state(iport) .eq. ip_expout .or. &
               ig_def_state(iport) .EQ. ip_ignout) THEN
              CALL psmile_write_4(iport,rd_field,il_newtime)
              kinfo = PRISM_Output
          ENDIF
#endif
!*
!*   If the user indicated in the namcouple that the field is
!*   a coupling field (keyword EXPORTED','EXPOUT','IGNORED' or 'IGNOUT' 
!*   at the end of the field 1st line),do the export here.
!*
          IF (ig_def_state(iport) .eq. ip_expout .or. &
               ig_def_state(iport) .eq. ip_exported .or. &
               ig_def_state(iport) .eq. ip_ignored .or. &
               ig_def_state(iport) .eq. ip_ignout .or. &
               ig_def_state(iport) .eq. ip_auxilary) THEN
!
!*       check for connected ports (in)
!        ------------------------------
!
          WRITE(nulprt,FMT='(A,A)') 'Put - ', cports(iport)
!
          ityp = myport(2,iport)
          ibyt = myport(3,iport)
!
          DO ip=1,myport(5,iport)
!
             ilk  = myport(5+ip,iport)
             imod = mylink(1,ilk)
             itid = mylink(2,ilk)
             itag = mylink(3,ilk) - il_newtime / ig_frqmin
             iseg = mylink(4,ilk)
! 
#if defined use_comm_MPI1 || defined use_comm_MPI2    
             ilgb = 0
             iposbuf = 0
             DO is=1,iseg
                ioff = mylink(4+2*is-1,ilk) * 2 + 1
                il_len = mylink(4+2*is,ilk)
!     
                IF ( ityp .EQ. PRISM_Real ) THEN
                   CALL MPI_Pack ( rd_field(ioff), il_len, &
                        MPI_REAL,pkwork_field, ig_maxtype_field, iposbuf, &
                        mpi_comm, info )
                ELSE
                   WRITE(nulprt,*)'Put - pb type incorrect ', ityp
                   kinfo = CLIM_BadType
                   GO TO 1010
                ENDIF
                ilgb = ilgb + il_len
             ENDDO
             IF (info.ne.0 .or. ilgb*ibyt .gt. ig_maxtype_field) THEN
                kinfo = CLIM_Pack
                WRITE(nulprt,FMT='(A,I3,I8,A)') &
                     'Put - pb pack<mpi ',info,ilgb*ibyt,'>'
             ELSE
                 IF (lg_clim_bsend) THEN
!*
!*   Buffered send
!*   -> if fields are not sent and received in the same order, and
!*   and on architectures on which MPI_Send is not implemented with a 
!*   mailbox (e.g. NEC SX5)
!*
                CALL MPI_BSend ( pkwork_field, iposbuf, MPI_PACKED, &
                   itid, itag, mpi_comm, info )
                ELSE
!*
!*   Standard blocking send: To be used
!*   -> if fields are necessarily sent and received in the same order, 
!*   -> or on architectures on which MPI_Send is implemented with a 
!*      mailbox (e.g. VPPs); in this case, make sure that your mailbox
!*      size is large enough.
!
                CALL MPI_Send ( pkwork_field, iposbuf, MPI_PACKED, &
                   itid, itag, mpi_comm, info )
!
            ENDIF
!
                IF (info.eq.CLIM_ok) THEN
                   isend = isend + 1
                   nbsend = nbsend + ilgb * ibyt
                   WRITE(nulprt,FMT='(A,I2,A,I6,A,I7,A,I2,A,I10,A)') &
                        'Put - <dest:',imod, &
                        '> <step:',il_newtime, &
                        '> <len:',ilgb, &
                        '> <type:',ibyt, &
                        '> <tag:',itag,'>'
                ELSE
                   kinfo = CLIM_Pvm
                   WRITE(nulprt,FMT='(A,I3,A)') &
                        'Put - pb send <mpi ',info,'>'
                ENDIF
             ENDIF
!
#elif defined use_comm_GSIP
                if (myport(5,iport) .ne. 1) CALL prism_abort_proto &
                   (0,'prism_put_proto', 'STOP -- only one send to Oasis,  myport(5,iport) should be 1') 
                if (imod .ne. 0) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if sent to Oasis, imod should be 0')                   
                if (itid .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if sent to Oasis, itid should be 1') 
!
!               Fill pkworkps with segments of rd_field
                ilgb = 0
                il_rst = 0
                il_ren = 0
                DO is=1,iseg
                  ioff = mylink(4+2*is-1,ilk) + 1
                  il_len = mylink(4+2*is,ilk)
                  il_rst = il_ren + 1
                  il_ren = il_rst + il_len - 1
                  pkworkps(il_rst:il_ren) = rd_field(ioff:ioff+il_len-1) 
!     
                  ilgb = ilgb + il_len
                ENDDO
                IF (ilgb .GT. ig_CLIMmax) THEN
                   WRITE(UNIT = nulprt,FMT = *) &
                       '1- prism_put_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_put_proto', &
                       'STOP - sum of segments greater than pkworkps size')
                ENDIF
!
!               Write the field in channel to Oasis (no DIRECT communication)
                il_errgsip = mgi_write (ig_gsipw, pkworkps, ig_CLIMmax, 'R')
                IF (il_errgsip .GE. 0) THEN
                    WRITE(UNIT = nulprt,FMT = *) &
                       'prism_put_proto - pkworkps written OK:', il_errgsip
                    WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                       'Put - <dest:',imod, '> <step:',kstep,'> <len:', &
                       ilgb, '> <type:',ibyt, '> <noproc:',itid,'>'
                    isend = isend + 1
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '2- prism_put_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_put_proto', &
                       'STOP - pkworkps not written OK)')
                ENDIF
#endif      
          ENDDO
! 
          IF (kinfo .EQ. PRISM_Output) THEN
              kinfo = PRISM_SentOut
          ELSE
              kinfo = PRISM_Sent
          ENDIF

          WRITE(nulprt,FMT='(A,I3,A)') & 
               'Put r24- ',isend,' fields exported'
       ENDIF
    ENDIF
 ENDIF
!
!     ----------------------------------------------------------------
!
1010 CONTINUE
 CALL FLUSH(nulprt)
 RETURN
END SUBROUTINE prism_put_proto_r24
#endif
  SUBROUTINE prism_put_proto_r28(id_port_id,kstep,rd_field_2d,kinfo)
!
!*    *** PRISM_put ***   PRISM 1.0
!
!     purpose:
!     --------
!        give rd_field to Oasis or models connected to port id_port_id at the 
!        time kstep
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
!        mpi-1
!
!     author:
!     -------
!         Arnaud Caubel  - Fecit (08/02 - created from CLIM_Export)
!         S. Valcke, CERFACS, 24/10/2004: Added GSIP
!     ----------------------------------------------------------------
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
    USE mathelp_psmile
#if defined use_comm_GSIP 
      USE mod_gsip_model
#endif
    IMPLICIT NONE
#if defined use_comm_MPI1 || defined use_comm_MPI2
#include <mpif.h>
#endif
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p)       kstep, kinfo, id_port_id
    REAL(kind=ip_double_p), DIMENSION(:,:) :: rd_field_2d
!     ----------------------------------------------------------------
    REAL(kind=ip_double_p), DIMENSION(myport(4,id_port_id)) :: rd_field
    INTEGER (kind=ip_intwp_p)    il_newtime
    INTEGER (kind=ip_intwp_p)    info
    INTEGER (kind=ip_intwp_p)	   isend, ip, iport, ilk, iseg, is, ilgb, &
         imod, itid, itag, il_len, ioff, ityp, ibyt
    INTEGER (kind=ip_intwp_p)    iposbuf
    INTEGER (kind=ip_intwp_p) :: ib, il_nbopp, il_nbin, il_nbout
    INTEGER (kind=ip_intwp_p), PARAMETER :: ip_nbopp_max = 10
    INTEGER (kind=ip_intwp_p), PARAMETER :: ip_nbindex=1
    INTEGER (kind=ip_intwp_p) :: il_nindex(ip_nbindex)
    REAL(kind=ip_double_p) :: rl_tmp_scal(ip_nbopp_max)
    REAL(kind=ip_double_p),PARAMETER :: ip_missing_val=1.e20
    REAL(kind=ip_double_p), DIMENSION(myport(4,id_port_id)) :: rl_field_aux
    CHARACTER(len=80) :: cl_topps, cl_str
    CHARACTER(len=7) :: cl_tmp_topp, cl_tmp_sopp(ip_nbopp_max)
#ifdef use_comm_GSIP
     INTEGER(kind=ip_intwp_p)     ::  il_rst, il_ren, il_errgsip 
     INTEGER :: mgi_write
#endif
!     ----------------------------------------------------------------
!
    rl_field_aux(:)=0
    cl_tmp_sopp(:)=' '
    il_nindex(:)=0
    rl_tmp_scal(:)=0
!
!*    0. Entering
!     -----------
!
    kinfo = PRISM_Ok
    IF (ip_realwp_p == ip_single_p) CALL prism_abort_proto (0,'prism_put_proto', &
       'STOP -- PSMILe compiled with single precision REAL; prism_put_proto_r28 should not be called')
    lg_dgfield = .true.
!
!*    1. check for this port in my list
!     ---------------------------------
!
    isend = 0
    iport = -1
!
!
!   Test if the field is defined in the namcouple and if its coupling period
!   is not greater than the time of the simulation.
    IF (ig_def_freq(id_port_id) .eq. 0 .or. &
         ig_def_freq(id_port_id) .gt. ig_ntime) THEN 
       GOTO 1010
    ENDIF
!     if a field exported at a certain time should match an import
!     at a time+lag, add the lag here; the lag is given by the user
!     in the namcouple at the end of the field 2nd line.
    IF (myport(1,id_port_id).eq.CLIM_Out) THEN
       iport=id_port_id
       il_newtime = kstep + ig_def_lag(iport)
    ENDIF
    IF (iport.lt.0) THEN
       kinfo = CLIM_BadPort
       WRITE(nulprt,FMT='(A,A)') &
            'Put - WARNING - Invalid port out: ', &
            cports(id_port_id)
       GO TO 1010
    ENDIF
!
!   Reshape the 2d field
!
    rd_field(1:myport(4,id_port_id)) = RESHAPE (rd_field_2d(:,:), &
         (/myport(4,id_port_id)/))
!
!   If the user indicated in the namcouple that the field must be
!   accumulated or averaged (keyword 'AVERAGE' or 'ACCUMUL' at the
!   end of the field 2nd line), do the local transformations.
!    
    IF (ig_def_trans(iport) .EQ. ip_instant) THEN 
        cl_str = 'inst(ident(X))'
        dg_field_trans(:,iport) = rd_field (:)
    ELSEIF (ig_def_trans(iport) .EQ. ip_average .OR. &
       ig_def_trans(iport) .EQ. ip_accumul .OR. &
       ig_def_trans(iport) .EQ. ip_min .OR. &
       ig_def_trans(iport) .EQ. ip_max) THEN
        IF (ig_number(iport) .EQ. 0) dg_field_trans(:,iport) = 0
        il_nbin = myport(4,iport)
        il_nbout = il_nbin
        cl_topps = 'ave, inst, t_sum, t_min, t_max'
        IF (ig_def_trans(iport) .EQ. ip_average) THEN
            cl_str = 'ave(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_accumul) THEN 
            cl_str = 't_sum(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_min) THEN
            cl_str = 't_min(ident(X))'
        ELSEIF (ig_def_trans(iport) .EQ. ip_max) THEN
            cl_str = 't_max(ident(X))'
        ENDIF
        CALL buildop (cl_str,cl_topps,cl_tmp_topp,ip_nbopp_max, &
           ip_missing_val,cl_tmp_sopp,rl_tmp_scal,il_nbopp)
      
        CALL mathop (cl_tmp_sopp(1), il_nbin, rd_field, ip_missing_val, &
           ip_nbindex, il_nindex, rl_tmp_scal(1), il_nbout, &
           rl_field_aux)

        IF ((cl_tmp_topp(:LEN_TRIM(cl_tmp_topp)) .NE. 'inst')) &
           CALL moycum(cl_tmp_topp, il_nbin, dg_field_trans(:,iport), &
           rl_field_aux, ig_number(iport))
        ig_number(iport) = ig_number(iport) + 1
        IF (MOD(il_newtime,ig_def_freq(iport)) .EQ. 0) THEN
            ig_number(iport) = 0
            DO ib = 1, myport(4,iport)
              rd_field(ib) = dg_field_trans(ib,iport)
            ENDDO
        ENDIF
        kinfo = PRISM_LocTrans
    ENDIF
!
!*    Test if field must be written to restart file i.e.
!*    - current time is time at the end of simulation +
!*    - lag of current field is greater from 0
!
    IF (il_newtime.eq.ig_ntime.and.ig_def_lag(iport).gt.0) THEN
!ac
       IF (ig_def_state(iport) .ne. ip_output) THEN
!ac
!
!*       Note: A model can have several restart files but same restart 
!*       file can't be used by different models
!
       IF (mydist(CLIM_Strategy,iport) .eq. CLIM_Serial) THEN
          CALL write_filer8(rd_field,cports(iport),iport)
       ELSE
          CALL write_file_parar8(rd_field,cports(iport),iport)
       ENDIF
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
		CALL psmile_write_8(iport,rd_field,il_newtime)
              	kinfo = PRISM_Output
	  ELSEIF (ig_def_state(iport) .eq. ip_expout .or. &
        	       ig_def_state(iport) .EQ. ip_ignout) THEN
		CALL psmile_write_8(iport,rd_field,il_newtime)
                kinfo = PRISM_ToRestOut
          ENDIF	
!ac
!          IF (ig_def_state(iport) .EQ. ip_output .OR. &
!               ig_def_state(iport) .eq. ip_expout .or. &
!               ig_def_state(iport) .EQ. ip_ignout) THEN
!              CALL psmile_write_8(iport,rd_field,il_newtime)
!              kinfo = PRISM_ToRestOut
!          ENDIF
#endif
      ENDIF
    ELSE

!    Test if the current time is a coupling (or I/O) time  
       IF (mod(il_newtime,ig_def_freq(iport)).eq.0) THEN
!
#if !defined key_noIO
!*   If the user indicated in the namcouple that the field is
!*   a field output-to-file (keyword 'OUTPUT', 'IGNOUT' or 'EXPOUT'
!*   at the end of the field 1st line), do the writing to file here, e.g.:
          IF (ig_def_state(iport) .EQ. ip_output .OR. &
               ig_def_state(iport) .eq. ip_expout .or. &
               ig_def_state(iport) .eq. ip_ignout) THEN
             call psmile_write_8(iport,rd_field,il_newtime)
             kinfo = PRISM_Output
         ENDIF
#endif
!*
!*   If the user indicated in the namcouple that the field is
!*   a coupling field (keyword EXPORTED','EXPOUT','IGNORED' or 'IGNOUT' 
!*   at the end of the field 1st line),do the export here.
!*
          IF (ig_def_state(iport) .eq. ip_expout .or. &
               ig_def_state(iport) .eq. ip_exported .or. &
               ig_def_state(iport) .eq. ip_ignored .or. &
               ig_def_state(iport) .eq. ip_ignout .or. &
               ig_def_state(iport) .eq. ip_auxilary) THEN
!
!*       check for connected ports (in)
!        ------------------------------
!
             WRITE(nulprt,FMT='(A,A)') 'Put - ', cports(iport)
!
             ityp = myport(2,iport)
             ibyt = myport(3,iport)
!
             DO ip=1,myport(5,iport)
!
                ilk  = myport(5+ip,iport)
                imod = mylink(1,ilk)
                itid = mylink(2,ilk)
                itag = mylink(3,ilk) - il_newtime / ig_frqmin
                iseg = mylink(4,ilk)
!
#if defined use_comm_MPI1 || defined use_comm_MPI2      
                ilgb = 0
                iposbuf = 0
                DO is=1,iseg
                   ioff = mylink(4+2*is-1,ilk) + 1
                   il_len = mylink(4+2*is,ilk)
                   !     
                   IF ( ityp .EQ. PRISM_Real ) THEN
                       CALL MPI_Pack(rd_field(ioff),il_len, &
                           MPI_DOUBLE_PRECISION, &
                           pkwork_field, ig_maxtype_field, iposbuf, &
                           mpi_comm, info )
                   ELSE
                      WRITE(nulprt,*)'Put - pb type incorrect ', ityp
                      kinfo = CLIM_BadType
                      GO TO 1010
                   ENDIF
                   ilgb = ilgb + il_len
                ENDDO
                IF (info.ne.0 .or. ilgb*ibyt .gt. ig_maxtype_field) THEN
                   kinfo = CLIM_Pack
                   WRITE(nulprt,FMT='(A,I3,I8,A)') &
                        'Put - pb pack<mpi ',info,ilgb*ibyt,'>'
                ELSE
                    IF (lg_clim_bsend) THEN
!*
!*   Buffered send
!*   -> if fields are not sent and received in the same order, and
!*   and on architectures on which MPI_Send is not implemented with a 
!*   mailbox (e.g. NEC SX5)
!*
                        CALL MPI_BSend ( pkwork_field, iposbuf, &
                           MPI_PACKED, itid, itag, mpi_comm, info )
                    ELSE
!*
!*   Standard blocking send: To be used
!*   -> if fields are necessarily sent and received in the same order, 
!*   -> or on architectures on which MPI_Send is implemented with a 
!*      mailbox (e.g. VPPs); in this case, make sure that your mailbox
!*      size is large enough.
!
                        CALL MPI_Send ( pkwork_field, iposbuf, &
                           MPI_PACKED, itid, itag, mpi_comm, info )
!
                    ENDIF
!
                   IF (info.eq.CLIM_ok) THEN
                      isend = isend + 1
                      nbsend = nbsend + ilgb * ibyt
                      WRITE(nulprt,FMT='(A,I2,A,I6,A,I7,A,I2,A,I10,A)') &
                           'Put - <dest:',imod, &
                           '> <step:',il_newtime, &
                           '> <len:',ilgb, &
                           '> <type:',ibyt, &
                           '> <tag:',itag,'>'
                   ELSE
                      kinfo = CLIM_Pvm
                      WRITE(nulprt,FMT='(A,I3,A)') &
                           'Put - pb send <mpi ',info,'>'
                   ENDIF
                ENDIF
!
#elif defined use_comm_GSIP
                if (myport(5,iport) .ne. 1) CALL prism_abort_proto &
                   (0,'prism_put_proto', 'STOP -- only one send to Oasis,  myport(5,iport) should be 1') 
                if (imod .ne. 0) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if sent to Oasis, imod should be 0')                   
                if (itid .ne. 1) CALL prism_abort_proto &
                   (0,'prism_get_proto', 'STOP -- if sent to Oasis, itid should be 1') 
!
!               Fill pkworkps with segments of rd_field
                ilgb = 0
                il_rst = 0
                il_ren = 0
                DO is=1,iseg
                  ioff = mylink(4+2*is-1,ilk) + 1
                  il_len = mylink(4+2*is,ilk)
                  il_rst = il_ren + 1
                  il_ren = il_rst + il_len - 1
                  pkworkps(il_rst:il_ren) = rd_field(ioff:ioff+il_len-1) 
!     
                  ilgb = ilgb + il_len
                ENDDO
                IF (ilgb .GT. ig_CLIMmax) THEN
                   WRITE(UNIT = nulprt,FMT = *) &
                       '1- prism_put_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_put_proto', &
                       'STOP - sum of segments greater than pkworkps size')
                ENDIF
!
!               Write the field in channel to Oasis (no DIRECT communication)
                il_errgsip = mgi_write (ig_gsipw, pkworkps, ig_CLIMmax, 'D')
                IF (il_errgsip .GE. 0) THEN
                    WRITE(UNIT = nulprt,FMT = *) &
                       'prism_put_proto - pkworkps written OK:', il_errgsip
                    WRITE(nulprt,FMT='(A,I2,A,I9,A,I7,A,I2,A,I10,A)') &
                       'Put - <dest:',imod, '> <step:',kstep,'> <len:', &
                       ilgb, '> <type:',ibyt, '> <noproc:',itid,'>'
                    isend = isend + 1
                ELSE
                    WRITE(UNIT = nulprt,FMT = *) &
                       '2- prism_put_proto - error :', il_errgsip
                    CALL prism_abort_proto (0, 'prism_put_proto', &
                       'STOP - pkworkps not written OK)')
                ENDIF
#endif     
             ENDDO
!
             IF (kinfo .EQ. PRISM_Output) THEN
                 kinfo = PRISM_SentOut
             ELSE
                 kinfo = PRISM_Sent
             ENDIF

             WRITE(nulprt,FMT='(A,I3,A)') & 
                  'Put r28 - ',isend,' fields exported'
          ENDIF
       ENDIF
    ENDIF
!
!     ----------------------------------------------------------------
!
1010 CONTINUE
    CALL FLUSH(nulprt)
    RETURN
  END SUBROUTINE prism_put_proto_r28

end module mod_prism_put_proto

