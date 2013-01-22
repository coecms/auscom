      SUBROUTINE prism_def_var_proto(id_nports, cdport, id_part, 
     $     id_var_nodims, kinout, id_var_shape, ktype, kinfo)
c     
c     **** Def_var ***   PRISM 1.0
c     
c     purpose:
c     --------
c        define a port
c
c     interface:
c     ----------
c        id_nports : port number of the field
c        cdport : symbolic name of the field
c        id_part : field decomposition 
c        id_var_nodims(1) : rank of array variable
c        id_var_nodims(2) : number of bundles
c        kinout : port status in/out
c        id_var_shape : min and max index for each DIMENSION of the field
c        ktype  : type of data
c	 kinfo	: output status
c
c     lib mp:
c     -------
c        mpi-1
c
c     author:
c     -------
c        Eric Sevault   - METEO FRANCE
c        Laurent Terray - CERFACS
c
c     ----------------------------------------------------------------
      USE mod_kinds_model
      USE mod_prism_proto
      USE mod_comprism_proto
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)       kinout, ktype, kinfo, id_nports, 
     $    id_part
      INTEGER (kind=ip_intwp_p)       id_var_nodims(2), 
     $    id_var_shape(2*id_var_nodims(1))
      CHARACTER*(*) cdport
c     ----------------------------------------------------------------
c
c*    0. Entering
c     -----------
c
      kinfo = CLIM_Ok
c
c*    1. define the port
c     ------------------
c
      WRITE(nulprt,*) 'Def_var - port name, status : ',cdport, kinout
      WRITE(nulprt,*) 'Def_var - data type: ',ktype
c
      IF ( ktype .ne. PRISM_Real ) THEN
          WRITE(nulprt,FMT='(A,I4)')
     $        'Def_var - ERROR - Bad data type:',ktype
          WRITE(nulprt,*)
     $        'Calling MPI_Abort in prism_def_var'
          CALL FLUSH(nulprt)
#if defined use_comm_MPI1 || defined use_comm_MPI2
          CALL MPI_ABORT (mpi_comm, 0, ierror)
#elif defined use_comm_GSIP
          CALL prism_abort_proto (0, 'prism_def_var_proto',
     $        'Def_var - ERROR - Bad data type')
#endif
      ENDIF
c
      IF ( kinout.EQ.CLIM_InOut ) THEN
         CALL prism_defport (id_nports, cdport, CLIM_In, ktype, id_part, 
     $        kinfo )
         IF (kinfo.EQ.CLIM_Ok) THEN
            CALL prism_defport (id_nports, cdport, CLIM_Out, ktype, 
     $           id_part, kinfo )
         ENDIF
      ELSE
         CALL prism_defport ( id_nports,cdport, kinout, ktype, id_part, 
     $        kinfo )
      ENDIF
c
c     ----------------------------------------------------------------
c
      CALL FLUSH(nulprt)
      RETURN
      END 
c
c ====================================================================

      SUBROUTINE prism_defport(id_nports,cdport,kinout,ktype,id_part,
     $     kinfo)
c
c*    *** Define ***   CLIM 2.2
c
c     purpose:
c     --------
c        define a port
c
c     interface:
c     ----------
c        id_nports : port number of the field
c        cdport : symbolic name of the field
c        kinout : port status in/out
c        ktype  : type of data
c        kparal : type of parallel decomposition
c        kinfo  : output status
c
c     lib mp:
c     -------
c        mpi-2
c
c     author:
c     -------
c        Eric Sevault   - METEO FRANCE
c        Laurent Terray - CERFACS
c        Jean Latour    - F.S.E.   (mpi-2)
c
c     ----------------------------------------------------------------
      USE mod_kinds_model
      USE mod_prism_proto
      USE mod_comprism_proto
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)       kinout, ktype, kinfo
      CHARACTER*(*) cdport
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)    ip, id_nports, id_part, ierror
      REAL(kind=ip_realwp_p)          rl_testvar
      REAL(kind=ip_double_p)   dl_testvar
      CHARACTER*32  cltest
      INTEGER (kind=ip_intwp_p) :: ii, il_bytesize, il_iosize
      INTEGER (kind=ip_intwp_p) :: il_int_iosize, il_rbyt
c     ----------------------------------------------------------------
c
      rl_testvar = 0.0_ip_realwp_p
      dl_testvar = 0.0_ip_double_p
c
c*    1. check if this port already exist
c     -----------------------------------
c
      cltest= cdport
c
      DO ip=1,nports
         IF (cltest.EQ.cports(ip).AND.kinout.EQ.myport(1,ip)) THEN
            kinfo = PRISM_DoubleDef
            WRITE(nulprt,FMT='(A,A)')
     $           'Def_var - WARNING - duplicate definition of port ',
     $           cdport
            GO TO 1010
         ENDIF
      ENDDO
c     
c*    2. save arguments as half a link
c     --------------------------------
c
      nports = nports + 1
      id_nports = nports
      DO il=1, ig_clim_nfield
        IF (cltest.EQ.cg_cnaminp(il).or.cltest.eq.cg_cnamout(il)) THEN
            IF (cltest .EQ. cg_cnaminp(il)) then
                cga_clim_locator(nports) = cga_clim_locatorbf(il)
            ELSE IF (cltest .EQ. cg_cnamout(il)) then
                cga_clim_locator(nports) = cga_clim_locatoraf(il)
            ENDIF
            IF (ig_clim_state(il) .eq. ip_exported .or. 
     $           ig_clim_state(il) .eq. ip_expout .or. 
     $           ig_clim_state(il) .eq. ip_auxilary) THEN 
               cports(nports) = cltest
               ig_def_lag(nports)=ig_clim_lag(il)
c
               IF (cltest.EQ.cg_cnamout(il)) THEN 
                   ig_def_reverse(nports)=ig_clim_reverse(il)
               ELSEIF  (cltest.EQ.cg_cnaminp(il)) THEN
                   ig_def_invert(nports)=ig_clim_invert(il)
               ENDIF
c
               ig_def_freq(nports)=ig_clim_freq(il)
               ig_def_seq(nports)=ig_clim_seq(il)
               cg_def_rstfile(nports)=cg_clim_rstfile(il)
               ig_def_norstfile(nports)=ig_clim_norstfile(il)
               ig_def_state(nports)=ig_clim_state(il)
               ig_def_trans(nports)=ig_clim_trans(il)
               cg_def_inpfile(nports)=cg_clim_inpfile(il)
               ig_def_numlab(nports)=ig_clim_numlab(il)
c
            ELSE
               IF (ig_clim_state(il) .eq. ip_ignout) 
     $              cg_ignout_field(nports) = cltest
               cports(nports) = cg_cnamout(il)
               ig_def_lag(nports)=ig_clim_lag(il)
c
               IF (cltest.EQ.cg_cnamout(il)) THEN 
                   ig_def_reverse(nports)=ig_clim_reverse(il)
               ELSEIF  (cltest.EQ.cg_cnaminp(il)) THEN
                   ig_def_invert(nports)=ig_clim_invert(il)
               ENDIF
c
               ig_def_freq(nports)=ig_clim_freq(il)
               ig_def_seq(nports)=ig_clim_seq(il)
               cg_def_rstfile(nports)=cg_clim_rstfile(il)
               ig_def_norstfile(nports)=ig_clim_norstfile(il)
               ig_def_state(nports)=ig_clim_state(il)
               ig_def_trans(nports)=ig_clim_trans(il)
               cg_def_inpfile(nports)=cg_clim_inpfile(il)
               ig_def_numlab(nports)=ig_clim_numlab(il)
c
            ENDIF
         ENDIF
      ENDDO
c     
      IF (ig_def_freq(nports) .eq. 0) THEN
         WRITE(nulprt,FMT=*) 
     $      'Def_var - WARNING '
         WRITE(nulprt,FMT=*) 
     $      'You have a field which is not defined in the namcouple '
         WRITE(nulprt,FMT=*) 
     $        'This field will not be exchanged !'
         GOTO 1010
      ELSEIF (ig_def_freq(nports) .gt. ig_ntime) THEN
         WRITE(nulprt,FMT=*) 
     $        'Def_var - WARNING '
         WRITE(nulprt,FMT=*) 
     $        'You have a field defined with a coupling period greater'
         WRITE(nulprt,FMT=*) 
     $        'than the time of the simulation. So, this file will not'
         WRITE(nulprt,FMT=*) 
     $        'be exchanged !'
         GOTO 1010 
      ELSE
         myport(1,nports) = kinout
         myport(5,nports) = 0
c     
         IF ( ktype .EQ. PRISM_Real ) THEN
            myport(2,nports) = PRISM_Real
            il_bytesize = BIT_SIZE(ii)/8
            INQUIRE (iolength=il_iosize) ii
            il_int_iosize = il_iosize
            INQUIRE (iolength=il_iosize) rl_testvar
            il_rbyt = il_iosize/il_int_iosize*il_bytesize            
            myport(3,nports) = il_rbyt
         ELSE
            kinfo = CLIM_BadType
            WRITE(nulprt,FMT='(A,I4)')
     $           'Def_var - WARNING - Bad data type:',ktype
         ENDIF
c     
         mydist (:,nports) = ig_def_part (:,id_part)
         myport (4,nports) = ig_length_part (id_part)
      ENDIF
c
c
c     ----------------------------------------------------------------
c
 1010 CONTINUE
      CALL FLUSH(nulprt)
      RETURN
      END 
