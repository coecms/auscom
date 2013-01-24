! *******************************************************************
! *******************************************************************
! *******************************************************************
!
!*    *** WRITE_FILER4***   PRISM 1.0
!
!     purpose:
!     --------
!        At last time step, writes fields to binary files or netcdf files.
!
!     interface:
!     ----------
!        out_fld : field to be written to the restart file
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
! ----------------------------------------------------------------
  SUBROUTINE write_filer4(out_fld, cd_port,id_port)
! ----------------------------------------------------------------
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
    IMPLICIT NONE
#ifdef use_netCDF
#include <netcdf.inc>
#endif
! ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p), intent(in) :: id_port
    REAL(kind=ip_single_p), intent(in) :: out_fld(myport(4,id_port))
    CHARACTER(len=8), intent(in) :: cd_port
! ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p) :: il_unit, il_nolocal, il_err
    INTEGER (kind=ip_intwp_p) :: ierror, il_varid, il_ncid, istatus
    LOGICAL :: ll_file
! ----------------------------------------------------------------
    WRITE(nulprt,*)'Entering Write_File '
!
!*    Test if restart file is in NETCDF format or not
!
#ifdef use_netCDF      
    IF (lg_ncdfrst) THEN
!
!* Case NETCDF format
!
       istatus = NF_OPEN(cg_def_rstfile(id_port),NF_WRITE,il_ncid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_put routine '
          STOP
       ENDIF
       istatus = NF_INQ_VARID(il_ncid, cd_port, il_varid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_put routine '
          STOP
       ENDIF
       istatus = NF_PUT_VAR_REAL (il_ncid, il_varid, out_fld)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_put routine '
          STOP
       ENDIF
       istatus = NF_CLOSE(il_ncid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_put routine '
          STOP
       ENDIF
    ELSE
#endif
!
!* Case binary format
!
       IF (.not.allocated(ig_aux)) THEN
          ALLOCATE (ig_aux(ig_nbr_rstfile), stat=il_err)
          IF (il_ERR.ne.0) &
          WRITE(nulprt,*)'Error in "ig_aux" allocation in PRISM_put routine ! '
          ig_aux(:) = 0
       ENDIF
       il_unit = nulprt + 1
       INQUIRE (il_unit,OPENED = ll_file)
       DO WHILE (ll_file)
          il_unit = il_unit + 1 
          INQUIRE (il_unit,OPENED = ll_file)
       END DO
       il_nolocal = ig_def_norstfile(id_port)
       ig_aux(il_nolocal) = ig_aux(il_nolocal) + 1
       IF (ig_aux(il_nolocal).eq.1) THEN
          OPEN (il_unit, FILE=cg_def_rstfile(id_port), &
               FORM='UNFORMATTED')
       ELSE 
          OPEN (il_unit, FILE=cg_def_rstfile(id_port), &
               position='append', FORM='UNFORMATTED') 
       ENDIF
       CALL locwriter4(cd_port,out_fld,myport(4,id_port),il_unit, &
            ierror, nulprt) 
       CLOSE (il_unit)
#ifdef use_netCDF
    ENDIF
#endif
  END SUBROUTINE write_filer4

! ********************************************************************
! ********************************************************************
! ********************************************************************
!
!*    *** WRITE_FILER8***   PRISM 1.0
!
!     purpose:
!     --------
!        At last time step, writes fields to binary files or netcdf files.
!
!     interface:
!     ----------
!        out_fld : field to be written to the restart file
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
  SUBROUTINE write_filer8(out_fld, cd_port, id_port)
!     ---------------------------------------------------------------- 
    USE mod_kinds_model
    USE mod_prism_proto
    USE mod_comprism_proto
    IMPLICIT NONE
#ifdef use_netCDF
#include <netcdf.inc>
#endif
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p), intent(in) :: id_port
    CHARACTER(len=8), intent(in) :: cd_port
    REAL(kind=ip_double_p), intent(in) :: out_fld(myport(4,id_port))
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p) il_unit, il_nolocal, il_err
    INTEGER (kind=ip_intwp_p) ierror, il_varid, il_ncid, istatus
    LOGICAL ll_file
!     ----------------------------------------------------------------
    WRITE(nulprt,*)'Entering Write_File '
!
!*    Test if restart file is in NETCDF format or not
! 
#ifdef use_netCDF    
    IF (lg_ncdfrst) THEN
!
!* Case NETCDF format
!
       istatus = NF_OPEN(cg_def_rstfile(id_port),NF_WRITE,il_ncid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_put routine '
          STOP
       ENDIF
       istatus = NF_INQ_VARID(il_ncid, cd_port, il_varid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_put routine '
          STOP
       ENDIF
       istatus = NF_PUT_VAR_DOUBLE (il_ncid, il_varid, out_fld) 
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_put routine '
          STOP
       ENDIF
       istatus = NF_CLOSE(il_ncid)
       IF (istatus.ne.NF_NOERR) THEN
          WRITE(nulprt,*) NF_STRERROR(istatus)
          WRITE(nulprt,*)' stop in PRISM_put routine '
          STOP
       ENDIF
    ELSE
#endif
!
!* Case binary format
!
       IF (.not.allocated(ig_aux)) THEN
          ALLOCATE (ig_aux(ig_nbr_rstfile), stat=il_err)
          IF (il_ERR.ne.0) &
          WRITE(nulprt,*)'Error in "ig_aux" allocation in PRISM_put routine ! '
          ig_aux(:) = 0
       ENDIF
       il_unit = nulprt + 1
       INQUIRE (il_unit,OPENED = ll_file)
       DO WHILE (ll_file)
          il_unit = il_unit + 1 
          INQUIRE (il_unit,OPENED = ll_file)
       END DO
       il_nolocal = ig_def_norstfile(id_port)
       ig_aux(il_nolocal) = ig_aux(il_nolocal) + 1
       IF (ig_aux(il_nolocal).eq.1) THEN
          OPEN (il_unit, FILE=cg_def_rstfile(id_port), & 
               FORM='UNFORMATTED')
       ELSE 
          OPEN (il_unit, FILE=cg_def_rstfile(id_port), &
               position='append', FORM='UNFORMATTED') 
       ENDIF
       CALL locwriter8(cd_port,out_fld,myport(4,id_port),il_unit, &
            ierror, nulprt) 
       CLOSE (il_unit)
#ifdef use_netCDF
    ENDIF
#endif
  END SUBROUTINE write_filer8
! ********************************************************************
! ********************************************************************
! ********************************************************************
!
!*    *** WRITE_FILE_PARAR4***   PRISM 1.0
!
!     purpose:
!     --------
!        At first time step, write fields to binary files or netcdf files.
!
!     interface:
!     ----------
!        out_fld : field to be read from the restart file
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
  SUBROUTINE write_file_parar4(out_fld, cd_port,id_port)
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
!     ----------------------------------------------------------------
    CHARACTER(len=8), intent(in) :: cd_port
    INTEGER (kind=ip_intwp_p), intent(in) :: id_port
    REAL(kind=ip_single_p), intent(in) :: out_fld(myport(4,id_port))
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p),PARAMETER :: ip_tag=100
    INTEGER (kind=ip_intwp_p) il_unit, il_nolocal, il_len, il_aux
    INTEGER (kind=ip_intwp_p) ierror, il_varid, il_ncid, il_status_ncdf
    INTEGER (kind=ip_intwp_p) il_maxgrd, il_maxbyte, ib, ib_rank, iposbuf, il_off
    INTEGER (kind=ip_intwp_p), DIMENSION(MPI_STATUS_SIZE) :: istatus
    INTEGER (kind=ip_intwp_p), DIMENSION(:,:), ALLOCATABLE :: il_paral_mast
    REAL(kind=ip_single_p), DIMENSION(:), ALLOCATABLE :: rl_end, rl_work
    REAL(kind=ip_single_p), DIMENSION(:), ALLOCATABLE :: rl_work_mast
    LOGICAL ll_file
    INTEGER(kind=ip_intwp_p)::il_sndreq
!     ----------------------------------------------------------------
    WRITE(nulprt,*)'Entering Write_File_Para '
!
!* Each process of local communicator sends his decomposition to master proc
!
    IF (mpi_rank .NE. 0) THEN        
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
!
!* Master only copies to prevent deadlock
!
       il_paral_mast(:,1) = mydist(:,id_port)
       il_aux = myport(4,id_port)
       il_maxgrd = il_maxgrd + il_aux
!
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
!
!* Each process sends hid part of the field to Master process
!      
    ALLOCATE(rl_work(myport(4,id_port)))
    rl_work(:) = 0
    iposbuf = 0
    il_off=1
    il_len=myport(4,id_port)
    call MPI_Pack(out_fld(il_off:il_off+il_len-1), &
         il_len,MPI_REAL,rl_work, &
         myport(4,id_port)*4,iposbuf,ig_local_comm,ierror)
    if(mpi_rank.gt.0)then
    CALL MPI_Send ( rl_work, iposbuf, MPI_PACKED, 0, &
         ip_tag+2, ig_local_comm, ierror )
    else
    CALL MPI_ISend ( rl_work, iposbuf, MPI_PACKED, 0, &
         ip_tag+2, ig_local_comm, il_sndreq, ierror )
    endif
!
!* Master process receives the part of the field from each process and writes
!  the entire field to restart file
!
    IF (mpi_rank.eq.0) THEN
       ALLOCATE(rl_work_mast(il_maxgrd))
       ALLOCATE (rl_end(il_maxgrd))
       rl_work_mast(:)=0
       rl_end(:)=0
       DO ib_rank = 0, kbcplproc(ig_mynummod) - 1
          CALL MPI_Recv ( rl_work_mast, il_maxbyte, MPI_PACKED, ib_rank , &
               ip_tag+2, ig_local_comm, istatus, ierror )
          iposbuf=0
          DO ib=1,il_paral_mast(clim_segments,ib_rank+1)
             il_off=il_paral_mast(clim_segments+2*ib-1,ib_rank+1)+1
             il_len=il_paral_mast(clim_segments+2*ib,ib_rank+1)
             CALL MPI_Unpack(rl_work_mast, il_maxbyte, iposbuf, &
                  rl_end(il_off:il_off+il_len-1),il_len, &
                  MPI_REAL, ig_local_comm, ierror)
          END DO
       END DO
       CALL mpi_wait(il_sndreq,istatus,ierror)
       DEALLOCATE (rl_work_mast)
       DEALLOCATE (il_paral_mast)
!
!* Test if restart file is in NETCDF format or not
!
#ifdef use_netCDF
       IF (lg_ncdfrst) THEN
!
!* Case NETCDF format
!
          il_status_ncdf = NF_OPEN(cg_def_rstfile(id_port), &
               NF_WRITE,il_ncid)
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
          il_status_ncdf = NF_PUT_VAR_REAL (il_ncid, il_varid, &
               rl_end)
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
          IF (.not.allocated(ig_aux)) THEN
             ALLOCATE (ig_aux(ig_nbr_rstfile), stat=ierror)
             IF (ierror.ne.0) &
             WRITE(nulprt,*)'Error in "ig_aux" allocation in PRISM_put routine ! '
             ig_aux(:) = 0
          ENDIF
          il_unit = nulprt + 1
          INQUIRE (il_unit,OPENED = ll_file)
          DO WHILE (ll_file)
             il_unit = il_unit + 1 
             INQUIRE (il_unit,OPENED = ll_file)
          END DO
          il_nolocal = ig_def_norstfile(id_port)
          ig_aux(il_nolocal) = ig_aux(il_nolocal) + 1
          IF (ig_aux(il_nolocal).eq.1) THEN
             OPEN (il_unit, FILE=cg_def_rstfile(id_port), &
                  FORM='UNFORMATTED')
          ELSE 
             OPEN (il_unit, FILE=cg_def_rstfile(id_port), &
                  position='append', FORM='UNFORMATTED') 
          ENDIF
          CALL locwriter4(cd_port,rl_end, il_maxgrd, &
               il_unit, ierror, nulprt) 
          CLOSE (il_unit)
       ENDIF
       DEALLOCATE(rl_end)
#ifdef use_netCDF
    ENDIF
#endif
#endif
  END SUBROUTINE write_file_parar4

! ********************************************************************
! ********************************************************************
! ********************************************************************
!
!*    *** WRITE_FILE_PARAR8***   PRISM 1.0
!
!     purpose:
!     --------
!        At first time step, write fields to binary files or netcdf files.
!
!     interface:
!     ----------
!        out_fld : field to be read from the restart file
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
  SUBROUTINE write_file_parar8(out_fld, cd_port,id_port)
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
!     ----------------------------------------------------------------
    CHARACTER(len=8), intent(in) :: cd_port
    INTEGER (kind=ip_intwp_p), intent(in) :: id_port
    REAL(kind=ip_double_p), intent(in) :: out_fld(myport(4,id_port))
!     ----------------------------------------------------------------
    INTEGER (kind=ip_intwp_p),PARAMETER :: ip_tag=100
    INTEGER (kind=ip_intwp_p) il_unit, il_nolocal, il_len, il_aux
    INTEGER (kind=ip_intwp_p) ierror, il_varid, il_ncid, il_status_ncdf
    INTEGER (kind=ip_intwp_p) il_maxgrd, il_maxbyte, ib, ib_rank, iposbuf, il_off
    INTEGER (kind=ip_intwp_p), DIMENSION(MPI_STATUS_SIZE) :: istatus
    INTEGER (kind=ip_intwp_p), DIMENSION(:,:), ALLOCATABLE :: il_paral_mast
    REAL(kind=ip_double_p), DIMENSION(:), ALLOCATABLE :: rl_end, rl_work
    REAL(kind=ip_double_p), DIMENSION(:), ALLOCATABLE :: rl_work_mast
    LOGICAL ll_file
    integer(kind=ip_intwp_p)::il_sndreq
!     ----------------------------------------------------------------
    WRITE(nulprt,*)'Entering Write_File_Para '
!
!* Each process of local communicator sends his decomposition to master proc
!
    IF (mpi_rank .NE. 0) THEN              
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
!
!* Master only copies to prevent deadlock
!
       il_paral_mast(:,1) = mydist(:,id_port)
       il_aux = myport(4,id_port)
       il_maxgrd = il_maxgrd + il_aux
!
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
!
!* Each process sends hid part of the field to Master process
!      
    ALLOCATE(rl_work(myport(4,id_port)))
    rl_work(:) = 0
    iposbuf = 0
    il_off=1
    il_len=myport(4,id_port)
    call MPI_Pack(out_fld(il_off:il_off+il_len-1), &
         il_len,MPI_DOUBLE_PRECISION,rl_work, &
        myport(4,id_port)*8,iposbuf,ig_local_comm,ierror)
    if(mpi_rank.gt.0)then
    CALL MPI_Send ( rl_work, iposbuf, MPI_PACKED, 0, &
         ip_tag+2, ig_local_comm, ierror )
    else
    CALL MPI_ISend ( rl_work, iposbuf, MPI_PACKED, 0, &
         ip_tag+2, ig_local_comm, il_sndreq, ierror )
    endif
!
!* Master process receives the part of the field from each process and writes
!  the entire field to restart file
!
    IF (mpi_rank.eq.0) THEN
       ALLOCATE(rl_work_mast(il_maxgrd))
       ALLOCATE (rl_end(il_maxgrd))
       rl_work_mast(:)=0
       rl_end(:)=0
       DO ib_rank = 0, kbcplproc(ig_mynummod) - 1
          CALL MPI_Recv ( rl_work_mast, il_maxbyte, MPI_PACKED, ib_rank , &
               ip_tag+2, ig_local_comm, istatus, ierror )
          iposbuf=0
          DO ib=1,il_paral_mast(clim_segments,ib_rank+1)
             il_off=il_paral_mast(clim_segments+2*ib-1,ib_rank+1)+1
             il_len=il_paral_mast(clim_segments+2*ib,ib_rank+1)
             CALL MPI_Unpack(rl_work_mast, il_maxbyte, iposbuf, &
                  rl_end(il_off:il_off+il_len-1),il_len, &
                  MPI_DOUBLE_PRECISION, ig_local_comm, ierror)
          END DO
       END DO
       call mpi_wait(il_sndreq,istatus,ierror)
       DEALLOCATE (rl_work_mast)
       DEALLOCATE (il_paral_mast)
!
!* Test if restart file is in NETCDF format or not
!
#ifdef use_netCDF
       IF (lg_ncdfrst) THEN
!
!* Case NETCDF format
!
          il_status_ncdf = NF_OPEN(cg_def_rstfile(id_port), &
               NF_WRITE,il_ncid)
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
          il_status_ncdf = NF_PUT_VAR_DOUBLE (il_ncid, il_varid, rl_end) 
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
          IF (.not.allocated(ig_aux)) THEN
             ALLOCATE (ig_aux(ig_nbr_rstfile), stat=ierror)
             IF (ierror.ne.0) &
             WRITE(nulprt,*)'Error in "ig_aux" allocation in PRISM_put routine ! '
             ig_aux(:) = 0
          ENDIF
          il_unit = nulprt + 1
          INQUIRE (il_unit,OPENED = ll_file)
          DO WHILE (ll_file)
             il_unit = il_unit + 1 
             INQUIRE (il_unit,OPENED = ll_file)
          END DO
          il_nolocal = ig_def_norstfile(id_port)
          ig_aux(il_nolocal) = ig_aux(il_nolocal) + 1
          IF (ig_aux(il_nolocal).eq.1) THEN
             OPEN (il_unit, FILE=cg_def_rstfile(id_port), &
                  FORM='UNFORMATTED')
          ELSE 
             OPEN (il_unit, FILE=cg_def_rstfile(id_port), &
                  position='append', FORM='UNFORMATTED') 
          ENDIF
          CALL locwriter8(cd_port,rl_end, il_maxgrd, &
               il_unit, ierror, nulprt) 
          CLOSE (il_unit)
       ENDIF
       DEALLOCATE(rl_end)
#ifdef use_netCDF
    ENDIF
#endif
#endif
  END SUBROUTINE write_file_parar8

! ********************************************************************
! ********************************************************************
! ********************************************************************
!
  SUBROUTINE locwriter4 (cdfldn, pfield, kdimax, knulre, kflgre, kout)
    USE mod_kinds_model
    IMPLICIT none
!
!**** *LOCWRITER4*  - Write binary field on unit knulre
!
!     Purpose:
!     -------
!     Write string cdfldn and array pfield on unit knulre
!
!**   Interface:
!     ---------
!       *CALL*  *locwrite (cdfldn, pfield, kdimax, knulre, kflgre, kout)*
!
!     Input:
!     -----
!                cdfldn : character string locator
!                kdimax : dimension of field to be written 
!                knulre : logical unit to be written
!                pfield : field array (real 1D) 
!                kout   : logical unit to write messages
!
!     Output:
!     ------
!                kflgre : error status flag
!
!     Workspace:
!     ---------
!     None
!
!     Externals:
!     ---------
!     None
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
    INTEGER (kind=ip_intwp_p) kdimax, knulre, kflgre, kout
    REAL(kind=ip_single_p) ::  pfield(kdimax)
    CHARACTER*8 cdfldn
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
1001 FORMAT(5X,' Write binary file connected to unit = ',I4)
!
!     2. Find field in file
!        ------------------
!
!* Write string
    WRITE (UNIT = knulre, ERR = 210) cdfldn
!* Write associated field
    WRITE (UNIT = knulre, ERR = 210) pfield
!* Writing done and ok
    kflgre = 0
    GO TO 220
!* Problem in Writing
210 kflgre = 1
220 CONTINUE
!
!
!*    3. End of routine
!        --------------
!
    WRITE (UNIT = kout,FMT = *) 'Locwrite : done'
    CALL FLUSH (kout)
    RETURN
  END SUBROUTINE locwriter4

! ********************************************************************
! ********************************************************************
! ********************************************************************
!
  SUBROUTINE locwriter8 (cdfldn, pfield, kdimax, knulre, kflgre, kout)
    USE mod_kinds_model
    IMPLICIT none
!
!**** *LOCWRITER8*  - Write binary field on unit knulre
!
!     Purpose:
!     -------
!     Write string cdfldn and array pfield on unit knulre
!
!**   Interface:
!     ---------
!       *CALL*  *locwrite (cdfldn, pfield, kdimax, knulre, kflgre, kout)*
!
!     Input:
!     -----
!                cdfldn : character string locator
!                kdimax : dimension of field to be written 
!                knulre : logical unit to be written
!                pfield : field array (real 1D) 
!                kout   : logical unit to write messages
!
!     Output:
!     ------
!                kflgre : error status flag
!
!     Workspace:
!     ---------
!     None
!
!     Externals:
!     ---------
!     None
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
    INTEGER (kind=ip_intwp_p) kdimax, knulre, kflgre, kout
    REAL(kind=ip_double_p) ::  pfield(kdimax)
    CHARACTER*8 cdfldn
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
1001 FORMAT(5X,' Write binary file connected to unit = ',I4)
!
!     2. Find field in file
!        ------------------
!
!* Write string
    WRITE (UNIT = knulre, ERR = 210) cdfldn
!* Write associated field
    WRITE (UNIT = knulre, ERR = 210) pfield
!* Writing done and ok
    kflgre = 0
    GO TO 220
!* Problem in Writing
210 kflgre = 1
220 CONTINUE
!
!
!*    3. End of routine
!        --------------
!
    WRITE (UNIT = kout,FMT = *) 'Locwrite : done'
    CALL FLUSH (kout)
    RETURN
  END SUBROUTINE locwriter8
