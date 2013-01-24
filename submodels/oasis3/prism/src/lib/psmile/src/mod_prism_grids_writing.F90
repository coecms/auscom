!-----------------------------------------------------------------------
! BOP
!
! !MODULE:  mod_prism_grids_writing
! !REMARKS: 
! !REVISION HISTORY:
! 2003.07.07 Veronika Gayler initial version
!
! !PUBLIC MEMBER FUNCTIONS:
! 
!      subroutine prism_start_grids_writing(iwrite)
!             This subroutine initializes grids writing by receiving a 
!             starting command from OASIS.
!
!      subroutine prism_write_grid(cgrid, nx, ny, lon, lat)
!	      This subroutine writes longitudes and latitudes for a model
!             grid.
!
!      subroutine prism_write_corner(cgrid, nx, ny, nc, clon, clat)
!	      This subroutine writes the longitudes and latitudes of the
!             grid cell corners.
!
!      subroutine prism_write_mask(cgrid, nx, ny, mask)
!	      This subroutine writes the mask for a model grid
!
!      subroutine prism_write_area(cgrid, nx, ny, area)
!	      This subroutine writes the grid cell areas for a model grid.
!
!      subroutine prism_terminate_grids_writing()
!             This subroutine terminates grids writing by sending a flag
!             to OASIS, stating the all needed grid information was written.
!       

MODULE mod_prism_grids_writing

! !USES:
  USE mod_kinds_model
  USE mod_comprism_proto
  
  IMPLICIT NONE
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
#endif  
  INTEGER(kind=ip_intwp_p), PARAMETER :: itagcol=9876
  INTEGER(kind=ip_intwp_p)  :: tag          ! MPI message tag
  INTEGER(kind=ip_intwp_p)  :: len          ! MPI message length
  INTEGER(kind=ip_intwp_p)  :: type         ! MPI message type
  LOGICAL        :: gridswrite      ! grid writing is needed
  LOGICAL        :: netcdf          ! grids file format is netCDF
  CHARACTER*5    :: cgrdnam         ! grids file name
  CHARACTER*5    :: cmsknam         ! masks file name
  CHARACTER*5    :: csurnam         ! areas file name
  CHARACTER*4    :: cglonsuf        ! suffix for longitudes
  CHARACTER*4    :: cglatsuf        ! suffix for latitudes
  CHARACTER*4    :: crnlonsuf       ! suffix for longitudes
  CHARACTER*4    :: crnlatsuf       ! suffix for latitudes
  CHARACTER*4    :: cmsksuf         ! suffix for masks
  CHARACTER*4    :: csursuf         ! suffix for areas
!---------------------------------------------------------------------------

CONTAINS

!--------------------------------------------------------------------------
!
  SUBROUTINE prism_start_grids_writing(iwrite)
!--------------------------------------------------------------------------
! Routine to start the grids writing. To syncronize access to the
! grids file all component models have to wait for the starting 
! message from OASIS (via MPI; see prism_init_comp_proto)
!--------------------------------------------------------------------------
#if defined use_comm_GSIP 
      USE mod_gsip_model
#endif
    IMPLICIT NONE
  
    INTEGER(kind=ip_intwp_p), INTENT (OUT) :: iwrite ! flag to state whether
                                            ! grids file needs to be written
    INTEGER(kind=ip_intwp_p) :: source   ! rank of the sending process
#if defined use_comm_MPI1 || defined use_comm_MPI2
    INTEGER(kind=ip_intwp_p), DIMENSION(MPI_STATUS_SIZE) :: status
#elif defined use_comm_GSIP
    INTEGER :: mgi_write, mgi_read
    INTEGER :: il_errgsip
    CHARACTER*8, DIMENSION(9) :: cla_work
#endif
!--------------------------------------------------------------------------

#ifdef __VERBOSE
    write(nulprt,*) ' '
    write(nulprt,*) 'Start - - prism_start_grids_writing'
#endif
!
!-- Initialisation
!
#ifdef use_netCDF
    netcdf = .true.
#else
    netcdf = .false.
#endif
    gridswrite = .false.
    iwrite = 0
    cgrdnam = '-----'
    cmsknam = '-----'
    csurnam = '-----'
    cglonsuf = '----'
    cglatsuf = '----'
    crnlonsuf = '----'
    crnlatsuf = '----'
    cmsksuf = '----'
    csursuf = '----'
    source = 0            ! OASIS: process 0 of global communicator

    IF (grids_start == 1) THEN
       gridswrite = .true.
!
!--    receive grid file name
!
#if defined use_comm_MPI1 || defined use_comm_MPI2
       WRITE (nulprt,FMT='(A)') 'Recv - cgrdnam'
       len = 5
       type = MPI_CHARACTER
       tag = itagcol+4
       CALL MPI_Recv (cgrdnam, len, type, source, tag, mpi_comm, status,mpi_err)
       IF (mpi_err == MPI_SUCCESS) THEN
          WRITE(nulprt,FMT='(A,I2,A,I6,A,I2,A,I3,A,I5,A,A5)') &
               'Recv - <from:',source,'> <comm:',mpi_comm,'> <len:',len,  &
               '> <type:',type,'> <tag:',tag,'> ::  ',cgrdnam
       ELSE
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'start_grids_writing: error receiving cgrdnam'
          WRITE (nulprt,*) 'start_grids_writing: err= ', mpi_err 
          WRITE (nulprt,*) 'start_grids_writing: STOP'
          STOP
       ENDIF
!
!--    receive mask file name
!
       WRITE (nulprt,FMT='(A)') 'Recv - cmsknam'
       len = 5
       type = MPI_CHARACTER
       tag = itagcol+5
       CALL MPI_Recv (cmsknam, len, type, source, tag, mpi_comm, status,mpi_err)
       IF (mpi_err == MPI_SUCCESS) THEN 
          WRITE(nulprt,FMT='(A,I2,A,I6,A,I2,A,I3,A,I5,A,A5)') &
               'Recv - <from:',source,'> <comm:',mpi_comm,'> <len:',len,  &
               '> <type:',type,'> <tag:',tag,'> ::  ',cmsknam
       ELSE
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'start_grids_writing: error receiving cmsknam'
          WRITE (nulprt,*) 'start_grids_writing: err= ', mpi_err 
          WRITE (nulprt,*) 'start_grids_writing: STOP'
          STOP
       ENDIF
!
!--    receive areas file name
!
       WRITE (nulprt,FMT='(A)') 'Recv - csurnam'
       len = 5
       type = MPI_CHARACTER
       tag = itagcol+6
       CALL MPI_Recv (csurnam, len, type, source, tag, mpi_comm, status,mpi_err)
       IF (mpi_err == MPI_SUCCESS) THEN 
          WRITE(nulprt,FMT='(A,I2,A,I6,A,I2,A,I3,A,I5,A,A5)') &
               'Recv - <from:',source,'> <comm:',mpi_comm,'> <len:',len, &
               '> <type:',type,'> <tag:',tag,'> ::  ',csurnam
       ELSE
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'start_grids_writing: error receiving csurnam'
          WRITE (nulprt,*) 'start_grids_writing: err= ', mpi_err 
          WRITE (nulprt,*) 'start_grids_writing: STOP'
          STOP
       ENDIF
!
!--    receive suffix for longitudes
!
       WRITE (nulprt,FMT='(A)') 'Recv - cglonsuf'
       len = 4
       type = MPI_CHARACTER
       tag = itagcol+7
       CALL MPI_Recv (cglonsuf, len, type, source, tag, mpi_comm,status,mpi_err)
       IF (mpi_err == MPI_SUCCESS) THEN 
          WRITE(nulprt,FMT='(A,I2,A,I6,A,I2,A,I3,A,I5,A,A4)') &
               'Recv - <from:',source,'> <comm:',mpi_comm,'> <len:',len, &
               '> <type:',type,'> <tag:',tag,'> ::  ',cglonsuf
       ELSE
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'start_grids_writing: error receiving cglonsuf'
          WRITE (nulprt,*) 'start_grids_writing: err= ', mpi_err 
          WRITE (nulprt,*) 'start_grids_writing: STOP'
          STOP
       ENDIF
!
!--    receive suffix for latitudes
!
       WRITE (nulprt,FMT='(A)') 'Recv - cglatsuf'
       len = 4
       type = MPI_CHARACTER
       tag = itagcol+8
       CALL MPI_Recv (cglatsuf, len, type, source, tag, mpi_comm,status,mpi_err)
       IF (mpi_err == MPI_SUCCESS) THEN 
          WRITE(nulprt,FMT='(A,I2,A,I6,A,I2,A,I3,A,I5,A,A4)') &
               'Recv - <from:',source,'> <comm:',mpi_comm,'> <len:',len, &
               '> <type:',type,'> <tag:',tag,'> ::  ',cglatsuf
       ELSE
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'start_grids_writing: error receiving cglatsuf'
          WRITE (nulprt,*) 'start_grids_writing: err= ', mpi_err 
          WRITE (nulprt,*) 'start_grids_writing: STOP'
          STOP
       ENDIF
!
!--    receive suffix for longitudes of grid cell corners
!
       WRITE (nulprt,FMT='(A)') 'Recv - crnlonsuf'
       len = 4
       type = MPI_CHARACTER
       tag = itagcol+9
       CALL MPI_Recv (crnlonsuf, len, type, source, tag, mpi_comm,status,mpi_err)
       IF (mpi_err == MPI_SUCCESS) THEN 
          WRITE(nulprt,FMT='(A,I2,A,I6,A,I2,A,I3,A,I5,A,A4)') &
               'Recv - <from:',source,'> <comm:',mpi_comm,'> <len:',len, &
               '> <type:',type,'> <tag:',tag,'> ::  ',crnlonsuf
       ELSE
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'start_grids_writing: error receiving crnlonsuf'
          WRITE (nulprt,*) 'start_grids_writing: err= ', mpi_err
          WRITE (nulprt,*) 'start_grids_writing: STOP'
          STOP
       ENDIF
!
!--    receive suffix for latitudes
!
       WRITE (nulprt,FMT='(A)') 'Recv - crnlatsuf'
       len = 4
       type = MPI_CHARACTER
       tag = itagcol+10
       CALL MPI_Recv (crnlatsuf, len, type, source, tag, mpi_comm,status,mpi_err)
       IF (mpi_err == MPI_SUCCESS) THEN 
          WRITE(nulprt,FMT='(A,I2,A,I6,A,I2,A,I3,A,I5,A,A4)') &
               'Recv - <from:',source,'> <comm:',mpi_comm,'> <len:',len, &
               '> <type:',type,'> <tag:',tag,'> ::  ',crnlatsuf
       ELSE
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'start_grids_writing: error receiving crnlatsuf'
          WRITE (nulprt,*) 'start_grids_writing: err= ', mpi_err 
          WRITE (nulprt,*) 'start_grids_writing: STOP'
          STOP
       ENDIF
!
!--    receive suffix for masks
!
       WRITE (nulprt,FMT='(A)') 'Recv - cmsksuf'
       len = 4
       type = MPI_CHARACTER
       tag = itagcol+11
       CALL MPI_Recv (cmsksuf, len, type, source, tag, mpi_comm, status,mpi_err)
       IF (mpi_err == MPI_SUCCESS) THEN 
          WRITE(nulprt,FMT='(A,I2,A,I6,A,I2,A,I3,A,I5,A,A4)') &
               'Recv - <from:',source,'> <comm:',mpi_comm,'> <len:',len, &
               '> <type:',type,'> <tag:',tag,'> ::  ',cmsksuf
       ELSE
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'start_grids_writing: error receiving cmsksuf'
          WRITE (nulprt,*) 'start_grids_writing: err= ', mpi_err 
          WRITE (nulprt,*) 'start_grids_writing: STOP'
          STOP
       ENDIF
!
!--    receive suffix for areas
!
       WRITE (nulprt,FMT='(A)') 'Recv - csursuf'
       len = 4
       type = MPI_CHARACTER
       tag = itagcol+12
       CALL MPI_Recv (csursuf, len, type, source, tag, mpi_comm, status,mpi_err)
       IF (mpi_err == MPI_SUCCESS) THEN 
          WRITE(nulprt,FMT='(A,I2,A,I6,A,I2,A,I3,A,I5,A,A4)') &
               'Recv - <from:',source,'> <comm:',mpi_comm,'> <len:',len, &
               '> <type:',type,'> <tag:',tag,'> ::  ',csursuf
       ELSE
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'start_grids_writing: error receiving csursuf'
          WRITE (nulprt,*) 'start_grids_writing: err= ', mpi_err 
          WRITE (nulprt,*) 'start_grids_writing: STOP'
          STOP
       ENDIF
#ifdef __VERBOSE
       WRITE (nulprt,*) ' '
       WRITE (nulprt,*) '  grids file name:              ', cgrdnam
       WRITE (nulprt,*) '  masks file name:              ', cmsknam
       WRITE (nulprt,*) '  areas file name:              ', csurnam
       WRITE (nulprt,*) '  suffix for longitudes:        ', cglonsuf
       WRITE (nulprt,*) '  suffix for latitudes:         ', cglatsuf
       WRITE (nulprt,*) '  suffix for corner longitudes: ', crnlonsuf
       WRITE (nulprt,*) '  suffix for corner latitudes:  ', crnlatsuf
       WRITE (nulprt,*) '  suffix for masks:             ', cmsksuf
       WRITE (nulprt,*) '  suffix for areas:             ', csursuf
#endif
#endif

   ELSE IF (grids_start == 0) THEN
!
!--     grids file already exists, no writing needed
!
#ifdef __VERBOSE
       WRITE (nulprt,*) '  grids file exists, no writing needed'
#endif
       gridswrite = .false.
    ELSE
       WRITE (nulprt,*) ' '
       WRITE (nulprt,*) 'start_grids_writing: no valid flag received'
       WRITE (nulprt,*) 'start_grids_writing: grids_start= ', grids_start 
       WRITE (nulprt,*) 'start_grids_writing: STOP'
       STOP         
    ENDIF

    IF (gridswrite) THEN
       iwrite = 1
    ENDIF

#ifdef __VERBOSE
    write(nulprt,*) 'End - - prism_start_grids_writing'
    write(nulprt,*) ''
    CALL FLUSH(nulprt)
#endif
    RETURN

  END SUBROUTINE prism_start_grids_writing

!--------------------------------------------------------------------------
!
  SUBROUTINE prism_write_grid(cgrid, nx, ny, lon, lat)
!--------------------------------------------------------------------------
! Routine to create a new grids file or to add a grid description to an
! existing grids file.
!--------------------------------------------------------------------------
#if defined use_comm_GSIP 
    USE mod_gsip_model
#endif
    IMPLICIT NONE
#ifndef use_comm_GSIP
#ifdef use_netCDF
#include <netcdf.inc>
#endif
#else
    INTEGER :: mgi_write, il_errgsip
#endif
    CHARACTER*4,              INTENT (IN) :: cgrid      ! grid acronym
    INTEGER(kind=ip_intwp_p), INTENT (IN) :: nx         ! number of longitudes
    INTEGER(kind=ip_intwp_p), INTENT (IN) :: ny         ! number of latitudes
    REAL(kind=ip_realwp_p),   INTENT (IN) :: lon(nx,ny) ! longitudes
    REAL(kind=ip_realwp_p),   INTENT (IN) :: lat(nx,ny) ! latitudes

    INTEGER(kind=ip_intwp_p) :: iost         ! i/o status
    INTEGER(kind=ip_intwp_p) :: nulgrd       ! logical unit of grids file
    INTEGER(kind=ip_intwp_p) :: stat         ! netcdf status
    INTEGER(kind=ip_intwp_p) :: ncid         ! netcdf file id
    INTEGER(kind=ip_intwp_p) :: idx, idy     ! netcdf dimension ids
    INTEGER(kind=ip_intwp_p) :: idlon, idlat ! netcdf variable ids
    INTEGER(kind=ip_intwp_p) :: dims(2)      ! netcdf variable dimensions
    INTEGER(kind=ip_intwp_p) :: il_nftype    ! netcdf type

    LOGICAL :: existent    !true if grids file is already existing
    LOGICAL :: grdopen     !true if grids file is opened

    CHARACTER*8 :: clon    !locator sring (binary) / variable name (netcdf)
    CHARACTER*8 :: clat    !locator sring (binary) / variable name (netcdf)

!--------------------------------------------------------------------------

#ifdef __VERBOSE
    write(nulprt,*) ' '
    write(nulprt,*) 'Start - - prism_write_grid'
    write(nulprt,*) '  grid acronym: ', cgrid
#endif
!
!-- Return, if grids files already exists
!
    IF (.NOT. gridswrite) THEN
       write(nulprt,*) ' '
       write(nulprt,*) 'No grid writing needed'
       RETURN
    ENDIF
!
#if defined use_comm_GSIP 
!
!-- Send flag to coupler to tell it that grids will be send
       il_errgsip = mgi_write (ig_gsipw, 100, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, cgrid, 4, 'C')
       il_errgsip = mgi_write (ig_gsipw, nx, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, ny, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, lon, nx*ny, 'R')
       il_errgsip = mgi_write (ig_gsipw, lat, nx*ny, 'R')
!
#else
#ifdef use_netCDF
    IF (netcdf) THEN
!      -------------
!--    netCDF format
!      -------------
!
      il_nftype = NF_DOUBLE
      IF (ll_single) il_nftype = NF_REAL
!
!      open grids file
!      ---------------
       stat = NF_OPEN(cgrdnam//'.nc', NF_WRITE, ncid)
       IF (stat /= NF_NOERR) THEN
           stat = NF_CREATE(cgrdnam//'.nc', NF_CLOBBER, ncid)
          IF (stat /= NF_NOERR) THEN
             WRITE (nulprt,*) 'prism_write_grid: error opening grids file.'
             WRITE (nulprt,*) 'prism_write_grid: STOP'
             STOP
          ENDIF
       ENDIF
!
!--   define dimensions
!     -----------------
      stat = NF_REDEF(ncid)
      stat = NF_INQ_DIMID(ncid, 'x_'//cgrid, idx)
      IF (stat .NE. NF_NOERR) THEN
          stat = NF_DEF_DIM(ncid, 'x_'//cgrid, nx, idx)
          IF (stat /= NF_NOERR) THEN
              WRITE (nulprt,*) 'ERROR defining dimension x_',cgrid
          ENDIF
      ELSE
          WRITE(nulprt,*)'Grid already defined: ', cgrid
          RETURN
      ENDIF
      stat = NF_DEF_DIM(ncid, 'y_'//cgrid, ny, idy)
      IF (stat /= NF_NOERR) THEN
          WRITE (nulprt,*) 'ERROR defining dimension y_',cgrid
      ENDIF

      dims(1) = idx
      dims(2) = idy
! 
!--   define longitudes
!     -----------------
      clon=cgrid//cglonsuf
      stat = NF_DEF_VAR (ncid, clon , il_nftype, 2, dims, idlon)
      IF (stat /= NF_NOERR) THEN
          WRITE (nulprt,*) 'ERROR defining variable ', clon
      ENDIF
      stat = NF_PUT_ATT_TEXT(ncid,idlon,'long_name',18,'Longitudes of '//cgrid)
      IF (stat /= NF_NOERR) THEN
          WRITE (nulprt,*) 'ERROR creating att. for longitudes of ', cgrid
      ENDIF
      stat = NF_PUT_ATT_TEXT(ncid, idlon, 'units', 8, 'degree_E')
      IF (stat /= NF_NOERR) THEN
          WRITE (nulprt,*) 'ERROR creating att. for longitudes of ', cgrid
      ENDIF
!
!--   define latitudes
!     ---------------- 
      clat=cgrid//cglatsuf
      stat = NF_DEF_VAR (ncid, clat , il_nftype, 2, dims, idlat)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR defining variable ', clat
      ENDIF
      stat = NF_PUT_ATT_TEXT(ncid, idlat,'long_name',17,'Latitudes of '//cgrid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR creating att. for latitudes of ', cgrid
      ENDIF
      stat = NF_PUT_ATT_TEXT(ncid, idlat, 'units', 8, 'degree_N')
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR creating att. for latitudes of ', cgrid
      ENDIF
!
!--   switch to data mode
!     -------------------
      stat = NF_ENDDEF(ncid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR: file ', ncid, ' still in define mode'
      ENDIF
!
!--   write longitudes
!     ----------------
      IF (ll_single) THEN
          stat = NF_PUT_VAR_REAL (ncid, idlon, lon(:,:))
      ELSE
          stat = NF_PUT_VAR_DOUBLE (ncid, idlon, lon(:,:))
      ENDIF
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR writing lonitudes of ', cgrid
      ENDIF
!
!--   write latitudes
!     ---------------
      IF (ll_single) THEN
          stat = NF_PUT_VAR_REAL (ncid, idlat, lat(:,:))
      ELSE
          stat = NF_PUT_VAR_DOUBLE (ncid, idlat, lat(:,:))
      ENDIF
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR writing latitudes of ', cgrid
      ENDIF
!
!--   close grids file
!     ----------------
      stat = NF_CLOSE(ncid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR closing file', cgrdnam
      ENDIF
      
    ELSE
#endif
!      -------------
!--    binary format
!      -------------
!
!      open grids file
!      ---------------
       INQUIRE (FILE = cgrdnam, EXIST = existent, OPENED = grdopen)
       IF (existent .AND. grdopen) THEN
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'prism_write_grid: grids file already opened'
          WRITE (nulprt,*) 'prism_write_grid: STOP'
          STOP     
       ENDIF
       iost = 0
       nulgrd = 7
       INQUIRE (nulgrd,OPENED = grdopen)
       DO WHILE (grdopen)
          nulgrd = nulgrd + 1 
          INQUIRE (nulgrd,OPENED = grdopen)
       END DO

       OPEN (UNIT=nulgrd, FILE=cgrdnam, STATUS='UNKNOWN',                 &
             ACCESS='SEQUENTIAL', FORM='UNFORMATTED', POSITION='APPEND',  &
             ACTION='WRITE', IOSTAT=iost, ERR=110)

110    CONTINUE
       IF (iost /= 0) THEN
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'prism_write_grid: Error opening grids file'
          WRITE (nulprt,*) 'prism_write_grid: STOP'
          STOP
       ENDIF

!      write longitudes
!      ----------------
       clon=cgrid//cglonsuf
       WRITE (UNIT = nulgrd) clon
       WRITE (UNIT = nulgrd) lon(:,:)
                           
!      write latitudes     
!      ----------------    
       clat=cgrid//cglatsuf
       WRITE (UNIT = nulgrd) clat
       WRITE (UNIT = nulgrd) lat(:,:)

!      close grids file
!      ----------------
       CLOSE (nulgrd)
#ifdef use_netCDF
    ENDIF
#endif
#endif
#ifdef __VERBOSE
    write(nulprt,*) 'End - - prism_write_grid'
    CALL FLUSH(nulprt)
#endif

    RETURN

  END SUBROUTINE prism_write_grid
!--------------------------------------------------------------------------
!
  SUBROUTINE prism_write_corner(cgrid, nx, ny, nc, clon, clat)
!--------------------------------------------------------------------------
! Routine to add longitudes and latitudes of grid cell corners to an
! existing grids file.
!--------------------------------------------------------------------------
#if defined use_comm_GSIP 
    USE mod_gsip_model
#endif
    IMPLICIT NONE
#ifndef use_comm_GSIP
#ifdef use_netCDF
#include <netcdf.inc>
#endif
#else
    INTEGER :: mgi_write, il_errgsip
#endif
    CHARACTER*4,              INTENT (IN) :: cgrid  ! grid acronym
    INTEGER(kind=ip_intwp_p), INTENT (IN) :: nx     ! number of longitudes
    INTEGER(kind=ip_intwp_p), INTENT (IN) :: ny     ! number of latitudes
    INTEGER(kind=ip_intwp_p), INTENT (IN) :: nc     ! number of corners per cell
    REAL(kind=ip_realwp_p),   INTENT (IN) :: clon(nx,ny,nc) ! longitudes
    REAL(kind=ip_realwp_p),   INTENT (IN) :: clat(nx,ny,nc) ! latitudes

    INTEGER(kind=ip_intwp_p) :: stat           ! netcdf status
    INTEGER(kind=ip_intwp_p) :: ncid           ! netcdf file id
    INTEGER(kind=ip_intwp_p) :: idx, idy, idc  ! netcdf dimension ids
    INTEGER(kind=ip_intwp_p) :: idclon, idclat ! netcdf variable ids
    INTEGER(kind=ip_intwp_p) :: dims(3)        ! netcdf variable dimensions

    INTEGER(kind=ip_intwp_p) :: il_nftype      ! netcdf type

    CHARACTER*8 :: crnlon    !locator sring (binary) / variable name (netcdf)
    CHARACTER*8 :: crnlat    !locator sring (binary) / variable name (netcdf)
    
!--------------------------------------------------------------------------

#ifdef __VERBOSE
    write(nulprt,*) ' '
    write(nulprt,*) 'Start - - prism_write_corner'
    write(nulprt,*) '  grid acronym: ', cgrid
    CALL FLUSH(nulprt)
#endif

!
!-- Return, if grids files was written in a former run
!
    IF (.NOT. gridswrite) THEN
       write(nulprt,*) ' '
       write(nulprt,*) 'No grid writing needed'
       RETURN
    ENDIF
!
#if defined use_comm_GSIP 
!
!-- Send flag to coupler to tell it that grids will be send
       il_errgsip = mgi_write (ig_gsipw, 200, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, cgrid, 4, 'C')
       il_errgsip = mgi_write (ig_gsipw, nx, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, ny, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, nc, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, clon, nx*ny*nc, 'R')
       il_errgsip = mgi_write (ig_gsipw, clat, nx*ny*nc, 'R')
       ig_noc = nc
!
#else
#ifdef use_netCDF
    IF (netcdf) THEN
!      -------------
!--    netCDF format
!      -------------
!
      il_nftype = NF_DOUBLE
      IF (ll_single) il_nftype = NF_REAL
!
!      open grids file
!      ---------------
       stat = NF_OPEN(cgrdnam//'.nc', NF_WRITE, ncid)
       IF (stat /= NF_NOERR) THEN
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'prism_write_corner: ERROR'
          WRITE (nulprt,*) 'prism_write_corner:   grids.nc file does not exist!'
          WRITE (nulprt,*) 'prism_write_corner:   call prism_write_grid first!'
          WRITE (nulprt,*) 'prism_write_corner:   STOP'
          STOP
       ENDIF
!
!--   define corner dimensions
!     ------------------------
      stat = NF_REDEF(ncid)
      stat = NF_INQ_DIMID(ncid, 'x_'//cgrid, idx)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR finding out dimension id of x_',cgrid
      ENDIF
      stat = NF_INQ_DIMID(ncid, 'y_'//cgrid, idy)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR finding out dimension id of y_',cgrid
      ENDIF
      stat = NF_DEF_DIM(ncid, 'crn_'//cgrid, nc, idc)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR defining dimension crn_',cgrid
      ENDIF

      dims(1) = idx
      dims(2) = idy
      dims(3) = idc
      ig_noc = nc
! 
!--   define corner longitudes
!     ------------------------
      crnlon=cgrid//crnlonsuf
      stat = NF_INQ_VARID (ncid, crnlon, idclon)
      IF (stat .NE. NF_NOERR) THEN
          stat = NF_DEF_VAR (ncid, crnlon , il_nftype, 3, dims, idclon)
          IF (stat /= NF_NOERR) THEN
              WRITE (nulprt,*) 'ERROR defining variable ', crnlon
          ENDIF
          stat = NF_PUT_ATT_TEXT   &
             (ncid,idclon,'long_name',39,'Longitudes of grid cell corners of '//cgrid)
          IF (stat /= NF_NOERR) THEN
              WRITE (nulprt,*) 'ERROR creating att. for corner longitudes of ', cgrid
          ENDIF
          stat = NF_PUT_ATT_TEXT(ncid, idclon, 'units', 8, 'degree_E')
          IF (stat /= NF_NOERR) THEN
              WRITE (nulprt,*) 'ERROR creating att. for corner longitudes of ', cgrid
          ENDIF
      ELSE
          WRITE(nulprt,*)'Corners already defined: ', cgrid
          RETURN
      ENDIF
!
!--   define corner latitudes
!     ----------------------- 
      crnlat=cgrid//crnlatsuf
      stat = NF_DEF_VAR (ncid, crnlat , il_nftype, 3, dims, idclat)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR defining variable ', crnlat
      ENDIF
      stat = NF_PUT_ATT_TEXT   &
         (ncid, idclat,'long_name',38,'Latitudes of grid cell corners of '//cgrid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR creating att. for corner latitudes of ', cgrid
      ENDIF
      stat = NF_PUT_ATT_TEXT(ncid, idclat, 'units', 8, 'degree_N')
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR creating att. for corner latitudes of ', cgrid
      ENDIF
!
!--   switch to data mode
!     -------------------
      stat = NF_ENDDEF(ncid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR: file ', ncid, ' still in define mode'
      ENDIF
!
!--   write longitudes
!     ----------------
      IF (ll_single) THEN
          stat = NF_PUT_VAR_REAL (ncid, idclon, clon(:,:,:)) 
      ELSE
          stat = NF_PUT_VAR_DOUBLE (ncid, idclon, clon(:,:,:))
      ENDIF
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR writing corner lonitudes of ', cgrid
      ENDIF
!
!--   write latitudes
!     ---------------
      IF (ll_single) THEN
          stat = NF_PUT_VAR_REAL (ncid, idclat, clat(:,:,:))
      ELSE
          stat = NF_PUT_VAR_DOUBLE (ncid, idclat, clat(:,:,:))
      ENDIF
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR writing corner latitudes of ', cgrid
      ENDIF
!
!--   close grids file
!     ----------------
      stat = NF_CLOSE(ncid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR closing file', cgrdnam
      ENDIF
      
    ELSE
#endif
!      -------------
!--    binary format
!      -------------
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'prisn_write_corner: WARNING: '
          WRITE (nulprt,*) 'prism_write_corner:   Binary format not supported'
          WRITE (nulprt,*) 'prism_write_corner:   No corners added'
#ifdef use_netCDF
    ENDIF
#endif
#endif
#ifdef __VERBOSE
    write(nulprt,*) 'End - - prism_write_corner'
    CALL FLUSH(nulprt)
#endif

    RETURN

  END SUBROUTINE prism_write_corner
!--------------------------------------------------------------------------
!
  SUBROUTINE prism_write_mask(cgrid, nx, ny, mask)
!--------------------------------------------------------------------------
! Routine to create a new masks file or to add a land see mask to an
! existing masks file.
!--------------------------------------------------------------------------
#if defined use_comm_GSIP 
    USE mod_gsip_model
#endif
    IMPLICIT NONE
#ifndef use_comm_GSIP
#ifdef use_netCDF
#include <netcdf.inc>
#endif
#else
    INTEGER :: mgi_write, il_errgsip
#endif
    CHARACTER*4,              INTENT (IN) :: cgrid       ! grid acronym
    INTEGER(kind=ip_intwp_p), INTENT (IN) :: nx          ! number of longitudes
    INTEGER(kind=ip_intwp_p), INTENT (IN) :: ny          ! number of latitudes
    INTEGER(kind=ip_intwp_p), INTENT (IN) :: mask(nx,ny) ! mask

    INTEGER(kind=ip_intwp_p) :: iost         ! i/o status
    INTEGER(kind=ip_intwp_p) :: nulmsk       ! logical unit of masks file
    INTEGER(kind=ip_intwp_p) :: stat         ! netcdf status
    INTEGER(kind=ip_intwp_p) :: ncid         ! netcdf file id
    INTEGER(kind=ip_intwp_p) :: idx, idy     ! netcdf dimension ids
    INTEGER(kind=ip_intwp_p) :: idmsk        ! netcdf variable id
    INTEGER(kind=ip_intwp_p) :: dims(2)      ! netcdf variable dimensions

    LOGICAL :: existent    !true if masks file is already existing
    LOGICAL :: mskopen     !true if masks file is opened

    CHARACTER*8 :: cmsk    !locator sring (binary) / variable name (netcdf)
    
!--------------------------------------------------------------------------

#ifdef __VERBOSE
    write(nulprt,*) ' '
    write(nulprt,*) 'Start - - prism_write_mask'
    write(nulprt,*) '  grid acronym: ', cgrid
    CALL FLUSH(nulprt)
#endif

!
!-- Return, if masks files already exists
!
    IF (.NOT. gridswrite) THEN
       write(nulprt,*) ' '
       write(nulprt,*) 'No mask writing needed'
       RETURN
    ENDIF
#if defined use_comm_GSIP 
!
!-- Send flag to coupler to tell it that grids will be send
       il_errgsip = mgi_write (ig_gsipw, 300, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, cgrid, 4, 'C')
       il_errgsip = mgi_write (ig_gsipw, nx, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, ny, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, mask, nx*ny, 'I')
!
#else
#ifdef use_netCDF
    IF (netcdf) THEN
!      -------------
!--    netCDF format
!      -------------
!
!      open masks file
!      ---------------
       stat = NF_OPEN(cmsknam//'.nc', NF_WRITE, ncid)
       IF (stat /= NF_NOERR) THEN
           stat = NF_CREATE(cmsknam//'.nc', NF_CLOBBER, ncid)
          IF (stat /= NF_NOERR) THEN
             WRITE (nulprt,*) 'prism_write_mask: error opening masks file.'
             WRITE (nulprt,*) 'prism_write_mask: STOP'
             STOP
          ENDIF
      ENDIF
!
!--   define dimensions
!     -----------------
      stat = NF_REDEF(ncid)
      stat = NF_INQ_DIMID(ncid, 'x_'//cgrid, idx)
      IF (stat .NE. NF_NOERR) THEN
          stat = NF_DEF_DIM(ncid, 'x_'//cgrid, nx, idx)
          IF (stat /= NF_NOERR) THEN
              WRITE (nulprt,*) 'ERROR defining dimension x_',cgrid
          ENDIF
      ELSE
          WRITE(nulprt,*)'Mask already defined: ', cgrid
          RETURN
      ENDIF
      stat = NF_DEF_DIM(ncid, 'y_'//cgrid, ny, idy)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR defining dimension y_',cgrid
      ENDIF

      dims(1) = idx
      dims(2) = idy
! 
!--   define mask
!     -----------
      cmsk=cgrid//cmsksuf
      stat = NF_DEF_VAR (ncid, cmsk , NF_INT, 2, dims, idmsk)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR defining variable ', cmsk
      ENDIF
      stat = NF_PUT_ATT_TEXT(ncid,idmsk,'long_name',12,'Mask of '//cgrid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR creating att. for mask of ', cgrid
      ENDIF
      stat = NF_PUT_ATT_TEXT(ncid, idmsk, 'units', 1, '1')
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR creating att. for mask of ', cgrid
      ENDIF
!
!--   switch to data mode
!     -------------------
      stat = NF_ENDDEF(ncid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR: file ', ncid, ' still in define mode'
      ENDIF
!
!--   write mask
!     ----------
      stat = NF_PUT_VAR_INT (ncid, idmsk, mask(:,:))
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR writing mask of ', cgrid
      ENDIF
!
!--   close masks file
!     ----------------
      stat = NF_CLOSE(ncid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR closing file', cmsknam
      ENDIF
      
    ELSE
#endif
!      -------------
!--    binary format
!      -------------
!
!      open masks file
!      ---------------
       INQUIRE (FILE = cmsknam, EXIST = existent, OPENED = mskopen)
       IF (existent .AND. mskopen) THEN
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'prism_write_mask: masks file already opened'
          WRITE (nulprt,*) 'prism_write_mask: STOP'
          STOP     
       ENDIF
       iost = 0
       nulmsk = 7
       INQUIRE (nulmsk,OPENED = mskopen)
       DO WHILE (mskopen)
          nulmsk = nulmsk + 1 
          INQUIRE (nulmsk,OPENED = mskopen)
       END DO

       OPEN (UNIT=nulmsk, FILE=cmsknam, STATUS='UNKNOWN',                 &
             ACCESS='SEQUENTIAL', FORM='UNFORMATTED', POSITION='APPEND',  &
             ACTION='WRITE', IOSTAT=iost, ERR=110)

110    CONTINUE
       IF (iost /= 0) THEN
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'prism_write_mask: Error opening masks file'
          WRITE (nulprt,*) 'prism_write_mask: STOP'
          STOP
       ENDIF

!      write maks
!      ----------
       cmsk=cgrid//cmsksuf
       WRITE (UNIT = nulmsk) cmsk
       WRITE (UNIT = nulmsk) mask(:,:)
                           
!      close grids file
!      ----------------
       CLOSE (nulmsk)
#ifdef use_netCDF
    ENDIF
#endif
#endif
#ifdef __VERBOSE
    write(nulprt,*) 'End - - prism_write_mask'
    CALL FLUSH(nulprt)
#endif

    RETURN

  END SUBROUTINE prism_write_mask
!--------------------------------------------------------------------------
!
  SUBROUTINE prism_write_area(cgrid, nx, ny, area)
!--------------------------------------------------------------------------
! Routine to create a new areas file or to add areas of a grid to an
! existing areas file.
!--------------------------------------------------------------------------
#if defined use_comm_GSIP
    USE mod_gsip_model
#endif
    IMPLICIT NONE
#ifndef use_comm_GSIP
#ifdef use_netCDF
#include <netcdf.inc>
#endif
#else
    INTEGER :: mgi_write, il_errgsip
#endif
    CHARACTER*4,              INTENT (IN) :: cgrid       ! grid acronym
    INTEGER(kind=ip_intwp_p), INTENT (IN) :: nx          ! number of longitudes
    INTEGER(kind=ip_intwp_p), INTENT (IN) :: ny          ! number of latitudes
    REAL(kind=ip_realwp_p),   INTENT (IN) :: area(nx,ny) ! areas

    INTEGER(kind=ip_intwp_p) :: iost         ! i/o status
    INTEGER(kind=ip_intwp_p) :: nulsrf       ! logical unit of areas file
    INTEGER(kind=ip_intwp_p) :: stat         ! netcdf status
    INTEGER(kind=ip_intwp_p) :: ncid         ! netcdf file id
    INTEGER(kind=ip_intwp_p) :: idx, idy     ! netcdf dimension ids
    INTEGER(kind=ip_intwp_p) :: idsrf        ! netcdf variable id
    INTEGER(kind=ip_intwp_p) :: dims(2)      ! netcdf variable dimensions
    INTEGER(kind=ip_intwp_p) :: il_nftype    ! netcdf type

    LOGICAL :: existent    !true if areas file is already existing
    LOGICAL :: srfopen     !true if areas file is opened

    CHARACTER*8 :: csrf    !locator sring (binary) / variable name (netcdf)
    
!--------------------------------------------------------------------------

#ifdef __VERBOSE
    write(nulprt,*) ' '
    write(nulprt,*) 'Start - - prism_write_area'
    write(nulprt,*) '  grid acronym: ', cgrid
    CALL FLUSH(nulprt)
#endif

!
!-- Return, if areas files already exists
!
    IF (.NOT. gridswrite) THEN
       write(nulprt,*) ' '
       write(nulprt,*) 'No areas writing needed'
       RETURN
    ENDIF
!
#if defined use_comm_GSIP 
!
!-- Send flag to coupler to tell it that grids will be send
       il_errgsip = mgi_write (ig_gsipw, 400, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, cgrid, 4, 'C')
       il_errgsip = mgi_write (ig_gsipw, nx, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, ny, 1, 'I')
       il_errgsip = mgi_write (ig_gsipw, area, nx*ny, 'R')
!
#else
#ifdef use_netCDF
    IF (netcdf) THEN
!      -------------
!--    netCDF format
!      -------------
!
      il_nftype = NF_DOUBLE
      IF (ll_single) il_nftype = NF_REAL
!
!      open areas file
!      ---------------
       stat = NF_OPEN(csurnam//'.nc', NF_WRITE, ncid)
       IF (stat /= NF_NOERR) THEN
           stat = NF_CREATE(csurnam//'.nc', NF_CLOBBER, ncid)
          IF (stat /= NF_NOERR) THEN
             WRITE (nulprt,*) 'prism_write_area: error opening areas file.'
             WRITE (nulprt,*) 'prism_write_area: STOP'
             STOP
          ENDIF
       ENDIF
!
!--   define dimensions
!     -----------------
      stat = NF_REDEF(ncid)
      stat = NF_INQ_DIMID(ncid, 'x_'//cgrid, idx)
      IF (stat .NE. NF_NOERR) THEN
          stat = NF_DEF_DIM(ncid, 'x_'//cgrid, nx, idx)
          IF (stat /= NF_NOERR) THEN
              WRITE (nulprt,*) 'ERROR defining dimension x_',cgrid
          ENDIF
      ELSE
          WRITE(nulprt,*)'Areas already defined: ', cgrid
          RETURN
      ENDIF
      stat = NF_DEF_DIM(ncid, 'y_'//cgrid, ny, idy)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR defining dimension y_',cgrid
      ENDIF

      dims(1) = idx
      dims(2) = idy
! 
!--   define areas
!     ------------
      csrf=cgrid//csursuf
      stat = NF_DEF_VAR (ncid, csrf , il_nftype, 2, dims, idsrf)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR defining variable ', csrf
      ENDIF
      stat = NF_PUT_ATT_TEXT(ncid,idsrf,'long_name',13,'Areas of '//cgrid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR creating att. for areas of ', cgrid
      ENDIF
      stat = NF_PUT_ATT_TEXT(ncid, idsrf, 'units', 2, 'm2')
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR creating att. for areas of ', cgrid
      ENDIF
!
!--   switch to data mode
!     -------------------
      stat = NF_ENDDEF(ncid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR: file ', ncid, ' still in define mode'
      ENDIF
!
!--   write areas
!     -----------
      IF (ll_single) THEN
          stat = NF_PUT_VAR_REAL (ncid, idsrf, area(:,:))
      ELSE
          stat = NF_PUT_VAR_DOUBLE (ncid, idsrf, area(:,:))
      ENDIF
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR writing area of ', cgrid
      ENDIF
!
!--   close areas file
!     ----------------
      stat = NF_CLOSE(ncid)
      IF (stat /= NF_NOERR) THEN
         WRITE (nulprt,*) 'ERROR closing file', csurnam
      ENDIF
      
    ELSE
#endif
!      -------------
!--    binary format
!      -------------
!
!      open areas file
!      ---------------
       INQUIRE (FILE = csurnam, EXIST = existent, OPENED = srfopen)
       IF (existent .AND. srfopen) THEN
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'prism_write_area: areas file already opened'
          WRITE (nulprt,*) 'prism_write_area: STOP'
          STOP     
       ENDIF
       iost = 0
       nulsrf = 7
       INQUIRE (nulsrf,OPENED = srfopen)
       DO WHILE (srfopen)
          nulsrf = nulsrf + 1 
          INQUIRE (nulsrf,OPENED = srfopen)
       END DO

       OPEN (UNIT=nulsrf, FILE=csurnam, STATUS='UNKNOWN',                 &
             ACCESS='SEQUENTIAL', FORM='UNFORMATTED', POSITION='APPEND',  &
             ACTION='WRITE', IOSTAT=iost, ERR=110)

110    CONTINUE
       IF (iost /= 0) THEN
          WRITE (nulprt,*) ' '
          WRITE (nulprt,*) 'prism_write_area: Error opening areas file'
          WRITE (nulprt,*) 'prism_write_area: STOP'
          STOP
       ENDIF

!      write areas
!      -----------
       csrf=cgrid//csursuf
       WRITE (UNIT = nulsrf) csrf
       WRITE (UNIT = nulsrf) area(:,:)
                           
!      close areas file
!      ----------------
       CLOSE (nulsrf)
#ifdef use_netCDF
    ENDIF
#endif
#endif
#ifdef __VERBOSE
    write(nulprt,*) 'End - - prism_write_area'
    CALL FLUSH(nulprt)
#endif

    RETURN

  END SUBROUTINE prism_write_area

!--------------------------------------------------------------------------
!
  SUBROUTINE prism_terminate_grids_writing
!--------------------------------------------------------------------------
! Routine to terminate the grids writing. Sent a message to OASIS 
! saying that the model has written all grids and that 
! the next model can start editing the grids file.
!--------------------------------------------------------------------------
#if defined use_comm_GSIP 
    USE mod_gsip_model
#endif
    IMPLICIT NONE
#ifdef use_comm_GSIP
    INTEGER :: mgi_write, mgi_read, il_errgsip
#endif
    INTEGER(kind=ip_intwp_p) :: grids_done ! flag to state  that grids 
                                           ! writing is done
    INTEGER(kind=ip_intwp_p) :: dest       ! rank of the receiving process

!--------------------------------------------------------------------------
#ifdef __VERBOSE
    write(nulprt,*) ' '
    write(nulprt,*) 'Start - - prism_terminate_grids_writing'
    CALL FLUSH(nulprt)
#endif

    IF (.NOT. gridswrite) THEN
       write(nulprt,*) ' '
       write(nulprt,*) 'call to routine not needed'
       RETURN
    ENDIF

    grids_done = 1
    dest = 0                   ! OASIS: process 0 of global communicator

    WRITE (nulprt,FMT='(A)') 'Send - grids_done'
    len = 1
#if defined use_comm_MPI1 || defined use_comm_MPI2
    type = MPI_INTEGER
    tag = itagcol+13
    CALL MPI_Send (grids_done, len, type, dest, tag, mpi_comm, mpi_err)
    IF (mpi_err == MPI_SUCCESS) THEN
       WRITE(nulprt,FMT='(A,I2,A,I6,A,I2,A,I3,A,I5,A,I1)') &
            'Send - <dest:',dest,'> <comm:',mpi_comm,'> <len:',len,  &
            '> <type:',type,'> <tag:',tag,'> ::  ',grids_done
    ELSE
       WRITE (nulprt,*) ' '
       WRITE (nulprt,*) 'terminate_grids_writing: an error occured'
       WRITE (nulprt,*) 'terminate_grids_writing: err= ', mpi_err 
       WRITE (nulprt,*) 'terminate_grids_writing: STOP'
       CALL FLUSH(nulprt)
       STOP
    ENDIF
#elif defined use_comm_GSIP
!
!      Write grids_done in channel
!c
       il_errgsip = mgi_write (ig_gsipw, grids_done, len, 'I')
       IF (il_errgsip .GE. 0) THEN
           WRITE(UNIT = nulprt,FMT = *) 'prism_grid_writing - GSIP grids_done OK:', grids_done
       ELSE
           WRITE(UNIT = nulprt,FMT = *) 'prism_grid_writing - GSIP grids_done not OK:', il_errgsip
          CALL prism_abort_proto (ig_mynummod, 'prism_terminate_grids_writing', &
          'GSIP grids_done not written OK in channel')
       ENDIF
#endif
#ifdef __VERBOSE
    write(nulprt,*) 'End - - prism_terminate_grids_writing'
    write(nulprt,*) ' '
    CALL FLUSH(nulprt)
#endif

    RETURN
  END SUBROUTINE prism_terminate_grids_writing
!--------------------------------------------------------------------------

END MODULE mod_prism_grids_writing
!--------------------------------------------------------------------------


     
