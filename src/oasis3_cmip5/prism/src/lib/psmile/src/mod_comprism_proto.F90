MODULE mod_comprism_proto
!     - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
  USE mod_prism_proto
  USE mod_kinds_model
!
!-----Unit number for trace file
!
  INTEGER(kind=ip_i4_p)	:: nulprt
!
!-----Models descriptors
!
!     ncplprocs: total number of processes involved in the coupling
!                counting oasis process (indirect case)
  INTEGER(kind=ip_i4_p) :: ncplprocs, mynum, mytid, ig_mynummod, knmods
#if defined use_comm_MPI1 || defined use_comm_MPI2
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: modtid
  CHARACTER*32, DIMENSION(:), ALLOCATABLE :: cnames  
#endif
  CHARACTER(len=6), DIMENSION(:), ALLOCATABLE :: cg_modnam
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: kbtotproc, kbcplproc
  INTEGER(kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: iga_unitmod
!
!-----Minimum exchange frequency, total time of the simulation, total 
!     number of fields exchanged in the simulation, local communicator of 
!     each component, maximum size of grids, initial date of the simulation
! 
  INTEGER(kind=ip_i4_p) :: ig_frqmin, ig_ntime, ig_clim_nfield, ig_local_comm
  INTEGER(kind=ip_i4_p) :: ig_CLIM_Maxgrd, ig_inidate(6)
!
!-----Lag of exported fields, coupling period of fields, sequential index 
!     of fields, name of exchanged fields and restart files, restart files 
!     numbers, number of restart files, logical indicating if one field 
!     (at least) goes through Oasis or not, field I/O status, logical 
!     indicating a restart file netcdf or not, logical indicating if all 
!     fields go through Oasis or not.
!
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_clim_lag, ig_def_lag
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_clim_reverse, ig_def_reverse
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_clim_invert, ig_def_invert
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_clim_freq, ig_def_freq
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_clim_seq, ig_def_seq
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_clim_norstfile, ig_def_norstfile
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_aux, ig_clim_state
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_def_state
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_clim_trans, ig_def_trans
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_clim_numlab
!RV Will contain label numbers of fields according to their port number
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_def_numlab
!RV
  INTEGER(kind=ip_i4_p) :: ig_nbr_rstfile
  CHARACTER(len=8), DIMENSION(:), ALLOCATABLE :: cg_cnaminp, cg_cnamout
  CHARACTER(len=8), DIMENSION(:), ALLOCATABLE :: cg_clim_rstfile
  CHARACTER(len=8), DIMENSION(:), ALLOCATABLE :: cg_def_rstfile
  CHARACTER(len=32), DIMENSION(:), ALLOCATABLE :: cg_clim_inpfile
  CHARACTER(len=32), DIMENSION(:), ALLOCATABLE :: cg_def_inpfile
  CHARACTER(len=8), DIMENSION(:), ALLOCATABLE :: cg_ignout_field
  LOGICAL :: lg_ncdfrst, lg_oasis_field
!
! For lons and lats and corner outputs
  CHARACTER(len=5) :: cg_clim_cgrdnam
  CHARACTER(len=4) :: cg_clim_lonsuf, cg_clim_latsuf
  CHARACTER(len=4) :: crn_clim_lonsuf, crn_clim_latsuf
  CHARACTER(len=4), DIMENSION(:), ALLOCATABLE :: &
     cga_clim_locatorbf, cga_clim_locatoraf, cga_clim_locator
  INTEGER(kind=ip_intwp_p)  :: ig_noc       ! Number of corners

!
!-----Local transformations descriptors
!
  REAL(kind=ip_single_p), DIMENSION(:,:), ALLOCATABLE :: rg_field_trans
  REAL(kind=ip_double_p), DIMENSION(:,:), ALLOCATABLE :: dg_field_trans
  INTEGER(kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: ig_number
  LOGICAL :: lg_dgfield
!
!-----Decomposition descriptors
!
  INTEGER(kind=ip_i4_p) :: ig_nbpart
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_length_part
  INTEGER(kind=ip_i4_p), DIMENSION(:,:), ALLOCATABLE :: ig_def_part 
!
!-----Ports descriptors
!
  INTEGER(kind=ip_i4_p) :: nports 
  INTEGER(kind=ip_i4_p), DIMENSION(:,:), ALLOCATABLE :: myport, irport
  INTEGER(kind=ip_i4_p), DIMENSION(:,:), ALLOCATABLE :: mydist, irdist
!
!
!-----Links descriptors
!
  INTEGER(kind=ip_i4_p) :: nlinks 
  INTEGER(kind=ip_i4_p), DIMENSION(:,:), ALLOCATABLE :: mylink
!
!-----Data encoding
!
  INTEGER(kind=ip_i4_p) :: mycode
!
!-----Inquiry descriptors
!
  INTEGER(kind=ip_i4_p) :: nbsend, nbrecv
!
!-----Timesteps descriptors
!
  INTEGER(kind=ip_i4_p) :: mystep, mystdt, myfcpl
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: ig_nstep
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: nstdt
  INTEGER(kind=ip_i4_p), DIMENSION(:), ALLOCATABLE :: nfcpl 
!
!-----Character strings
!
  CHARACTER*8 :: cgroup
  CHARACTER*16 :: cnaprt
  CHARACTER*32, DIMENSION(:), ALLOCATABLE :: cports, clrport
  CHARACTER*32 :: cmynam
  CHARACTER*4 :: ctype
!
!-----Packing area for Export and Import
!
#if defined use_comm_MPI1 || defined use_comm_MPI2
  REAL(kind=ip_double_p), DIMENSION(:), ALLOCATABLE :: pkwork 
  REAL(kind=ip_double_p), DIMENSION(:), ALLOCATABLE :: pkwork_field
#elif defined use_comm_GSIP
  REAL(kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: pkworkps
#endif
!
!-----mpi_rank :  Rank of process in MPI communicator
!-----mpi_size :  Size of communicator
!-----mpi_err :   Error code of MPI calls
!-----mpi_comm :  Communicator handle

  INTEGER(kind=ip_i4_p) :: mpi_rank, mpi_size, mpi_err, mpi_comm
!
!-----Buffer and logical for MPI_BSend
!
  LOGICAL :: lg_clim_bsend 
#if defined use_comm_MPI1 || defined use_comm_MPI2
  REAL(kind=ip_double_p), DIMENSION(:), ALLOCATABLE :: dg_bufsend
#endif
!
!-----lg_mpiflag: logical true if MPI_Init has been called by application
!
  LOGICAL :: lg_mpiflag
!
!-----flag to start grids writing
!
  INTEGER(kind=ip_i4_p) :: grids_start
!
    END MODULE mod_comprism_proto


