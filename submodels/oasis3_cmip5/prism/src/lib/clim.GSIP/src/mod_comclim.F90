MODULE mod_comclim
!
!     - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*    =comclim.h=  CLIM 1.1 internal include file
!                  Coupling Library for Interfacing Models
!
!     - - - - - - - - - - - - - - - - - - - - - - - - - - -
#if defined use_comm_GSIP
  USE mod_kinds_oasis
  USE mod_clim
!
!-----Unit number for trace file
!
  INTEGER(kind=ip_intwp_p)	:: nulprt
!
!-----Models descriptors
!
!     ncplprocs: total number of processes involved in the coupling
!                counting oasis process (1)
!     ig_nmods: number of component models in the simulation
  INTEGER(kind=ip_intwp_p) :: ncplprocs, ig_nmods
!     iga_totproc: total number of processes per component model 
  INTEGER(kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: iga_totproc
!
!-----Minimum exchange frequency, total time of the simulation, total 
!     number of fields exchanged in the simulation of each component.
! 
  INTEGER(kind=ip_intwp_p) :: ig_frqmin, ig_ntime, ig_clim_nfield
!
!-----Ports descriptors
!
  INTEGER(kind=ip_intwp_p) :: nports 
  INTEGER(kind=ip_intwp_p), DIMENSION(:,:), ALLOCATABLE :: myport, irport
  INTEGER(kind=ip_intwp_p), DIMENSION(:,:), ALLOCATABLE :: mydist, irdist
!
!
!-----Links descriptors
!
  INTEGER(kind=ip_intwp_p) :: nlinks 
  INTEGER(kind=ip_intwp_p), DIMENSION(:,:), ALLOCATABLE :: mylink
!
!-----Data encoding
!
  INTEGER(kind=ip_intwp_p), DIMENSION(:), ALLOCATABLE :: ncode
!
!-----Character strings
!
  CHARACTER*8 :: cgroup
  CHARACTER*12 :: cnaprt
  CHARACTER*32, DIMENSION(:), ALLOCATABLE :: cports, clrport
  CHARACTER*32 :: cmynam
  CHARACTER*32, DIMENSION(:), ALLOCATABLE :: cnames  
!
!-----Packing area for Export and Import
!
  REAL(kind=ip_realwp_p), DIMENSION(:), ALLOCATABLE :: pkwork  
!
#endif
END MODULE mod_comclim


