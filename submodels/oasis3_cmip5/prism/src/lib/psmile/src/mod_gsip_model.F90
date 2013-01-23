MODULE mod_gsip_model
!
#if defined use_comm_GSIP
!
! -- mod_gsip_model  
!    **************
!             22-10-04    S. Valcke    Created      
!@
!@  Contents : variables Gossip channels
!@  --------
!@
!@ -- ig_gsipw : handle associated to model channel for writing  
!@                  information to Oasis
!@ -- ig_gsipr : handle associated to model channel for reading 
!@                  information from Oasis
!@ -- ig_numproc: process number in total number of processes 
!@                  for the model
!     -------------------------------------------------------------------
!
  USE mod_kinds_model
  INTEGER (kind=ip_intwp_p) :: ig_gsipw, ig_gsipr, ig_numproc
!
!     -------------------------------------------------------------------
#endif
!
END MODULE mod_gsip_model
