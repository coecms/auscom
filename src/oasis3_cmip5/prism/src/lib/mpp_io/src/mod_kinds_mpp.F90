#ifndef key_noIO
!-------------------------------------------------------------------
! BOP
!
! !MODULE: mod_kinds_mpp
MODULE mod_kinds_mpp
!
! !USES:
!
! !PUBLIC TYPES:
IMPLICIT NONE
SAVE
!
! !PUBLIC MEMBER FUNCTIONS:
!
! !PUBLIC DATA MEMBERS:
!
! !PARAMETERS:
  INTEGER, PARAMETER :: ip_single_mpp = SELECTED_REAL_KIND(6,37)
  INTEGER, PARAMETER :: ip_double_mpp = SELECTED_REAL_KIND(12,307)
  INTEGER, PARAMETER :: ip_i2_mpp = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER :: ip_i4_mpp = SELECTED_INT_KIND(9)
#ifdef SX
  INTEGER, PARAMETER :: ip_i8_mpp = SELECTED_INT_KIND(15)
#else
  INTEGER, PARAMETER :: ip_i8_mpp = SELECTED_INT_KIND(18)
#endif
!
! !DESCRIPTION:
! This modules contains the parameters defining the precision used for 
! real and integer variables. For OASIS3, it has to be coherent with
! equivalent parameters in mod_kinds_model.F90
!   
  END MODULE mod_kinds_mpp

#endif
