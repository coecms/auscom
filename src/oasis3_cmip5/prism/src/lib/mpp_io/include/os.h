#ifdef __sgi
#ifdef _COMPILER_VERSION
!the MIPSPro compiler defines _COMPILER_VERSION
#define sgi_mipspro
#else
#define sgi_generic
#endif
#endif

#if defined(sgi_mipspro)
#define SGICRAY
#define SGICRAY_MPP
#endif

!most compilers support Cray pointers
!if you find a compiler that does not, #undef this inside a suitable #ifdef

#define use_CRI_pointers

#ifdef __NAGf95
#undef use_CRI_pointers
#endif

#ifdef __SX
! Some elder revision (older than revison 270) of the SX Fortran compiler
! have only limited Cray pointer support. For these use_CRI_pointers
! needs to be undefined.
#endif

#ifdef __SXdbl4
! When -A dbl4 is used on NEC-SX both 4-byte reals become 8-byte reals.
! (and 8-byte reals stay 8-byte reals, so they are both the same)
	! by forbidding 4-byte reals, 4-byte cmplx is also forbidden. Also variables
	! declared with a kind parameter are modified. When -A dbl4 shall be used
	! no_4byte_reals needs to be applied in fft.f90.
	! We recommend to use -A idbl4 instead which does not touch variables declared
	! with the KIND parameter. RRedler
#define no_4byte_reals
	! I think by redefining FLOAT_KIND to 8, I no longer need to redefine NF_*
	! but I will ll leave these in for now. Balaji.
#ifndef __PARNETCDF
#define FLOAT_KIND 8
#define NF_GET_VAR_REAL nf_get_var_double
#define NF_GET_VARA_REAL nf_get_vara_double
#define NF_GET_ATT_REAL nf_get_att_double
#else
#define FLOAT_KIND 8
#define NFMPI_GET_VAR_REAL nfmpi_get_var_double
#define NFMPI_GET_VARA_REAL nfmpi_get_vara_double
#define NFMPI_GET_ATT_REAL nfmpi_get_att_double
#endif
#endif

!Modifications for Oasis - next lines commented and replaced
!values of kind: double and long are 8-byte, float and int are 4-byte
!#if defined(SGICRAY)
!#define DOUBLE_KIND 8
!#define FLOAT_KIND 4
!#define LONG_KIND 8
!#define INT_KIND 4
!#define SHORT_KIND 2
!#define POINTER_KIND 4
!#else
!!these might be different on non-SGICRAY, I believe
!#define DOUBLE_KIND 8
!#define FLOAT_KIND 4
!#define LONG_KIND 8
!#define INT_KIND 4
!#define SHORT_KIND 2
!#define POINTER_KIND 8
!#endif
!
!#ifdef sgi_generic
!!this is for the Edinburgh n32/o32 compiler, which will not accept 8-byte ints at! any price
!#define no_8byte_integers
!#define LONG_KIND 4
!#endif
!
#define DOUBLE_KIND ip_double_mpp 
#define FLOAT_KIND ip_single_mpp
#define LONG_KIND ip_i8_mpp
#define INT_KIND ip_i4_mpp
#define SHORT_KIND ip_i2_mpp

#ifdef __crayx1
#undef use_CRI_pointers

#define no_4byte_reals
#define no_4byte_cmplx
#define  __no_8byte_integers
#define NF_GET_VAR_REAL nf_get_var_double
#define NF_GET_VARA_REAL nf_get_vara_double
#define NF_GET_ATT_REAL nf_get_att_double

!
#define DOUBLE_KIND ip_double_mpp 
#define FLOAT_KIND ip_single_mpp
#define LONG_KIND ip_i8_mpp
#define INT_KIND ip_i4_mpp
#define SHORT_KIND ip_i4_mpp
#endif
