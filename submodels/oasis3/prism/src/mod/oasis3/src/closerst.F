      SUBROUTINE closerst
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL C *
C               * -------------     ------- *
C               *****************************
C
C**** *closerst*  - Closes restart files
C
C**   Interface:
C     ---------
C       *CALL*  *closerst*
C
C     Input:
C     -----
C     None
C
C     Output:
C     ------
C     None
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     none
C
C     History:
C     -------
C       Version   Programmer     Date        Description
C       -------   ----------     ----        -----------  
C       3_2.5     S. Valcke      2007/09/04  Created
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------- Include files and USE of modules---------------------------
C
      USE mod_kinds_oasis
      USE mod_string
      USE mod_parameter
      USE mod_unit
      USE mod_printing
      USE mod_unitncdf

      IMPLICIT NONE

#ifdef use_netCDF
#include <netcdf.inc>
#endif
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER (kind=ip_intwp_p) :: il_samefic, jj, jf, il_unit 
      INTEGER (kind=ip_intwp_p) :: il_ind, il_status
      CHARACTER*8 clfic
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '  Entering  ROUTINE closerst  -  Level C'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
      ENDIF
C
C
C*    2. Close units or netcdf restart files
C        -----------------------------------
C
      DO 100 jf = 1, ig_nfield
        il_samefic=0
        DO 200 jj = 1, jf-1
          IF (nluinp(jf) .eq. nluinp(jj)) THEN
              il_samefic=il_samefic+1
          ENDIF
 200    END DO
        IF (il_samefic .lt. 1) THEN
            il_unit = nluinp(jf)
            clfic = cficinp(jf)
C
C*  Find the index of jf in total number of field
            DO jj = 1, ig_total_nfield
              if ( jf .eq. ig_number_field(jj)) il_ind = jj
            END DO
#if !defined use_comm_NONE
            IF (ig_lag(il_ind) .gt. 0) THEN
#endif
#ifdef use_netCDF
                IF (lncdfrst) THEN
                    il_status= NF_CLOSE(nc_inpid(jf))
                    IF (il_status .ne. NF_NOERR) THEN
#endif
                        CLOSE (UNIT = il_unit, IOSTAT = il_status)
                        IF (il_status .ne. 0) THEN
                            CALL prtout ('Problem in closing unit',
     $                                    il_unit,2)
                            CALL prtout ('Error message number is = ',
     $                                    il_status,2)
                            CALL HALTE('STOP in closerst')
                        ELSE
                            IF (nlogprt .GE. 2) THEN
                                WRITE (UNIT = nulou,FMT = 2200)
     $                              il_unit,clfic 
                            ENDIF
                        ENDIF
#ifdef use_netCDF
                    ELSE
                        WRITE(nulou,*) 'Closed netcdf file with id =', 
     $                   nc_inpid(jf)   

                    ENDIF
                ENDIF
#endif
#if !defined use_comm_NONE
            ENDIF
#endif
        ENDIF
 100  END DO

 2200 FORMAT(/,5X,' Unit ',I2,' has been disconnected from file ',A8)
C
C*    3. End of program
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine closerst ---------'
          CALL FLUSH (nulou)
      ENDIF
      RETURN
      END

