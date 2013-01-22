MODULE errioipsl_psmile
!---------------------------------------------------------------------
CONTAINS
!=
   SUBROUTINE histerr (plev, pcname, pstr1, pstr2, pstr3)
!---------------------------------------------------------------------
!- INPUT
!- plev   : Category of message to be reported to the user
!-          1 = Note to the user
!-          2 = Warning to the user
!-          3 = Fatal error
!- pcname : Name of subroutine which has called histerr
!- pstr1   
!- pstr2  : String containing the explanations to the user
!- pstr3
!---------------------------------------------------------------------
   USE mod_kinds_model
   IMPLICIT NONE
!-
   INTEGER (kind=ip_intwp_p) :: plev
   CHARACTER(LEN=*) :: pcname,pstr1,pstr2,pstr3
!-
   CHARACTER(LEN=30),DIMENSION(3) :: pemsg = &
  &  (/ "NOTE TO THE USER FROM ROUTINE ", &
  &     "WARNING FROM ROUTINE          ", &
  &     "FATAL ERROR FROM ROUTINE      " /)
!---------------------------------------------------------------------
   IF ( (plev >= 1).AND.(plev <= 3) ) THEN
     WRITE(*,'("     ")')
     WRITE(*,'(A," ",A)') TRIM(pemsg(plev)),TRIM(pcname)
     WRITE(*,'(" --> ",a)') pstr1
     WRITE(*,'(" --> ",a)') pstr2
     WRITE(*,'(" --> ",a)') pstr3
   ENDIF
   IF (plev == 3) THEN
     STOP 'Fatal error from IOIPSL. See stdout for more details'
   ENDIF
!------------------------
   END SUBROUTINE histerr
!=
!-------------------
END MODULE errioipsl_psmile
