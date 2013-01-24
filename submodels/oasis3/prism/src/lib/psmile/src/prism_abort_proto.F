      SUBROUTINE prism_abort_proto(id_compid, cd_routine, cd_message)
C--------------------------------------------------------------------
C BOP
C
C !ROUTINE:  prism_abort_proto
C
C !INTERFACE:
C -----------
C    ierror: returned error code
C     
C !DESCRIPTION:
C
C - stop the whole MPI coupled model.
C
C !REVISION HISTORY:
C 03.07.15  S. Legutke - created
C 03.07.22  S. Valcke - revised
C 04.10.24  S. Valcke - GSIP added
C EOP
C--------------------------------------------------------------------
C $Id: prism_abort_proto.F,v 1.3 2006/03/10 17:05:03 valcke Exp $
C--------------------------------------------------------------------
C 
      USE mod_comprism_proto
      IMPLICIT NONE
#if defined use_comm_MPI1 || defined use_comm_MPI2 
#include <mpif.h>
#endif
C--------------------------------------------------------------------
      INTEGER,          INTENT(in) :: id_compid
      CHARACTER(len=*), INTENT(in) :: cd_routine
      CHARACTER(len=*), INTENT(in) :: cd_message
      INTEGER                      :: ierror
      IF (id_compid .ne. 0) THEN
          WRITE (*,'(a,a,a,a,a)') 'Calling PRISM_ABORT in ',cd_routine
     $        , ' by model ',cg_modnam(id_compid),':'
      ELSE
          WRITE (*,'(a,a,a,a,a)') 'Calling PRISM_ABORT in ',cd_routine
      ENDIF
      WRITE (*,'(a,a)')   '        ',cd_message
#if defined use_comm_MPI1 || defined use_comm_MPI2
      CALL MPI_ABORT (mpi_comm, 0, ierror)

      IF (ierror /= MPI_SUCCESS) THEN
          WRITE (*,'(a)') ' MPI_ABORT failed'
          WRITE (*,'(a,i4)') ' Error =  ', ierror
          STOP
      END IF
#elif defined use_comm_GSIP
      CALL abort
#endif
      END



