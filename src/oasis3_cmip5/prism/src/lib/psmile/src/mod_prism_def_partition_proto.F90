MODULE mod_prism_def_partition_proto
!
!
!**** DEFINE
!
!     Purpose:
!      Routine prism_def_partition is in a module.
!     
!     Interface:
!       none
!    
!     Method:
!       Uses assumed shape array method to dimension local arrays.        
!
!     External:
!       none
!
!     Files:
!       none
!   
!     References:
!
!     History:
!     --------
!       Version   Programmer     Date        Description
!       ------------------------------------------------
!       2.5       A.Caubel       2002/06     created
!
!*-----------------------------------------------------------------------

CONTAINS
!
  SUBROUTINE prism_def_partition_proto (id_part, kparal, kinfo)
!
!*    *** Def_partition ***   PRISM 1.0
!
!     purpose:
!     --------
!        define a decomposition
!
!     interface:
!     ----------
!        id_part : field decomposition id
!        kparal : type of parallel decomposition
!	 kinfo	: output status
!
!     author:
!     -------
!        Arnaud Caubel - FECIT
!
!  ----------------------------------------------------------------
   USE mod_kinds_model
   USE mod_prism_proto
   USE mod_comprism_proto
!  ----------------------------------------------------------------
   INTEGER (kind=ip_intwp_p)       kinfo, id_part
   INTEGER (kind=ip_intwp_p), DIMENSION(:) :: kparal
!  ----------------------------------------------------------------

   ig_nbpart = ig_nbpart + 1
   id_part = ig_nbpart
   IF (kparal(CLIM_Strategy).EQ.CLIM_Serial) THEN
!
      ig_def_part(CLIM_Strategy,ig_nbpart)   = CLIM_Serial
      ig_def_part(CLIM_Segments,ig_nbpart)   = 1
      ig_def_part(CLIM_Segments+1,ig_nbpart) = 0
      ig_def_part(CLIM_Segments+2,ig_nbpart) = kparal(CLIM_Length)
      ig_length_part(ig_nbpart) = kparal(CLIM_Length)
!
   ELSEIF (kparal(CLIM_Strategy).EQ.CLIM_Apple) THEN
!
      ig_def_part(CLIM_Strategy,ig_nbpart)   = CLIM_Apple
      ig_def_part(CLIM_Segments,ig_nbpart)   = 1
      ig_def_part(CLIM_Segments+1,ig_nbpart) = kparal(CLIM_Offset)
      ig_def_part(CLIM_Segments+2,ig_nbpart) = kparal(CLIM_Length)
      ig_length_part(ig_nbpart) = kparal(CLIM_Length)
!
   ELSEIF (kparal(CLIM_strategy).EQ.CLIM_Box) THEN
!
       IF (kparal(CLIM_SizeY) .GT. CLIM_MaxSegments) THEN
           WRITE (nulprt, *) &
              'Parameter CLIM_MaxSegments must be greater or equal to the field local extent in Y.'
           WRITE (nulprt, *) &
              'Adjust CLIM_MaxSegments in mod_prism_proto.F90 and recompile your model.'
           CALL prism_abort_proto (0, 'prism_def_partition_proto', &
            'Adjust CLIM_MaxSegments in mod_prism_proto.F90 and recompile your model.')
       ENDIF
      ig_def_part(CLIM_Strategy,ig_nbpart)   = CLIM_Box
      ig_def_part(CLIM_Segments,ig_nbpart)   = kparal(CLIM_SizeY)
      DO is=1,kparal(CLIM_SizeY)
         ig_def_part(CLIM_Segments+2*is-1,ig_nbpart) = &
              kparal(CLIM_Offset) + (is-1) * kparal(CLIM_LdX)
         ig_def_part(CLIM_Segments+2*is,ig_nbpart) = kparal(CLIM_SizeX)
      ENDDO
      ig_length_part(ig_nbpart) = kparal(CLIM_SizeX) * kparal(CLIM_SizeY)
!
   ELSEIF (kparal(CLIM_strategy).EQ.CLIM_Orange) THEN
!
       IF (kparal(CLIM_Segments) .GT. CLIM_MaxSegments) THEN
           WRITE (nulprt, *) &
              'Parameter CLIM_MaxSegments must be greater or equal to the number of segments.'
           WRITE (nulprt, *) &
              'Adjust CLIM_MaxSegments in mod_prism_proto.F90 and recompile your model.'
           CALL prism_abort_proto (0, 'prism_def_partition_proto', &
             'Adjust CLIM_MaxSegments in mod_prism_proto.F90 and recompile your model.')            
       ENDIF
      ig_def_part(CLIM_Strategy,ig_nbpart)   = CLIM_Orange
      ig_def_part(CLIM_Segments,ig_nbpart)   = kparal(CLIM_Segments)
      myport(4,ig_nbpart) = 0
      DO is=1,2*kparal(CLIM_Segments)
         ig_def_part(CLIM_Segments+is,ig_nbpart) = kparal(CLIM_Segments+is)
         IF (MOD(is,2).EQ.0) THEN
            ig_length_part(ig_nbpart) = ig_length_part(ig_nbpart) + &
                 kparal(CLIM_Segments+is)
         ENDIF
      ENDDO
   ENDIF
 END SUBROUTINE prism_def_partition_proto

END MODULE mod_prism_def_partition_proto
