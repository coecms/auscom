      SUBROUTINE prism_get_freq(id_fldid, id_freq, id_info)
c     
c     **** prism_get_freq ***   
c     
c     purpose:
c     --------
c        retrieve the coupling frequency of a field
c
c     interface:
c     ----------
c        id_fldid : coupling field id (input)
c        id_freq  : coupling field frequency (output)
c        id_info  : output status (output)
c
c     author:
c     -------
c        Sophie Valcke - CERFACS/RPN
c
c     ----------------------------------------------------------------
      USE mod_kinds_model
      USE mod_prism_proto
      USE mod_comprism_proto
c     ----------------------------------------------------------------
      INTEGER (kind=ip_intwp_p)       id_fldid, id_freq, id_info
c     ----------------------------------------------------------------
c
c*    0. Entering
c     -----------
c
      id_info = CLIM_Ok
      WRITE(nulprt,*) 'Entering prism_get_freq for field with id', id_fldid
c
c*    1. Retrieving the coupling frequency
c     ------------------------------------
c
      id_freq = ig_def_freq(id_fldid)
      WRITE(nulprt,*) 'Coupling frequency of this field is ',id_freq
      IF (id_freq .le. 0) THEN
          WRITE(nulprt,*) 'The coupling frequency is < or equal to 0'
          id_info = PRISM_NotFreq
      ENDIF
c
c*    2. Leaving
c     ------------
      WRITE(nulprt,*) 'Leaving prism_get_freq for field with id', id_fldid     
c
      CALL FLUSH(nulprt)
      RETURN
      END 
c
c     ----------------------------------------------------------------
