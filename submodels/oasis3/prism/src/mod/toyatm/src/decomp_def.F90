SUBROUTINE decomp_def(id_part_id, id_length, id_imjm, &
   id_rank, id_nbcplproc, ld_comparal, ld_mparout)
  !
  USE mod_prism_proto
  USE mod_prism_def_partition_proto
  IMPLICIT NONE
  INTEGER, DIMENSION(:), ALLOCATABLE :: il_paral ! Decomposition for each proc
  INTEGER :: ig_nsegments  ! Number of segments of process decomposition 
  INTEGER :: ig_parsize    ! Size of array decomposition
  INTEGER :: id_nbcplproc  ! Number of processes involved in the coupling
  INTEGER :: id_part_id    ! Local partition ID
  INTEGER :: id_imjm       ! Total grid dimension, ib, ierror, il_rank
  INTEGER :: id_length     ! Size of partial field for each process
  INTEGER :: id_rank       ! Rank of process
  INTEGER :: ld_mparout    ! Unit of log file
  LOGICAL :: ld_comparal
  INTEGER :: ib, ierror
  CHARACTER(len=80), PARAMETER :: cdec='APPLE'
  !
  ! Refer to oasis/psmile/prism/modules/mod_prism_proto.F90 for integer value
  ! of clim_xxxx parameters
  !
  IF ( .NOT. ld_comparal .AND. id_rank == 0) THEN
      ! Monoprocess atm, or parallel atm with only master process involved 
      ! in coupling: the entire field will be exchanged by the process. 
      ig_nsegments = 1
      ig_parsize = 3
      ALLOCATE(il_paral(ig_parsize))
      !
      il_paral ( clim_strategy ) = clim_serial
      il_paral ( clim_offset   ) = 0
      il_paral ( clim_length   ) = id_imjm
      id_length = id_imjm
      !
      CALL prism_def_partition_proto (id_part_id, il_paral, ierror)
      DEALLOCATE(il_paral)
      !
  ELSE
      ! Parallel atm with all process involved in the coupling
      !
      IF (cdec .EQ. 'APPLE') THEN
          ! Each process is responsible for a part of field defined by
          ! the number of grid points and the offset of the first point
          !
          WRITE (ld_mparout,*) 'APPLE partitioning'
          ig_nsegments = 1
          ig_parsize = 3
          ALLOCATE(il_paral(ig_parsize))
          WRITE(ld_mparout,*)'ig_parsize',ig_parsize
          !
          IF (id_rank .LT. (id_nbcplproc-1)) THEN
              il_paral ( clim_strategy ) = clim_apple
              il_paral ( clim_length   ) = id_imjm/id_nbcplproc
              il_paral ( clim_offset   ) = id_rank*(id_imjm/id_nbcplproc)
          ELSE
              il_paral ( clim_strategy ) = clim_apple
              il_paral ( clim_length   ) = id_imjm-(id_rank*(id_imjm/id_nbcplproc))
              il_paral ( clim_offset   ) = id_rank*(id_imjm/id_nbcplproc)
          ENDIF
          id_length = il_paral(clim_length) 
          !
          CALL prism_def_partition_proto (id_part_id, il_paral, ierror)
          DEALLOCATE(il_paral)
          !
      ELSE IF (cdec .EQ. 'BOX') THEN
          ! Each process is responsible for a rectangular box (here two procs
          ! treat 64*24 points and one proc treats 128*12 points)
          !
          WRITE (ld_mparout,*) 'BOX partitioning'
          IF (id_rank .EQ. 0) THEN
              ig_nsegments = 24
          ELSEIF (id_rank .EQ. 1) THEN
              ig_nsegments = 24
          ELSE IF (id_rank .EQ. 2) THEN
              ig_nsegments = 12
          ENDIF
          ig_parsize = 5
          ALLOCATE(il_paral(ig_parsize))
          WRITE(ld_mparout,*)'ig_parsize',ig_parsize
          !
          IF (id_rank .eq. 0) THEN
              il_paral ( clim_strategy ) = clim_box
              il_paral ( clim_LdX      ) = 128
              il_paral ( clim_offset   ) = 0
              il_paral ( clim_sizeX    ) = 64
              il_paral ( clim_sizeY    ) = 24
          ELSEIF (id_rank .eq. 1) THEN
              il_paral ( clim_strategy ) = clim_box
              il_paral ( clim_LdX      ) = 128
              il_paral ( clim_offset   ) = 64
              il_paral ( clim_sizeX    ) = 64
              il_paral ( clim_sizeY    ) = 24
          ELSE IF (id_rank .eq. 2) THEN
              il_paral ( clim_strategy ) = clim_box
              il_paral ( clim_LdX      ) = 128
              il_paral ( clim_offset   ) = 3072
              il_paral ( clim_sizeX    ) = 128
              il_paral ( clim_sizeY    ) = 12
          ENDIF
          id_length = il_paral(clim_sizeX) * il_paral(clim_sizeY)
          !
          CALL prism_def_partition_proto (id_part_id, il_paral, ierror)
          DEALLOCATE(il_paral)
          !
      ELSE IF (cdec .EQ. 'ORANGE') THEN
          ! Each process is responsible for arbitrarily distributed
          ! pieces of the field (here two segments by process)
          !
          WRITE (ld_mparout,*) 'ORANGE partitioning'
          ig_nsegments = 2
          ig_parsize = 2 * ig_nsegments + 2
          WRITE(ld_mparout,*)'ig_parsize',ig_parsize
          ALLOCATE(il_paral(ig_parsize))
          !
          il_paral ( clim_strategy   ) = clim_orange
          il_paral ( clim_segments   ) = 2
          il_paral ( clim_segments+1 ) = id_rank*768
          il_paral ( clim_segments+2 ) = 768
          il_paral ( clim_segments+3 ) = (id_rank+3)*768
          il_paral ( clim_segments+4 ) = 768
          id_length = 0
          DO ib=1,2*il_paral(clim_segments) 
            IF (mod(ib,2).eq.0) THEN
                id_length = id_length + il_paral(clim_segments+ib)
            ENDIF
          END DO
          !
          CALL prism_def_partition_proto (id_part_id, il_paral, ierror)
          DEALLOCATE(il_paral)
          !
      ELSE
          WRITE (ld_mparout,*) 'incorrect decomposition '
      ENDIF
  ENDIF
END SUBROUTINE decomp_def

