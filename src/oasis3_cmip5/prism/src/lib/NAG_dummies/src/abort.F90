SUBROUTINE abort
  WRITE(*,'('' Routine abort from prism/src/lib/NAG_dummies is called.'')')
  WRITE(*,'('' It performs a FORTRAN STOP.'')')
  STOP 'Routine abort from prism/src/lib/NAG_dummies is called'
  RETURN
END SUBROUTINE abort
