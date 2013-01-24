
SRCS4 =  $(shell ls ../../../../src/lib/anaism/src/*.f)
OBJS4 =  $(patsubst ../../../../src/lib/anaism/src/%.f, %.o, $(SRCS4))

VPATH =	../../../../src/lib/anaism/src:../../../../src/mod/$(COUPLE)/src

LIBRARY	= ../../../lib/libanaism.a

clean:
	rm -f i.* *.o *.mod

all:	$(LIBRARY)

$(LIBRARY): $(OBJS4)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS4)

.SUFFIXES:
.SUFFIXES: .o .f .F .f90 .F90 .c

%.o: %.F90
	$(F90) $(F90FLAGS) $(DOUBLEOPT) $(INCLS)  -c   $<

%.o: %.f90
	$(f90) $(f90FLAGS) $(DOUBLEOPT) $(INCLS)  -c   $<

%.o: %.F
	$(F) $(FFLAGS) $(DOUBLEOPT) $(INCLS)  -c   $<

%.o: %.f
	$(f) $(fFLAGS) $(DOUBLEOPT) $(INCLS)  -c   $<

%.o: %.c
	$(CC) $(CCFLAGS) $(INCLSC) -c   $<

#
# ALL dependencies ...
#

mod_parameter.o: mod_kinds_oasis.o
mod_printing.o: mod_kinds_oasis.o
mod_unit.o: mod_kinds_oasis.o
namset.o: mod_unit.o mod_printing.o mod_kinds_oasis.o
pcssph.o: mod_unit.o mod_kinds_oasis.o
plsst.o: mod_unit.o mod_kinds_oasis.o
pmesh.o: mod_unit.o mod_kinds_oasis.o
pminm.o: mod_unit.o mod_kinds_oasis.o
pmrhal.o: mod_unit.o mod_kinds_oasis.o
pmrho.o: mod_unit.o mod_kinds_oasis.o

