
SRCS4 	= $(shell ls ../../../../src/lib/anaisg/src/*.f)
OBJS4 	= $(patsubst ../../../../src/lib/anaisg/src/%.f, %.o, $(SRCS4))

VPATH 	= ../../../../src/lib/anaisg/src:../../../../src/mod/$(COUPLE)/src

LIBRARY	= ../../../lib/libanaisg.a

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
mod_analysis.o: mod_kinds_oasis.o
nagset.o: mod_parameter.o mod_analysis.o mod_unit.o mod_printing.o mod_kinds_oasis.o
nagsst.o: mod_unit.o mod_printing.o mod_kinds_oasis.o
qcscur.o: mod_unit.o mod_kinds_oasis.o
qgrhal.o: mod_unit.o mod_kinds_oasis.o
qgrho.o: mod_unit.o mod_kinds_oasis.o
qlgaus.o: mod_unit.o mod_kinds_oasis.o
qlins.o: mod_unit.o mod_kinds_oasis.o
qlsort.o: mod_unit.o mod_kinds_oasis.o
qlsst.o: mod_unit.o mod_kinds_oasis.o
