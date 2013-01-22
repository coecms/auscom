
SRCS4 =  $(shell ls ../../../../src/lib/fscint/src/*.f)
OBJS4 =  $(patsubst ../../../../src/lib/fscint/src/%.f, %.o, $(SRCS4))

VPATH =	../../../../src/lib/fscint/src:../../../../src/mod/$(COUPLE)/src:

LIBRARY	= ../../../lib/libfscint.a

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

memoir.o: mod_kinds_oasis.o
discendo.o: memoir.o

