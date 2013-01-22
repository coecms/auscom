
SRCS1 =  $(shell ls ../../../../src/lib/clim.GSIP/src/*.F90)
OBJS1 =  $(patsubst ../../../../src/lib/clim.GSIP/src/%.F90, %.o, $(SRCS1))

SRCS3 =  $(shell ls ../../../../src/lib/clim.GSIP/src/*.F)
OBJS3 =  $(patsubst ../../../../src/lib/clim.GSIP/src/%.F, %.o, $(SRCS3))

VPATH =	./:../../../../src/lib/clim.GSIP/src:\
	   ../../../../src/mod/$(COUPLE)/src:

LIBRARY	= ../../../lib/libclim.GSIP.a

clean:
	rm -f i.* *.o *.mod

all:	$(LIBRARY)

$(LIBRARY): $(OBJS1) $(OBJS3)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS1) $(OBJS3)

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

#
# ALL dependencies ...
#

mod_clim.o: mod_kinds_oasis.o
mod_comclim.o: mod_clim.o mod_kinds_oasis.o
mod_clim_def.o: mod_kinds_oasis.o mod_clim.o mod_comclim.o
CLIM_Export.o: mod_clim.o mod_comclim.o mod_kinds_oasis.o mod_gsip.o
CLIM_Import.o: mod_clim.o mod_comclim.o mod_kinds_oasis.o mod_gsip.o
CLIM_Init_Oasis.o: mod_clim.o mod_comclim.o mod_parameter.o \
	mod_timestep.o mod_string.o mod_unitncdf.o mod_gsip.o\
	mod_kinds_oasis.o mod_hardware.o mod_calendar.o mod_label.o
CLIM_Quit.o: mod_clim.o mod_comclim.o mod_kinds_oasis.o
CLIM_Start.o: mod_clim.o mod_comclim.o mod_kinds_oasis.o
CLIM_Start_MPI.o: mod_clim.o mod_comclim.o mod_kinds_oasis.o mod_gsip.o
CLIM_Stepi.o: mod_clim.o mod_comclim.o mod_kinds_oasis.o
