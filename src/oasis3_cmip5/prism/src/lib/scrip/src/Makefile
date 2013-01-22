
SRCS1 =  $(shell ls ../../../../src/lib/scrip/src/*.F90)
OBJS1 =  $(patsubst ../../../../src/lib/scrip/src/%.F90, %.o, $(SRCS1))

SRCS3   = $(shell ls ../../../../src/lib/scrip/src/*.F)
OBJS3   = $(patsubst ../../../../src/lib/scrip/src/%.F, %.o, $(SRCS3))

SRCS4 =  $(shell ls ../../../../src/lib/scrip/src/*.f)
OBJS4 =  $(patsubst ../../../../src/lib/scrip/src/%.f, %.o, $(SRCS4))

VPATH =	../../../../src/lib/scrip/src:../../../../src/mod/$(COUPLE)/src

LIBRARY	= ../../../lib/libscrip.a

clean:
	rm -f i.* *.o *.mod

all:	$(LIBRARY)

$(LIBRARY): $(OBJS1)  $(OBJS3) $(OBJS4)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS1) $(OBJS3) $(OBJS4)

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

kinds_mod.o: mod_kinds_oasis.o
constants.o: kinds_mod.o
corners.o: kinds_mod.o mod_parameter.o mod_unitncdf.o mod_label.o mod_printing.o mod_unit.o
distance.o: kinds_mod.o constants.o
fracnnei.o: kinds_mod.o constants.o grids.o remap_vars.o mod_unit.o mod_printing.o
gradient.o: kinds_mod.o constants.o
gradient_bicubic.o: kinds_mod.o constants.o
grids.o: kinds_mod.o constants.o iounits.o
iounits.o: kinds_mod.o
netcdf.o: kinds_mod.o constants.o
remap_bicubic.o: kinds_mod.o constants.o grids.o remap_vars.o mod_unit.o mod_printing.o
remap_bicubic_reduced.o: kinds_mod.o constants.o grids.o remap_vars.o mod_unit.o
remap_bilinear.o: kinds_mod.o constants.o grids.o remap_vars.o mod_unit.o mod_printing.o
remap_bilinear_reduced.o: kinds_mod.o constants.o grids.o remap_vars.o mod_unit.o mod_printing.o
remap_conserv.o: kinds_mod.o constants.o timers.o grids.o remap_vars.o
remap_distwgt.o: kinds_mod.o constants.o grids.o remap_vars.o
remap_gauswgt.o: kinds_mod.o constants.o grids.o remap_vars.o
remap_vars.o: kinds_mod.o constants.o grids.o
remap_write.o: kinds_mod.o constants.o grids.o remap_vars.o netcdf.o
scrip.o: kinds_mod.o constants.o iounits.o timers.o grids.o remap_vars.o remap_conserv.o remap_distwgt.o remap_gauswgt.o remap_bilinear.o remap_bicubic.o remap_bilinear_reduced.o remap_bicubic_reduced.o remap_write.o
scriprmp.o: grids.o remap_vars.o mod_unit.o mod_printing.o
timers.o: kinds_mod.o
rotations.o: kinds_mod.o constants.o mod_parameter.o  mod_unitncdf.o mod_unit.o mod_printing.o vector.o
vector.o: kinds_mod.o constants.o mod_parameter.o  mod_unitncdf.o mod_unit.o mod_printing.o
scriprmp_vector.o: grids.o rotations.o vector.o

#clean:
#	rm -f $(OBJS) *.mod

