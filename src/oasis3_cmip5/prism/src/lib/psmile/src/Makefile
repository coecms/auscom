SRCS1	= $(shell ls ../../../../src/lib/psmile/src/*.F90)
OBJS1	= $(patsubst ../../../../src/lib/psmile/src/%.F90, %.o, $(SRCS1))

SRCS2 =  $(shell ls ../../../../src/lib/psmile/src/*.f90)
OBJS2 =  $(patsubst ../../../../src/lib/psmile/src/%.f90, %.o, $(SRCS2))

SRCS3 	= $(shell ls ../../../../src/lib/psmile/src/*.F)
OBJS3 	= $(patsubst ../../../../src/lib/psmile/src/%.F, %.o, $(SRCS3))

VPATH 	= ./:../../../../src/lib/psmile/src:\
	     ../../../../src/lib/psmile/include:\
             ../../../../src/lib/mpp_io/src:\
             ../../../../src/lib/mpp_io/include:\
	     ../../lib/mpp_io:\
	     $(NETCDF_INCLUDE):$(MPI_INCLUDE):$(SYS_INCLUDE)

LIBRARY = ../../../lib/libpsmile.${CHAN}.a

clean:
	rm -f i.* *.o *.mod

all:	$(LIBRARY)

$(LIBRARY): $(OBJS1) $(OBJS2) $(OBJS3) 
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS1) $(OBJS2) $(OBJS3)

INCLS = -I../../../../src/lib/psmile/include \
	-I../../../../src/lib/mpp_io/include \
	-I../../lib/mpp_io \

INCLSC = -I../../../../src/lib/psmile/include \
	-I../../../../src/lib/mpp_io/include \
	-I../../lib/mpp_io \

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

mod_psmile_io.o: mod_psmile_io.F90
	$(F90) $(MPP_IOOPT) $(F90FLAGS) $(INCLS)  -c   $<

#
# ALL dependencies ...
#
mod_psmile_date_and_time.o: mod_kinds_model.o
mod_psmile_io_interfaces.o: mod_kinds_model.o
mod_gsip_model.o: mod_kinds_model.o
mod_psmile_io.o: mod_kinds_model.o mod_prism_proto.o mod_comprism_proto.o mpp_mod_oa.o \
	mpp_io_mod_oa.o mpp_domains_mod_oa.o mod_psmile_io_interfaces.o mod_psmile_date_and_time.o
mod_prism_proto.o: mod_kinds_model.o
mod_comprism_proto.o: mod_kinds_model.o mod_prism_proto.o
mod_prism_get_proto.o: mod_kinds_model.o mod_prism_proto.o mod_comprism_proto.o \
	psmile_os.h mod_gsip_model.o
mod_prism_put_proto.o: mod_kinds_model.o mod_prism_proto.o mod_comprism_proto.o \
	write_file.o mathelp_psmile.o psmile_os.h mod_gsip_model.o
prism_put_restart_proto.o: mod_kinds_model.o mod_prism_proto.o mod_comprism_proto.o \
	write_file.o
prism_put_inquire_proto.o: mod_kinds_model.o mod_prism_proto.o mod_comprism_proto.o
mod_prism_def_partition_proto.o: mod_kinds_model.o mod_prism_proto.o mod_comprism_proto.o prism_abort_proto.o
mod_prism_grids_writing: mod_kinds_model.o mod_comprism_proto.o mod_gsip_model.o prism_abort_proto.o
prism_abort_proto.o: mod_comprism_proto.o
prism_def_var_proto.o: mod_kinds_model.o mod_prism_proto.o mod_comprism_proto.o prism_abort_proto.o
prism_enddef_proto.o: mod_kinds_model.o mod_prism_proto.o mod_comprism_proto.o \
	mod_psmile_io_interfaces.o prism_abort_proto.o mod_gsip_model.o
prism_get_freq.o: mod_kinds_model.o mod_prism_proto.o mod_comprism_proto.o
prism_get_localcomm_proto.o: mod_kinds_model.o mod_prism_proto.o \
	mod_comprism_proto.o
prism_init_comp_proto.o: mod_kinds_model.o mod_prism_proto.o mod_comprism_proto.o mod_gsip_model.o \
	mod_psmile_io_interfaces.o prism_abort_proto.o
prism_terminate_proto.o: mod_kinds_model.o mod_prism_proto.o mod_comprism_proto.o \
	mod_psmile_io_interfaces.o
errioipsl_psmile.o: mod_kinds_model.o
mathelp_psmile.o: errioipsl_psmile.o stringop_psmile.o mod_kinds_model.o psmile_os.h


