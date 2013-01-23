
# This file is generated automatically

SRCS1 = $(shell find ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC)) -name '*.F90' -print)
OBJS1 = $(patsubst ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC))/%.F90,  %.o, $(SRCS1))

VPATH = .:../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC)):\
	  ../../../../src/mod/$(MODEL_DIR)/include:\
	  ../../lib/psmile.${CHAN}:\
	  ../../../lib/$(MODEL_DIR):\
	  ../../../lib:\
	  $(NETCDF_INCLUDE):$(MPI_INCLUDE):$(SYS_INCLUDE)

LIBRARY	= ../../../lib/$(MODEL_DIR)/lib$(MODLIB).a

PROG =  ../../../bin/$(EXEC).x

INCLS   = -I../../../../src/mod/$(MODEL_DIR)/include \
	  -${I4mods}../../lib/psmile.${CHAN}

clean:
	rm -f $(LIBRARY)
	rm -f i.* *.o *.mod
	rm -f Top_Makefile_*

all: $(PROG)

lib: $(LIBRARY)

$(PROG): $(DEPLIBS) $(LIBRARY)
	$(F90) $(LDFLAGS) -o $@ $(MAINPRG).o $(LIBS)

$(LIBRARY): $(OBJS1)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS1)

.SUFFIXES:
.SUFFIXES: .o .F90

%.o: %.F90
	$(F90) $(INCLS) $(F90FLAGS) -c $<

#
# All dependencies ...
#

atm.o: mod_kinds_model.o mod_prism_get_proto.o mod_prism_grids_writing.o \
	mod_prism_proto.o mod_prism_put_proto.o
decomp_def.o: mod_prism_def_partition_proto.o mod_prism_proto.o
