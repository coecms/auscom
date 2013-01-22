
# This file is generated automatically
SRCS1 =	$(shell find ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC)) -name '*.F90' -print)
OBJS1 =	$(patsubst ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC))/%.F90,  %.o, $(SRCS1))

SRCS2 =  $(shell find ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC)) -name '*.f90' -print)
OBJS2 =  $(patsubst ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC))/%.f90, %.o, $(SRCS2))

SRCS3 =  $(shell find ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC)) -name '*.F' -print)
OBJS3 =  $(patsubst ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC))/%.F, %.o, $(SRCS3))

SRCS4 =  $(shell find ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC)) -name '*.f' -print)
OBJS4 =  $(patsubst ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC))/%.f, %.o, $(SRCS4))

SRCS5 =  $(shell find ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC)) -name '*.c' -print)
OBJS5 =  $(patsubst ../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC))/%.c, %.o, $(SRCS5))

VPATH = .:../../../../src/mod/$(MODEL_DIR)/$(strip $(SRC)):\
	  ../../../../src/mod/$(MODEL_DIR)/include:\
	  ../../lib/clim.${CHAN}:\
	  ../../../lib/$(MODEL_DIR):\
	  ../../../lib:\
	  $(NETCDF_INCLUDE):$(MPI_INCLUDE):$(SYS_INCLUDE):\
	  ../../../../src/mod/$(MODEL_DIR)/make_dummies

LIBRARY	= ../../../lib/$(MODEL_DIR)/lib$(MODLIB).a

PROG =  ../../../bin/$(EXEC).x

INCLS   = -I../../../../src/mod/$(MODEL_DIR)/include \
	  -${I4mods}../../lib/clim.${CHAN} \
	  -${I4mods}../../lib/climgossip

INCLSC  = -I../../../../src/mod/$(MODEL_DIR)/include

clean:
	rm -f $(LIBRARY)
	rm -f i.* *.o *.mod
	rm -f Top_Makefile_*

all: $(PROG)

lib: $(LIBRARY)

$(PROG): $(DEPLIBS) $(LIBRARY)
	$(F90) $(LDFLAGS) -o $@ $(MAINPRG).o $(LIBS) $(LIBS)

$(LIBRARY): $(OBJS1) $(OBJS2) $(OBJS3) $(OBJS4) $(OBJS5)
	$(AR) $(ARFLAGS) $(LIBRARY) *.o
	$(AR) $(ARFLAGS) $(LIBRARY) \
	../../lib/clim.${CHAN}/mod_kinds_oasis.o   \
	../../lib/clim.${CHAN}/mod_parameter.o  \
	../../lib/clim.${CHAN}/mod_timestep.o  \
	../../lib/clim.${CHAN}/mod_string.o  \
	../../lib/clim.${CHAN}/mod_unitncdf.o  \
	../../lib/clim.${CHAN}/mod_kinds_oasis.o  \
	../../lib/clim.${CHAN}/mod_hardware.o  \
	../../lib/clim.${CHAN}/mod_calendar.o  \
	../../lib/clim.${CHAN}/mod_label.o

.SUFFIXES:
.SUFFIXES: .o .c .f .F .f90 .F90

%.o: %.F90
	$(F90) $(F90FLAGS) $(INCLS)  -c   $<

%.o: %.f90
	$(f90) $(f90FLAGS) $(INCLS)  -c   $<

%.o: %.F
	$(F) $(FFLAGS) $(INCLS)  -c   $<

%.o: %.f
	$(f) $(fFLAGS) $(INCLS)  -c   $<

%.o: %.c
	$(CC) $(CCFLAGS) $(INCLSC) -c   $<

#
# All dependencies ...
#

mod_anais.o: mod_kinds_oasis.o mod_parameter.o
mod_analysis.o: mod_kinds_oasis.o
mod_calendar.o: mod_kinds_oasis.o
mod_coast.o: mod_kinds_oasis.o
mod_experiment.o: mod_kinds_oasis.o
mod_extrapol.o: mod_kinds_oasis.o mod_parameter.o
mod_gauss.o: mod_kinds_oasis.o
mod_hardware.o: mod_kinds_oasis.o
mod_memory.o: mod_kinds_oasis.o mod_parameter.o
mod_nproc.o: mod_kinds_oasis.o
mod_parallel.o: mod_kinds_oasis.o
mod_parameter.o: mod_kinds_oasis.o
mod_printing.o: mod_kinds_oasis.o
mod_rainbow.o: mod_kinds_oasis.o mod_parameter.o
mod_sipc.o: mod_kinds_oasis.o
mod_smooth.o: mod_kinds_oasis.o
mod_string.o: mod_kinds_oasis.o
mod_timestep.o: mod_kinds_oasis.o
mod_unit.o: mod_kinds_oasis.o
mod_unitncdf.o: mod_kinds_oasis.o
alloc.o: mod_parameter.o
alloc_src.o: mod_anais.o mod_analysis.o mod_coast.o mod_experiment.o \
	mod_extrapol.o mod_kinds_oasis.o mod_memory.o mod_nproc.o \
	mod_parallel.o mod_parameter.o mod_pipe.o mod_rainbow.o mod_sipc.o \
	mod_string.o mod_timestep.o mod_unitncdf.o
closerst.o: mod_kinds_oasis.o mod_string.o mod_parameter.o mod_unit.o \
        mod_printing.o mod_unitncdf.o
dealloc.o: mod_parameter.o
dealloc_src.o: mod_anais.o mod_analysis.o mod_coast.o mod_experiment.o \
	mod_extrapol.o mod_kinds_oasis.o mod_memory.o mod_nproc.o \
	mod_parallel.o mod_parameter.o mod_pipe.o mod_rainbow.o mod_sipc.o \
	mod_string.o mod_timestep.o mod_unitncdf.o
parse.o: mod_kinds_oasis.o mod_unit.o
parseblk.o: mod_kinds_oasis.o mod_unit.o
skip.o: mod_kinds_oasis.o mod_unit.o
chksgc.o: mod_hardware.o mod_printing.o mod_unit.o
extrap.o: mod_extrapol.o mod_kinds_oasis.o mod_parameter.o mod_printing.o \
	mod_unit.o
ferror.o: mod_experiment.o mod_hardware.o mod_kinds_oasis.o mod_nproc.o \
	mod_parameter.o mod_unit.o
getfld.o: mod_analysis.o mod_clim.o mod_experiment.o mod_hardware.o \
	mod_kinds_oasis.o mod_label.o mod_memory.o mod_parameter.o \
	mod_printing.o mod_sipc.o mod_string.o mod_timestep.o mod_unit.o \
	mod_unitncdf.o 
givfld.o: mod_analysis.o mod_calendar.o mod_clim.o mod_experiment.o \
	mod_hardware.o mod_kinds_oasis.o mod_label.o mod_memory.o \
	mod_parameter.o mod_printing.o mod_sipc.o mod_string.o mod_timestep.o \
	mod_unit.o mod_unitncdf.o 
halte.o: mod_comclim.o mod_hardware.o mod_unit.o
inicmc.o: mod_analysis.o mod_clim.o mod_clim_def.o mod_comclim.o \
	mod_experiment.o mod_gauss.o mod_hardware.o mod_kinds_oasis.o \
	mod_label.o mod_parallel.o mod_parameter.o mod_printing.o \
	mod_string.o mod_timestep.o mod_unit.o mod_unitncdf.o
iniiof.o: mod_anais.o mod_analysis.o mod_extrapol.o mod_hardware.o \
	mod_label.o mod_parameter.o mod_printing.o mod_string.o mod_unit.o \
	mod_unitncdf.o 
inipar.o: mod_anais.o mod_analysis.o mod_calendar.o mod_clim.o mod_coast.o \
	mod_experiment.o mod_extrapol.o mod_hardware.o mod_kinds_oasis.o \
	mod_label.o mod_parallel.o mod_parameter.o mod_printing.o \
	mod_rainbow.o mod_string.o mod_timestep.o mod_unit.o mod_unitncdf.o
inipar_alloc.o: mod_anais.o mod_analysis.o mod_calendar.o mod_clim.o \
	mod_coast.o mod_experiment.o mod_extrapol.o mod_gauss.o \
	mod_hardware.o mod_kinds_oasis.o mod_label.o mod_parallel.o \
	mod_parameter.o mod_printing.o mod_rainbow.o mod_string.o \
	mod_timestep.o mod_unit.o mod_unitncdf.o 
modsgc.o: mod_hardware.o mod_printing.o mod_unit.o
preproc.o: mod_analysis.o mod_extrapol.o mod_gauss.o mod_kinds_oasis.o \
	mod_label.o mod_memory.o mod_parameter.o mod_printing.o mod_string.o \
	mod_unit.o
waitpc.o: mod_clim.o mod_experiment.o mod_hardware.o mod_kinds_oasis.o \
	mod_nproc.o mod_parameter.o mod_pipe.o mod_printing.o mod_sipc.o \
	mod_unit.o
blasnew.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
blasold.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
calend.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
chkfld.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
chkpar.o: mod_experiment.o mod_extrapol.o mod_hardware.o mod_kinds_oasis.o \
	mod_parameter.o mod_printing.o mod_string.o mod_timestep.o mod_unit.o
coasts.o: mod_coast.o mod_kinds_oasis.o mod_parameter.o mod_printing.o \
	mod_smooth.o mod_unit.o
conserv.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
cookart.o: mod_analysis.o mod_kinds_oasis.o mod_memory.o mod_parameter.o \
	mod_printing.o mod_rainbow.o mod_string.o mod_unit.o
correct.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
couple.o: mod_unit.o
driver.o: mod_analysis.o mod_experiment.o mod_hardware.o mod_kinds_oasis.o \
	mod_memory.o mod_parameter.o mod_string.o mod_timestep.o mod_unit.o
empty.o: mod_unit.o
extraw.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
fiasco.o: mod_anais.o mod_kinds_oasis.o mod_parameter.o mod_printing.o \
	mod_timestep.o mod_unit.o
filling.o: mod_analysis.o mod_calendar.o mod_coast.o mod_kinds_oasis.o \
	mod_parameter.o mod_printing.o mod_smooth.o mod_unit.o
getfpe.o: mod_unit.o
glored.o: mod_gauss.o mod_kinds_oasis.o mod_printing.o mod_unit.o
grstat.o: mod_kinds_oasis.o mod_unit.o
hdlerr.o: mod_kinds_oasis.o mod_unit.o 
icoor.o: mod_kinds_oasis.o mod_unit.o
idivmax.o: mod_kinds_oasis.o mod_unit.o
ilenstr.o: mod_unit.o
imaxim.o: mod_kinds_oasis.o mod_unit.o
iminim.o: mod_kinds_oasis.o mod_unit.o
inidya.o: mod_kinds_oasis.o mod_memory.o mod_parameter.o mod_printing.o \
	mod_string.o mod_unit.o
inigrd.o: mod_analysis.o mod_kinds_oasis.o mod_label.o mod_memory.o \
	mod_parameter.o mod_printing.o mod_string.o mod_unit.o mod_unitncdf.o
inilun.o: mod_hardware.o mod_parameter.o mod_printing.o mod_string.o \
	mod_unit.o
initim.o: mod_calendar.o mod_parameter.o mod_printing.o mod_string.o \
	mod_timestep.o mod_unit.o
interp.o: mod_anais.o mod_analysis.o mod_extrapol.o mod_kinds_oasis.o \
	mod_memory.o mod_parameter.o mod_printing.o mod_rainbow.o \
	mod_string.o mod_unit.o
invert.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
isumi.o: mod_kinds_oasis.o mod_unit.o
izero.o: mod_kinds_oasis.o mod_unit.o
jcoor.o: mod_kinds_oasis.o mod_unit.o
locread.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
locreadh.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
locrint.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
locwrint.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
locwrite.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
locwrith.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
masq.o: mod_hardware.o mod_kinds_oasis.o mod_printing.o mod_unit.o
mozaic.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
nextbl.o: mod_unit.o
nextch.o: mod_unit.o
postpro.o: mod_analysis.o mod_extrapol.o mod_gauss.o mod_kinds_oasis.o \
	mod_label.o mod_memory.o mod_parameter.o mod_printing.o mod_string.o \
	mod_unit.o
prcout.o: mod_unit.o
prtout.o: mod_kinds_oasis.o mod_unit.o
redglo.o: mod_gauss.o mod_kinds_oasis.o mod_label.o mod_parameter.o \
	mod_printing.o mod_unit.o mod_unitncdf.o 
reset.o: mod_memory.o mod_parameter.o mod_printing.o mod_unit.o
reverse.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
revmsk.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
rmaxim.o: mod_kinds_oasis.o mod_unit.o
rminim.o: mod_kinds_oasis.o mod_unit.o
sqdis.o: mod_kinds_oasis.o mod_unit.o
ssumr.o: mod_kinds_oasis.o mod_unit.o
subgrid.o: mod_kinds_oasis.o mod_printing.o mod_unit.o
szero.o: mod_kinds_oasis.o mod_unit.o
updtim.o: mod_calendar.o mod_kinds_oasis.o mod_parameter.o mod_printing.o \
	mod_timestep.o mod_unit.o
