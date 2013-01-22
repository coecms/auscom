
# This file is generated automatically

SRCS1 =	$(shell find ../../../../src/lib/mpp_io/src/ -name '*.F90' -print)
OBJS1 =	$(patsubst ../../../../src/lib/mpp_io/src/%.F90,  %.o, $(SRCS1))

SRCS5 = $(shell find ../../../../src/lib/mpp_io/src/ -name '*.c' -print)
OBJS5 = $(patsubst ../../../../src/lib/mpp_io/src/%.c, %.o, $(SRCS5))

VPATH 	= ./:../../../../src/lib/mpp_io/src:\
	../../../../src/lib/mpp_io/include:\
	$(NETCDF_INCLUDE):$(MPI_INCLUDE):$(SYS_INCLUDE):$(XML_INCLUDE):\

LIBRARY = ../../../lib/libmpp_io.a

default: all

clean:
	rm -f $(LIBRARY)
	ls | xargs rm -f

all: $(LIBRARY)

$(LIBRARY): $(OBJS1) $(OBJS5)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS1) $(OBJS5)

INCLS = -I../../../../src/lib/mpp_io/include \

INCLSC = -I../../../../src/lib/mpp_io/include \

.SUFFIXES:
.SUFFIXES: .o .f .F .f90 .F90 .c

%.o: %.F90
	$(F90) $(MPP_IOOPT) $(F90FLAGS) $(INCLS)  -c   $<

%.o: %.c
	$(CC) $(CCFLAGS) $(INCLSC) -c   $<

#
# ALL dependencies ...
#

mpp_domains_mod_oa.o: mod_kinds_mpp.o mpp_mod_oa.o
mpp_io_mod_oa.o: mod_kinds_mpp.o mpp_domains_mod_oa.o mpp_mod_oa.o
mpp_mod_oa.o: mod_kinds_mpp.o
