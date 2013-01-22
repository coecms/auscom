
SRCS1 =  $(shell ls ../../../../src/lib/NAG_dummies/src/*.F90)
OBJS1 =  $(patsubst ../../../../src/lib/NAG_dummies/src/%.F90, %.o, $(SRCS1))

VPATH =	./:../../../../src/lib/NAG_dummies/src

LIBRARY	= ../../../lib/libNAG_dummies.a

clean:
	rm -f i.* *.o *.mod

all:	$(LIBRARY)

$(LIBRARY): $(OBJS1)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS1)

.SUFFIXES:
.SUFFIXES: .o  .F90

%.o: %.F90
	$(F90) $(F90FLAGS) $(INCLS)  -c   $<



#
# ALL dependencies ...
#
