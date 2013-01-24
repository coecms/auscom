
SRCS5 =  $(shell ls ../../../../src/lib/gossip/src/*.c)
OBJS5 =  $(patsubst ../../../../src/lib/gossip/src/%.c, %.o, $(SRCS5))

VPATH =	../../../../src/lib/gossip/src:../../../../src/lib/gossip/include:

LIBRARY	= ../../../lib/libgossip.a

clean:
	rm -f i.* *.o *.mod

all:	$(LIBRARY)

$(LIBRARY): $(OBJS5)
	$(AR) $(ARFLAGS) $(LIBRARY) $(OBJS5)

INCLS 	= -I../../../../src/lib/gossip/include

INCLSC	= -I../../../../src/lib/gossip/include

.SUFFIXES:
.SUFFIXES: .o .c

%.o: %.c
	$(CC) $(CCFLAGS) $(INCLSC) -c   $<


#
# ALL dependencies ...
#

gossip_sock.o:

gossip_timeout.o:

mgilib2.o:

