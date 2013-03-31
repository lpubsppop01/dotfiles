# Makefile for qkc

CC     = cc
BIND   = $(CC)
CFLAGS = -O
LFLAGS = -o qkc $(CFLAGS)
OBJ    = qkc.o qkcopt.o qkchelp.o qkcconv.o qkctbl.o

.c.o:
	$(CC) $(CFLAGS) -c $<

qkc: $(OBJ)
	$(BIND) $(LFLAGS) $(OBJ)

$(OBJ): qkc.h

clean:
	rm -f *.o qkc
