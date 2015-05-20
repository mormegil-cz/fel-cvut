OBJS   = main.o linalgebra.o sgi.o
PROGS  = zpg
CC     = gcc
LIBS   = -lGL -lGLU -lglut -lm
CCOPTS = $(LIBS) -g


zpg: Makefile $(OBJS)
	$(CC) $(CCOPTS) $(OBJS) -o $@

mkcity: Makefile mkcity.o
	$(CC) $(CCOPTS) mkcity.o -o $@

linalgebra.o: linalgebra.c linalgebra.h
	$(CC) $(CCOPTS) -c linalgebra.c

main.o: main.c linalgebra.h sgi.h
	$(CC) $(CCOPTS) -c main.c

sgi.o: sgi.c
	$(CC) $(CCOPTS) -c sgi.c

mkcity.o: mkcity.c
	$(CC) $(CCOPTS) -c mkcity.c

.PHONY: clean

clean:
	rm $(OBJS) $(PROGS) mkcity.o mkcity 2> /dev/null
