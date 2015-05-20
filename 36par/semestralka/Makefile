CC = gcc
MPICC = mpicc
CCOPTS = -c -g -Wall -Wno-format
LDOPTS = -g -Wall

all: nec_sekv nec_par mkgraph

.PHONY: clean distclean

nec_sekv: nec_sekv.o stack.o utils.o
	$(CC) $(LDOPTS) -o $@ nec_sekv.o stack.o utils.o

nec_par: nec_par.o stack.o utils.o
	$(MPICC) $(LDOPTS) -o $@ nec_par.o stack.o utils.o

nec_sekv.o: nec_sekv.c stack.h utils.h
	$(CC) $(CCOPTS) -o $@ nec_sekv.c

nec_par.o: nec_par.c stack.h utils.h
	$(MPICC) $(CCOPTS) -o $@ nec_par.c

stack.o: stack.c stack.h utils.h
	$(CC) $(CCOPTS) -o $@ stack.c

utils.o: utils.c utils.h
	$(CC) $(CCOPTS) -o $@ utils.c

mkgraph: mkgraph.o graph.o
	$(CC) $(LDOPTS) -o $@ mkgraph.o graph.o

mkgraph.o: mkgraph.c graph.h
	$(CC) $(CCOPTS) -o $@ mkgraph.c

graph.o: graph.c graph.h
	$(CC) $(CCOPTS) -o $@ graph.c

clean:
	rm -f nec_sekv.o stack.o mkgraph.o graph.o utils.o

distclean: clean
	rm -f nec_sekv mkgraph
