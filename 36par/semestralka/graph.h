#ifndef __GRAPH_H
#define __GRAPH_H

typedef unsigned char connected_t, *connected_p;

#ifndef YES
#define YES 1
#endif

#ifndef NO
#define NO 0
#endif

typedef connected_p matrix_t;

typedef struct {
		unsigned nodes;
		matrix_t matrix;
	} graph_t, *graph_p;


graph_p init_graph(graph_p g, unsigned nodes);
graph_p alloc_graph(unsigned nodes);


#endif /* ifndef __GRAPH_H */
