#include <stdlib.h>
#include "graph.h"

graph_p init_graph(graph_p g, unsigned nodes)
{
  unsigned i;
  matrix_t p;

  g->nodes = nodes;
  g->matrix = (matrix_t) calloc(nodes * nodes, sizeof(connected_t));
  
  for (i = 0, p = g->matrix; i < nodes; i++, p += nodes + 1)
    *p = YES;
  
  return g;
}

graph_p alloc_graph(unsigned nodes)
{
  graph_p g = (graph_p) malloc(sizeof(graph_t));
  return init_graph(g, nodes);
}
