#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "graph.h"

int n, k;
graph_p graph;

unsigned node_degree(graph_p g, unsigned node)
{
  connected_p p = g->matrix + (node * g->nodes);
  unsigned i;
  unsigned result = 0;
  
  for (i = 0; i < g->nodes; i++, p++) result += *p;

  /*assert(result > 0);  */
  return result - 1;
}

int main(int argc, char *argv[])
{
  int i;
  matrix_t p;
  
  FILE *f;
  
  if (argc < 2) {
    fprintf(stderr, "Pouziti: %s soubor [-vypis]\n", argv[0]);
    return 2;
  }
  
  printf("n = ");
  scanf("%d", &n);
  printf("k = ");
  scanf("%d", &k);
  
  graph = alloc_graph(n);
  
  for (i = 0; i < n; i++) {
    unsigned desired_degree = rand() % k + 1;
    while (node_degree(graph, i) < desired_degree) {
      unsigned t = rand() % n;
      graph->matrix[i * n + t] = YES;
      graph->matrix[t * n + i] = YES;
    }
  }

  for (i = 0, p = graph->matrix; i < n; i++, p += n + 1)
    *p = NO;

  
  if (argc >= 3) {
  p = graph->matrix;
  for (i = 0; i < n; i++) {
    int j;
    for (j = 0; j < n; j++, p++) {
      printf("%d",*p);
    }
    putchar('\n');
  }
  putchar('\n');
  } else printf("Graf ma %d uzlu\n", n);
  
  f = fopen(argv[1], "wb");
  fwrite(&n, sizeof(n), 1, f);
  fwrite(graph->matrix, sizeof(graph->matrix[0]), n * n, f);
  fclose(f);
  printf("OK\n");

  return 0;
}
