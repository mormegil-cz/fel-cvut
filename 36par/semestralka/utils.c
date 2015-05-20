#include <stdlib.h>

#undef TRACE_ENABLE

#include "utils.h"

void *_mallocate(size_t size)
{
  void *p = malloc(size);
  TRACE2("malloc(%ld) => %p\n",size, p);
  if (p == NULL) {
    fprintf(stderr, "Failed to malloc(%ld)\n", size);
    exit(64);
  }
  return p;
}

void *_callocate(size_t nelem, size_t elsize)
{
  void *p = calloc(nelem, elsize);
  TRACE3("calloc(%ld,%ld) => %p\n",nelem,elsize,p);
  if (p == NULL) {
    fprintf(stderr, "Failed to calloc(%ld,%ld)\n", nelem, elsize);
    exit(65);
  }
  return p;
}

void *_reallocate(void *ptr, size_t size)
{
  void *p = realloc(ptr, size);
  TRACE3("realloc(%p,%ld) => %p\n",ptr,size,p);
  if (p == NULL) {
    fprintf(stderr, "Failed to realloc(%p, %ld)\n", ptr, size);
    exit(66);
  }
  return p;
}

void _mfree(void *ptr)
{
  TRACE1("free(%p)\n",ptr);
  if (ptr == NULL) {
    fprintf(stderr, "Trying to free(NULL)\n");
    exit(67);
  }
  free(ptr);
}

