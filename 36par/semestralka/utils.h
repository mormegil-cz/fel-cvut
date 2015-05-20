#ifndef __UTILS_H
#define __UTILS_H

#include <stdio.h>
#include <stdlib.h>

#undef TRACE0
#undef TRACE1
#undef TRACE2
#undef TRACE3

#ifdef TRACE_ENABLE
#define TRACE0(str) {fprintf(stdout, str);fflush(stdout);}
#define TRACE1(str,a) {fprintf(stdout, str, a);fflush(stdout);}
#define TRACE2(str,a,b) {fprintf(stdout, str, a, b);fflush(stdout);}
#define TRACE3(str,a,b,c) {fprintf(stdout, str, a, b, c);fflush(stdout);}
#else
#define TRACE0(str) /* */
#define TRACE1(str,a) /* */
#define TRACE2(str,a,b) /* */
#define TRACE3(str,a,b,c) /* */
#endif

#ifdef NDEBUG

#define mallocate(size) malloc(size)
#define callocate(nelem,elsize) calloc(nelem, elsize)
#define reallocate(ptr,size) realloc(ptr, size)
#define mfree(ptr) free(ptr)

#else

#define mallocate(size) _mallocate(size)
#define callocate(nelem,elsize) _callocate(nelem, elsize)
#define reallocate(ptr,size) _reallocate(ptr, size)
#define mfree(ptr) _mfree(ptr)

#endif

void *_mallocate(size_t size);
void *_callocate(size_t nelem, size_t elsize);
void *_reallocate(void *ptr, size_t size);
void _mfree(void *ptr);

#endif

