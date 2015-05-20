#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "stack.h"

#undef TRACE_ENABLE
//#define TRACE_ENABLE
#include "utils.h"

void **stack = NULL;
void **stack_bottom = NULL;
unsigned long stack_len = 0;
unsigned long stack_alloced = 0;
unsigned long stack_remaining = 0;

void init_stack(void)
{
  free_stack();
}

void free_stack(void)
{
  void *s = stack;
  stack_len = 0;
  stack_alloced = 0;
  stack_remaining = 0;
  stack = NULL;
  stack_bottom = NULL;
  free(s);
}

void push(void *obj)
{
  TRACE1("push(%p)\n",obj);
  assert(stack_len <= stack_alloced);
  assert(stack_bottom >= stack);
  if (stack_remaining == 0) {
    if (stack_bottom > stack) {
      TRACE0("move to bottom\n");
      memmove(stack, stack_bottom, stack_len * sizeof(*stack));
      stack_remaining += stack_bottom - stack;
      stack_bottom = stack;
    } else {
      TRACE1("realloc from %lu\n", stack_alloced);
      stack_remaining += stack_alloced + 1;
      stack_alloced = (stack_alloced + 1) << 1;
      stack = reallocate(stack, stack_alloced * sizeof(void *));
      stack_bottom = stack;
      assert(stack != NULL);
    }
  }
  
  stack_bottom[stack_len++] = obj;
  stack_bottom[stack_len] = NULL;
  stack_remaining--;
}

void *pop(void)
{
  void *result;

  TRACE2("pop(): stack_len = %d, stack_bottom = %p\n", stack_len, stack_bottom);

  assert(stack_len > 0);
  assert(stack_bottom >= stack);

  stack_len--;
  stack_remaining++;
  result = stack_bottom[stack_len];
  stack_bottom[stack_len] = NULL;

  TRACE1("pop() => %p\n", result);

  return result;
}

char stack_empty(void)
{
  return (char)(stack_len == 0);
}

long stack_length(void)
{
  return stack_len;
}

void **get_stack_bottom(void)
{
  return stack_bottom;
}

void *stack_remove_bottom(void)
{
  TRACE2("stack_remove_bottom(): stack_len = %d, stack_bottom = %p\n", stack_len, stack_bottom);
  assert(stack_len > 0);
  assert(stack_bottom >= stack);
  stack_len--;
  return *(stack_bottom++);
}
