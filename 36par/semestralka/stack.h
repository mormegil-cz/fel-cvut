#ifndef __STACK_H
#define __STACK_H

void init_stack(void);
void free_stack(void);
void push(void *obj);
void *pop(void);
char stack_empty(void);
long stack_length(void);
void **get_stack_bottom(void);
void *stack_remove_bottom(void);

#endif

