#ifndef _INCLUDE_GC_H
#define _INCLUDE_GC_H

#include "vm_internal.h"

void gc_set_stack(stack_frame *stack, int *stack_pointer);

// returns the next free position on the new heap
int gc_collect(vm_value *old_heap, vm_value *new_heap, int heap_size);


#endif
