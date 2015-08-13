#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gc.h"
#include "vm_internal.h"


stack_frame *stack = 0;
int *stack_pointer = 0;

void gc_set_stack(stack_frame *stack_arg, int *stack_pointer_arg) {
  stack = stack_arg;
  stack_pointer = stack_pointer_arg;
}


int gc_collect(vm_value *old_heap, vm_value *new_heap, int heap_size) {
  fprintf(stderr, "TODO: implement garbage collector\n");
  exit(-1);
  return 0;
}
