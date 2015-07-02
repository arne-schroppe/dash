#include <stdio.h>
#include <string.h>
#include "gc.h"
#include "vm_internal.h"


stack_frame *stack = 0;
int *stack_pointer = 0;

void gc_set_stack(stack_frame *stack_arg, int *stack_pointer_arg) {
  stack = stack_arg;
  stack_pointer = stack_pointer_arg;
}


int gc_collect(vm_value *old_heap, vm_value *new_heap, int heap_size) {
  fprintf(stderr, "GC Start\n");

  int new_heap_max = heap_size; //TODO delete

  /*
  // right now only closures (which for us are the same as partial applications)
  // and spilled arguments take space on the heap. Everything else is in the constant 
  // pool.
  
  int new_heap_max = 1;

  int stack_p = *stack_pointer;
  for(int i = 0; i <= stack_p; ++i) {

    stack_frame *frame = stack + i;
    for(int j = 0; j < NUM_REGS; ++j) {

      int object = frame->reg[j];
      int tag = get_tag(object);

      if(tag == vm_tag_forward_pointer) { //TODO this doesn't make much sense
        printf("found forward pointer\n");
        vm_value new_index = from_val(object);
        frame->reg[j] = val(new_index, vm_tag_pap);
      }
      else if(tag == vm_tag_pap) {

        // TODO encode the size calculation for a closure somewhere
        int addr = from_val(object);
        int num_args = pap_var_count(old_heap[addr]);
        int new_index = new_heap_max;
        printf("copy cl from %d to %d\n", addr, new_index);
        memcpy(new_heap + new_index, old_heap + addr, (num_args + 2) * sizeof(vm_value));
        new_heap_max += (num_args + 2);
        frame->reg[j] = val(new_index, vm_tag_pap);
        old_heap[addr] = val(new_index, vm_tag_forward_pointer);
      }

    }

    if(frame->spilled_arguments != 0) {
        printf("found spilled args\n");
    }

  }

  int heap_p = 1;
  while(heap_p < new_heap_max) {

    vm_value object = new_heap[heap_p];

    // TODO heap objects should also have a tag

    int num_args = pap_var_count(object);

    for(int i = 0; i < num_args; ++i) {

      int sub_addr = heap_p + 1 + i;
      vm_value sub_object = new_heap[sub_addr];
      int tag = get_tag(sub_object);

      if(tag == vm_tag_pap) {

        int addr = from_val(object);
        if( get_tag(new_heap[addr]) == vm_tag_forward_pointer ) {
          vm_value forw_addr = from_val(new_heap[addr]);
          printf("heap forward p: %d\n", forw_addr);
          new_heap[sub_addr] = val(forw_addr, vm_tag_pap);
        }
        else {
          int new_index = new_heap_max;
          printf("heap pap, to %d\n", new_index);
          int num_args = pap_var_count(old_heap[addr]);
          memcpy(new_heap + new_index, old_heap + addr, (num_args + 2) * sizeof(vm_value));
          new_heap_max += (num_args + 2);
          new_heap[sub_addr] = val(new_index, vm_tag_pap);
          old_heap[addr] = val(new_index, vm_tag_forward_pointer);
        }

      }
    }

    heap_p += num_args + 2;

    */


    /*
    int tag = get_tag(object);

    if(tag == vm_tag_pap) {
      printf("found cl on heap\n");

      int num_args = pap_var_count(object);
      heap_p += num_args + 2;
    }
    else {
      printf("unknown %d\n", tag);
      break;

    }
    * /


  } */

  fprintf(stderr, "GC End\n");
  return new_heap_max;
}
