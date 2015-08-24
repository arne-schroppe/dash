#ifndef _INCLUDE_VM_INTERNAL_H
#define _INCLUDE_VM_INTERNAL_H

#include "vm.h"
#include "defs.h"
#include "heap.h"


typedef struct {
  vm_value reg[num_regs];
  int return_address;
  int result_register;

  //TODO type for this should be heap_address
  int spilled_arguments; // Used for over-saturated calls.
} stack_frame ;




typedef struct {
  stack_frame stack[stack_size];
  int stack_pointer;
  int program_pointer;
  vm_value *const_table;
  int const_table_length;
} vm_state;


#define current_frame (state->stack[state->stack_pointer])
#define next_frame (state->stack[state->stack_pointer + 1])

vm_value new_heap_string(char *content);

char *read_string(vm_state *state, vm_value string_value);
char *value_to_type_string(vm_value value);


#endif
