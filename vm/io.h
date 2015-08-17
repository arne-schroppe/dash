#ifndef _INCLUDE_IO_H
#define _INCLUDE_IO_H

#include "vm_internal.h"

typedef enum {
  no_io_action = 0,
  intermediary_io_action = 1,
  final_io_action = 2
} io_action_result;

io_action_result check_io_action(vm_state *state, vm_value value, vm_instruction *program, vm_value *result, vm_value *next_action);
bool is_io_action(vm_value value);


#endif

