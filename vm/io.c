#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "io.h"
#include "defs.h"
#include "encoding.h"
#include "heap.h"


#define panic_stop_io_processing() { *result = make_tagged_val(symbol_id_error, vm_tag_plain_symbol); return final_io_action; }


bool is_io_action(vm_value value) {

  if(get_tag(value) != vm_tag_dynamic_compound_symbol) {
    return false;
  }

  vm_value addr = get_val(value);
  vm_value *p = heap_get_pointer(addr);
  vm_value header = p[0];
  if(compound_symbol_id(header) != symbol_id_io) {
    return false;
  }

  return true;
}

io_action_result check_io_action(vm_state *state, vm_value value, vm_instruction *program, vm_value *result, vm_value *next_closure) {

  // check if this is a valid io action
  if(!is_io_action(value)) {
    return no_io_action;
  }

  vm_value addr = get_val(value);
  vm_value *p = heap_get_pointer(addr);

  vm_value action_type = p[1] - int_bias;
  vm_value action_param = p[2];
  vm_value next_action = p[3];

  if(get_tag(action_type) != vm_tag_number) {
    fprintf(stderr, "Malformed io action: %d\n", action_type);
    panic_stop_io_processing();
  }


  // find out which io action
  int action_id = get_val(action_type);
  vm_value next_param = make_tagged_val(symbol_id_false, vm_tag_plain_symbol);

  switch (action_id) {

    case action_id_printline: {
        char *param = read_string(state, action_param);
        if(param == NULL) {
          fprintf(stderr, "io.print_ln: Expected a string, got %s\n", value_to_type_string(action_param));
          panic_stop_io_processing();
        }
        printf("%s", param);
        fflush(stdout);
        next_param = make_tagged_val(symbol_id_nil, vm_tag_plain_symbol);
      }
      break;

    case action_id_readline: {
        char *line = NULL;
        size_t buffer_size = 0;
        int length = getline(&line, &buffer_size, stdin);
        if(length == -1) {
          next_param = make_tagged_val(symbol_id_eof, vm_tag_plain_symbol);
        }
        else
        {
          //cut off trailing newline
          length -= 1;
          line[length] = '\0';

          next_param = new_heap_string(line);
        }
      }
      break;

    case action_id_return:
      next_param = action_param;
      break;

    default:
      fprintf(stderr, "malformed io action: %d\n", action_id);
      panic_stop_io_processing();
  }

  if(get_tag(next_action) == vm_tag_function || get_tag(next_action) == vm_tag_pap) {
    // The io action includes a bound lambda. Set up the vm so that it is called
    // with the result form our io action.

    *next_closure = next_action;
    *result = next_param;

    return intermediary_io_action;
  }
  else {
    // There is no binding following this io action. The action's result
    // is the final result.

    if(result) {
      *result = next_param;
    }
    return final_io_action;
  }

  fprintf(stderr, "Error in io action\n");
  panic_stop_io_processing();
}


