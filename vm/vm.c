#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "vm_internal.h"
#include "opcodes.h"
#include "heap.h"
#include "gc.h"
#include "vm_tags.h"

/*

TODO add optimized instructions for functions with few arguments ? (measure if this is effective)

TODO use computed Goto

*/



// #define VM_DEBUG 1

// TODO this code needs some cleaning up

// TODO unify func vs fun
// TODO unify addr vs address
//
// TODO use low bits for tag, use pointers directly?

#ifdef VM_DEBUG
#  define debug(x) do { x; } while (0)
#else
#  define debug(x) do {} while (0)
#endif

static int invocation = 0;

const int max_integer = 0x1FFFFF;
const int number_bias = 0xFFFFF;

const int char_per_string_chunk = sizeof(vm_value) / sizeof(char);

// Constants
// KEEP THESE IN SYNC WITH COMPILER
static const vm_value symbol_id_false = 0;
static const vm_value symbol_id_true = 1;
static const vm_value symbol_id_eof = 3;


static const int fun_header_size = 1;
static const int pap_header_size = 2;
static const int compound_symbol_header_size = 1;
static const int string_header_size = 1;

const vm_value vm_tag_number = 0x0;
const vm_value vm_tag_plain_symbol = 0x4;
const vm_value vm_tag_compound_symbol = 0x5;
const vm_value vm_tag_pap = 0x6;
const vm_value vm_tag_function = 0x7;
const vm_value vm_tag_dynamic_compound_symbol = 0x8;
const vm_value vm_tag_string = 0x9;
const vm_value vm_tag_dynamic_string = 0xA;
const vm_value vm_tag_match_data = 0xF;

// match data will never appear on the heap, so we can reuse the tag
const vm_value vm_tag_forward_pointer = vm_tag_match_data;


// vm State
#define STACK_SIZE 255
static stack_frame stack[STACK_SIZE];
static int stack_pointer = 0;
static int program_pointer = 0;
static vm_value *const_table = 0;
static int const_table_length = 0;



typedef enum {
  no_io_action = 0,
  intermediary_io_action = 1,
  final_io_action = 2
} io_action_result;

io_action_result check_io_action(vm_value result, vm_instruction *program, vm_value *final_result);


const vm_value vm_failure_result = make_tagged_val(symbol_id_false, vm_tag_plain_symbol);

#define panic_stop_vm() { return vm_failure_result; }
#define panic_stop_io_processing() { *final_result = vm_failure_result; return final_io_action; }



char *tag_to_string(vm_value tag) {

  switch(tag) {

    case vm_tag_number:
      return "number";

    case vm_tag_plain_symbol:
    case vm_tag_compound_symbol:
    case vm_tag_dynamic_compound_symbol:
      return "symbol";

    case vm_tag_pap:
    case vm_tag_function:
      return "function";

    case vm_tag_string:
    case vm_tag_dynamic_string:
      return "string";

    case vm_tag_match_data:
      return "match pattern";

    default:
      return "unknown";
  }

  return "";
}


//TODO do this once instead of all the time
#define check_ctable_index(x) if( (x) >= const_table_length || (x) < 0) { \
    printf("Ctable index out of bounds: %i at %i\n", (x), __LINE__ ); \
    return false; }

#define check_reg(i) { int r = (i); if(r >= num_regs) { fprintf(stderr, "Illegal register: %i", r); panic_stop_vm(); }}
#define get_reg(i) stack[stack_pointer].reg[(i)]
#define current_frame (stack[stack_pointer])
#define next_frame (stack[stack_pointer + 1])

#define do_call(frame, func_reg, instr)         \
  int return_pointer;                 \
  bool call_failed = false;              \
  {                                   \
    check_reg(func_reg); \
    int func = get_reg(func_reg);     \
    if(get_tag(func) != vm_tag_function) { \
      fprintf(stderr, "expected a function (do call)\n"); \
      call_failed = true;                \
    } \
    else {                            \
      int func_address = get_val(func); \
      if(frame != &next_frame) { \
        int num_args = get_arg_r2(instr); \
        memcpy(frame->reg, next_frame.reg, num_args * sizeof(vm_value)); \
      } \
      return_pointer = program_pointer; \
      program_pointer = func_address + fun_header_size; \
    } \
  }


// TODO it could be that func_address is not valid anymore after doing heap_alloc
#define build_pap(num_pap_args, pap_arity, offset, num_args, func_address) \
vm_value pap_value; \
vm_value *pap_pointer; \
{ \
  heap_address pap_address = heap_alloc(num_pap_args + pap_header_size ); \
  pap_pointer = heap_get_pointer(pap_address);  \
  *pap_pointer = pap_header(pap_arity, num_pap_args); /* write header */ \
  *(pap_pointer + 1) = func_address; \
  memcpy(pap_pointer + pap_header_size + offset, next_frame.reg, num_args * sizeof(vm_value)); \
  pap_value = make_tagged_val( (vm_value) pap_address, vm_tag_pap ); \
}


#define prep_oversaturated_call(arity, num_args) \
{ \
  int num_remaining_args = num_args - arity; \
  /* store remaining args */ \
  heap_address addr = heap_alloc(num_args + 1); \
  vm_value *arg_pointer = heap_get_pointer(addr); \
  *arg_pointer = num_remaining_args; \
  memcpy(arg_pointer + 1, &(next_frame.reg[arity]), num_remaining_args * sizeof(vm_value)); \
  current_frame.spilled_arguments = addr; \
  /* Return back to this instruction */ \
  int return_pointer = program_pointer - 1; \
  next_frame.return_address = return_pointer; \
  next_frame.result_register = get_arg_r0(instr); \
}

// TODO turn this into a macro
// TODO add a result register to the tail-call variety of this opcode!! (right now it's pure coincidence that things work, because we set the missing return reg to 0 by default)
/*
  How to handle oversaturated calls:

  When a call is oversaturated, we set up the first call as usual. All additional
  arguments are stored on the heap, with a very simple format: First word is the
  number of stored arguments, followed by the arguments we want to store away. The
  heap address of that arg array is stored in a special field in the frame.
  The call is instructed to return to the gen_ap call. The result will be stored in
  the calls result register (which is the only safe place to store it).

  When returning to the gen_ap instruction, the field for oversaturated calls is checked.
  If it is set, the arguments are copied to the next_frame and the number of arguments
  is set to the number of arguments we had stored. The frame field for oversaturated calls
  is reset to 0.

  After this the call proceeds as usual, possibly leading to another oversaturated call.
*/
int do_gen_ap(stack_frame *frame, vm_value instr, vm_instruction *program) {

  // TODO remove code duplication (in here, stack push, etc)

  // TODO find a better term for "function or closure" than lambda
  int lambda_reg = get_arg_r1(instr);
  int num_args = get_arg_r2(instr);


  // Check whether we are currently applying an oversaturated call
  if(current_frame.spilled_arguments != 0) {
    vm_value *addr = heap_get_pointer(current_frame.spilled_arguments);
    num_args = *addr;
    memcpy(next_frame.reg, addr + 1, num_args * sizeof(vm_value));

    lambda_reg = get_arg_r0(instr);
    current_frame.spilled_arguments = 0;
  }

  check_reg(lambda_reg);
  vm_value lambda = get_reg(lambda_reg);

  vm_value tag = get_tag(lambda);
  if(tag == vm_tag_pap ) {

    heap_address cl_address = (heap_address)get_val(lambda);

    vm_value *cl_pointer = heap_get_pointer(cl_address);
    int header = *cl_pointer;
    int arity = pap_arity(header);
    int num_cl_vars = pap_var_count(header);

    // Saturated closure application
    if (num_args == arity) {
      memmove(&(frame->reg[num_cl_vars]), next_frame.reg, num_args * sizeof(vm_value));
      memcpy(&(frame->reg[0]), cl_pointer + pap_header_size, num_cl_vars * sizeof(vm_value));

      // do the call
      vm_value func_address = *(cl_pointer + 1);
      int return_pointer = program_pointer;
      program_pointer = func_address + fun_header_size;
      return return_pointer;
    }
    // Undersaturated closure application
    else if (num_args < arity) {
      // create a new PAP by copying the old one and adding the new arguments

      vm_value func_address = *(cl_pointer + 1);
      vm_value reg0 = get_arg_r0(instr);

      int num_pap_args = num_cl_vars + num_args;
      int pap_arity = arity - num_args;
      int offset = num_cl_vars;

      build_pap(num_pap_args, pap_arity, offset, num_args, func_address)
      memcpy(pap_pointer + pap_header_size, cl_pointer + pap_header_size, num_cl_vars * sizeof(vm_value));

      check_reg(reg0);
      get_reg(reg0) = pap_value;
      return -1;
    }
    // Oversaturated closure application
    else { // num_args > arity

      prep_oversaturated_call(arity, num_args)

      // set arguments
      memmove(&(next_frame.reg[num_cl_vars]), &(next_frame.reg[0]), arity * sizeof(vm_value));
      memcpy(&(next_frame.reg[0]), cl_pointer + pap_header_size, num_cl_vars * sizeof(vm_value));

      // do the call
      vm_value func_address = *(cl_pointer + 1);
      program_pointer = func_address + fun_header_size;

      ++stack_pointer;

      return -1;
    }
  }

  else if (tag == vm_tag_function) {

    int func_address = get_val(lambda);
    vm_instruction fun_header = program[func_address];
    //TODO check fun header "opcode"
    int arity = get_arg_i(fun_header);

    // saturated function application
    if (num_args == arity) {

      do_call(frame, lambda_reg, instr);
      if(call_failed) {
        fprintf(stderr, "Call failed (not a function)\n");
        return -1; //TODO exit here?
      }
      return return_pointer;
    }
    // Unsersaturated function application
    else if (num_args < arity) {
      //vm_value function_header = program[func_address];
      //TODO check that it's actually a function

      vm_value reg0 = get_arg_r0(instr);
      int pap_arity = arity - num_args;

      build_pap(num_args, pap_arity, 0, num_args, func_address)

      check_reg(reg0);
      get_reg(reg0) = pap_value;
      return -1;
    }
    // Oversaturated function application
    else {

      prep_oversaturated_call(arity, num_args)
      do_call((&next_frame), lambda_reg, instr);
      ++stack_pointer;

      return -1;
    }
  }

  else {
    fprintf(stderr, "Expected a function, but got %s \n", tag_to_string(tag));
    //exit(-1);
  }

  return -1;
}



// TODO try to do this without recursive function calls, then turn into a macro
bool is_equal(vm_value l, vm_value r) {

  vm_value l_tag = get_tag(l);

  vm_value r_tag = get_tag(r);

  if ( (l_tag == vm_tag_compound_symbol) || (l_tag == vm_tag_dynamic_compound_symbol) ) {

    if ( (r_tag != vm_tag_compound_symbol) && (r_tag != vm_tag_dynamic_compound_symbol) ) {
      return false;
    }

    int l_addr = get_val(l);
    int r_addr = get_val(r);

    vm_value *l_pointer;
    vm_value *r_pointer;

    if(l_tag == vm_tag_compound_symbol) {
      l_pointer = const_table + l_addr;
    }
    else {
      l_pointer = heap_get_pointer(l_addr);
    }

    if(r_tag == vm_tag_compound_symbol) {
      r_pointer = const_table + r_addr;
    }
    else {
      r_pointer = heap_get_pointer(r_addr);
    }

    vm_value l_header = *l_pointer;
    vm_value r_header = *r_pointer;


    int count = compound_symbol_count(l_header);
    if( (compound_symbol_id(l_header) != compound_symbol_id(r_header))
        || (count != compound_symbol_count(r_header)) ) {
      return false;
    }

    for(int i = compound_symbol_header_size; i < compound_symbol_header_size + count; ++i) {
      if(! is_equal(l_pointer[i], r_pointer[i])) {
        return false;
      }
    }

    return true;
  }

  if(l_tag == vm_tag_string && r_tag == vm_tag_string) {

    int l_addr = get_val(l);
    int r_addr = get_val(r);

    vm_value *l_pointer = NULL;
    vm_value *r_pointer = NULL;

    if(l_tag == vm_tag_string) {
      l_pointer = const_table + l_addr;
    }
    else {
      l_pointer = heap_get_pointer(l_addr);
    }

    if(r_tag == vm_tag_string) {
      r_pointer = const_table + r_addr;
    }
    else {
      r_pointer = heap_get_pointer(r_addr);
    }

    char *l_str_start = (char *) (l_pointer + 1);
    char *r_str_start = (char *) (r_pointer + 1);

    return strcmp(l_str_start, r_str_start) == 0;
  }

  if(l_tag != r_tag) {
    return false;
  }

  if (l_tag == vm_tag_plain_symbol || l_tag == vm_tag_number) {
    if(l != r) {
      return false;
    }
    return true;
  }

  return false;
}


// TODO can we inline this?
// TODO document the algorithm
bool does_value_match(vm_value pattern, vm_value subject, int start_register) {

  vm_value pattern_tag = get_tag(pattern);

  //if bit 27 (NUM_REGS - 5) is set, this is a match header, otherwise it is a variable
  bool is_match_var = !(pattern & 0x8000000);
  if(pattern_tag == vm_tag_match_data && is_match_var) {
    //capturing match
    int relative_reg = from_match_value(pattern);
    if(relative_reg != match_wildcard_value) {
      check_reg(start_register + relative_reg);
      get_reg(start_register + relative_reg) = subject;
    }
    return true;
  }

  vm_value subject_tag = get_tag(subject);
  if(pattern_tag != vm_tag_compound_symbol
      && pattern_tag != subject_tag) {
    return false;
  }

  switch(pattern_tag) {
    case vm_tag_number:
    case vm_tag_plain_symbol:
      return pattern == subject;

    case vm_tag_compound_symbol: {
      if(subject_tag != vm_tag_compound_symbol
          && subject_tag != vm_tag_dynamic_compound_symbol) {
        return false;
      }

      vm_value pattern_address = get_val(pattern);
      check_ctable_index(pattern_address)

      vm_value pattern_header = const_table[pattern_address];
      vm_value pattern_id = compound_symbol_id(pattern_header);

      vm_value subject_address = get_val(subject);
      vm_value *subject_pointer;
      if(subject_tag == vm_tag_compound_symbol) {
        check_ctable_index(subject_address)
        subject_pointer = &const_table[subject_address];
      }
      else {
        subject_pointer = heap_get_pointer(subject_address);
      }


      vm_value subject_header = subject_pointer[0];
      vm_value subject_id = compound_symbol_id(subject_header);
      if(pattern_id != subject_id) {
        return false;
      }

      vm_value pattern_count = compound_symbol_count(pattern_header);
      vm_value subject_count = compound_symbol_count(subject_header);

      if(pattern_count != subject_count) {
        return false;
      }

      int i = 0;
      for(; i<pattern_count; ++i) {
        int rel_pattern_address = pattern_address + compound_symbol_header_size + i;
        int rel_subject_address = compound_symbol_header_size + i;

        check_ctable_index(rel_pattern_address);
        //check_ctable_index(rel_subject_address);
        if( !does_value_match(const_table[rel_pattern_address], subject_pointer[rel_subject_address], start_register) ) {
          return false;
        }
      }

      return true;
    }

    default:
      return false;
  }
}

void reset() {
  program_pointer = 0;
  stack_pointer = 0;
  const_table = 0;
  const_table_length = 0;

  // Invariant: spilled_arguments field in all frames must be 0 from the beginning
  memset(stack, 0x0, sizeof(stack_frame) * STACK_SIZE);
  heap_init();
  gc_set_stack(stack, &stack_pointer);
}


vm_value vm_execute(vm_instruction *program, int program_length, vm_value *ctable, int ctable_length) {
  ++invocation;
  reset();
  const_table = ctable;
  const_table_length = ctable_length;


  bool is_running = true;

  debug( printf("----- start %d\n", invocation) );

restart:
  while(is_running && program_pointer < program_length) {
    //debug( print_registers(current_frame) );

    is_running = true;
    vm_value instr = program[program_pointer];
    vm_opcode opcode = get_opcode(instr);
    ++program_pointer;

    switch (opcode) {

      case OP_LOAD_i: {
        int reg0 = get_arg_r0(instr);
        int val = get_arg_i(instr);
        check_reg(reg0);
        get_reg(reg0) = val;
        debug( printf("LOADi  r%02i #%i\n", reg0, val) );
      }
      break;


      case OP_LOAD_ps: {
        int reg0 = get_arg_r0(instr);
        int value = get_arg_i(instr);
        check_reg(reg0);
        get_reg(reg0) = make_tagged_val(value, vm_tag_plain_symbol);
        debug( printf("LOADss  r%02i #%i\n", reg0, value) );
      }
      break;


      case OP_LOAD_cs: {
        int reg0 = get_arg_r0(instr);
        int value = get_arg_i(instr);
        check_reg(reg0);
        get_reg(reg0) = make_tagged_val(value, vm_tag_compound_symbol);
        debug( printf("LOADcs r%02i #%i\n", reg0, value) );
      }
      break;


      case OP_LOAD_c: {
        int reg1 = get_arg_r0(instr);
        int table_index = get_arg_i(instr);

        check_ctable_index(table_index)
        check_reg(reg1);
        get_reg(reg1) = const_table[table_index];
        debug( printf("LOADc  r%02i #%i value: %i\n", reg1, table_index, const_table[table_index]) );
      }
      break;


      case OP_LOAD_f: {
        int reg0 = get_arg_r0(instr);
        int value = get_arg_i(instr);
        check_reg(reg0);
        get_reg(reg0) = make_tagged_val(value, vm_tag_function);
        debug( printf("LOADf  r%02i #%i\n", reg0, value) );
      }
      break;


      case OP_LOAD_str: {
        int reg0 = get_arg_r0(instr);
        int value = get_arg_i(instr);
        check_reg(reg0);
        get_reg(reg0) = make_tagged_val(value, vm_tag_string);
        debug( printf("LOADstr  r%02i #%i\n", reg0, value) );
      }
      break;


      case OP_ADD: {
        int reg1 = get_arg_r1(instr);
        int reg2 = get_arg_r2(instr);
        check_reg(reg1);
        int arg1 = get_reg(reg1);
        check_reg(reg2);
        int arg2 = get_reg(reg2);
        int reg0 = get_arg_r0(instr);
        if(get_tag(arg1) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s \n", tag_to_string(get_tag(arg1)));
          panic_stop_vm();
        }

        else if(get_tag(arg2) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s \n", tag_to_string(get_tag(arg2)) );
          panic_stop_vm();
        }

        check_reg(reg0);
        int result = ((arg1 - number_bias) + (arg2 - number_bias)) + number_bias;
        if(result < 0 || result > max_integer) {
          fprintf(stderr, "Int overflow\n");
          panic_stop_vm();
        }
        get_reg(reg0) = result;
        debug( printf("ADD    r%02i r%02i=%x r%02i=%x\n", reg0, reg1, arg1, reg2, arg2) );
      }
      break;


      case OP_SUB: {
        int reg1 = get_arg_r1(instr);
        int reg2 = get_arg_r2(instr);
        check_reg(reg1);
        int arg1 = get_reg(reg1);
        check_reg(reg2);
        int arg2 = get_reg(reg2);
        int reg0 = get_arg_r0(instr);
        if(get_tag(arg1) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s \n", tag_to_string(get_tag(arg1)) );
          panic_stop_vm();
        }

        if(get_tag(arg2) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s \n", tag_to_string(get_tag(arg2)) );
          panic_stop_vm();
        }

        check_reg(reg0);
        int result = ((arg1 - number_bias) - (arg2 - number_bias)) + number_bias;
        if(result < 0 || result > max_integer) {
          fprintf(stderr, "Int overflow\n");
          panic_stop_vm();
        }
        get_reg(reg0) = result;
        debug( printf("SUB    r%02i r%02i=%x r%02i=%x\n", reg0, reg1, arg1, reg2, arg2) );
      }
      break;


      case OP_MUL: {
        int reg1 = get_arg_r1(instr);
        int reg2 = get_arg_r2(instr);
        check_reg(reg1);
        int arg1 = get_reg(reg1);
        check_reg(reg2);
        int arg2 = get_reg(reg2);
        int reg0 = get_arg_r0(instr);
        if(get_tag(arg1) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s \n", tag_to_string(get_tag(arg1)) );
          panic_stop_vm();
        }

        if(get_tag(arg2) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s \n", tag_to_string(get_tag(arg2)) );
          panic_stop_vm();
        }

        check_reg(reg0);
        int result = ((arg1 - number_bias) * (arg2 - number_bias)) + number_bias;
        if(result < 0 || result > max_integer) {
          fprintf(stderr, "Int overflow\n");
          panic_stop_vm();
        }
        get_reg(reg0) = result;
        debug( printf("MUL    r%02i r%02i r%02i\n", reg0, reg1, reg2) );
      }
      break;


      case OP_DIV: {
        int reg1 = get_arg_r1(instr);
        int reg2 = get_arg_r2(instr);
        check_reg(reg1);
        int arg1 = get_reg(reg1);
        check_reg(reg2);
        int arg2 = get_reg(reg2);
        if(arg2 == 0) {
          fprintf(stderr, "Division by 0\n");
          panic_stop_vm();
        }

        if(get_tag(arg1) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s \n", tag_to_string(get_tag(arg1)) );
          panic_stop_vm();
        }

        if(get_tag(arg2) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s \n", tag_to_string(get_tag(arg2)) );
          panic_stop_vm();
        }


        int reg0 = get_arg_r0(instr);
        check_reg(reg0);
        int result = ((arg1 - number_bias) / (arg2 - number_bias)) + number_bias;
        if(result < 0 || result > max_integer) {
          fprintf(stderr, "Int overflow\n");
          panic_stop_vm();
        }
        get_reg(reg0) = result;
        debug( printf("DIV    r%02i r%02i r%02i\n", reg0, reg1, reg2) );
      }
      break;


      case OP_MOVE: {
        int reg0 = get_arg_r0(instr);
        int reg1 = get_arg_r1(instr);
        check_reg(reg0);
        check_reg(reg1);
        get_reg(reg0) = get_reg(reg1);
        debug( printf("MOVE   r%02i r%02i\n", reg0, reg1) );
      }
      break;


      case OP_CALL: {
        if (stack_pointer + 1 == STACK_SIZE) {
          printf("STACK OVERFLOW (call)!\n");
          panic_stop_vm();
        }

        // this macro will create `return_pointer`
        do_call((&next_frame), get_arg_r1(instr), instr);
        if (call_failed) {
          fprintf(stderr, "call failed\n"); //TODO give a better error description
          panic_stop_vm();
        }

        next_frame.return_address = return_pointer;
        next_frame.result_register = get_arg_r0(instr);
        ++stack_pointer;

        debug( printf("CALL\n") );
      }
      break;


      case OP_TAIL_CALL: {
        do_call((&current_frame), get_arg_r1(instr), instr);
        if (call_failed) {
          fprintf(stderr, "call failed\n"); //TODO give a better error description
          panic_stop_vm();
        }

        debug( printf("TL CALL\n") );
      }
      break;


      case OP_GEN_AP: {
        if (stack_pointer + 1 == STACK_SIZE) {
          printf("Stack overflow (call cl)!\n");
          panic_stop_vm();
        }

        int return_pointer = do_gen_ap((&next_frame), instr, program);

        if (return_pointer != -1) {
          next_frame.return_address = return_pointer;
          next_frame.result_register = get_arg_r0(instr);
          ++stack_pointer;
        }

        debug( printf("GEN AP\n") );
      }
      break;


      // TODO It's not entirely clear yet what happens when this returns a new PAP
      case OP_TAIL_GEN_AP: {
        do_gen_ap(&current_frame, instr, program);
        debug( printf("TL GEN AP\n") );
      }
      break;


      case OP_RET: {
        int return_val_reg = get_arg_r0(instr);
        if (stack_pointer == 0) {
          //We simply copy the result value to register 0, so that the runtime can find it
          current_frame.reg[0] = current_frame.reg[return_val_reg];
          debug( printf("RET (end) %x\n", current_frame.reg[return_val_reg]) );
          is_running = false;
          break;
        }
        --stack_pointer;
        current_frame.reg[next_frame.result_register] = next_frame.reg[return_val_reg];
        debug( printf("RET %x\n", next_frame.reg[return_val_reg]) );
        program_pointer = next_frame.return_address;
      }
      break;


      case OP_JMP: {
        int offset = get_arg_i(instr) - number_bias;
        program_pointer += offset;
        if(program_pointer < 0 || program_pointer > program_length) {
          fprintf(stderr, "Illegal address!\n");
          panic_stop_vm();
        }
        debug( printf("JMP %i\n", offset) );
      }
      break;

      case OP_JMP_TRUE: {
        check_reg(get_arg_r0(instr));
        vm_value bool_value = get_reg(get_arg_r0(instr));

        if( is_equal(bool_value, make_tagged_val(symbol_id_true, vm_tag_plain_symbol) )) {
          int offset = get_arg_i(instr) - number_bias;
          program_pointer += offset;
          if(program_pointer < 0 || program_pointer > program_length) {
            fprintf(stderr, "Illegal address: %i\n", program_pointer);
            panic_stop_vm();
          }
        }
        // else: do nothing

        //debug( printf("JMP_EQ %i\n", offset) );
      }
      break;

      case OP_MATCH: {
        check_reg(get_arg_r0(instr));
        int subject = get_reg(get_arg_r0(instr));
        check_reg(get_arg_r1(instr));
        int patterns_addr = get_reg(get_arg_r1(instr));
        int capture_reg = get_arg_r2(instr);
        check_reg(capture_reg);

        check_ctable_index(patterns_addr)
        vm_value match_header = const_table[patterns_addr];
        int number_of_patterns = from_match_value(match_header);
        int i = 0;

        debug( printf("MATCH subj_reg=%04i pat_addr=%04d capt_reg=%02i\n", get_arg_r0(instr), patterns_addr, capture_reg) );
        for(i=0; i<number_of_patterns; ++i) {
          int rel_pat_addr = patterns_addr + 1 + i;

          check_ctable_index(rel_pat_addr)
          vm_value pat = const_table[rel_pat_addr];
          if(does_value_match(pat, subject, capture_reg)) {
            break;
          }
          else {
            continue;
          }
        }

        if(i == number_of_patterns) {
          fprintf(stderr, "Pattern match failed!\n");
          panic_stop_vm();
        }

        program_pointer += i;
      }
      break;


      case OP_SET_ARG: {
        int target_arg = get_arg_r0(instr);
        int source_reg = get_arg_r1(instr);
        int extra_amount = get_arg_r2(instr);
        memcpy(&next_frame.reg[target_arg], &current_frame.reg[source_reg], (1 + extra_amount) * sizeof(vm_value));
        debug( printf("SETARG a%02i r%02i n%02i\n", target_arg, source_reg, extra_amount) );
      }
      break;


      case OP_SET_CL_VAL: {
        int cl_reg = get_arg_r0(instr);
        check_reg(cl_reg);
        vm_value closure = get_reg(cl_reg);

        if( get_tag(closure) != vm_tag_pap ) {
          fprintf(stderr, "Expected a closure, but got: %s\n", tag_to_string(get_tag(closure)));
          panic_stop_vm();
        }

        heap_address cl_address = get_val(closure);
        check_reg(get_arg_r1(instr));
        vm_value new_value = get_reg(get_arg_r1(instr));
        int arg_index = get_arg_r2(instr);

        vm_value *cl_pointer = heap_get_pointer(cl_address);
        int header = *cl_pointer;
        int num_env_args = pap_var_count(header);
        if(arg_index >= num_env_args) {
          fprintf(stderr, "Illegal closure modification (index: %i, num env vars: %i)\n", arg_index, num_env_args);
          panic_stop_vm();
        }
        cl_pointer[pap_header_size + arg_index] = new_value;

        debug( printf("SETCLARG r%02i r%02i n%02i\n", get_arg_r0(instr), get_arg_r1(instr), arg_index) );
      }
      break;


      case OP_PART_AP: {
        int reg0 = get_arg_r0(instr);
        int func_reg = get_arg_r1(instr);
        check_reg(func_reg);
        int func = get_reg(func_reg);

        if( get_tag(func) != vm_tag_function ) {
          fprintf(stderr, "Expected a function, but got: %s \n", tag_to_string(get_tag(func)));
          panic_stop_vm();
        }

        int func_address = get_val(func);
        int num_args = get_arg_r2(instr);

        vm_value function_header = program[func_address];
        //TODO check that it's actually a function
        int arity = get_arg_i(function_header);

        // TODO this was >= earlier, which apparently gave false positives. Find out why, and find out if > is the correct choice
        if(num_args > arity) {
          fprintf(stderr, "Illegal partial application (num args: %i, arity: %i)\n", num_args, arity);
          panic_stop_vm();
        }

        int pap_arity = arity - num_args;

        build_pap(num_args, pap_arity, 0, num_args, func_address);
        check_reg(reg0);
        get_reg(reg0) = pap_value;
        debug( printf("PART_AP\n") );
      }
      break;


      case OP_EQ: {
        check_reg(get_arg_r1(instr));
        check_reg(get_arg_r2(instr));
        vm_value l = get_reg(get_arg_r1(instr));
        vm_value r = get_reg(get_arg_r2(instr));
        int result_reg = get_arg_r0(instr);
        check_reg(result_reg);

        if( is_equal(l, r)) {
          get_reg(result_reg) = make_tagged_val(symbol_id_true, vm_tag_plain_symbol);
        }
        else {
          get_reg(result_reg) = make_tagged_val(symbol_id_false, vm_tag_plain_symbol);
        }
      }
      break;

      case OP_LT: {
        check_reg(get_arg_r1(instr));
        check_reg(get_arg_r2(instr));
        vm_value l = get_reg(get_arg_r1(instr));
        vm_value r = get_reg(get_arg_r2(instr));
        int result_reg = get_arg_r0(instr);
        check_reg(result_reg);

        if(get_tag(l) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s\n", tag_to_string(get_tag(l)));
          panic_stop_vm();
        }
        else if(get_tag(r) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s\n", tag_to_string(get_tag(r)));
          panic_stop_vm();
        }

        if( l < r) {
          get_reg(result_reg) = make_tagged_val(symbol_id_true, vm_tag_plain_symbol);
        }
        else {
          get_reg(result_reg) = make_tagged_val(symbol_id_false, vm_tag_plain_symbol);
        }
      }
      break;

      case OP_GT: {
        check_reg(get_arg_r1(instr));
        check_reg(get_arg_r2(instr));
        vm_value l = get_reg(get_arg_r1(instr));
        vm_value r = get_reg(get_arg_r2(instr));
        int result_reg = get_arg_r0(instr);
        check_reg(result_reg);

        if(get_tag(l) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s\n", tag_to_string(get_tag(l)));
          panic_stop_vm();
        }
        else if(get_tag(r) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s\n", tag_to_string(get_tag(r)));
          panic_stop_vm();
        }

        if( l > r) {
          get_reg(result_reg) = make_tagged_val(symbol_id_true, vm_tag_plain_symbol);
        }
        else {
          get_reg(result_reg) = make_tagged_val(symbol_id_false, vm_tag_plain_symbol);
        }
      }
      break;


      case OP_COPY_SYM: {
        check_reg(get_arg_r0(instr));
        check_reg(get_arg_r1(instr));
        vm_value const_symbol = get_reg(get_arg_r1(instr));

        if ( get_tag(const_symbol) != vm_tag_compound_symbol ) {
          fprintf(stderr, "Expected a const symbol, but got %s \n", tag_to_string(get_tag(const_symbol)));
          panic_stop_vm();
        }

        int c_addr = get_val(const_symbol);
        vm_value c_sym_header = const_table[c_addr];

        int count = compound_symbol_count(c_sym_header);

        size_t total_size = compound_symbol_header_size + count;
        heap_address dyn_sym_address = heap_alloc(total_size);
        vm_value *sym_pointer = heap_get_pointer(dyn_sym_address);  \
        memcpy(sym_pointer, &(const_table[c_addr]), total_size * sizeof(vm_value));

        get_reg(get_arg_r0(instr)) = make_tagged_val(dyn_sym_address, vm_tag_dynamic_compound_symbol);

      }
      break;

      case OP_SET_SYM_FIELD: {
        check_reg(get_arg_r0(instr));
        check_reg(get_arg_r1(instr));
        vm_value heap_symbol = get_reg(get_arg_r0(instr));

        if ( get_tag(heap_symbol) != vm_tag_dynamic_compound_symbol ) {
          fprintf(stderr, "Expected a dynamic symbol, but got %s \n", tag_to_string(get_tag(heap_symbol)));
          panic_stop_vm();
        }

        int h_addr = get_val(heap_symbol);
        vm_value *p = heap_get_pointer(h_addr);
        vm_value h_sym_header = *p;

        int count = compound_symbol_count(h_sym_header);

        int index = get_arg_r2(instr);
        if(index < 0 || index >= count) {
          fprintf(stderr, "Illegal index while setting symbol field: %d\n", index);
          panic_stop_vm();
        }

        p[compound_symbol_header_size + index] = get_reg(get_arg_r1(instr));
      }
      break;


      case OP_STR_LEN: {
        check_reg(get_arg_r0(instr));
        check_reg(get_arg_r1(instr));

        vm_value str = get_reg(get_arg_r1(instr));
        int tag = get_tag(str);
        if (tag != vm_tag_string && tag != vm_tag_dynamic_string ) {
          fprintf(stderr, "Expected a string, but got %s \n", tag_to_string(tag));
          panic_stop_vm();
        }

        int str_addr = get_val(str);
        vm_value *str_pointer;

        if(tag == vm_tag_string) {
          str_pointer = const_table + str_addr;
        }
        else {
          str_pointer = heap_get_pointer(str_addr);
        }

        vm_value str_header = *str_pointer;

        int count = string_length(str_header);
        get_reg(get_arg_r0(instr)) = make_tagged_val(count + number_bias, vm_tag_number);
      }
      break;


      case OP_NEW_STR: {
        int result_reg = get_arg_r0(instr);
        check_reg(result_reg);
        check_reg(get_arg_r1(instr));

        vm_value length_value = get_reg(get_arg_r1(instr));
        if(get_tag(length_value) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s\n", tag_to_string(get_tag(length_value)));
          panic_stop_vm();
        }

        int length = length_value - number_bias;
        if(length < 0) {
          fprintf(stderr, "Negative length for new string, got: %d\n", length);
          panic_stop_vm();
        }

        int adjusted_length = length + 1; // allow space for trailing '\0'
        int num_chunks = adjusted_length / char_per_string_chunk;
        if( (adjusted_length % char_per_string_chunk) != 0 ) {
          num_chunks += char_per_string_chunk - (adjusted_length % char_per_string_chunk);
        }

        size_t total_size = string_header_size + num_chunks;
        heap_address string_address = heap_alloc(total_size);
        vm_value *str_pointer = heap_get_pointer(string_address);

        memset(str_pointer, 0, total_size * sizeof(vm_value));
        *str_pointer = string_header(length, num_chunks);

        get_reg(result_reg) = make_tagged_val(string_address, vm_tag_dynamic_string);

      }
      break;


      case OP_GET_CHAR: {
        int result_reg = get_arg_r0(instr);
        check_reg(result_reg);
        check_reg(get_arg_r1(instr));
        check_reg(get_arg_r2(instr));

        vm_value str = get_reg(get_arg_r1(instr));
        vm_value str_tag = get_tag(str);
        if(str_tag != vm_tag_string && str_tag != vm_tag_dynamic_string) {
          fprintf(stderr, "Expected a string, but got: %s\n", tag_to_string(str_tag));
          panic_stop_vm();
        }

        int str_addr = get_val(str);
        vm_value *str_pointer;

        if(str_tag == vm_tag_string) {
          str_pointer = const_table + str_addr;
        }
        else {
          str_pointer = heap_get_pointer(str_addr);
        }

        vm_value str_header = *str_pointer;

        int index = get_reg(get_arg_r2(instr)) - number_bias;
        int str_length = string_length(str_header);
        if(index < 0 || index > str_length) {
          fprintf(stderr, "Illegal string index: %d\n", index);
          panic_stop_vm();
        }

        char *char_pointer = (char *) (str_pointer + string_header_size);
        int character = char_pointer[index];

        get_reg(result_reg) = make_tagged_val(character, vm_tag_number);

      }
      break;


      case OP_PUT_CHAR: {
        check_reg(get_arg_r0(instr));
        check_reg(get_arg_r1(instr));
        check_reg(get_arg_r2(instr));

        vm_value str = get_reg(get_arg_r1(instr));
        vm_value str_tag = get_tag(str);
        if(str_tag != vm_tag_dynamic_string) {
          fprintf(stderr, "Expected a dynamic string, but got: %s\n", tag_to_string(str_tag));
          panic_stop_vm();
        }

        int character = get_reg(get_arg_r0(instr));
        if(get_tag(character) != vm_tag_number) {
          fprintf(stderr, "Expected a number, but got: %s\n", tag_to_string(get_tag(character)));
          panic_stop_vm();
        }

        int str_addr = get_val(str);
        vm_value *str_pointer = heap_get_pointer(str_addr);

        vm_value str_header = *str_pointer;

        int index = get_reg(get_arg_r2(instr)) - number_bias;
        int str_length = string_length(str_header);
        if(index < 0 || index > str_length) {
          fprintf(stderr, "Illegal string index: %d\n", index);
          panic_stop_vm();
        }

        char *char_pointer = (char *) (str_pointer + string_header_size);
        char_pointer[index] = (char) character;
      }
      break;


      case OP_OR: {
        int reg1 = get_arg_r1(instr);
        int reg2 = get_arg_r2(instr);
        check_reg(reg1);
        int arg1 = get_reg(reg1);
        check_reg(reg2);
        int arg2 = get_reg(reg2);
        int reg0 = get_arg_r0(instr);
        check_reg(reg0);

        vm_value result = make_tagged_val(symbol_id_false, vm_tag_plain_symbol);
        vm_value true_sym = make_tagged_val(symbol_id_true, vm_tag_plain_symbol);
        if(arg1 == true_sym || arg2 == true_sym) {
          result = true_sym;
        }
        get_reg(reg0) = result;
      }
      break;


      case OP_AND: {
        int reg1 = get_arg_r1(instr);
        int reg2 = get_arg_r2(instr);
        check_reg(reg1);
        int arg1 = get_reg(reg1);
        check_reg(reg2);
        int arg2 = get_reg(reg2);
        int reg0 = get_arg_r0(instr);
        check_reg(reg0);

        vm_value result = make_tagged_val(symbol_id_false, vm_tag_plain_symbol);
        vm_value true_sym = make_tagged_val(symbol_id_true, vm_tag_plain_symbol);
        if(arg1 == true_sym && arg2 == true_sym) {
          result = true_sym;
        }
        get_reg(reg0) = result;
      }
      break;


      case OP_NOT: {
        int reg1 = get_arg_r1(instr);
        check_reg(reg1);
        int arg1 = get_reg(reg1);
        int reg0 = get_arg_r0(instr);
        check_reg(reg0);

        vm_value result = make_tagged_val(symbol_id_false, vm_tag_plain_symbol);
        vm_value true_sym = make_tagged_val(symbol_id_true, vm_tag_plain_symbol);
        if(arg1 != true_sym) {
          result = true_sym;
        }
        //else: result is already set to false
        get_reg(reg0) = result;
      }
      break;


      default:
        fprintf(stderr, "UNKNOWN OPCODE: %04x\n", opcode);
        panic_stop_vm();
    }


  }


  vm_value result = stack[stack_pointer].reg[0];


  vm_value io_result_value = 0;
  io_action_result action_result = check_io_action(result, program, &io_result_value);

  switch(action_result) {
    case no_io_action:
      // do nothing
      break;

    case intermediary_io_action:
      is_running = true;
      goto restart;

    case final_io_action:
      result = io_result_value;
      break;

    default:
      fprintf(stderr, "Unknown result of io action: %d\n", action_result);
      panic_stop_vm();
  }


  debug( printf("Result: %u\n", result) );
  return result;
}


// TODO put into separate file and keep in sync with compiler
const int symbol_id_io = 2;
const int action_id_printline = 2;
const int action_id_readline = 1;
const int action_id_return = 0;

// TODO turn into macro, also use it for new_str opcode
vm_value new_string(size_t length) {

  size_t adjusted_length = length + 1; // allow space for trailing '\0'
  int num_chunks = adjusted_length / char_per_string_chunk;
  if( (adjusted_length % char_per_string_chunk) != 0 ) {
    num_chunks += char_per_string_chunk - (adjusted_length % char_per_string_chunk);
  }

  size_t total_size = string_header_size + num_chunks;
  heap_address string_address = heap_alloc(total_size);
  vm_value *str_pointer = heap_get_pointer(string_address);

  memset(str_pointer, 0, total_size * sizeof(vm_value));
  *str_pointer = string_header(length, num_chunks);

  return string_address;
}


char *read_string(vm_value string_value) {

  if(get_tag(string_value) != vm_tag_dynamic_string
      && get_tag(string_value) != vm_tag_string) {
    fprintf(stderr, "Expected a string, but got: %s\n", tag_to_string(get_tag(string_value)));
    return NULL;
  }

  int str_addr = get_val(string_value);

  vm_value *str_p;
  if(get_tag(string_value) ==vm_tag_dynamic_string) {
    str_p = heap_get_pointer(str_addr);
  }
  else {
    str_p = const_table + str_addr;
  }

  vm_value *string_start = str_p + 1;
  return (char *)string_start;
}



vm_value *vm_get_heap_pointer(vm_value addr) {
  return heap_get_pointer(addr);
}



// Put vm state into a struct and move this to another file

io_action_result check_io_action(vm_value result, vm_instruction *program, vm_value *final_result) {

  if(get_tag(result) != vm_tag_dynamic_compound_symbol) {
    return no_io_action;
  }

  vm_value addr = get_val(result);
  vm_value *p = heap_get_pointer(addr);
  vm_value header = p[0];
  if(compound_symbol_id(header) != symbol_id_io) {
    return no_io_action;
  }

  vm_value action_type = p[1] - number_bias;
  vm_value action_param = p[2];
  vm_value next_action = p[3];

  if(get_tag(action_type) != vm_tag_number) {
    fprintf(stderr, "Malformed io action: %d\n", action_type);
    panic_stop_io_processing();
  }
  int action_id = get_val(action_type);

  vm_value next_param = make_tagged_val(symbol_id_false, vm_tag_plain_symbol);
  switch (action_id) {

    case action_id_printline: {
        char *param = read_string(action_param);
        if(param == NULL) {
          panic_stop_io_processing();

        }
        printf("%s", param);
        next_param = make_tagged_val(symbol_id_true, vm_tag_plain_symbol);
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

          vm_value string_address = new_string(length);
          vm_value *str_pointer = heap_get_pointer(string_address);
          vm_value *str_start = str_pointer + 1;

          strcpy((char *)str_start, line);
          next_param = make_tagged_val(string_address, vm_tag_dynamic_string);
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
    stack_pointer = 0;

    current_frame.reg[0] = next_action;
    next_frame.reg[0] = next_param;
    vm_instruction instr = op_gen_ap(0, 0, 1);
    int return_pointer = do_gen_ap(&current_frame, instr, program);
    if (return_pointer != -1) {
      current_frame.return_address = return_pointer;
      current_frame.result_register = 0;

      return intermediary_io_action;
    }
    else {
      // TODO is this malformed?
      fprintf(stderr, "malformed bound lambda in io action\n");
      panic_stop_io_processing();
    }
  }
  else {
    // There is no binding following this io action. The action's result
    // is the final result.
    if(final_result) {
      *final_result = next_param;
    }
    return final_io_action;
  }

  fprintf(stderr, "Error in io action\n");
  panic_stop_io_processing();
}

