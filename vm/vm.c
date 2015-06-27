#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "vm.h"
#include "opcodes.h"
#include "heap.h"

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

// Constants
static const vm_value symbol_id_false = 0;
static const vm_value symbol_id_true = 1;
static const int fun_header_size = 1;

const vm_value vm_tag_number = 0x0;
const vm_value vm_tag_plain_symbol = 0x4;
const vm_value vm_tag_compound_symbol = 0x5;
const vm_value vm_tag_pap = 0x6;
const vm_value vm_tag_function = 0x7;
const vm_value vm_tag_match_data = 0xF;


// vm State
#define STACK_SIZE 255
static stack_frame stack[STACK_SIZE];
static int stack_pointer = 0;
static int program_pointer = 0;
static vm_value *const_table = 0;
static int const_table_length = 0;



#define check_ctable_index(x) if( (x) >= const_table_length || (x) < 0) { \
    printf("Ctable index out of bounds: %i at %i\n", (x), __LINE__ ); \
    return false; }

#define get_reg(i) stack[stack_pointer].reg[(i)]
#define current_frame (stack[stack_pointer])
#define next_frame (stack[stack_pointer + 1])

#define do_call(frame, func_reg, instr)         \
  int return_pointer;                 \
  bool call_failed = false;              \
  {                                   \
    int func = get_reg(func_reg);     \
    if(get_tag(func) != vm_tag_function) { \
      fprintf(stderr, "expected a function (do call)\n"); \
      call_failed = true;                \
    } \
    else {                            \
      int func_address = from_val(func); \
      if(frame != &next_frame) { \
        int num_args = get_arg_r2(instr); \
        memcpy(frame->reg, next_frame.reg, num_args * sizeof(vm_value)); \
      } \
      return_pointer = program_pointer; \
      program_pointer = func_address + fun_header_size; \
    } \
  }


// TODO turn this into a macro
// TODO add a result register to the tail-call variety of this opcode!! (right now it's pure coincidence that things work, because we set the missing return reg to 0 by default)
/*
  How to handle oversaturated calls:

  When a call is oversaturated, we set up the first call as normal. All additional
  arguments are stored on the heap, with a very simple format: First word is the
  number of stored arguments, followed by the arguments we want to store away. The
  heap address of that arg array is stored in a special field in the frame.
  The call is instructed to return to the gen_ap call and to store its result into
  the register that originally held the function.

  When returning to the gen_ap instruction, the field for oversaturated calls is checked.
  If it is set, the arguments are copied to the argument_frame and the number of arguments
  is set to the number of arguments we had stored. The frame field for oversaturated calls
  is reset to 0.

  After this the call proceeds as usual, possibly leading to another oversaturated call.


*/
int do_gen_ap(stack_frame *frame, vm_value instr, vm_instruction *program) {

  // TODO remove code duplication (in here, stack push, etc)

  // TODO find a better term for "function or closure" than lambda
  int lambda_reg = get_arg_r1(instr);
  int num_args = get_arg_r2(instr);


  if(current_frame.spilled_arguments != 0) {
    vm_value *addr = heap_get_pointer(current_frame.spilled_arguments);
    num_args = *addr;
    memcpy(next_frame.reg, addr + 1, num_args * sizeof(vm_value));
    printf("Args %d addr %d sp %d \n", num_args, (int) current_frame.spilled_arguments, stack_pointer);

    printf("Restoring\n");
    for(int i=0; i < num_args; ++i) {
      printf("%02d: %d\n", i, next_frame.reg[i]);
    }

    lambda_reg = get_arg_r0(instr);
    current_frame.spilled_arguments = 0;
  }


  vm_value lambda = get_reg(lambda_reg);

  vm_value tag = get_tag(lambda);
  if(tag == vm_tag_pap ) {

    heap_address cl_address = (heap_address)from_val(lambda);

    vm_value *cl_pointer = heap_get_pointer(cl_address);
    int header = *cl_pointer;
    int arity = closure_arity(header);
    int num_cl_vars = closure_var_count(header);

    if (num_args == arity) {
      memmove(&(frame->reg[num_cl_vars]), &(next_frame.reg[0]), num_args * sizeof(vm_value));
      memcpy(&(frame->reg[0]), cl_pointer + 1, num_cl_vars * sizeof(vm_value));

      // do the call
      vm_value func_address = *(cl_pointer + num_cl_vars + 1);
      int return_pointer = program_pointer;
      program_pointer = func_address + fun_header_size;
      return return_pointer;
    }
    // Undersaturated application
    else if (num_args < arity) {
      // create a new PAP by copying the old one and adding the new arguments

      vm_value func_address = *(cl_pointer + num_cl_vars + 1);
      vm_value reg0 = get_arg_r0(instr);

      heap_address new_cl_address = heap_alloc(num_cl_vars + num_args + 2); /* old args + new args + pap header + pointer to function */
      vm_value *new_cl_pointer = heap_get_pointer(new_cl_address);
      *new_cl_pointer = closure_header((arity - num_args), (num_cl_vars + num_args)); /* write header */
      memcpy(new_cl_pointer + 1, cl_pointer + 1, num_cl_vars * sizeof(vm_value));
      memcpy(&new_cl_pointer[num_cl_vars + 1], &(next_frame.reg[0]), num_args * sizeof(vm_value));
      *(new_cl_pointer + num_cl_vars + num_args + 1) = func_address;
      get_reg(reg0) = val( (vm_value) new_cl_address, vm_tag_pap );
      return -1;
    }
    // Oversaturated application
    else { // num_args > arity

      int num_remaining = num_args - arity;


      // store remaining args
      heap_address addr = heap_alloc(num_args + 1);
      vm_value *arg_pointer = heap_get_pointer(addr);

      *arg_pointer = num_remaining;
      memcpy(arg_pointer + 1, &(next_frame.reg[arity]), num_remaining * sizeof(vm_value));

      printf("Saving\n");
      for(int i=0; i < num_remaining; ++i) {
        printf("%02d: %d\n", i, arg_pointer[i + 3]);
      }

      //TODO this needs to be the current frame in all cases
      current_frame.spilled_arguments = addr;

      printf("Spilling %d args, addr %d, sp %d \n", num_remaining, (int) addr, stack_pointer);


      // set arguments
      memmove(&(next_frame.reg[num_cl_vars]), &(next_frame.reg[0]), arity * sizeof(vm_value));
      memcpy(&(next_frame.reg[0]), cl_pointer + 1, num_cl_vars * sizeof(vm_value));

      // do the call
      vm_value func_address = *(cl_pointer + num_cl_vars + 1);
      int return_pointer = program_pointer - 1; //Return back to this instruction
      program_pointer = func_address + fun_header_size;


      next_frame.return_address = return_pointer;
      next_frame.result_register = get_arg_r0(instr);
      ++stack_pointer;

      return -1;

    }
  }
  else if (tag == vm_tag_function) {

    int func_address = from_val(lambda);
    vm_instruction fun_header = program[func_address];
    //TODO check fun header "opcode"
    int arity = get_arg_i(fun_header);

    if (num_args == arity) {

      do_call(frame, lambda_reg, instr);
      if(call_failed) {
        fprintf(stderr, "Call failed (not a function)\n");
        return -1; //TODO exit here?
      }
      return return_pointer;
    }
    else if (num_args < arity) {
      // same case as part_ap TODO use same code
      int reg0 = get_arg_r0(instr);

      //vm_value function_header = program[func_address];
      //TODO check that it's actually a function

      heap_address cl_address = heap_alloc(num_args + 2); /* args + pap header + pointer to function */
      vm_value *cl_pointer = heap_get_pointer(cl_address);
      *cl_pointer = closure_header((arity - num_args), num_args); /* write header */
      memcpy(cl_pointer + 1, next_frame.reg, num_args * sizeof(vm_value));
      *(cl_pointer + num_args + 1) = func_address;
      get_reg(reg0) = val( (vm_value) cl_address, vm_tag_pap);
    }
    else { // over-saturated call
      printf("over\n");
      int num_remaining = num_args - arity;

      // store remaining args
      heap_address addr = heap_alloc(num_args + 1);
      vm_value *arg_pointer = heap_get_pointer(addr);

      *arg_pointer = num_remaining;
      memcpy(arg_pointer + 1, &next_frame.reg[arity], num_remaining * sizeof(vm_value));
      current_frame.spilled_arguments = addr;

      int oversat_ret_pointer = program_pointer - 1; //Return back to this instruction
      do_call((&next_frame), lambda_reg, instr);

      next_frame.return_address = oversat_ret_pointer;
      next_frame.result_register = get_arg_r0(instr);
      ++stack_pointer;

      return -1;
    }
  }
  else {
    fprintf(stderr, "Expected a function: %i (gen ap)\n", tag);
    //exit(-1);
  }

  return -1;
}



// TODO try to do this without recursive function calls, then turn into a macro
bool is_equal(vm_value l, vm_value r) {

  vm_value l_tag = get_tag(l);

  if(l_tag != get_tag(r)) {
    return false;
  }

  if (l_tag == vm_tag_plain_symbol || l_tag == vm_tag_number) {
    if(l != r) {
      return false;
    }
    return true;
  }

  if ( l_tag == vm_tag_compound_symbol ) {
    int l_addr = from_val(l);
    int r_addr = from_val(r);
    vm_value l_header = const_table[l_addr];
    vm_value r_header = const_table[r_addr];

    int count = compound_symbol_count(l_header);
    if( (compound_symbol_id(l_header) != compound_symbol_id(r_header))
        || (count != compound_symbol_count(r_header)) ) {
      return false;
    }

    for(int i = 1; i < count + 1; ++i) {
      if(! is_equal(const_table[l_addr + i], const_table[r_addr + i])) {
        return false;
      }
    }

    return true;
  }

  return false;
}


// TODO can we inline this?
bool does_value_match(vm_value pat, vm_value subject, int start_register) {

  vm_value pat_tag = get_tag(pat);

  // TODO only write to register if it actually matches?
  if(pat_tag == vm_tag_match_data && !(pat & __single_bit(1, 5) )) {
    //capturing match
    int relative_reg = from_match_value(pat);
    get_reg(start_register + relative_reg) = subject;
    return true;
  }

  if(pat_tag != get_tag(subject)) {
    return false;
  }

  switch(pat_tag) {
    case vm_tag_number:
    case vm_tag_plain_symbol:
      return pat == subject;

    case vm_tag_compound_symbol: {
      vm_value pat_address = from_val(pat);

      check_ctable_index(pat_address)
      vm_value pat_header = const_table[pat_address];
      vm_value pat_id = compound_symbol_id(pat_header);

      vm_value subject_address = from_val(subject);

      check_ctable_index(subject_address)
      vm_value subject_header = const_table[subject_address];
      vm_value subject_id = compound_symbol_id(subject_header);
      if(pat_id != subject_id) {
        return false;
      }

      vm_value pat_count = compound_symbol_count(pat_header);
      vm_value subject_count = compound_symbol_count(subject_header);

      if(pat_count != subject_count) {
        return false;
      }

      int i=0;
      for(; i<pat_count; ++i) {
        int rel_pat_address = pat_address + 1 + i;
        int rel_subject_address = subject_address + 1 + i;

        check_ctable_index(rel_pat_address);
        check_ctable_index(rel_subject_address);
        if( !does_value_match(const_table[rel_pat_address], const_table[rel_subject_address], start_register) ) {
          return false;
        }
      }

      return true;
    }

    default:
      return false;
  }
}


void print_registers(stack_frame frame) {
  int i;
  const int num_displayed_regs = 5;
  const int reg_display_size = 16;
  char buffer[num_displayed_regs * (reg_display_size + 1) + 1] = {0};
  for(i=0; i<num_displayed_regs; ++i) {
    sprintf(&buffer[i*(reg_display_size + 1)], "%016x ", frame.reg[i]);
  }
  printf("regs: %s\n", buffer);
}

void reset() {
  program_pointer = 0;
  stack_pointer = 0;
  const_table = 0;
  const_table_length = 0;
  memset(stack, 0x0, sizeof(stack_frame) * STACK_SIZE); //TODO delete the memset line later, just for debugging
  init_heap();
}

void print_program(vm_instruction *program) {
  int i = 0;
  printf("----\n");
  for(i=0; i < 10; ++i) {
    printf("%04i: %016x\n", i, program[i]);
  }
  printf("^^^^\n");
}


vm_value vm_execute(vm_instruction *program, int program_length, vm_value *ctable, int ctable_length) {
  //print_program(program);
  ++ invocation;
  reset();
  const_table = ctable;
  const_table_length = ctable_length;
  bool is_running = true;

  debug( printf("----- start %d\n", invocation) );
  while(is_running && program_pointer < program_length) {
    //debug( print_registers(current_frame) );
    int old_program_pointer = program_pointer;
    ++program_pointer;

    is_running = true;
    vm_value instr = program[old_program_pointer];
    vm_opcode opcode = get_opcode(instr);

    switch (opcode) {

      case OP_LOAD_i: {
        int reg0 = get_arg_r0(instr);
        int val = get_arg_i(instr);
        get_reg(reg0) = val;
        debug( printf("LOADi  r%02i #%i\n", reg0, val) );
      }
      break;


      case OP_LOAD_ps: {
        int reg0 = get_arg_r0(instr);
        int value = get_arg_i(instr);
        get_reg(reg0) = val(value, vm_tag_plain_symbol);
        debug( printf("LOADss  r%02i #%i\n", reg0, value) );
      }
      break;


      case OP_LOAD_cs: {
        int reg0 = get_arg_r0(instr);
        int value = get_arg_i(instr);
        get_reg(reg0) = val(value, vm_tag_compound_symbol);
        debug( printf("LOADcs r%02i #%i\n", reg0, value) );
      }
      break;


      case OP_LOAD_c: {
        int reg1 = get_arg_r0(instr);
        int table_index = get_arg_i(instr);

        check_ctable_index(table_index)
        get_reg(reg1) = const_table[table_index];
        debug( printf("LOADc  r%02i #%i value: %i\n", reg1, table_index, const_table[table_index]) );
      }
      break;

      case OP_LOAD_f: {
        int reg0 = get_arg_r0(instr);
        int value = get_arg_i(instr);
        get_reg(reg0) = val(value, vm_tag_function);
        debug( printf("LOADf  r%02i #%i\n", reg0, value) );
      }
      break;

      case OP_ADD: {
        int reg1 = get_arg_r1(instr);
        int reg2 = get_arg_r2(instr);
        int arg1 = get_reg(reg1);
        int arg2 = get_reg(reg2);
        int reg0 = get_arg_r0(instr);
        get_reg(reg0) = arg1 + arg2;
        debug( printf("ADD    r%02i r%02i=%x r%02i=%x\n", reg0, reg1, arg1, reg2, arg2) );
      }
      break;


      case OP_SUB: {
        int reg1 = get_arg_r1(instr);
        int reg2 = get_arg_r2(instr);
        int arg1 = get_reg(reg1);
        int arg2 = get_reg(reg2);
        int reg0 = get_arg_r0(instr);
        get_reg(reg0) = arg1 - arg2;
        debug( printf("SUB    r%02i r%02i=%x r%02i=%x\n", reg0, reg1, arg1, reg2, arg2) );
      }
      break;


      case OP_MUL: {
        int reg1 = get_arg_r1(instr);
        int reg2 = get_arg_r2(instr);
        int arg1 = get_reg(reg1);
        int arg2 = get_reg(reg2);
        int reg0 = get_arg_r0(instr);
        get_reg(reg0) = arg1 * arg2;
        debug( printf("MUL    r%02i r%02i r%02i\n", reg0, reg1, reg2) );
      }
      break;


      case OP_DIV: {
        int reg1 = get_arg_r1(instr);
        int reg2 = get_arg_r2(instr);
        int arg1 = get_reg(reg1);
        int arg2 = get_reg(reg2);
        int reg0 = get_arg_r0(instr);
        get_reg(reg0) = arg1 / arg2;
        debug( printf("MUL    r%02i r%02i r%02i\n", reg0, reg1, reg2) );
      }
      break;


      case OP_MOVE: {
        int reg0 = get_arg_r0(instr);
        int reg1 = get_arg_r1(instr);
        get_reg(reg0) = get_reg(reg1);
        debug( printf("MOVE   r%02i r%02i\n", reg0, reg1) );
      }
      break;


      case OP_CALL: {
        if (stack_pointer + 1 == STACK_SIZE) {
          printf("STACK OVERFLOW (call)!\n");
          is_running = false;
          break;
        }

        // this macro will create `return_pointer`
        do_call((&next_frame), get_arg_r1(instr), instr);
        if (call_failed) {
          is_running = false;
          break;
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
          is_running = false;
        }

        debug( printf("TL CALL\n") );
      }
      break;


      case OP_GEN_AP: {
        if (stack_pointer + 1 == STACK_SIZE) {
          printf("Stack overflow (call cl)!\n");
          is_running = false;
          break;
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
        int offset = get_arg_i(instr);
        program_pointer += offset;
        debug( printf("JMP %i\n", offset) );
      }
      break;


      case OP_MATCH: {
        int subject = get_reg(get_arg_r0(instr));
        int patterns_addr = get_reg(get_arg_r1(instr));
        int capture_reg = get_arg_r2(instr);

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
          is_running = false;
          break;
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
        vm_value closure = get_reg(cl_reg);

        if( get_tag(closure) != vm_tag_pap ) {
          fprintf(stderr, "Expected a closure!\n");
          is_running = false;
          break;
        }

        heap_address cl_address = from_val(closure);
        vm_value new_value = get_reg(get_arg_r1(instr));
        int arg_index = get_arg_r2(instr);

        vm_value *cl_pointer = heap_get_pointer(cl_address);
        int header = *cl_pointer;
        int num_env_args = closure_var_count(header);
        if(arg_index >= num_env_args) {
          fprintf(stderr, "Illegal closure modification (index: %i, num env vars: %i)\n", arg_index, num_env_args);
          is_running = false;
          break;
        }
        cl_pointer[arg_index + 1] = new_value;

        debug( printf("SETCLARG r%02i r%02i n%02i\n", get_arg_r0(instr), get_arg_r1(instr), arg_index) );
      }
      break;


      // TODO delete make_cl and use this instead
      // TODO allow gen_ap and tail_gen_ap to create PAPs
      case OP_PART_AP: {
        int reg0 = get_arg_r0(instr);
        int func_reg = get_arg_r1(instr);
        int func = get_reg(func_reg);

        if( get_tag(func) != vm_tag_function ) {
          fprintf(stderr, "Expected a function (op_part_ap)\n");
          is_running = false;
          break;
        }

        int func_address = from_val(func);
        int num_args = get_arg_r2(instr);

        vm_value function_header = program[func_address];
        //TODO check that it's actually a function
        int num_params = get_arg_i(function_header);

        // TODO this was >= earlier, which apparently gave false positives. Find out why, and find out if > is the correct choice
        if(num_args > num_params) {
          fprintf(stderr, "Illegal partial application (num args: %i, num params: %i)\n", num_args, num_params);
          is_running = false;
          break;
        }

        heap_address cl_address = heap_alloc(num_args + 2); /* args + pap header + pointer to function */
        vm_value *cl_pointer = heap_get_pointer(cl_address);
        *cl_pointer = closure_header((num_params - num_args), num_args); /* write header */
        memcpy(cl_pointer + 1, &next_frame.reg[0], num_args * sizeof(vm_value));
        *(cl_pointer + num_args + 1) = func_address;
        get_reg(reg0) = val( (vm_value) cl_address, vm_tag_pap);
        debug( printf("PART_AP\n") );
      }
      break;

      case OP_EQ: {
        vm_value l = get_reg(get_arg_r1(instr));
        vm_value r = get_reg(get_arg_r2(instr));
        int result_reg = get_arg_r0(instr);

        if( is_equal(l, r)) {
          get_reg(result_reg) = val(symbol_id_true, vm_tag_plain_symbol);
        }
        else {
          get_reg(result_reg) = val(symbol_id_false, vm_tag_plain_symbol);
        }
      }
      break;


      default:
        fprintf(stderr, "UNKNOWN OPCODE: %04x\n", opcode);
        is_running = false;
        break;
    }


    //debug( print_registers(current_frame) );
  }

  //fprintf(stderr, "End invocation: %i\n", invocation);

  vm_value result = stack[stack_pointer].reg[0];
  debug( printf("Result: %u\n", result) );
  return result;
}




