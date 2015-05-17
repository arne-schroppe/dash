#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "vm.h"
#include "opcodes.h"
#include "heap.h"


#ifdef VM_DEBUG
#  define debug(x) do { x; } while (0)
#else
#  define debug(x) do {} while (0)
#endif

const vm_value vm_tag_number = 0x0;
const vm_value vm_tag_plain_symbol = 0x4;
const vm_value vm_tag_compound_symbol = 0x5;
const vm_value vm_tag_match_data = 0xF;

static vm_value *const_table = 0;
static int const_table_length = 0;


#define STACK_SIZE 255
static stack_frame stack[STACK_SIZE];
static int stack_pointer = 0;
static int program_pointer = 0;

static vm_value arg_reg[NUM_REGS];

#define check_ctable_index(x) if( (x) >= const_table_length || (x) < 0) { \
    printf("Ctable index out of bounds: %i at %i\n", (x), __LINE__ ); \
    return false; }




#define get_reg(i) stack[stack_pointer].reg[(i)]

#define current_frame stack[stack_pointer]
#define next_frame stack[stack_pointer + 1]

#define get_tag(x) (x >> (sizeof(vm_value) * 8 - __tag_bits))

void print_registers(stack_frame frame);
bool does_value_match(vm_value pat, vm_value subject, int start_reg);


bool execute_instruction(vm_instruction instr) {
  vm_opcode opcode = get_opcode(instr);

  switch (opcode) {

    case OP_LOADi: {
      int reg0 = get_arg_r0(instr);
      int val = get_arg_i(instr);
      get_reg(reg0) = val;
      debug( printf("LOADi  r%02i #%i\n", reg0, val) );
    }
    break;

    case OP_LOADps: {
      int reg0 = get_arg_r0(instr);
      int value = get_arg_i(instr);
      get_reg(reg0) = val(value, vm_tag_plain_symbol);
      debug( printf("LOADss  r%02i #%i\n", reg0, value) );
    }
    break;

    case OP_LOADcs: {
      int reg0 = get_arg_r0(instr);
      int value = get_arg_i(instr);
      get_reg(reg0) = val(value, vm_tag_compound_symbol);
      debug( printf("LOADcs r%02i #%i\n", reg0, value) );
    }
    break;

    case OP_LOADc: {
      int reg1 = get_arg_r0(instr);
      int table_index = get_arg_i(instr);

      check_ctable_index(table_index)
      get_reg(reg1) = const_table[table_index];
      debug( printf("LOADc  r%02i #%i value: %i\n", reg1, table_index, const_table[table_index]) );
    }
    break;

    case OP_ADD: {
      int reg1 = get_arg_r1(instr);
      int reg2 = get_arg_r2(instr);
      int arg1 = get_reg(reg1);
      int arg2 = get_reg(reg2);
      int reg0 = get_arg_r0(instr);
      get_reg(reg0) = arg1 + arg2;
      debug( printf("ADD    r%02i r%02i r%02i\n", reg0, reg1, reg2) );
    }
    break;

    case OP_SUB: {
      int reg1 = get_arg_r1(instr);
      int reg2 = get_arg_r2(instr);
      int arg1 = get_reg(reg1);
      int arg2 = get_reg(reg2);
      int reg0 = get_arg_r0(instr);
      get_reg(reg0) = arg1 - arg2;
      debug( printf("SUB    r%02i r%02i r%02i\n", reg0, reg1, reg2) );
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
        printf("Stack overflow!\n");
        return false;
      }
      int func_address_reg = get_arg_r1(instr);
      int func_address = get_reg(func_address_reg);

                    printf("CALL %i %i\n", func_address, program_pointer);
      int num_args = get_arg_r2(instr);
      memcpy(&next_frame.reg[0], &arg_reg[0], num_args * sizeof(vm_value));
      next_frame.return_address = program_pointer;
      next_frame.result_register = get_arg_r0(instr);
      debug( printf("CALL   r%02i r%02i r%02i\n", get_arg_r0(instr), func_address_reg, num_args) );
      program_pointer = func_address;
      ++stack_pointer;
    }
    break;


    case OP_TAIL_CALL: {
                    printf("TAIL CALL %i %i \n", (current_frame.return_address), program_pointer);
      int func_address_reg = get_arg_r1(instr);
      int func_address = get_reg(func_address_reg);
      int num_args = get_arg_r2(instr);
      memcpy(&current_frame.reg[0], &arg_reg[0], num_args * sizeof(vm_value));
      // next_frame.return_address = program_pointer;
      // next_frame.result_register = get_arg_r0(instr);
      debug( printf("TL CALL   r%02i r%02i r%02i\n", get_arg_r0(instr), func_address_reg, num_args) );
      program_pointer = func_address;
      // ++stack_pointer;
    }
    break;

    case OP_CALLCL: {
                    printf("CALL CL\n");
      if (stack_pointer + 1 == STACK_SIZE) {
        printf("Stack overflow!\n");
        return false;
      }
      int cl_address_reg = get_arg_r1(instr);
      heap_address cl_address = (heap_address)get_reg(cl_address_reg);
      int num_args = get_arg_r2(instr);

      memcpy(&next_frame.reg[0], &arg_reg[0], num_args * sizeof(vm_value));
      next_frame.return_address = program_pointer;
      next_frame.result_register = get_arg_r0(instr);

      vm_value *cl_pointer = heap_get_pointer(cl_address);
      int num_env_args = *cl_pointer;
      memcpy(&next_frame.reg[num_args], cl_pointer + 1, num_env_args * sizeof(vm_value));
      vm_value func_address = *(cl_pointer + num_env_args + 1);

      debug( printf("CALLCL r%02i r%02i r%02i\n", get_arg_r0(instr), cl_address_reg, num_args) );
      program_pointer = func_address;
      ++stack_pointer;
    }
    break;

    case OP_TAIL_CALLCL: {
                    printf("TAIL CALL CL %i %i \n", (current_frame.return_address), program_pointer);
      int cl_address_reg = get_arg_r1(instr);
      heap_address cl_address = (heap_address)get_reg(cl_address_reg);
      int num_args = get_arg_r2(instr);

      memcpy(&current_frame.reg[0], &arg_reg[0], num_args * sizeof(vm_value));
      // next_frame.return_address = program_pointer;
      // next_frame.result_register = get_arg_r0(instr);

      vm_value *cl_pointer = heap_get_pointer(cl_address);
      int num_env_args = *cl_pointer;
      memcpy(&current_frame.reg[num_args], cl_pointer + 1, num_env_args * sizeof(vm_value));
      vm_value func_address = *(cl_pointer + num_env_args + 1);

      debug( printf("TL CALLCL r%02i r%02i r%02i\n", get_arg_r0(instr), cl_address_reg, num_args) );
      program_pointer = func_address;
      // ++stack_pointer;
    }
    break;


    case OP_MAKECL: {
      int reg0 = get_arg_r0(instr);
      int func_address_reg = get_arg_r1(instr);
      int func_address = get_reg(func_address_reg);
      int num_args = get_arg_r2(instr);

                    printf("MAKE CL %i\n", func_address);
      heap_address cl_address = heap_alloc(num_args + 2); /* args + closure header + pointer to function */
      vm_value *cl_pointer = heap_get_pointer(cl_address);
      *cl_pointer = num_args; /* write header */
      memcpy(cl_pointer + 1, &arg_reg[0], num_args * sizeof(vm_value));
      *(cl_pointer + num_args + 1) = func_address;
      get_reg(reg0) = (vm_value) cl_address;
      debug( printf("MAKECL r%02i r%02i r%02i\n", reg0, func_address_reg, num_args) );
    }
    break;

    case OP_RET: {
      int return_val_reg = get_arg_r0(instr);
      if (stack_pointer == 0) {
        //We simply copy the result value to register 0, so that the runtime can find it
        current_frame.reg[0] = current_frame.reg[return_val_reg];
        return false;
      }
      --stack_pointer;
      current_frame.reg[next_frame.result_register] = next_frame.reg[return_val_reg];
      debug( printf("RET\n") );
      program_pointer = next_frame.return_address;
      printf("RET %i\n", program_pointer);
    }
    break;

    case OP_JMP: {
      int offset = get_arg_i(instr);
      program_pointer += offset;
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
        return false;
      }

      program_pointer += i;
    }
    break;

    case OP_SETARG: {
      int target_arg = get_arg_r0(instr);
      int source_reg = get_arg_r1(instr);
      int extra_amount = get_arg_r2(instr);
      memcpy(&arg_reg[target_arg], &current_frame.reg[source_reg], (1 + extra_amount) * sizeof(vm_value));
      debug( printf("SETARG a%02i r%02i n%02i\n", target_arg, source_reg, extra_amount) );
    }
    break;

    default:
      printf("UNKNOWN OPCODE: %04x\n", opcode);
      exit(-1); //TODO exit gracefully?
      break;

  }

  return true;

}

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
      vm_value pat_address = from_val(pat, vm_tag_compound_symbol);

      check_ctable_index(pat_address)
      vm_value pat_header = const_table[pat_address];
      vm_value pat_id = compound_symbol_id(pat_header);

      vm_value subject_address = from_val(subject, vm_tag_compound_symbol);

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
  for(i=0; i<num_displayed_regs; ++i) {
    sprintf(&buffer[i*(reg_display_size + 1)], "%016x ", arg_reg[i]);
  }
  printf("args: %s\n", buffer);
}

void reset() {
  program_pointer = 0;
  stack_pointer = 0;
  const_table = 0;
  const_table_length = 0;
  memset(stack, 0, sizeof(stack_frame) * STACK_SIZE);
  memset(arg_reg, 0, sizeof(vm_value) * NUM_REGS);
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
  reset();
  const_table = ctable;
  const_table_length = ctable_length;
  bool is_running = true;

  while(is_running && program_pointer < program_length) {
    debug( printf("-----\n") );
    debug( print_registers(current_frame) );
    int old_program_pointer = program_pointer;
    ++program_pointer;
    is_running = execute_instruction(program[old_program_pointer]);
    debug( print_registers(current_frame) );
  }
  vm_value result = stack[stack_pointer].reg[0];
  debug( printf("Result: %llu\n", result) );
  return result;
}



vm_type type_of_value(vm_value value) {
  switch (get_tag(value)) {
    case vm_tag_number:
      return vm_type_number;

    case vm_tag_plain_symbol:
      return vm_type_plain_symbol;

    case vm_tag_compound_symbol:
      return vm_type_compound_symbol;

    default:
      return vm_type_invalid;
  }
}


