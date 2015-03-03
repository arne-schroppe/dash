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
const vm_value vm_tag_symbol = 0x4;
const vm_value vm_tag_data_symbol = 0x5;
const vm_value vm_tag_match_data = 0xF;

static vm_value *const_table = 0;


#define STACK_SIZE 255
static stack_frame stack[STACK_SIZE];
static int stack_pointer = 0;
static int program_pointer = 0;




#define get_reg(i) stack[stack_pointer].reg[(i)]

#define current_frame stack[stack_pointer]
#define next_frame stack[stack_pointer + 1]

#define get_tag(x) (x >> (sizeof(vm_value) * 8 - 4))

void print_registers(stack_frame frame);
bool does_value_match(vm_value pat, vm_value subject, int start_reg);


void execute_instruction(vm_instruction instr) {
  vm_opcode opcode = get_opcode(instr);

  switch (opcode) {

    case OP_LOADi: {
      int reg0 = get_arg_r0(instr);
      int val = get_arg_i(instr);
      get_reg(reg0) = val;
      debug( printf("LOADi  r%02i #%i\n", reg0, val) );
    }
    break;

    case OP_LOADs: {
      int reg0 = get_arg_r0(instr);
      int value = get_arg_i(instr);
      get_reg(reg0) = val(value, vm_tag_symbol);
      debug( printf("LOADs  r%02i #%i\n", reg0, val) );
    }
    break;

    case OP_LOADsd: {
      int reg0 = get_arg_r0(instr);
      int value = get_arg_i(instr);
      get_reg(reg0) = val(value, vm_tag_data_symbol);
      debug( printf("LOADsd r%02i #%i\n", reg0, val) );
    }
    break;

    case OP_LOADc: {
      int reg1 = get_arg_r0(instr);
      int table_index = get_arg_i(instr);
      get_reg(reg1) = const_table[table_index];
      debug( printf("LOADc  r%02i #%i\n", reg1, val) );
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
      int func_address_reg = get_arg_r1(instr);
      int func_address = get_reg(func_address_reg);
      int num_args = get_arg_r2(instr);
      memcpy(&next_frame.reg[1], &current_frame.reg[func_address_reg + 1], num_args * sizeof(vm_value));
      next_frame.return_address = program_pointer;
      next_frame.result_register = get_arg_r0(instr);
      debug( printf("CALL   r%02i r%02i r%02i\n", get_arg_r0(instr), func_address_reg, num_args) );
      program_pointer = func_address;
      ++stack_pointer;
    }
    break;

    case OP_CALLCL: {
      int cl_address_reg = get_arg_r1(instr);
      heap_address cl_address = (heap_address)get_reg(cl_address_reg);
      int num_args = get_arg_r2(instr);

      memcpy(&next_frame.reg[1], &current_frame.reg[cl_address_reg + 1], num_args * sizeof(vm_value));
      next_frame.return_address = program_pointer;
      next_frame.result_register = get_arg_r0(instr);

      vm_value *cl_pointer = heap_get_pointer(cl_address);
      int num_env_args = *cl_pointer;
      memcpy(&next_frame.reg[num_args + 1], cl_pointer + 1, num_env_args * sizeof(vm_value));
      vm_value func_address = *(cl_pointer + num_env_args + 1);

      debug( printf("CALLCL r%02i r%02i r%02i\n", get_arg_r0(instr), cl_address_reg, num_args) );
      program_pointer = func_address;
      ++stack_pointer;
    }
    break;

    case OP_MAKECL: {
      int reg0 = get_arg_r0(instr);
      int func_address_reg = get_arg_r1(instr);
      int func_address = get_reg(func_address_reg);
      int num_args = get_arg_r2(instr); //TODO check that num_args > 0
      heap_address cl_address = heap_alloc(num_args + 2); /* args + closure header + pointer to function */
      vm_value *cl_pointer = heap_get_pointer(cl_address);
      *cl_pointer = num_args; /* write header */
      memcpy(cl_pointer + 1, &get_reg(func_address_reg + 1), num_args);
      *(cl_pointer + num_args + 1) = func_address;
      get_reg(reg0) = (vm_value) cl_address;
      debug( printf("MAKECL r%02i r%02i r%02i\n", reg0, func_address_reg, num_args) );
    }
    break;

    case OP_RET: {
      --stack_pointer;
      current_frame.reg[next_frame.result_register] = next_frame.reg[0];
      debug( printf("RET\n") );
      program_pointer = next_frame.return_address;
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
      vm_value match_header = const_table[patterns_addr];
      int number_of_patterns = from_match_value(match_header);
      int i = 0;

      for(i=0; i<number_of_patterns; ++i) {
        vm_value pat = const_table[patterns_addr + 1 + i];
        if(does_value_match(pat, subject, capture_reg)) {
          break;
        }
        else {
          continue;
        }
      }

      if(i == number_of_patterns) {
        fprintf(stderr, "Pattern match failed!\n");
        exit(-1); //TODO handle this more gracefuly
      }

      program_pointer += i;
    }
    break;

    default:
      printf("UNKNOWN OPCODE: %04x\n", opcode);
      exit(-1);
    break;

  }

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
    case vm_tag_symbol:
      return pat == subject;

    case vm_tag_data_symbol: {
      vm_value pat_address = from_val(pat, vm_tag_data_symbol);
      vm_value pat_header = const_table[pat_address];
      vm_value pat_id = data_symbol_id(pat_header);

      vm_value subject_address = from_val(subject, vm_tag_data_symbol);
      vm_value subject_header = const_table[subject_address];
      vm_value subject_id = data_symbol_id(subject_header);
      if(pat_id != subject_id) {
        return false;
      }

      vm_value pat_count = data_symbol_count(pat_header);
      vm_value subject_count = data_symbol_count(subject_header);

      if(pat_count != subject_count) {
        return false;
      }

      int i=0;
      for(; i<pat_count; ++i) {
        if( !does_value_match(const_table[pat_address + 1 + i], const_table[subject_address + 1 + i], start_register) ) {
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
  printf("%s\n", buffer);
}

void reset() {
  program_pointer = 0;
  stack_pointer = 0;
  const_table = 0;
  memset(stack, 0, sizeof(stack_frame) * STACK_SIZE);
}

void print_program(vm_instruction *program) {
  int i = 0;
  printf("----\n");
  for(i=0; i < 10; ++i) {
    printf("%04i: %016x\n", i, program[i]);
  }
  printf("^^^^\n");
}


vm_value vm_execute(vm_instruction *program, vm_value *const_table_arg) {
  //print_program(program);
  reset();
  const_table = const_table_arg;

  while(get_opcode(program[program_pointer]) != OP_HALT) {
    debug( printf("-----\n") );
    debug( print_registers(current_frame) );
    int old_program_pointer = program_pointer;
    ++program_pointer;
    execute_instruction(program[old_program_pointer]);
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

    case vm_tag_symbol:
      return vm_type_symbol;

    case vm_tag_data_symbol:
      return vm_type_data_symbol;

    default:
      return vm_type_invalid;
  }
}


