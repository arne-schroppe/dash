#ifndef _INCLUDE_VM_H
#define _INCLUDE_VM_H

#include <stdint.h>

#define NUM_REGS 32


typedef uint32_t vm_instruction;
typedef uint32_t vm_value;

struct _stack_frame {
  vm_value reg[NUM_REGS];
  int return_address;
  int result_register;
  int spilled_arguments_addr; // Used for over-saturated calls. TODO find a better way to handle this! (stack, vm refactoring,e tc)
};
typedef struct _stack_frame stack_frame;


vm_value vm_execute(vm_instruction *program, int program_length, vm_value *const_table, int const_table_length);


typedef enum {
  vm_type_invalid = 0,
  vm_type_number = 1,
  vm_type_plain_symbol = 2,
  vm_type_compound_symbol = 3,
  vm_type_closure = 4,
  vm_type_function = 5
} vm_type;

vm_type type_of_value(vm_value value);

extern const vm_value vm_tag_number;
extern const vm_value vm_tag_plain_symbol;
extern const vm_value vm_tag_compound_symbol;

extern const vm_value vm_tag_closure;
extern const vm_value vm_tag_function;

extern const vm_value vm_tag_match_data;


#define __tag_bits 4

#define __tag_mask(t) ((t & 0xF) << (sizeof(vm_value) * 8 - __tag_bits))
#define val(x, t) (x | __tag_mask(t))
#define from_val(x, t) (x & ~__tag_mask(t))   // TODO change from_val so that it just cuts away the tag
#define get_tag(x) (x >> (sizeof(vm_value) * 8 - __tag_bits))


#define number(x) val(x, vm_type_number)
#define plain_symbol(x) val(x, vm_type_plain_symbol)
#define compound_symbol(x) val(x, vm_type_compound_symbol)


#define closure_header(arity, num_vars) ((arity << 16) | num_vars)
#define closure_arity(header) ((header & 0xFFFF0000) >> 16)
#define closure_var_count(header) (header & 0xFFFF)

#define compound_symbol_header(id, n) ((id << 16) | n)
#define compound_symbol_id(header) ((header & 0xFFFF0000) >> 16)
#define compound_symbol_count(header) (header & 0xFFFF)


#define __single_bit(v, n) ((v & 1) << (sizeof(vm_value) * 8 - n)) //TODO this is super weird
#define __match_data_mask(t, n) ( __tag_mask(vm_tag_match_data) | __single_bit(t, 5) | (n & 0x7FFFFFF) )
#define match_header(n) __match_data_mask(1, n)
#define match_wildcard __match_data_mask(0, 0xFFFFFFF)
#define match_var(n) __match_data_mask(0, n)

#define from_match_value(v) (v & 0x7FFFFFF)
//TODO rename match to something with pattern

#endif
