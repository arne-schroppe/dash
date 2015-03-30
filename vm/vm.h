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
};
typedef struct _stack_frame stack_frame;




//TODO add size parameters
vm_value vm_execute(vm_instruction *program, int program_length, vm_value *const_table, int const_table_length);


typedef enum {
  vm_type_invalid,
  vm_type_number,
  vm_type_symbol,
  vm_type_data_symbol,
} vm_type;

vm_type type_of_value(vm_value value);

extern const vm_value vm_tag_number;
extern const vm_value vm_tag_symbol;
extern const vm_value vm_tag_data_symbol;

extern const vm_value vm_tag_match_data;

#define __tag_mask(t) ((t & 0xF) << (sizeof(vm_value) * 8 - 4))
#define val(x, t) (x | __tag_mask(t))
#define from_val(x, t) (x & ~__tag_mask(t)) //TODO mask instead

#define number(x) val(x, vm_type_number)
#define symbol(x) val(x, vm_type_symbol)
#define data_symbol(x) val(x, vm_type_data_symbol)

#define data_symbol_header(id, n) ((id << 16) | n)
#define data_symbol_id(header) ((header & 0xFFFF0000) >> 16)
#define data_symbol_count(header) (header & 0xFFFF)


#define __single_bit(v, n) ((v & 1) << (sizeof(vm_value) * 8 - n)) //this is super weird
#define __match_data_mask(t, n) ( __tag_mask(vm_tag_match_data) | __single_bit(t, 5) | (n & 0x7FFFFFF) )
#define match_header(n) __match_data_mask(1, n)
#define match_wildcard __match_data_mask(0, 0xFFFFFFF)
#define match_var(n) __match_data_mask(0, n)

#define from_match_value(v) (v & 0x7FFFFFF)
//TODO rename match to something with pattern

#endif
