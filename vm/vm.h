#ifndef _INCLUDE_VM_H
#define _INCLUDE_VM_H

#include <stdint.h>



typedef uint32_t vm_instruction;
typedef uint32_t vm_value;



vm_value vm_execute(vm_instruction *program, int program_length, vm_value *const_table, int const_table_length);


extern const vm_value vm_tag_number;
extern const vm_value vm_tag_plain_symbol;
extern const vm_value vm_tag_compound_symbol;

extern const vm_value vm_tag_closure;
extern const vm_value vm_tag_function;

extern const vm_value vm_tag_match_data;



#endif
