#ifndef _INCLUDE_VM_H
#define _INCLUDE_VM_H

#include <stdint.h>



typedef uint32_t vm_instruction;
typedef uint32_t vm_value;



vm_value vm_execute(vm_instruction *program, int program_length, vm_value *const_table, int const_table_length);





#endif
