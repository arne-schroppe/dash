#ifndef _INCLUDE_VM_H
#define _INCLUDE_VM_H

#include <stdint.h>

typedef uint32_t vm_instruction;
typedef uint32_t vm_value;

// TODO find a way to get errors back into the calling code. also stop on error (i.e. pattern match fail) as long as we don't have exception handling
vm_value vm_execute(vm_instruction *program, int program_length, vm_value *const_table, int const_table_length);
vm_value *vm_get_heap_pointer(vm_value addr);

#endif

