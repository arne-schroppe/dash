#ifndef _INCLUDE_HEAP_H
#define _INCLUDE_HEAP_H

#include <stdint.h>
#include <stdlib.h>
#include "vm.h"

typedef size_t heap_address;

heap_address heap_alloc(size_t size);
vm_value *heap_get_pointer(heap_address addr);


#endif
