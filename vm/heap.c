#include "heap.h"
#include "gc.h"
#include <stdlib.h>
#include <stdio.h>

static vm_value *heap = 0;
static vm_value *other_heap = 0;
static size_t heap_size = 2048;
static heap_address next_free_address = 0;

static void run_gc();

void heap_init() {
  if (heap) {
    free(heap);
  }
  if (other_heap) {
    free(other_heap);
  }
  next_free_address = 1; // address 0 is reserved to indicate that no address has been set
  heap = calloc(heap_size, sizeof(vm_value));
  other_heap = calloc(heap_size, sizeof(vm_value));
}

heap_address heap_alloc(size_t size) {
  heap_address old = next_free_address;
  next_free_address += size;
  if(next_free_address >= heap_size) {

    run_gc();

    if(next_free_address >= heap_size) {
      printf("Out of memory!\n");
      exit(-1);
    }
    //TODO update returned address
  }
  return old;
}

vm_value *heap_get_pointer(heap_address addr) {
  if(addr > heap_size) {
    printf("Illegal memory address: %zu!\n", addr);
    exit(-1);
  }
  return &heap[addr];
}

static void swap_pointers(vm_value **p1, vm_value **p2) {
  vm_value *temp = *p1;
  *p2 = *p1;
  *p1 = temp;
}

static void run_gc() {
  next_free_address = gc_collect(heap, other_heap, heap_size);
  swap_pointers(&heap, &other_heap);
}

