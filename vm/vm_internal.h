#ifndef _INCLUDE_VM_INTERNAL_H
#define _INCLUDE_VM_INTERNAL_H

#include "vm.h"

#define num_regs 32

extern const int max_integer;
extern const int number_bias;

struct _stack_frame {
  vm_value reg[num_regs];
  int return_address;
  int result_register;

  //TODO type for this should be heap_address
  int spilled_arguments; // Used for over-saturated calls.
};
typedef struct _stack_frame stack_frame;


#define __tag_bits 4

// reading values

#define __tag_mask(t) ((t & 0xF) << (sizeof(vm_value) * 8 - __tag_bits))
#define make_tagged_val(x, t) (x | __tag_mask(t))
#define get_val(v) (v & 0x0FFFFFFF)
#define get_tag(x) (x >> (sizeof(vm_value) * 8 - __tag_bits))

#define pap_header(arity, num_vars) ((arity << 16) | num_vars)
#define pap_arity(header) ((header & 0xFFFF0000) >> 16)
#define pap_var_count(header) (header & 0xFFFF)

#define compound_symbol_header(id, n) ((id << 16) | n)
#define compound_symbol_id(header) ((header & 0xFFFF0000) >> 16)
#define compound_symbol_count(header) (header & 0xFFFF)

#define from_match_value(v) (v & 0x7FFFFFF)
//TODO rename match to something with pattern


// creating values

#define number(x) get_val(x, vm_type_number)
#define plain_symbol(x) get_val(x, vm_type_plain_symbol)
#define compound_symbol(x) get_val(x, vm_type_compound_symbol)

// In addition to the usual tag, match data also uses the bit after the tag (currently the
// fifth bit from the left) to encode additional information. If the bit is set, the value
// is a match header. If it isn't set, it is a variable to be captured. The wildcard ("_")
// has a special value of 0xFFFFFFF.


#define match_wildcard_value 0xFFFFFFF
#define __single_bit(b, n) (b << (sizeof(vm_value) * 8 - n))
#define __match_data_mask(t, n) ( __tag_mask(vm_tag_match_data) | __single_bit(t, 5) | (n & 0x7FFFFFF) )
#define match_header(n) __match_data_mask(1, n)
#define match_wildcard __match_data_mask(0, match_wildcard_value)
#define match_var(n) __match_data_mask(0, n)

#endif
