#ifndef _INCLUDE_VM_INTERNAL_H
#define _INCLUDE_VM_INTERNAL_H

#include "vm.h"
#include "vm_tags.h"

extern const vm_value vm_tag_number;
extern const vm_value vm_tag_plain_symbol;
extern const vm_value vm_tag_compound_symbol;
extern const vm_value vm_tag_dynamic_compound_symbol;
extern const vm_value vm_tag_string;
extern const vm_value vm_tag_dynamic_string;
extern const vm_value vm_tag_pap; // partial applications and closures are currently the same
extern const vm_value vm_tag_function;
extern const vm_value vm_tag_opaque_symbol;
extern const vm_value vm_tag_match_data;

#define num_regs 32

extern const int max_integer;
extern const int int_bias;

struct _stack_frame {
  vm_value reg[num_regs];
  int return_address;
  int result_register;

  //TODO type for this should be heap_address
  int spilled_arguments; // Used for over-saturated calls.
};
typedef struct _stack_frame stack_frame;


#define __tag_bits 4

//TODO 14 bits for symbol id's is actually not that much if all module fields
//are encoded as symbols

#define low_28_bits 0x0FFFFFFF
#define low_27_bits 0x07FFFFFF
#define low_14_bits 0x3FFF
#define high_14_bits 0xFFFC000

#define __tag_mask(t) ((t & 0xF) << (sizeof(vm_value) * 8 - __tag_bits))
#define make_tagged_val(x, t) (x | __tag_mask(t))
#define get_val(v) (v & low_28_bits)
#define get_tag(x) (x >> (sizeof(vm_value) * 8 - __tag_bits))

#define pap_header(arity, num_vars) (make_tagged_val(((arity << 14) | num_vars), vm_tag_pap))
#define pap_arity(header) ((get_val(header) & high_14_bits) >> 14)
#define pap_var_count(header) (get_val(header) & low_14_bits)

#define compound_symbol_header(id, n) (make_tagged_val(((id << 14) | n), vm_tag_compound_symbol))
#define compound_symbol_id(header) ((get_val(header) & high_14_bits) >> 14)
#define compound_symbol_count(header) (get_val(header) & low_14_bits)

//opaque symbols use almost the same headers as compound symbols but have a second
//header field: The owner of this opaque symbol. The owner is again a symbol id
//because modules are also opaque symbols (see below).
#define opaque_symbol_header(id, n) (make_tagged_val(((id << 14) | n), vm_tag_opaque_symbol))

//Modules are implemented as opaque symbols. They have an internal name that can't
//be used in the program code, so you can't do pattern matching on modules. Their
//owner is always 0, this way we know that it is a module.


#define string_header(len, num_chunks) (make_tagged_val(((len << 14) | num_chunks), vm_tag_string))
#define string_length(header) ((get_val(header) & high_14_bits) >> 14)
#define string_chunk_count(header) (get_val(header) & low_14_bits)


#define number(x) get_val(x, vm_type_number)
#define plain_symbol(x) get_val(x, vm_type_plain_symbol)
#define compound_symbol(x) get_val(x, vm_type_compound_symbol)


// In addition to the usual tag, match data also uses the bit after the tag (currently the
// fifth bit from the left) to encode additional information. If the bit is set, the value
// is a match header. If it isn't set, it is a variable to be captured. The wildcard ("_")
// has a special value of 0xFFFFFFF (all low 28 bits set).
#define match_wildcard_value low_28_bits
#define __single_bit(b, n) (b << (sizeof(vm_value) * 8 - n))
#define __match_data_mask(t, n) ( __tag_mask(vm_tag_match_data) | __single_bit(t, 5) | (n & low_27_bits) )
#define match_header(n) __match_data_mask(1, n)
#define match_wildcard __match_data_mask(0, match_wildcard_value)
#define match_var(n) __match_data_mask(0, n)

#define from_match_value(v) (v & low_27_bits)
//TODO rename match to something with pattern




#endif
