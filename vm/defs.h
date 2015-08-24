#ifndef _INCLUDE_DEFS_H
#define _INCLUDE_DEFS_H


//Note: most of these a #defines so that they can be used as initializers

#define vm_tag_number 0x0
#define vm_tag_plain_symbol 0x4
#define vm_tag_compound_symbol 0x5
#define vm_tag_dynamic_compound_symbol 0x8
// partial applications and closures are currently the same
#define vm_tag_pap 0x6
#define vm_tag_function 0x7
#define vm_tag_string 0x9
#define vm_tag_dynamic_string 0xA
#define vm_tag_opaque_symbol 0xB
#define vm_tag_match_data 0xF

// match data will never appear on the heap, so we can reuse the tag.
// (Used by the garbace collector to mark the moved position of data.)
#define vm_tag_forward_pointer vm_tag_match_data;

#define symbol_id_false 0
#define symbol_id_true 1
#define symbol_id_io 2
#define symbol_id_eof 3
#define symbol_id_error 4

#define symbol_id_number 5
#define symbol_id_string 6
#define symbol_id_symbol 7
#define symbol_id_function 8


extern const int action_id_return;
extern const int action_id_readline;
extern const int action_id_printline;

extern const int fun_header_size;
extern const int pap_header_size;
extern const int compound_symbol_header_size;
extern const int string_header_size;

extern const int max_biased_int;
extern const int min_biased_int;
extern const int int_bias;

#define stack_size 255
#define num_regs 32

#define action_id_return 0
#define action_id_readline 1
#define action_id_printline 2

#endif

