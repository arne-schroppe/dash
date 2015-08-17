#include <stdio.h>
#include "vm_equality_spec.h"

#include "../vm_internal.h"
#include "../opcodes.h"
#include "../heap.h"
#include "../encoding.h"

#define array_length(x) (sizeof(x) / sizeof(x[0]))




it( compares_unequal_objects ) {
  vm_instruction program[] = {
    op_load_i(1, 11),
    op_load_ps(2, 11),
    op_eq(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));
}

it( compares_equal_numbers ) {
  vm_instruction program[] = {
    op_load_i(1, 11),
    op_load_i(2, 11),
    op_eq(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(1, vm_tag_plain_symbol));
}

it( compares_unequal_numbers ) {
  vm_instruction program[] = {
    op_load_i(1, 11),
    op_load_i(2, 22),
    op_eq(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));
}

it( compares_equal_plain_symbols ) {
  vm_instruction program[] = {
    op_load_ps(1, 11),
    op_load_ps(2, 11),
    op_eq(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(1, vm_tag_plain_symbol));
}

it( compares_unequal_plain_symbols ) {
  vm_instruction program[] = {
    op_load_ps(1, 11),
    op_load_ps(2, 22),
    op_eq(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));
}




// static equality
it( compares_equal_compound_symbols ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
  };

  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_eq(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(1, vm_tag_plain_symbol));
}

it( compares_compound_symbols_with_different_data ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(77, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_eq(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));
}

it( compares_compound_symbols_with_different_sym_ids ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(12, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_eq(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));
}

it( compares_compound_symbols_with_different_counts ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(11, 1),
    make_tagged_val(55, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_eq(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));
}

// Dynamic equality

it( compares_equal_dynamic_compound_symbols ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
  };

  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(3, 1),
    op_copy_sym(4, 2),
    op_eq(0, 3, 4),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(1, vm_tag_plain_symbol));
}

it( compares_dynamic_compound_symbols_with_different_data ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(77, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(3, 1),
    op_copy_sym(4, 2),
    op_eq(0, 3, 4),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));
}

it( compares_dynamic_compound_symbols_with_different_sym_ids ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(12, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(3, 1),
    op_copy_sym(4, 2),
    op_eq(0, 3, 4),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));
}

it( compares_dynamic_compound_symbols_with_different_counts ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(11, 1),
    make_tagged_val(55, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(3, 1),
    op_copy_sym(4, 2),
    op_eq(0, 3, 4),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));
}

// Dynamic and static equality
it( compares_equal_dynamic_and_static_compound_symbols ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
  };

  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(4, 2),
    op_eq(0, 1, 4),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(1, vm_tag_plain_symbol));

  vm_instruction program2[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(3, 1),
    op_eq(0, 3, 2),
    op_ret(0)
  };
  vm_value result2 = vm_execute(program2, array_length(program), const_table, array_length(const_table));
  is_equal(result2, make_tagged_val(1, vm_tag_plain_symbol));
}

it( compares_dynamic_and_static_compound_symbols_with_different_data ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(77, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(4, 2),
    op_eq(0, 1, 4),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));

  vm_instruction program2[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(3, 1),
    op_eq(0, 3, 2),
    op_ret(0)
  };
  vm_value result2 = vm_execute(program2, array_length(program), const_table, array_length(const_table));
  is_equal(result2, make_tagged_val(0, vm_tag_plain_symbol));
}

it( compares_dynamic_and_static_compound_symbols_with_different_sym_ids ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(12, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(4, 2),
    op_eq(0, 1, 4),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));

  vm_instruction program2[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(3, 1),
    op_eq(0, 3, 2),
    op_ret(0)
  };
  vm_value result2 = vm_execute(program2, array_length(program), const_table, array_length(const_table));
  is_equal(result2, make_tagged_val(0, vm_tag_plain_symbol));
}

it( compares_dynamic_and_static_compound_symbols_with_different_counts ) {
  vm_value const_table[] = {
    compound_symbol_header(11, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(11, 1),
    make_tagged_val(55, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(4, 2),
    op_eq(0, 1, 4),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(0, vm_tag_plain_symbol));

  vm_instruction program2[] = {
    op_load_cs(1, 0),
    op_load_cs(2, 3),
    op_copy_sym(3, 1),
    op_eq(0, 3, 2),
    op_ret(0)
  };
  vm_value result2 = vm_execute(program2, array_length(program), const_table, array_length(const_table));
  is_equal(result2, make_tagged_val(0, vm_tag_plain_symbol));
}


start_spec(vm_equality_spec)
  example(compares_unequal_objects)
  example(compares_equal_numbers)
  example(compares_unequal_numbers)
  example(compares_equal_plain_symbols)
  example(compares_unequal_plain_symbols)
  example(compares_equal_compound_symbols)
  example(compares_compound_symbols_with_different_data)
  example(compares_compound_symbols_with_different_sym_ids)
  example(compares_compound_symbols_with_different_counts)
  example(compares_equal_dynamic_compound_symbols)
  example(compares_dynamic_compound_symbols_with_different_data)
  example(compares_dynamic_compound_symbols_with_different_sym_ids)
  example(compares_dynamic_compound_symbols_with_different_counts)
  example(compares_equal_dynamic_and_static_compound_symbols)
  example(compares_dynamic_and_static_compound_symbols_with_different_data)
  example(compares_dynamic_and_static_compound_symbols_with_different_sym_ids)
  example(compares_dynamic_and_static_compound_symbols_with_different_counts)
end_spec

