#include <stdio.h>
#include "vm_spec.h"

#include "../vm.h"
#include "../opcodes.h"

#define array_length(x) (sizeof(x) / sizeof(x[0]))

it( load_as_a_number_into_a_register ) {
  vm_instruction program[] = {
    op_load_i(0, 55),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, val(55, vm_tag_number));
  is_equal(type_of_value(result), vm_type_number);
}


it( adds_two_numbers ) {
  vm_instruction program[] = {
    op_load_i(1, 11),
    op_load_i(2, 32),
    op_add(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, 43);
}


it( moves_a_register ) {
  vm_instruction program[] = {
    op_load_i(2, 37),
    op_move(1, 2),
    op_ret(1)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, 37);
}


it( directly_calls_a_function ) {
  const int fun_address = 7;
  vm_instruction program[] = {
    op_load_i(1, 15),
    op_load_i(2, 23),
    op_add(4, 1, 2),
    op_load_i(3, fun_address),
    op_set_arg(0, 4, 0),
    op_call(0, 3, 1), /* result reg, reg with function address, num parameters */
    op_ret(0),
    op_load_i(1, 100),
    op_add(2, 0, 1),
    op_ret(2)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, 138);
}


it( calls_a_closure_downwards ) {
  const int fun_address1 = 8;
  const int fun_address2 = 14;
  vm_instruction program[] = {
    op_load_i(2, fun_address2),
    op_load_i(3, 80),
    op_set_arg(0, 3, 0),
    op_make_cl(2, 2, 1, 2),
    op_load_i(1, fun_address1),
    op_set_arg(0, 2, 0),
    op_call(0, 1, 1), //call fun1 with a closure to fun2
    op_ret(0),

    // fun1
    op_load_i(2, 115), // addr 6
    op_load_i(3, 23),
    op_add(2, 2, 3),
    op_set_arg(0, 2, 0),
    op_gen_ap(3, 0, 1), //closure at register 1 with 1 argument
    op_ret(3),

    // fun2
    //fun_header(1, 1), /* 1 closed over value, 1 parameter */
    op_sub(2, 1, 0), // addr 11 // reg1 holds the function argument, reg0 is the single env value
    op_ret(2)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, 58); //115 + 23 - 80
}


it( calls_a_closure_upwards ) {
  const int fun_address1 = 7;
  const int fun_address2 = 12;
  vm_instruction program[] = {
    op_load_i(1, fun_address1),
    op_set_arg(0, 2, 0),
    op_call(1, 1, 1),
    op_load_i(2, 80),
    op_set_arg(0, 2, 0),
    op_gen_ap(0, 1, 1),
    op_ret(0),

    // fun 1
    op_load_i(1, fun_address2),
    op_load_i(2, 24),
    op_set_arg(0, 2, 0),
    op_make_cl(0, 1, 1, 2),
    op_ret(0),

    // fun 2
    op_sub(2, 1, 0),
    op_ret(2)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, 56); //80 - 24
}

it( modifies_a_closure ) {
  const int fun_address1 = 6;
  const int fun_address2 = 14;
  vm_instruction program[] = {
    op_load_i(1, fun_address1),
    op_call(1, 1, 0),
    op_load_i(2, 80),
    op_set_arg(0, 2, 0),
    op_gen_ap(0, 1, 1),
    op_ret(0),

    // fun 1
    op_load_i(1, fun_address2),
    op_load_i(2, 77),
    op_load_i(3, 55),
    op_set_arg(0, 2, 1),
    op_make_cl(0, 1, 2, 3),
    op_load_i(7, 33),
    op_set_cl_val(0, 7, 1),
    op_ret(0),

    // fun 2
    op_sub(3, 0, 1),
    op_ret(3)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, 44); //77 - 33
}

it( applies_a_number_tag_to_a_value ) {
  vm_value original = 44;
  vm_value number = val(original, vm_tag_number);
  is_equal(type_of_value(number), vm_type_number);
  is_not_equal(type_of_value(number), vm_type_plain_symbol);
  is_equal(from_val(number, vm_tag_number), original);
}


it( applies_a_symbol_tag_to_a_value ) {
  vm_value original = 12;
  vm_value symbol = val(original, vm_tag_plain_symbol);
  is_equal(type_of_value(symbol), vm_type_plain_symbol);
  is_not_equal(type_of_value(symbol), vm_type_number);
  is_equal(from_val(symbol, vm_tag_plain_symbol), original);
}


it( load_as_a_symbol_into_a_register ) {
  vm_instruction program[] = {
    op_load_ps(0, 12),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, val(12, vm_tag_plain_symbol));
  is_equal(type_of_value(result), vm_type_plain_symbol);
}


it( load_as_a_constant ) {
  vm_value const_table[] = {
    val(33, vm_tag_plain_symbol)
  };

  vm_instruction program[] = {
    op_load_c(0, 0),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, val(33, vm_tag_plain_symbol));
  is_equal(type_of_value(result), vm_type_plain_symbol);
}


it( load_as_a_compound_symbol ) {
  vm_value const_table[] = {
    /* this would contain the data symbol */
  };

  vm_instruction program[] = {
    op_load_cs(0, 1),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, 0);
  is_equal(result, val(1, vm_tag_compound_symbol));
  is_equal(type_of_value(result), vm_type_compound_symbol);
}


it( jumps_forward ) {
  vm_instruction program[] = {
    op_load_i(0, 66),
    op_jmp(1),
    op_ret(0),
    op_load_i(0, 70),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, val(70, vm_tag_number));
}


it( matches_a_number ) {
  vm_value const_table[] = {
    match_header(2),
    val(11, vm_tag_number),
    val(22, vm_tag_number),
  };

  vm_instruction program[] = {
    op_load_i(0, 600),
    op_load_i(1, 22), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(1, 2, 0),
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, 4),
    op_ret(0),
    op_load_i(0, 300),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, val(300, vm_tag_number));
}

it( matches_a_symbol ) {
  vm_value const_table[] = {
    match_header(2),
    val(11, vm_tag_plain_symbol),
    val(22, vm_tag_plain_symbol),
  };

  vm_instruction program[] = {
    op_load_i(0, 600),
    op_load_ps(1, 22), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(1, 2, 0),
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, 4),
    op_ret(0),
    op_load_i(0, 300),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, val(300, vm_tag_number));
}


it( matches_a_compound_symbol ) {

  vm_value const_table[] = {
    match_header(2),
    val(3, vm_tag_compound_symbol),
    val(6, vm_tag_compound_symbol),
    compound_symbol_header(1, 2),
    val(55, vm_tag_number),
    val(66, vm_tag_number),
    compound_symbol_header(1, 2),
    val(55, vm_tag_number),
    val(77, vm_tag_number),
    compound_symbol_header(1, 2), /* the subject */
    val(55, vm_tag_number),
    val(77, vm_tag_number),
  };

  vm_instruction program[] = {
    op_load_i(0, 600),
    op_load_cs(1, 9), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(1, 2, 0),
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, 4),
    op_ret(0),
    op_load_i(0, 300),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, val(300, vm_tag_number));

}

it( binds_a_value_in_a_match ) {

  vm_value const_table[] = {
    match_header(2),
    val(3, vm_tag_compound_symbol),
    val(6, vm_tag_compound_symbol),
    compound_symbol_header(1, 2),
    val(55, vm_tag_number),
    val(66, vm_tag_number),
    compound_symbol_header(1, 2),
    val(55, vm_tag_number),
    match_var(1), /* store this match in start_reg + 1 */
    compound_symbol_header(1, 2), /* the subject */
    val(55, vm_tag_number),
    val(77, vm_tag_number),
  };

  vm_instruction program[] = {
    op_load_i(0, 600), /* initial wrong value */
    op_load_i(4, 66), /* initial wrong value */

    op_load_cs(1, 9), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(1, 2, 3), /* after matching, reg 3 + 1 should contain the matched value (77) */
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, 22), /* case 1 */
    op_ret(0),
    op_move(0, 4), /* case 2 */
    op_ret(0)
  };

  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, val(77, vm_tag_number));
}


it( binds_a_value_in_a_nested_symbol ) {

  vm_value const_table[] = {
    match_header(2),
    val(3, vm_tag_compound_symbol),
    val(8, vm_tag_compound_symbol),

    compound_symbol_header(1, 2),
    val(6, vm_tag_compound_symbol),
    match_var(1),
    compound_symbol_header(3, 1),
    match_var(0),

    compound_symbol_header(1, 2),
    val(11, vm_tag_compound_symbol),
    match_var(1),
    compound_symbol_header(2, 1),
    match_var(0),

    compound_symbol_header(1, 2), /* the subject */ //13
    val(16, vm_tag_compound_symbol),
    val(55, vm_tag_number),
    compound_symbol_header(2, 1), //16
    val(66, vm_tag_number),

  };

  vm_instruction program[] = {
    op_load_i(0, 600), /* initial wrong value */

    op_load_cs(1, 13), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(1, 2, 3), /* after matching, reg 3 + 1 should contain the matched value (77) */
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, 100),
    op_ret(0),
    op_sub(0, 3, 4), /* there is only one case */
    op_ret(0)
  };

  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, val(11, vm_tag_number));
}

it( creates_an_explicit_partial_application ) {
  const int fun_address = 8;
  vm_instruction program[] = {
    op_load_i(1, 66),
    op_set_arg(0, 1, 0),
    op_load_i(2, fun_address),
    op_part_ap(3, 2, 1, 2),
    op_load_i(4, 98),
    op_set_arg(0, 4, 0),
    op_gen_ap(0, 3, 1),
    op_ret(0),

    //other function
    op_sub(0, 1, 0),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, val(32, vm_tag_number));
}

it( creates_a_partial_application_with_a_generic_application ) {
  const int fun_address = 11;
  vm_instruction program[] = {
    op_load_i(1, 33), // arg a
    op_set_arg(0, 1, 0),
    op_load_i(2, fun_address),
    op_make_cl(3, 2, 1, 3),
    op_load_i(4, 98), // arg b
    op_set_arg(0, 4, 0),
    op_gen_ap(5, 3, 1), // result register holds a partial application now
    op_load_i(6, 100), // arg c
    op_set_arg(0, 6, 0),
    op_gen_ap(0, 5, 1),
    op_ret(0),

    //other function
    // .\ a b c = c + (b - a)
    op_sub(3, 1, 0),
    op_add(0, 2, 3),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, val(165, vm_tag_number));
}


start_spec(vm_spec)
	example(load_as_a_number_into_a_register)
	example(adds_two_numbers)
  example(moves_a_register)
  example(directly_calls_a_function)
  example(calls_a_closure_downwards)
  example(calls_a_closure_upwards)
  example(modifies_a_closure)
  example(applies_a_number_tag_to_a_value)
  example(applies_a_symbol_tag_to_a_value)
  example(load_as_a_symbol_into_a_register)
  example(load_as_a_constant)
  example(load_as_a_compound_symbol)
  example(jumps_forward)
  example(matches_a_number)
  example(matches_a_symbol)
  example(matches_a_compound_symbol)
  example(binds_a_value_in_a_match)
  example(binds_a_value_in_a_nested_symbol)
  example(creates_an_explicit_partial_application)
  example(creates_a_partial_application_with_a_generic_application)
end_spec

