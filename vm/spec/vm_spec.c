#include <stdio.h>
#include "vm_spec.h"

#include "../vm_internal.h"
#include "../opcodes.h"
#include "../heap.h"

#define array_length(x) (sizeof(x) / sizeof(x[0]))

#define bias(n) (n + number_bias)

const int heap_start = 1;

it( loads_a_number_into_a_register ) {
  vm_instruction program[] = {
    op_load_i(0, 55),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(55, vm_tag_number));
}


it( adds_two_numbers ) {
  vm_instruction program[] = {
    op_load_i(1, bias(11)),
    op_load_i(2, bias(32)),
    op_add(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, bias(43));
}

it( subtracts_two_numbers ) {
  vm_instruction program[] = {
    op_load_i(1, bias(11)),
    op_load_i(2, bias(32)),
    op_sub(0, 2, 1),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, bias(21));
}


it( multiplies_two_numbers ) {
  vm_instruction program[] = {
    op_load_i(1, bias(5)),
    op_load_i(2, bias(4)),
    op_mul(0, 2, 1),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, bias(20));
}

it( divides_two_numbers ) {
  vm_instruction program[] = {
    op_load_i(1, bias(10)),
    op_load_i(2, bias(3)),
    op_div(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, bias(3));
}

it( has_a_less_than_opcode ) {
  vm_instruction program[] = {
    op_load_i(1, bias(2)),
    op_load_i(2, bias(3)),
    op_lt(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(1, vm_tag_plain_symbol));
}

it( has_a_greater_than_opcode ) {
  vm_instruction program[] = {
    op_load_i(1, bias(6)),
    op_load_i(2, bias(3)),
    op_gt(0, 1, 2),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(1, vm_tag_plain_symbol));
}

it( moves_a_register ) {
  vm_instruction program[] = {
    op_load_i(2, bias(37)),
    op_move(1, 2),
    op_ret(1)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, bias(37));
}


it( directly_calls_a_function ) {
  const int fun_address = 7;
  vm_instruction program[] = {
    op_load_i(1, bias(15)),
    op_load_i(2, bias(23)),
    op_add(4, 1, 2),
    op_load_f(3, fun_address),
    op_set_arg(0, 4, 0),
    op_call(0, 3, 1), /* result reg, reg with function address, num parameters */
    op_ret(0),

    fun_header(2),
    op_load_i(1, bias(100)),
    op_add(2, 0, 1),
    op_ret(2)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, bias(138));
}


it( calls_a_closure_downwards ) {
  const int fun_address1 = 8;
  const int fun_address2 = 15;
  vm_instruction program[] = {
    op_load_f(2, fun_address2),
    op_load_i(3, bias(80)),
    op_set_arg(0, 3, 0),
    op_part_ap(2, 2, 1), // we create a closure using a pap
    op_load_f(1, fun_address1),
    op_set_arg(0, 2, 0),
    op_call(0, 1, 1), //call fun1 with a closure to fun2
    op_ret(0),

    // fun1
    fun_header(1),
    op_load_i(2, bias(115)), // addr 6
    op_load_i(3, bias(23)),
    op_add(2, 2, 3),
    op_set_arg(0, 2, 0),
    op_gen_ap(3, 0, 1), //closure at register 1 with 1 argument
    op_ret(3),

    // fun2
    fun_header(2), /* 1 closed over value, 1 parameter */
    op_sub(2, 1, 0), // addr 11 // reg1 holds the function argument, reg0 is the single env value
    op_ret(2)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, bias(58)); //115 + 23 - 80
}


it( calls_a_closure_upwards ) {
  const int fun_address1 = 7;
  const int fun_address2 = 13;
  vm_instruction program[] = {
    op_load_f(1, fun_address1),
    op_set_arg(0, 2, 0),
    op_call(1, 1, 1),
    op_load_i(2, bias(80)),
    op_set_arg(0, 2, 0),
    op_gen_ap(0, 1, 1),
    op_ret(0),

    // fun 1
    fun_header(1),
    op_load_f(1, fun_address2),
    op_load_i(2, bias(24)),
    op_set_arg(0, 2, 0),
    op_part_ap(0, 1, 1),
    op_ret(0),

    // fun 2
    fun_header(2),
    op_sub(2, 1, 0),
    op_ret(2)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, bias(56)); //80 - 24
}


it( modifies_a_closure ) {
  const int fun_address1 = 6;
  const int fun_address2 = 15;
  vm_instruction program[] = {
    op_load_f(1, fun_address1),
    op_call(1, 1, 0),
    op_load_i(2, bias(80)),
    op_set_arg(0, 2, 0),
    op_gen_ap(0, 1, 1),
    op_ret(0),

    // fun 1
    fun_header(0),
    op_load_f(1, fun_address2),
    op_load_i(2, bias(77)),
    op_load_i(3, bias(55)),
    op_set_arg(0, 2, 1),
    op_part_ap(0, 1, 2),
    op_load_i(7, bias(33)),
    op_set_cl_val(0, 7, 1),
    op_ret(0),

    // fun 2
    fun_header(3),
    op_sub(3, 0, 1),
    op_ret(3)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, bias(44)); //77 - 33
}


it( applies_a_number_tag_to_a_value ) {
  vm_value original = 44;
  vm_value number = make_tagged_val(original, vm_tag_number);
  is_equal(get_val(number), original);
}


it( applies_a_symbol_tag_to_a_value ) {
  vm_value original = 12;
  vm_value symbol = make_tagged_val(original, vm_tag_plain_symbol);
  is_equal(get_val(symbol), original);
}


it( loads_a_symbol_into_a_register ) {
  vm_instruction program[] = {
    op_load_ps(0, 12),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(12, vm_tag_plain_symbol));
}


it( loads_a_constant ) {
  vm_value const_table[] = {
    make_tagged_val(33, vm_tag_plain_symbol)
  };

  vm_instruction program[] = {
    op_load_c(0, 0),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(33, vm_tag_plain_symbol));
}


it( loads_a_compound_symbol ) {
  vm_value const_table[] = {
    /* this would contain the data symbol */
  };

  vm_instruction program[] = {
    op_load_cs(0, 1),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, 0);
  is_equal(result, make_tagged_val(1, vm_tag_compound_symbol));
}


it( jumps_forward ) {
  vm_instruction program[] = {
    op_load_i(0, bias(66)),
    op_jmp(1),
    op_ret(0),
    op_load_i(0, bias(70)),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(bias(70), vm_tag_number));
}


it( matches_a_number ) {
  vm_value const_table[] = {
    match_header(2),
    make_tagged_val(bias(11), vm_tag_number),
    make_tagged_val(bias(22), vm_tag_number),
  };

  vm_instruction program[] = {
    op_load_i(0, 600),
    op_load_i(1, bias(22)), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(1, 2, 0),
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, bias(4)),
    op_ret(0),
    op_load_i(0, bias(300)),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(bias(300), vm_tag_number));
}

it( matches_a_symbol ) {
  vm_value const_table[] = {
    match_header(2),
    make_tagged_val(bias(11), vm_tag_plain_symbol),
    make_tagged_val(bias(22), vm_tag_plain_symbol),
  };

  vm_instruction program[] = {
    op_load_i(0, bias(600)),
    op_load_ps(1, bias(22)), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(1, 2, 0),
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, bias(4)),
    op_ret(0),
    op_load_i(0, bias(300)),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(bias(300), vm_tag_number));
}


it( matches_a_compound_symbol ) {

  vm_value const_table[] = {
    match_header(2),
    make_tagged_val(3, vm_tag_compound_symbol),
    make_tagged_val(6, vm_tag_compound_symbol),
    compound_symbol_header(1, 2),
    make_tagged_val(bias(55), vm_tag_number),
    make_tagged_val(bias(66), vm_tag_number),
    compound_symbol_header(1, 2),
    make_tagged_val(bias(55), vm_tag_number),
    make_tagged_val(bias(77), vm_tag_number),
    compound_symbol_header(1, 2), /* the subject */
    make_tagged_val(bias(55), vm_tag_number),
    make_tagged_val(bias(77), vm_tag_number),
  };

  vm_instruction program[] = {
    op_load_i(0, bias(600)),
    op_load_cs(1, 9), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(1, 2, 0),
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, bias(4)),
    op_ret(0),
    op_load_i(0, bias(300)),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(bias(300), vm_tag_number));

}

it( binds_a_value_in_a_match ) {

  vm_value const_table[] = {
    match_header(2),
    make_tagged_val(3, vm_tag_compound_symbol),
    make_tagged_val(6, vm_tag_compound_symbol),
    compound_symbol_header(1, 2),
    make_tagged_val(bias(55), vm_tag_number),
    make_tagged_val(bias(66), vm_tag_number),
    compound_symbol_header(1, 2),
    make_tagged_val(bias(55), vm_tag_number),
    match_var(1), /* store this match in start_reg + 1 */
    compound_symbol_header(1, 2), /* the subject */
    make_tagged_val(bias(55), vm_tag_number),
    make_tagged_val(bias(77), vm_tag_number),
  };

  vm_instruction program[] = {
    op_load_i(0, bias(600)), /* initial wrong value */
    op_load_i(4, bias(66)), /* initial wrong value */

    op_load_cs(1, 9), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(1, 2, 3), /* after matching, reg 3 + 1 should contain the matched value (77) */
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, bias(22)), /* case 1 */
    op_ret(0),
    op_move(0, 4), /* case 2 */
    op_ret(0)
  };

  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(bias(77), vm_tag_number));
}


it( binds_a_value_in_a_nested_symbol ) {

  vm_value const_table[] = {
    match_header(2),
    make_tagged_val(3, vm_tag_compound_symbol),
    make_tagged_val(8, vm_tag_compound_symbol),

    compound_symbol_header(1, 2),
    make_tagged_val(6, vm_tag_compound_symbol),
    match_var(1),
    compound_symbol_header(3, 1),
    match_var(0),

    compound_symbol_header(1, 2),
    make_tagged_val(11, vm_tag_compound_symbol),
    match_var(1),
    compound_symbol_header(2, 1),
    match_var(0),

    compound_symbol_header(1, 2), /* the subject */ //13
    make_tagged_val(16, vm_tag_compound_symbol),
    make_tagged_val(bias(55), vm_tag_number),
    compound_symbol_header(2, 1), //16
    make_tagged_val(bias(66), vm_tag_number),

  };

  vm_instruction program[] = {
    op_load_i(0, bias(600)), /* initial wrong value */

    op_load_cs(1, 13), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(1, 2, 3), /* after matching, reg 3 + 1 should contain the matched value (77) */
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, bias(100)),
    op_ret(0),
    op_sub(0, 3, 4), /* there is only one case */
    op_ret(0)
  };

  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(bias(11), vm_tag_number));
}



it( matches_a_dynamic_compound_symbol ) {

  vm_value const_table[] = {
    match_header(2),
    make_tagged_val(3, vm_tag_compound_symbol),
    make_tagged_val(6, vm_tag_compound_symbol),
    compound_symbol_header(1, 2),
    make_tagged_val(bias(55), vm_tag_number),
    make_tagged_val(bias(66), vm_tag_number),
    compound_symbol_header(1, 2),
    make_tagged_val(bias(55), vm_tag_number),
    make_tagged_val(bias(77), vm_tag_number),
    compound_symbol_header(1, 2), /* the subject */
    make_tagged_val(bias(55), vm_tag_number),
    make_tagged_val(bias(77), vm_tag_number),
  };

  vm_instruction program[] = {
    op_load_i(0, bias(600)),
    op_load_cs(1, 9),
    op_copy_sym(5, 1), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(5, 2, 0),
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, bias(4)),
    op_ret(0),
    op_load_i(0, bias(300)),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(bias(300), vm_tag_number));

}

it( binds_a_value_in_a_dynamic_symbol_match ) {

  vm_value const_table[] = {
    match_header(2),
    make_tagged_val(3, vm_tag_compound_symbol),
    make_tagged_val(6, vm_tag_compound_symbol),
    compound_symbol_header(1, 2),
    make_tagged_val(bias(55), vm_tag_number),
    make_tagged_val(bias(66), vm_tag_number),
    compound_symbol_header(1, 2),
    make_tagged_val(bias(55), vm_tag_number),
    match_var(1), /* store this match in start_reg + 1 */
    compound_symbol_header(1, 2), /* the subject */
    make_tagged_val(bias(55), vm_tag_number),
    make_tagged_val(bias(77), vm_tag_number),
  };

  vm_instruction program[] = {
    op_load_i(0, bias(600)), /* initial wrong value */
    op_load_i(4, bias(66)), /* initial wrong value */

    op_load_cs(1, 9),
    op_copy_sym(5, 1), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(5, 2, 3), /* after matching, reg 3 + 1 should contain the matched value (77) */
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, bias(22)), /* case 1 */
    op_ret(0),
    op_move(0, 4), /* case 2 */
    op_ret(0)
  };

  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(bias(77), vm_tag_number));
}


it( binds_a_value_in_a_nested_dynamic_symbol ) {

  vm_value const_table[] = {
    match_header(2),
    make_tagged_val(3, vm_tag_compound_symbol),
    make_tagged_val(8, vm_tag_compound_symbol),

    compound_symbol_header(1, 2),
    make_tagged_val(6, vm_tag_compound_symbol),
    match_var(1),
    compound_symbol_header(3, 1),
    match_var(0),

    compound_symbol_header(1, 2),
    make_tagged_val(11, vm_tag_compound_symbol),
    match_var(1),
    compound_symbol_header(2, 1),
    match_var(0),

    compound_symbol_header(1, 2), /* the subject */ //13
    make_tagged_val(16, vm_tag_compound_symbol),
    make_tagged_val(bias(55), vm_tag_number),
    compound_symbol_header(2, 1), //16
    make_tagged_val(bias(66), vm_tag_number),

  };

  vm_instruction program[] = {
    op_load_i(0, bias(600)), /* initial wrong value */

    op_load_cs(1, 13),
    op_copy_sym(5, 1), /* value to match */
    op_load_i(2, 0), /* address of match pattern */
    op_match(5, 2, 3), /* after matching, reg 3 + 1 should contain the matched value (77) */
    op_jmp(1),
    op_jmp(2),
    op_load_i(0, bias(100)),
    op_ret(0),
    op_sub(0, 3, 4), /* there is only one case */
    op_ret(0)
  };

  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(bias(11), vm_tag_number));
}


it( creates_an_explicit_partial_application ) {
  const int fun_address = 8;
  vm_instruction program[] = {
    op_load_i(1, bias(66)),
    op_set_arg(0, 1, 0),
    op_load_f(2, fun_address),
    op_part_ap(3, 2, 1),
    op_load_i(4, bias(98)),
    op_set_arg(0, 4, 0),
    op_gen_ap(0, 3, 1),
    op_ret(0),

    //other function
    fun_header(2),
    op_sub(0, 1, 0),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(bias(32), vm_tag_number));
}

it( creates_a_partial_application_with_a_generic_application ) {
  const int fun_address = 11;
  vm_instruction program[] = {
    op_load_i(1, bias(33)), // arg a
    op_set_arg(0, 1, 0),
    op_load_f(2, fun_address),
    op_part_ap(3, 2, 1),
    op_load_i(4, bias(98)), // arg b
    op_set_arg(0, 4, 0),
    op_gen_ap(5, 3, 1), // result register holds a partial application now
    op_load_i(6, bias(100)), // arg c
    op_set_arg(0, 6, 0),
    op_gen_ap(0, 5, 1),
    op_ret(0),

    //other function
    // .\ a b c = c + (b - a)
    fun_header(3),
    op_sub(3, 1, 0),
    op_add(0, 2, 3),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(bias(165), vm_tag_number));
}


it( does_a_generic_application_of_a_function ) {
  const int fun_address = 6;
  vm_instruction program[] = {
    op_load_i(1, bias(21)),
    op_load_i(2, bias(33)),
    op_set_arg(0, 1, 1),
    op_load_f(3, fun_address),
    op_gen_ap(0, 3, 2),
    op_ret(0),

    //other function
    fun_header(2),
    op_sub(0, 1, 0),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(bias(12), vm_tag_number));
}



it( loads_a_symbol_on_the_heap ) {
  vm_value const_table[] = {
    compound_symbol_header(5, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(7, 2),
    make_tagged_val(33, vm_tag_number),
    make_tagged_val(44, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(0, 0),
    op_load_cs(1, 3),
    op_copy_sym(0, 1),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(heap_start, vm_tag_dynamic_compound_symbol));

  vm_value *heap_p = heap_get_pointer(heap_start);
  vm_value sym_header = *heap_p;
  is_equal(compound_symbol_count(sym_header), 2);
  int header_size = 1;
  is_equal(heap_p[header_size + 0], 33);
  is_equal(heap_p[header_size + 1], 44);
}

it( modifies_a_heap_symbol ) {

  vm_value const_table[] = {
    compound_symbol_header(5, 2),
    make_tagged_val(bias(55), vm_tag_number),
    make_tagged_val(bias(66), vm_tag_number),
    compound_symbol_header(7, 2),
    make_tagged_val(bias(33), vm_tag_number),
    make_tagged_val(bias(44), vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(0, 0),
    op_load_cs(1, 3),
    op_copy_sym(0, 1),
    op_load_ps(5, 77),
    op_set_sym_field(0, 5, 1),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(heap_start, vm_tag_dynamic_compound_symbol));

  vm_value *heap_p = heap_get_pointer(heap_start);
  vm_value sym_header = *heap_p;
  is_equal(compound_symbol_count(sym_header), 2);
  int header_size = 1;
  is_equal(heap_p[header_size + 0], bias(33));
  is_equal(heap_p[header_size + 1], make_tagged_val(77, vm_tag_plain_symbol));
}


it( loads_a_constant_string_into_a_register ) {
  vm_instruction program[] = {
    op_load_str(0, 55),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), 0, 0);
  is_equal(result, make_tagged_val(55, vm_tag_string));
}


it( determines_the_length_of_a_string ) {
  vm_value const_table[] = {
    string_header(6, 2)
    // we're cheating here and leaving out the actual string content
  };
  vm_instruction program[] = {
    op_load_str(1, 0),
    op_str_len(0, 1),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(bias(6), vm_tag_number));
}


start_spec(vm_spec)
	example(loads_a_number_into_a_register)
	example(adds_two_numbers)
	example(subtracts_two_numbers)
	example(multiplies_two_numbers)
	example(divides_two_numbers)
  example(moves_a_register)
  example(directly_calls_a_function)
  example(calls_a_closure_downwards)
  example(calls_a_closure_upwards)
  example(modifies_a_closure)
  example(applies_a_number_tag_to_a_value)
  example(applies_a_symbol_tag_to_a_value)
  example(loads_a_symbol_into_a_register)
  example(loads_a_constant)
  example(loads_a_compound_symbol)
  example(jumps_forward)
  example(matches_a_number)
  example(matches_a_symbol)
  example(matches_a_compound_symbol)
  example(binds_a_value_in_a_match)
  example(binds_a_value_in_a_nested_symbol)
  example(matches_a_dynamic_compound_symbol)
  example(binds_a_value_in_a_dynamic_symbol_match)
  example(binds_a_value_in_a_nested_dynamic_symbol)
  example(creates_an_explicit_partial_application)
  example(creates_a_partial_application_with_a_generic_application)
  example(does_a_generic_application_of_a_function)
  example(loads_a_symbol_on_the_heap)
  example(modifies_a_heap_symbol)
  example(loads_a_constant_string_into_a_register)
  example(determines_the_length_of_a_string)
  example(has_a_less_than_opcode)
  example(has_a_greater_than_opcode)
end_spec

