#ifndef _INCLUDE_OPCODES_H
#define _INCLUDE_OPCODES_H

#include "vm.h"

// TODO add more mathematical operators
// TODO group opcodes in meaningful way
// TODO if we're keeping set_arg, then how about versions that allow to set values directly? (instead of from registers)
// TODO we could just use load_c for all const pool types if they had a tag in the pool

typedef enum {
  OP_RET = 0,
  OP_LOAD_i = 1,
  OP_LOAD_ps = 2,
  OP_LOAD_cs = 3,
  OP_LOAD_os = 4,
  OP_LOAD_f = 5,
  OP_ADD = 6,
  OP_SUB = 7,
  OP_MUL = 8,
  OP_DIV = 9,
  OP_MOVE = 10, //Maybe we don't need this, could be add with a zero (see MIPS instruction set)
  OP_AP = 11,
  OP_GEN_AP = 12, // General function application
  OP_TAIL_AP = 13,
  OP_TAIL_GEN_AP = 14,
  OP_PART_AP = 15,      // Do partial application of known function
  OP_JMP = 16,
  OP_MATCH = 17,
  OP_SET_ARG = 18,
  OP_SET_CL_VAL = 19, // TODO rename to SET_CL_FIELD
  OP_EQ = 20,
  OP_COPY_SYM = 21,  // copies a constant compound symbol to the heap
  OP_SET_SYM_FIELD = 22, // sets a field of a heap compound symbol
  OP_LOAD_str = 23,
  OP_STR_LEN = 24,
  OP_NEW_STR = 25,
  OP_GET_CHAR = 26,
  OP_PUT_CHAR = 27,
  OP_LT = 28,
  OP_GT = 29,
  OP_JMP_TRUE = 30,
  OP_OR = 31,
  OP_AND = 32,
  OP_NOT = 33,
  OP_GET_FIELD = 34,
  OP_CONVERT = 35,

  FUN_HEADER = 63
} vm_opcode;

#define instr_size (sizeof(vm_instruction) * 8)
#define __regb 5  /* Number of bits for registers */
#define __opcb 6 /* Number of bits for opcode */


#define get_opcode(instr) ((instr & 0xFC000000) >> (instr_size - __opcb))

// TODO these are not necessarily register. Rename to get_arg_0 etc
#define get_arg_r0(instr) ((instr & 0x03E00000) >> (instr_size - (__opcb + __regb)))
#define get_arg_r1(instr) ((instr & 0x001F0000) >> (instr_size - (__opcb + 2 * __regb)))
#define get_arg_r2(instr) ((instr & 0x0000F800) >> (instr_size - (__opcb + 3 * __regb)))
#define get_arg_i(instr)   (instr & 0x001FFFFF) //Obs! This is for opcode + reg0 + number


/* Used by tests */
#define instr_ri(op, reg, i) ((op << (instr_size - __opcb)) + (reg << (instr_size - (__opcb + __regb))) + i)

#define instr_rrr(op, reg0, reg1, reg2) ((op << (instr_size - __opcb)) + \
                                            (reg0 << (instr_size - (__opcb + __regb))) + \
                                            (reg1 << (instr_size - (__opcb + 2 * __regb))) + \
                                            (reg2 << (instr_size - (__opcb + 3 * __regb))))

// TODO make it clear which opcodes expect a function address and which expect a closure!!
// TODO describe all arguments!
#define op_load_i(r0, i) (instr_ri(OP_LOAD_i, r0, i))
#define op_load_ps(r0, i) (instr_ri(OP_LOAD_ps, r0, i))
#define op_load_cs(r0, i) (instr_ri(OP_LOAD_cs, r0, i))
#define op_load_os(r0, i) (instr_ri(OP_LOAD_os, r0, i))
#define op_load_f(r0, i) (instr_ri(OP_LOAD_f, r0, i))
#define op_add(r0, r1, r2) (instr_rrr(OP_ADD, r0, r1, r2))
#define op_sub(r0, r1, r2) (instr_rrr(OP_SUB, r0, r1, r2))
#define op_mul(r0, r1, r2) (instr_rrr(OP_MUL, r0, r1, r2))
#define op_div(r0, r1, r2) (instr_rrr(OP_DIV, r0, r1, r2))
#define op_move(r0, r1) (instr_rrr(OP_MOVE, r0, r1, 0))
#define op_ap(r0, fr, n) (instr_rrr(OP_AP, r0, fr, n)) // result reg, reg with function addr (code), num arguments
#define op_gen_ap(r0, clr, n) (instr_rrr(OP_GEN_AP, r0, clr, n)) // result reg, reg with closure addr (heap), num arguments
#define op_part_ap(r0, fr, n) (instr_rrr(OP_PART_AP, r0, fr, n)) // result reg, reg with function addr (code), num arguments
#define op_ret(r0) (instr_ri(OP_RET, r0, 0))
#define op_jmp(n) (instr_ri(OP_JMP, 0, n))
#define op_match(r1, r2, r3) (instr_rrr(OP_MATCH, r1, r2, r3)) // reg with subject, reg with pattern addr, start reg for captures
#define op_set_arg(arg, r, n) (instr_rrr(OP_SET_ARG, arg, r, n)) // target argument, value reg, number of extra args/regs to copy (when copying just one argument, set this to 0)
#define op_set_cl_val(clr, r1, n) (instr_rrr(OP_SET_CL_VAL, clr, r1, n)) // reg with closure addr (heap), value reg, argument index
#define op_part_ap(r0, fr, n) (instr_rrr(OP_PART_AP, r0, fr, n)) // result reg, reg with function addr (code), num arguments
#define op_eq(r0, r1, r2) (instr_rrr(OP_EQ, r0, r1, r2))
#define op_copy_sym(r0, r1) (instr_rrr(OP_COPY_SYM, r0, r1, 0)) // heap addr result reg, const addr reg
#define op_set_sym_field(symr, r1, n) (instr_rrr(OP_SET_SYM_FIELD, symr, r1, n)) // heap sym addr reg, new value reg, field index
#define op_load_str(r0, i) (instr_ri(OP_LOAD_str, r0, i)) // result reg, const addr
#define op_str_len(r0, r1) (instr_rrr(OP_STR_LEN, r0, r1, 0))
#define op_new_str(r0, r1) (instr_rrr(OP_NEW_STR, r0, r1, 0))
#define op_get_char(r0, r1, r2) (instr_rrr(OP_GET_CHAR, r0, r1, r2))
#define op_put_char(r0, r1, r2) (instr_rrr(OP_PUT_CHAR, r0, r1, r2))
#define op_lt(r0, r1, r2) (instr_rrr(OP_LT, r0, r1, r2))
#define op_gt(r0, r1, r2) (instr_rrr(OP_GT, r0, r1, r2))
#define op_jmp_true(b, n) (instr_ri(OP_JMP_TRUE, b, n))
#define op_or(r0, r1, r2) (instr_rrr(OP_OR, r0, r1, r2))
#define op_and(r0, r1, r2) (instr_rrr(OP_AND, r0, r1, r2))
#define op_not(r0, r1) (instr_rrr(OP_NOT, r0, r1))
#define op_get_field(r0, mod_r, sym_r) (instr_rrr(OP_GET_FIELD, r0, mod_r, sym_r))
#define op_convert(r0, r1, rt) (instr_rrr(OP_CONVERT, r0, r1, rt))
#define fun_header(arity) (instr_ri(FUN_HEADER, 0, arity))

#endif
