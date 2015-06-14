#ifndef _INCLUDE_OPCODES_H
#define _INCLUDE_OPCODES_H

#include "vm.h"

// TODO we need an opcode that allows us to compare two values
// TODO add more mathematical operators
// TODO group opcodes in meaningful way
// TODO if we're keeping set_arg, then how about versions that allow to set values directly? (instead of from registers)
// TODO really lrge numbers spill over into the tag. prevent that (in compiler and when adding)

typedef enum {
  OP_RET = 0,
  OP_LOAD_i = 1,
  OP_LOAD_ps = 2,
  OP_LOAD_cs = 3,
  OP_LOAD_c = 4,
  OP_ADD = 5,
  OP_SUB = 6,
  OP_MOVE = 7, //Maybe we don't need this, could be add with a zero (see MIPS instruction set)
  OP_CALL = 8,
  OP_GEN_AP = 9, // General function application
  OP_MAKE_CL = 10,
  OP_JMP = 11,
  OP_MATCH = 12,
  OP_SET_ARG = 13,
  OP_TAIL_CALL = 14,
  OP_TAIL_GEN_AP = 15,
  OP_SET_CL_VAL = 16,
  OP_PART_AP = 17,      // Do partial application of known function

  // 60 to 63 is reserved for fun header. It also acts as a tag, which currently only have 4 bits
  // Important! Keep this in sync with the function header tag!
  FUN_HEADER = 60
} vm_opcode;

#define instr_size (sizeof(vm_instruction) * 8)
#define __regb 5  /* Number of bits for registers */
#define __opcb 6 /* Number of bits for opcode */


#define get_opcode(instr) ((instr & 0xFC000000) >> (instr_size - __opcb))

// TODO these are not necessarily register. Rename to get_arg_0 etc
#define get_arg_r0(instr) ((instr & 0x03E00000) >> (instr_size - (__opcb + __regb)))
#define get_arg_r1(instr) ((instr & 0x001F0000) >> (instr_size - (__opcb + 2 * __regb)))
#define get_arg_r2(instr) ((instr & 0x0000F800) >> (instr_size - (__opcb + 3 * __regb)))
#define get_arg_r3(instr) ((instr & 0x000007C0) >> (instr_size - (__opcb + 4 * __regb)))
#define get_arg_i(instr)   (instr & 0x001FFFFF) //Obs! This is for opcode + reg0 + number


/* Used by tests */
#define instr_ri(op, reg, i) ((op << (instr_size - __opcb)) + (reg << (instr_size - (__opcb + __regb))) + i)

// TODO we only use the 4 argument form for partial applications. Should we use a function header to each function instead?
#define instr_rrrr(op, reg0, reg1, reg2, reg3) ((op << (instr_size - __opcb)) + \
                                            (reg0 << (instr_size - (__opcb + __regb))) + \
                                            (reg1 << (instr_size - (__opcb + 2 * __regb))) + \
                                            (reg2 << (instr_size - (__opcb + 3 * __regb))) + \
                                            (reg3 << (instr_size - (__opcb + 4 * __regb))))
#define instr_rrr(op, reg0, reg1, reg2) instr_rrrr(op, reg0, reg1, reg2, 0)

// TODO make it clear which opcodes expect a function address and which expect a closure!!
// TODO instead of rrrr opcodes, use a function header
#define op_load_i(r0, i) (instr_ri(OP_LOAD_i, r0, i))
#define op_load_ps(r0, i) (instr_ri(OP_LOAD_ps, r0, i))
#define op_load_cs(r0, i) (instr_ri(OP_LOAD_cs, r0, i))
#define op_load_c(r0, i) (instr_ri(OP_LOAD_c, r0, i))
#define op_add(r0, r1, r2) (instr_rrr(OP_ADD, r0, r1, r2))
#define op_sub(r0, r1, r2) (instr_rrr(OP_SUB, r0, r1, r2))
#define op_move(r0, r1) (instr_rrr(OP_MOVE, r0, r1, 0))
#define op_call(r0, fr, n) (instr_rrr(OP_CALL, r0, fr, n)) // result reg, reg with function addr (code), num arguments
#define op_gen_ap(r0, clr, n) (instr_rrr(OP_GEN_AP, r0, clr, n)) // result reg, reg with closure addr (heap), num arguments
#define op_ret(r0) (instr_ri(OP_RET, r0, 0))
#define op_make_cl(r0, fr, n, numFP) (instr_rrrr(OP_MAKE_CL, r0, fr, n, numFP)) // result reg, reg with function addr (code), num args
#define op_jmp(n) (instr_ri(OP_JMP, 0, n))
#define op_match(r1, r2, r3) (instr_rrr(OP_MATCH, r1, r2, r3)) // reg with subject, reg with pattern addr, start reg for captures
#define op_set_arg(arg, r, n) (instr_rrr(OP_SET_ARG, arg, r, n)) // target argument, value reg, number of extra args/regs to copy (when copying just one argument, set this to 0)
#define op_set_cl_val(clr, r1, n) (instr_rrr(OP_SET_CL_VAL, clr, r1, n)) // reg with closure addr (heap), value reg, argument index
#define op_part_ap(r0, fr, n, numFP) (instr_rrrr(OP_PART_AP, r0, fr, n, numFP)) // result reg, reg with function addr (code), num arguments, num formal params

#define fun_header(arity) (instr_ri(FUN_HEADER, 0, arity))
// #define op_space(n) (instr_ri(OP_SPACE, 0, n))

#endif
