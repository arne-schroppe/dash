#ifndef _INCLUDE_OPCODES_H
#define _INCLUDE_OPCODES_H

#include "vm.h"

typedef enum {
  OP_RET = 0,
  OP_LOADi = 1,
  OP_LOADps = 2,
  OP_LOADcs = 3,
  OP_LOADc = 4,
  OP_ADD = 5,
  OP_SUB = 6,
  OP_MOVE = 7, //Maybe we don't need this, could be add with a zero (see MIPS instruction set)
  OP_CALL = 8,
  OP_CALLCL = 9,
  OP_MAKECL = 10,
  OP_JMP = 11,
  OP_MATCH = 12,
  OP_SETARG = 13
  //OP_SPACE = 14, //Adds space for temporary values
} vm_opcode;

#define instr_size (sizeof(vm_instruction) * 8)
#define __regb 5  /* Number of bits for registers */
#define __opcb 4 /* Number of bits for opcode */


#define get_opcode(instr) ((instr & 0xF0000000) >> (instr_size - __opcb))

#define get_arg_r0(instr) ((instr & 0x0F800000) >> (instr_size - (__opcb + __regb)))
#define get_arg_r1(instr) ((instr & 0x001F0000) >> (instr_size - (__opcb + 2 * __regb)))
#define get_arg_r2(instr) ((instr & 0x0000F800) >> (instr_size - (__opcb + 3 * __regb)))
#define get_arg_i(instr)   (instr & 0x007FFFFF)


/* Uses by tests */
#define instr_ri(op, reg, i) ((op << (instr_size - __opcb)) + (reg << (instr_size - (__opcb + __regb))) + i)
#define instr_rrr(op, reg0, reg1, reg2) ((op << (instr_size - __opcb)) + \
                                            (reg0 << (instr_size - (__opcb + __regb))) + \
                                            (reg1 << (instr_size - (__opcb + 2 * __regb))) + \
                                            (reg2 << (instr_size - (__opcb + 3 * __regb))))

#define op_load_i(r0, i) (instr_ri(OP_LOADi, r0, i))
#define op_load_ps(r0, i) (instr_ri(OP_LOADps, r0, i))
#define op_load_cs(r0, i) (instr_ri(OP_LOADcs, r0, i))
#define op_load_c(r0, i) (instr_ri(OP_LOADc, r0, i))
#define op_add(r0, r1, r2) (instr_rrr(OP_ADD, r0, r1, r2))
#define op_sub(r0, r1, r2) (instr_rrr(OP_SUB, r0, r1, r2))
#define op_move(r0, r1) (instr_rrr(OP_MOVE, r0, r1, 0))
#define op_call(r0, fr, n) (instr_rrr(OP_CALL, r0, fr, n)) // result reg, reg with function addr, num arguments
#define op_callcl(r0, fr, n) (instr_rrr(OP_CALLCL, r0, fr, n)) // result reg, reg with function addr, num arguments
#define op_ret(r0) (instr_ri(OP_RET, r0, 0))
#define op_makecl(r0, fr, n) (instr_rrr(OP_MAKECL, r0, fr, n))
#define op_jmp(n) (instr_ri(OP_JMP, 0, n))
#define op_match(r1, r2, r3) (instr_rrr(OP_MATCH, r1, r2, r3)) // reg with subject, reg with pattern addr, start reg for captures
#define op_setarg(arg, r, n) (instr_rrr(OP_SETARG, arg, r, n)) // target argument, source reg, number of extra args/regs to copy (when copying just one argument, set this to 0)
// #define op_space(n) (instr_ri(OP_SPACE, 0, n))

#endif
