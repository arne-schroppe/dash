module Language.Spot.IR.Opcode where

data Opcode =
    Op_halt
  | Op_load_i Int Int
  | Op_load_f Int Int
  | Op_load_s Int Int
  | Op_load_sd Int Int
  | Op_load_c Int Int
  | Op_add Int Int Int
  | Op_sub Int Int Int
  | Op_move Int Int
  | Op_call Int Int Int
  | Op_call_cl Int Int Int
  | Op_ret
  | Op_make_cl Int Int Int
  | Op_jmp Int
  | Op_match Int Int Int
  deriving (Show)

