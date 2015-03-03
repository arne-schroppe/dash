module Language.Spot.IR.Opcode where

import Data.Word

-- TODO make this more type-safe, newtype that word32

data Opcode =
    Op_halt
  | Op_load_i Word32 Word32
  | Op_load_f Word32 Word32
  | Op_load_s Word32 Word32
  | Op_load_sd Word32 Word32
  | Op_load_c Word32 Word32
  | Op_add Word32 Word32 Word32
  | Op_sub Word32 Word32 Word32
  | Op_move Word32 Word32
  | Op_call Word32 Word32 Word32
  | Op_call_cl Word32 Word32 Word32
  | Op_ret
  | Op_make_cl Word32 Word32 Word32
  | Op_jmp Word32
  | Op_match Word32 Word32 Word32
  deriving (Show)

