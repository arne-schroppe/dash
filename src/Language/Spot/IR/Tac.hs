module Language.Spot.IR.Tac (
  ThreeAddressCode (..)
, Tac
) where

import Language.Spot.VM.Types
import Language.Spot.IR.Data



data ThreeAddressCode =
    Tac_ret Reg
  | Tac_load_i Reg VMWord
  | Tac_load_addr Reg ConstAddr
  | Tac_load_f Reg FunAddr
  | Tac_load_ps Reg SymId         -- load plain symbol
  | Tac_load_cs Reg ConstAddr     -- load compound symbol
  | Tac_load_c Reg ConstAddr      -- load constant
  | Tac_add Reg Reg Reg
  | Tac_sub Reg Reg Reg
  | Tac_move Reg Reg
  | Tac_call Reg Reg Int
  | Tac_call_cl Reg Reg Int
  | Tac_make_cl Reg Reg Int
  | Tac_jmp Int
  | Tac_match Reg Reg Reg     -- subj reg, pattern addr reg, start reg for captures
  | Tac_set_arg Int Reg Int
  | Tac_tail_call Reg Int
  | Tac_tail_call_cl Reg Int
  | Tac_set_cl_arg Reg Reg Int
  deriving (Show)

type Tac = ThreeAddressCode

