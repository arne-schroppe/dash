module Language.Spot.IR.Tac (
  ThreeAddressCode (..)
, Tac
) where

import Language.Spot.VM.Types
import Language.Spot.IR.Data


-- TODO should we express known and unknown functions through separate types or data constructors?

data ThreeAddressCode =
    Tac_ret Reg
  | Tac_load_i Reg VMWord
  | Tac_load_addr Reg ConstAddr
  | Tac_load_f Reg FunAddr        -- load function address (code)
  | Tac_load_ps Reg SymId         -- load plain symbol
  | Tac_load_cs Reg ConstAddr     -- load compound symbol
  | Tac_load_c Reg ConstAddr      -- load constant
  | Tac_add Reg Reg Reg
  | Tac_sub Reg Reg Reg
  | Tac_move Reg Reg
  | Tac_call Reg Reg Int          -- expects a function address (code)
  | Tac_gen_ap Reg Reg Int        -- expects a closure address (heap)
  | Tac_make_cl Reg Reg Int Int   -- expects a function address (code)
  | Tac_jmp Int
  | Tac_match Reg Reg Reg         -- subj reg, pattern addr reg, start reg for captures
  | Tac_set_arg Int Reg Int
  | Tac_tail_call Reg Int         -- expects a function address (code)
  | Tac_tail_gen_ap Reg Reg Int   -- result reg (since this might do partial application), closure reg (heap), num args
  | Tac_set_cl_val Reg Reg Int    -- expects closure address (heap)
  | Tac_part_ap Reg Reg Int Int   -- result, reg with function address (code), num args, num formal params
  deriving (Show)

type Tac = ThreeAddressCode

