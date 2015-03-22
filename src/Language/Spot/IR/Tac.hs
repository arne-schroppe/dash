module Language.Spot.IR.Tac where

import Language.Spot.VM.Types

type ConstTable = [VMWord]
type SymbolNameList = [String]

-- TODO make VMWord and Reg more typesafe and check range when constructing it
-- TODO don't use VMWord before actually getting to the VM specific parts of compilation?

type Reg = Int



data ThreeAddressCode =
    Tac_ret
  | Tac_load_i Reg VMWord
  | Tac_load_f Reg Int
  | Tac_load_s Reg VMWord
  | Tac_load_sd Reg VMWord
  | Tac_load_c Reg VMWord
  | Tac_add Reg Reg Reg
  | Tac_sub Reg Reg Reg
  | Tac_move Reg Reg
  | Tac_call Reg Reg Int
  | Tac_call_cl Reg Reg Int
  | Tac_make_cl Reg Reg Int
  | Tac_jmp Int
  | Tac_match Reg Reg Reg
  deriving (Show)

type Tac = ThreeAddressCode

