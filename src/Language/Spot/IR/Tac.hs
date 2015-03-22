module Language.Spot.IR.Tac where

import Language.Spot.VM.Types

type SymId = Int
type FunAddr = Int
type ConstAddr = VMWord

data PatternConstant =
    PatCSymbol SymId
  | PatCDataSymbol SymId [PatternConstant]
  | PatCNumber Int
  | PatCVar Int


data Constant =
    CSymbol SymId
  | CDataSymbol SymId [Constant]
  | CNumber Int
  | CMatchData [PatternConstant]

type ConstTable = [Constant]
type SymbolNameList = [String]

-- TODO make VMWord and Reg more typesafe and check range when constructing it
-- TODO don't use VMWord before actually getting to the VM specific parts of compilation?

type Reg = Int



data ThreeAddressCode =
    Tac_ret
  | Tac_load_i Reg VMWord
  | Tac_load_f Reg FunAddr
  | Tac_load_s Reg SymId
  | Tac_load_sd Reg ConstAddr
  | Tac_load_c Reg ConstAddr
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

