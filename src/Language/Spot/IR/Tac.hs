module Language.Spot.IR.Tac where

import Language.Spot.VM.Types

type SymId = Int
type FunAddr = Int
type ConstAddr = Int


data Constant =
    CPlainSymbol SymId
  | CCompoundSymbol SymId [Constant]
  | CNumber Int
  | CMatchData [Constant]
  | CMatchVar Int -- Can only be used inside CMatchData
  deriving (Show, Eq)

type ConstTable = [Constant] -- TODO move these out of here
type SymbolNameList = [String]

-- TODO make VMWord and Reg more typesafe and check range when constructing it
-- TODO don't use VMWord before actually getting to the VM specific parts of compilation?

type Reg = Int



data ThreeAddressCode var =
    Tac_ret
  | Tac_load_i var VMWord
  | Tac_load_addr var ConstAddr
  | Tac_load_f var FunAddr
  | Tac_load_ps var SymId         -- load plain symbol
  | Tac_load_cs var ConstAddr     -- load compound symbol
  | Tac_load_c var ConstAddr      -- load constant
  | Tac_add var var var
  | Tac_sub var var var
  | Tac_move var var
  | Tac_call var var Int
  | Tac_call_cl var var Int
  | Tac_make_cl var var Int
  | Tac_jmp Int
  | Tac_match var var var
  deriving (Show)

type Tac = ThreeAddressCode

