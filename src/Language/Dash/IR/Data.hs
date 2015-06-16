module Language.Dash.IR.Data (
  Constant (..)
, SymId
, FunAddr
, ConstAddr
, ConstTable
, SymbolNameList
, Reg
) where



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
