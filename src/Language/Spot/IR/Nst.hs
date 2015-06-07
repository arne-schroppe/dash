module Language.Spot.IR.Nst (
  NstExpr (..)
, NstAtomicExpr (..)
, NstVar (..)
, NstPrimOp (..)
) where


import           Language.Spot.IR.Ast
import           Language.Spot.IR.Data  (ConstAddr)
import           Language.Spot.VM.Types

-- TODO rename to NST (NstalizedSyntaxTree) or something

data NstExpr =
    NAtom NstAtomicExpr
  | NLet NstVar NstAtomicExpr NstExpr
  deriving (Eq, Show)


data NstAtomicExpr =
    NNumber Int
  | NPlainSymbol Int
  | NCompoundSymbol Bool ConstAddr  -- IsDynamic SymbolAddr
  | NVar NstVar -- This is only for returning a var as a result
  | NLambda [String] [String] NstExpr  -- FreeVars FormalParams Body -- TODO free vars and formal params should appear in the same order as later in assembly!
  | NPrimOp NstPrimOp
  | NFunCall NstVar [NstVar]
  | NMatch Int NstVar ConstAddr [([String], NstVar)] -- MaxCaptures Subject PatternAddr [MatchedVars, Var-With-Lambda]
  deriving (Eq, Show)


data NstVar =
    NLocalVar Int String
  | NFunParam String
  | NDynamicFreeVar String
  | NConstantFreeVar String -- We should rename this to StaticFreeVar
  | NRecursiveVar String
  deriving (Eq, Show)

data NstPrimOp =
    NPrimOpAdd NstVar NstVar
  | NPrimOpSub NstVar NstVar
  deriving (Eq, Show)




