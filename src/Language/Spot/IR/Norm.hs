module Language.Spot.IR.Norm (
  NormExpr (..)
, NormAtomicExpr (..)
, NormVar (..)
, NormPrimOp (..)
) where


import           Language.Spot.IR.Ast
import           Language.Spot.IR.Data  (ConstAddr)
import           Language.Spot.VM.Types

-- TODO rename to NST (NormalizedSyntaxTree) or something

data NormExpr =
    NAtom NormAtomicExpr
  | NLet NormVar NormAtomicExpr NormExpr
  deriving (Eq, Show)


data NormAtomicExpr =
    NNumber Int
  | NPlainSymbol Int
  | NCompoundSymbol Bool ConstAddr  -- IsDynamic SymbolAddr
  | NVar NormVar -- This is only for returning a var as a result
  | NLambda [String] [String] NormExpr  -- FreeVars FormalParams Body -- TODO free vars and formal params should appear in the same order as later in assembly!
  | NPrimOp NormPrimOp
  | NFunCall NormVar [NormVar]
  | NMatch Int NormVar ConstAddr [([String], NormVar)] -- MaxCaptures Subject PatternAddr [MatchedVars, Var-With-Lambda]
  deriving (Eq, Show)


data NormVar =
    NLocalVar Int String
  | NFunParam String
  | NDynamicFreeVar String
  | NConstantFreeVar String -- We should rename this to StaticFreeVar
  | NRecursiveVar String
  deriving (Eq, Show)

data NormPrimOp =
    NPrimOpAdd NormVar NormVar
  | NPrimOpSub NormVar NormVar
  deriving (Eq, Show)




