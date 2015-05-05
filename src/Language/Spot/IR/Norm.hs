module Language.Spot.IR.Norm where

-- TODO this is not ANF anymore. Just call it Norm

import Language.Spot.VM.Types
import Language.Spot.IR.Ast

data NormExpr =
    NAtom NormAtomicExpr
  | NLet NormVar NormAtomicExpr NormExpr
  deriving (Eq, Show)

data NormAtomicExpr =
    NNumber Int
  | NPlainSymbol Int
--  | NCompoundSymbol Int [NormVar]  -- this is somewhat complicated, as we have to distinguish between dynamic and static symbols
  | NFreeVar String
  | NResultVar NormVar
  | NLambda [String] [String] NormExpr  -- FreeVars FormalParams Body
  | NPrimOp NormPrimOp
  | NFunCall [NormVar]
  | NMatch Int NormVar [(Pattern, NormExpr)] -- MaxCaptures Subject (Patterns, Expr)
  deriving (Eq, Show)

newtype NormVar = NVar { normVarValue :: Int }
  deriving (Eq, Show)

data NormPrimOp =
    NPrimOpAdd NormVar NormVar
  | NPrimOpSub NormVar NormVar
  deriving (Eq, Show)




