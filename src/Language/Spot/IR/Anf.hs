module Language.Spot.IR.Anf where

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
  | NVar String
  | NLambda [String] [String] NormExpr  -- FreeVars FormalParams Body
  | NPrimOp NormPrimOp
  | NFunCall [NormVar]
  | NMatch Int NormVar [(Pattern, NormExpr)] -- MaxCaptures Subject (Patterns, Expr)
  deriving (Eq, Show)

data NormVar =
    NTempVar Int
  -- | NNamedVar String
  deriving (Eq, Show)

data NormPrimOp =
    NPrimOpAdd NormVar NormVar
  | NPrimOpSub NormVar NormVar
  deriving (Eq, Show)




