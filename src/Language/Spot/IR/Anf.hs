module Language.Spot.IR.Anf where

import Language.Spot.VM.Types
import Language.Spot.IR.Ast

data NormExpr =
    NAtom NormAtomicExpr
  | NMatch Int NormVar [(Pattern, NormExpr)] -- MaxCaptures Subject (Patterns, Expr)
  | NLet NormVar NormAtomicExpr NormExpr
  deriving (Eq, Show)

data NormAtomicExpr =
    NNumber Int
  | NPlainSymbol Int
  | NVar NormVar
  | NLambda [String] [String] NormExpr  -- FreeVars FormalParams Body
  | NPrimOp NormPrimOp
  | NFunCall [NormVar]
  deriving (Eq, Show)

data NormVar =
    NTempVar Int
  | NNamedVar String
  deriving (Eq, Show)

data NormPrimOp =
    NPrimOpAdd NormVar NormVar
  | NPrimOpSub NormVar NormVar
  deriving (Eq, Show)




