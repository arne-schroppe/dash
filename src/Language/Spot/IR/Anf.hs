module Language.Spot.IR.Anf where

import Language.Spot.VM.Types
import Language.Spot.IR.Ast

data NormVar =
    NTempVar Int
  | NNamedVar String
  deriving (Eq, Show)

data NormPrimOp =
    NPrimOpAdd NormVar NormVar
  | NPrimOpSub NormVar NormVar
  deriving (Eq, Show)

data NormAtomicExpr =
    NNumber Int
  | NPlainSymbol Int
  | NVar NormVar
  | NLambda [String] [String] NormExpr  -- FreeVars FormalParams Body
  deriving (Eq, Show)

data NormExpr =
    NAtom NormAtomicExpr
  | NPrimOp NormPrimOp
  | NLet NormVar NormAtomicExpr NormExpr
  | NMatch NormAtomicExpr [(Pattern, NormExpr)]
  deriving (Eq, Show)


