module Language.Spot.IR.Anf where

import Language.Spot.VM.Types

-- TODO Norm instead of Anf
-- TODO also, find a naming scheme that prevents `AnfVar AnfVar` confusion
data AnfVar =
    AnfTempVar Int
  | AnfNamedVar String
  deriving (Eq, Show)

data AnfPrimOp =
    AnfPrimOpAdd AnfVar AnfVar
  | AnfPrimOpSub AnfVar AnfVar
  deriving (Eq, Show)

data AnfAtomicExpr =
    AnfNumber Int
  | AnfPlainSymbol Int
  | AnfVar AnfVar
  | AnfLambda [String] [String] AnfExpr  -- FreeVars FormalParams Body
  deriving (Eq, Show)

data AnfExpr =
    AnfAtom AnfAtomicExpr
  | AnfPrimOp AnfPrimOp
  | AnfLet AnfVar AnfAtomicExpr AnfExpr
  deriving (Eq, Show)


