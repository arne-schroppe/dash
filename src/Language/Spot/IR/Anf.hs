module Language.Spot.IR.Anf where

import Language.Spot.VM.Types

data AnfExpr =
    AnfNumber Int
  | AnfPlainSymbol String
  deriving (Eq, Show)

{-
  | ANFString
  | ANFVar

data ANFExpr =
    ANFLet 

-}

