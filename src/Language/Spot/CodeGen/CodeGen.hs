module Language.Spot.CodeGen.CodeGen where

import Language.Spot.IR.Ast
import Language.Spot.IR.Opcode



generateCode :: Expr -> ([[Opcode]], ConstTable)
generateCode ast = case ast of
  LitNumber n -> (make_lit_number n, ConstTable [])
  _ -> error "Unknown expression"


make_lit_number n = [[Op_load_i 0 (fromIntegral n)]]
