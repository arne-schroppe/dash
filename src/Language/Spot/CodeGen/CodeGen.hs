module Language.Spot.CodeGen.CodeGen where

import Language.Spot.IR.Ast
import Language.Spot.IR.Opcode



generateCode :: Expr -> ([[Opcode]], ConstTable, SymbolNameList)
generateCode ast = case ast of
  LitNumber n -> make_lit_number n
  LitSymbol s _ -> make_lit_symbol s
  _ -> error "Unknown expression"


make_lit_number n = ([[Op_load_i 0 (fromIntegral n)]], [], [])
make_lit_symbol s = ([[Op_load_s 0 0]], [], [s])

