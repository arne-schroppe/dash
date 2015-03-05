module Language.Spot.CodeGen.CodeGen where

import Language.Spot.IR.Ast
import Language.Spot.IR.Opcode



generateCode :: Expr -> ([[Opcode]], ConstTable)
generateCode ast = ([[Op_halt]], ConstTable [])
