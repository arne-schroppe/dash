module Language.Spot.CodeGen.CodeGen where

import Language.Spot.IR.Ast


data Opcode = Opcode ()

generateCode :: Expr -> [Opcode]
generateCode = error "not implemented"
