module Language.Spot.CodeGen.CodeGen where

import Language.Spot.IR.Ast
import Language.Spot.IR.Opcode

import Control.Monad.State


data Code = Code {
  opcodes :: [[Opcode]]
, ctable :: ConstTable
, symnames :: SymbolNameList
}


generateCode :: Expr -> ([[Opcode]], ConstTable, SymbolNameList)
generateCode ast = (opcodes result, ctable result, reverse $ symnames result) --todo reverse most of this
  where result = execState (addFunction ast) startState

startState = Code { opcodes = [], ctable = [], symnames = [] }

addFunction :: Expr -> State Code ()
addFunction e = do
  beginFunction
  compileExpression e

compileExpression e = case e of
  LitNumber n   -> makeLitNumber n
  LitSymbol s _ -> makeLitSymbol s
  FunCall name args -> makeFunCall name args
  _ -> error "Unknown expression"

makeLitNumber n =
  addOpcodes [Op_load_i 0 (fromIntegral n)]

makeLitSymbol s = do
  addOpcodes [Op_load_s 0 0]
  addSymbolName s

makeFunCall (Var "add") ((LitNumber a):(LitNumber b):[]) = 
  addOpcodes [
    Op_load_i 1 (fromIntegral a), -- TODO check range
    Op_load_i 2 (fromIntegral b),
    Op_add 0 1 2 
  ]

makeFunCall _ _ = error "unknown function"


beginFunction = modifyOpcodes (\opcs -> [] : opcs)

addOpcodes opcs = modifyOpcodes $ changeHead (++ opcs)
addSymbolName s = modifySymNames (s :)

changeHead f l = (f $ head l) : tail l
modifyOpcodes f = modify (\st -> st { opcodes = f (opcodes st) })
modifySymNames f = modify (\st -> st { symnames = f (symnames st) })
