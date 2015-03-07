module Language.Spot.CodeGen.CodeGen (
  compile
) where

import Language.Spot.IR.Ast
import Language.Spot.IR.Opcode
import Language.Spot.VM.Bits

import Control.Monad.State
import Data.Word


data Code = Code {
  opcodes :: [[Opcode]]
, ctable :: ConstTable
, symnames :: SymbolNameList
}


compile :: Expr -> ([[Opcode]], ConstTable, SymbolNameList)
compile ast = (opcodes result, ctable result, reverse $ symnames result) --todo reverse most of this
  where result = execState (addFunction ast) startState

startState = Code { opcodes = [], ctable = [], symnames = [] }

addFunction :: Expr -> State Code ()
addFunction e = do
  beginFunction
  compileExpression e

compileExpression e = case e of
  LitNumber n   -> makeLitNumber n
  LitSymbol s vals -> makeLitSymbol s vals
  FunCall name args -> makeFunCall name args
  _ -> error "Unknown expression"

makeLitNumber n =
  addOpcodes [Op_load_i 0 (fromIntegral n)]


makeLitSymbol s [] = do
  newId <- addSymbolName s
  addOpcodes [Op_load_s 0 newId]

makeLitSymbol s args = do
  symId <- addSymbolName s
  let symHeader = encDataSymbolHeader symId (fromIntegral $ length args)
  let symEntry = symHeader : (map encodeAstValue args)
  newAddr <- addConstants symEntry
  addOpcodes [Op_load_sd 0 newAddr]


makeFunCall (Var "add") ((LitNumber op1):(LitNumber op2):[]) =
  makeMathFunc Op_add op1 op2

makeFunCall (Var "sub") ((LitNumber op1):(LitNumber op2):[]) =
  makeMathFunc Op_sub op1 op2

makeFunCall _ _ = error "unknown function"


makeMathFunc mf op1 op2 =
  addOpcodes [
    Op_load_i 1 (fromIntegral op1), -- TODO check range
    Op_load_i 2 (fromIntegral op2),
    mf 0 1 2
  ]



encodeAstValue (LitNumber n) = encNumber $ fromIntegral n
encodeAstValue _ = error "can't encode symbol"

beginFunction = modifyOpcodes (\opcs -> [] : opcs)

addOpcodes opcs = modifyOpcodes $ changeHead (++ opcs)

addSymbolName :: String -> State Code Word32
addSymbolName s = do
  nextId <- getCodeFieldLength symnames
  modifySymNames (s :)
  return $ fromIntegral nextId

addConstants :: [Word32] -> State Code Word32
addConstants cs = do
  nextAddr <- getCodeFieldLength ctable
  modifyConsts (++ cs)
  return $ fromIntegral nextAddr



getCodeFieldLength field = do
  st <- get
  return $ length (field st)

changeHead f l = (f $ head l) : tail l
modifyOpcodes f = modify (\st -> st { opcodes = f (opcodes st) })
modifySymNames f = modify (\st -> st { symnames = f (symnames st) })
modifyConsts f = modify (\st -> st { ctable = f (ctable st) })
