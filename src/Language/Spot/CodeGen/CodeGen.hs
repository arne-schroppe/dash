module Language.Spot.CodeGen.CodeGen (
  compile
) where

import Language.Spot.IR.Ast
import Language.Spot.IR.Opcode
import Language.Spot.VM.Bits

import Control.Monad.State
import Control.Applicative
import Data.Word
import Data.Maybe
import Control.Exception.Base
import qualified Data.Map as Map
import Debug.Trace


compile :: Expr -> ([[Opcode]], ConstTable, SymbolNameList)
compile ast = (opcodes result, ctable result, reverse $ symnames result) --todo reverse most of this
  where result = execState (addFunction ast) startState


addFunction :: Expr -> State Code ()
addFunction e = do
  beginFunction
  compileExpression e
  endFunction

compileExpression e = case e of
  LitNumber n       -> makeLitNumber n
  LitSymbol s vals  -> makeLitSymbol s vals
  FunCall name args -> makeFunCall name args
  LocalBinding (Binding name expr) body -> makeLocalBinding name expr body
  Var a             -> makeVar a
  a -> error $ "Can't compile: " ++ show a

makeLitNumber n = do
  r <- resultReg
  addOpcodes [Op_load_i r (fromIntegral n)]

makeLitSymbol s []   = do
  newId <- addSymbolName s
  r <- resultReg
  addOpcodes [Op_load_s r newId]
makeLitSymbol s args = do
  symId <- addSymbolName s
  r <- resultReg
  let symHeader = encDataSymbolHeader symId (fromIntegral $ length args)
  let symEntry = symHeader : (map encodeAstValue args)
  newAddr <- addConstants symEntry
  addOpcodes [Op_load_sd r newAddr]

makeFunCall (Var "add") (op1:op2:[]) =
  makeMathFunc Op_add op1 op2
makeFunCall (Var "sub") (op1:op2:[]) =
  makeMathFunc Op_sub op1 op2
makeFunCall _ _ = undefined

makeLocalBinding name expr body = do
  r <- reserveReg
  pushResultReg r
  compileExpression expr
  popResultReg
  addVar name r
  compileExpression body

makeVar a = do
  r <- resultReg
  r1 <- registerContainingVar a
  addOpcodes [ Op_move r r1 ]

makeMathFunc mf op1 op2 = do
  r <- resultReg
  r1 <- evalArgument op1
  r2 <- evalArgument op2
  addOpcodes [ mf r r1 r2 ]

evalArgument (Var n) = registerContainingVar n
evalArgument arg = do
  r <- reserveReg
  pushResultReg r
  compileExpression arg
  popResultReg
  return r

encodeAstValue (LitNumber n) = encNumber $ fromIntegral n
encodeAstValue _ = error "can't encode symbol"

beginFunction = do
  r <- reserveReg
  pushResultReg r
  modifyOpcodes (\opcs -> [] : opcs)

endFunction = popResultReg




------- State


reserveReg = do
  r <- gets reservedRegisters
  modify (\st -> st { reservedRegisters = r + 1 })
  return r

resultReg = gets $ head . resultRegStack

pushResultReg r =
  modify (\st -> st { resultRegStack = r : (resultRegStack st) })

popResultReg =
  modify (\st -> st { resultRegStack = tail $ resultRegStack st })

data Code = Code {
  opcodes :: [[Opcode]]
, ctable :: ConstTable
, symnames :: SymbolNameList

-- for current function only (TODO add stack)
, reservedRegisters :: Word32
, resultRegStack :: [Word32]
, bindings :: Map.Map String Word32
}

startState = Code {
  opcodes = []
, ctable = []
, symnames = []
, reservedRegisters = 0
, resultRegStack = []
, bindings = Map.empty
}

addVar n r = modify (\st -> st { bindings = Map.insert n r $ bindings st })

registerContainingVar n = gets $ fromJust . Map.lookup n . bindings

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

getCodeFieldLength field = gets $ length . field

changeHead f l = (f $ head l) : tail l

modifyOpcodes f = modify (\st -> st { opcodes = f (opcodes st) })

modifySymNames f = modify (\st -> st { symnames = f (symnames st) })

modifyConsts f = modify (\st -> st { ctable = f (ctable st) })
