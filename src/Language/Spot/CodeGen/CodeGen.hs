module Language.Spot.CodeGen.CodeGen (
  compile
) where

import Language.Spot.CodeGen.CodeGenState
import Language.Spot.IR.Ast
import Language.Spot.IR.Opcode
import Language.Spot.VM.Bits

import Control.Monad.State
import Control.Applicative
import Data.Word
import Data.Maybe
import Data.Monoid
import Control.Exception.Base




compile :: Expr -> ([[Opcode]], ConstTable, SymbolNameList)
compile ast = (getOpcodes result, getCTable result, reverse $ getSymNames result) --todo reverse most of this
  where result = execState (addFunction ast) emptyCode


addFunction e = do
  beginFunction
  compileExpression e
  addOpcodes [Op_halt] -- TODO get rid of this, use op_ret
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
makeFunCall (Var n) args = do
  resReg <- resultReg
  fr <- regContainingVar n
  -- TODO the registers for args must come immediately after the one for the
  -- function address. Right now we're not making sure that that happens. Create
  -- op_load_f in here and just store the function address for n
  argRegs <- replicateM (length args) reserveReg
  let regsAndArgs = zip argRegs args
  forM_ regsAndArgs (\(aReg, arg) -> do
    evalArgument arg aReg)
  addOpcodes [ Op_call resReg fr (fromIntegral $ length args) ]
makeFunCall _ _ = error "Unknown function"

makeLocalBinding name (FunDef args expr) body = do
  r <- reserveReg
  funAddr <- makeFunction args expr
  addOpcodes [Op_load_f r funAddr]
  addVar name r
  compileExpression body
makeLocalBinding name expr body = do
  r <- reserveReg
  pushResultReg r
  compileExpression expr
  popResultReg
  addVar name r
  compileExpression body

makeFunction args expr = do
  funAddr <- beginFunction
  forM_ args (\arg -> do
    r <- reserveReg
    addVar arg r)
  compileExpression expr
  endFunction
  return funAddr

makeVar a = do
  r <- resultReg
  r1 <- regContainingVar a
  addOpcodes [ Op_move r r1 ]

makeMathFunc mf op1 op2 = do
  r <- resultReg
  argRegs <- replicateM 2 reserveReg
  let regsAndArgs = zip argRegs [op1, op2]
  forM_ regsAndArgs (\(aReg, arg) -> do
    evalArgument arg aReg)
  addOpcodes [ mf r (argRegs !! 0) (argRegs !! 1) ]

evalArgument (Var n) r = do
  vr <- regContainingVar n
  when (vr /= r) $ addOpcodes [ Op_move r vr ]

evalArgument arg r   = do
  pushResultReg r
  compileExpression arg
  popResultReg

encodeAstValue (LitNumber n) = encNumber $ fromIntegral n
encodeAstValue _ = error "can't encode symbol"






