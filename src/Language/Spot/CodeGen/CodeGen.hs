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
compile ast = (getOpcodes result, getCTable result, getSymNames result) --todo reverse most of this
  where result = execState (addStartFunction ast) emptyCode


addStartFunction e = do
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
  Match e pats      -> makeMatch e pats
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

makeFunCall (Var "add") (op1:op2:[]) = -- do we really need opcodes for math stuff? How about built-in functions?
  makeMathFunc Op_add op1 op2
makeFunCall (Var "sub") (op1:op2:[]) =
  makeMathFunc Op_sub op1 op2
makeFunCall (Var funName) args = do
  resReg <- resultReg
  fr <- regContainingVar funName -- TODO it would be more efficient if we would simply load_f in here
  nfr <- ensureContinuousRegisters fr
  argRegs <- replicateM (length args) reserveReg
  let regsAndArgs = zip argRegs args
  forM_ regsAndArgs (\(aReg, arg) -> do
    evalArgument arg aReg)
  addOpcodes [ Op_call resReg nfr (fromIntegral $ length args) ]
makeFunCall _ _ = error "Unknown function"

-- The registers for args must come immediately after the one for the
-- function address. Here we're making sure that that holds true.
ensureContinuousRegisters funcReg = do
  nextRegister <- peekReg
  if (nextRegister /= (funcReg + 1)) then do
    newFr <- reserveReg
    addOpcodes [ Op_move newFr funcReg ]
    return newFr
  else
    return funcReg

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

makeMatch expr pes = do
  let numPats = length pes
  let patterns = map fst pes
  let expressions = map snd pes
  let matchData = encMatchHeader (fromIntegral numPats) : map encodePattern patterns
  matchDataAddr <- addConstants matchData
  subjR <- reserveReg
  evalArgument expr subjR
  patR <- reserveReg
  addOpcodes [Op_load_i patR matchDataAddr]
  addOpcodes [Op_match subjR patR 0]
  -- eval expressions
  -- create jump table
  -- add an op_jmp after every sub expression

encodePattern pat =
  case pat of
    PatNumber n -> encNumber $ fromIntegral n
    x -> error $ "Can't encode match pattern: " ++ (show x)

evalArgument (Var n) targetReg = do
  vr <- regContainingVar n -- This is also wasteful. In most cases, we'll just reserve two registers per var
  when (vr /= targetReg) $ do addOpcodes [ Op_move targetReg vr ]; return ()
evalArgument arg r   = do
  pushResultReg r
  compileExpression arg
  popResultReg

encodeAstValue (LitNumber n) = encNumber $ fromIntegral n
encodeAstValue _ = error "can't encode symbol"






