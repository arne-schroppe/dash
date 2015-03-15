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
  opcs <- compileExpression e
  addOpcodes $ opcs ++ [Op_halt] -- TODO get rid of this, use op_ret
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
  return [Op_load_i r (fromIntegral n)]

makeLitSymbol s []   = do
  newId <- addSymbolName s
  r <- resultReg
  return [Op_load_s r newId]
makeLitSymbol s args = do
  symId <- addSymbolName s
  r <- resultReg
  let symHeader = encDataSymbolHeader symId (fromIntegral $ length args)
  let symEntry = symHeader : (map encodeAstValue args)
  newAddr <- addConstants symEntry
  return [Op_load_sd r newAddr]

makeFunCall (Var "add") (op1:op2:[]) = -- do we really need opcodes for math stuff? How about built-in functions?
  makeMathFunc Op_add op1 op2
makeFunCall (Var "sub") (op1:op2:[]) =
  makeMathFunc Op_sub op1 op2
makeFunCall (Var funName) args = do
  resReg <- resultReg
  fr <- regContainingVar funName -- TODO it would be more efficient if we would simply load_f in here
  (code, nfr) <- ensureContinuousRegisters fr
  argRegs <- replicateM (length args) reserveReg
  let regsAndArgs = zip argRegs args
  argCode <- forM regsAndArgs (\(aReg, arg) -> do
    evalArgument arg aReg)
  return $ code ++ (concat argCode) ++ [ Op_call resReg nfr (fromIntegral $ length args) ]
makeFunCall _ _ = error "Unknown function"

-- The registers for args must come immediately after the one for the
-- function address. Here we're making sure that that holds true.
ensureContinuousRegisters funcReg = do
  nextRegister <- peekReg
  if (nextRegister /= (funcReg + 1)) then do
    newFr <- reserveReg
    let opcs = [ Op_move newFr funcReg ]
    return (opcs, newFr)
  else
    return ([], funcReg)

makeLocalBinding name (FunDef args expr) body = do
  r <- reserveReg
  funAddr <- makeFunction args expr
  addVar name r
  let opcs = [Op_load_f r funAddr]
  bodyOpcs <- compileExpression body
  return $ opcs ++ bodyOpcs
makeLocalBinding name expr body = do
  r <- reserveReg
  pushResultReg r
  expOpcs <- compileExpression expr
  popResultReg
  addVar name r
  bodyOpcs <- compileExpression body
  return $ expOpcs ++ bodyOpcs

makeFunction args expr = do
  funAddr <- beginFunction
  forM_ args (\arg -> do
    r <- reserveReg
    addVar arg r)
  opcs <- compileExpression expr
  addOpcodes opcs
  endFunction
  return funAddr

makeVar a = do
  r <- resultReg
  r1 <- regContainingVar a
  return [ Op_move r r1 ]

makeMathFunc mf op1 op2 = do
  r <- resultReg
  argRegs <- replicateM 2 reserveReg
  let regsAndArgs = zip argRegs [op1, op2]
  varCode <- forM regsAndArgs (\(aReg, arg) -> do
    evalArgument arg aReg)
  return $ (concat varCode) ++ [ mf r (argRegs !! 0) (argRegs !! 1) ]

makeMatch expr pes = do
  let numPats = length pes
  let patterns = map fst pes
  let expressions = map snd pes
  let matchData = encMatchHeader (fromIntegral numPats) : map encodePattern patterns
  matchDataAddr <- addConstants matchData
  subjR <- reserveReg
  argCode <- evalArgument expr subjR
  patR <- reserveReg
  let matchStart = [ Op_load_i patR matchDataAddr
                   , Op_match subjR patR 0 ]
  resultCode <- forM expressions (\expr -> compileExpression expr)
  let numCases = length expressions
  let idxs = init [0..numCases]
  jmpTargets <- forM idxs (\idx -> do
    let jmpToFirstExpr = (numCases - idx - 1)
    let jmpToActualExpr = foldl (\acc ls -> acc + 1 + length ls)  0 (take idx resultCode)
    let exprJmpTarget = fromIntegral $ jmpToFirstExpr + jmpToActualExpr
    let contJmpTarget = fromIntegral $ (foldl ( flip $ (+) . length) 0 (drop (idx + 1) resultCode)) + (numCases - idx) - 1
    return (exprJmpTarget, contJmpTarget) )

  jmpTableCode <- forM (map fst jmpTargets) (\exprJmpTarget -> return [ Op_jmp exprJmpTarget ])
  exprCode <- forM (zip idxs (map snd jmpTargets)) $ uncurry (\idx contJmpTarget -> 
                return $ (resultCode !! idx) ++ [ Op_jmp contJmpTarget] )

  return $ argCode ++ matchStart ++ (concat jmpTableCode) ++ (concat exprCode)

encodePattern pat =
  case pat of
    PatNumber n -> encNumber $ fromIntegral n
    x -> error $ "Can't encode match pattern: " ++ (show x)

evalArgument (Var n) targetReg = do
  vr <- regContainingVar n -- This is also wasteful. In most cases, we'll just reserve two registers per var
  if (vr /= targetReg) then do return [ Op_move targetReg vr ]
  else return []
evalArgument arg r   = do
  pushResultReg r
  opcs <- compileExpression arg
  popResultReg
  return opcs

encodeAstValue (LitNumber n) = encNumber $ fromIntegral n
encodeAstValue _ = error "can't encode symbol"






