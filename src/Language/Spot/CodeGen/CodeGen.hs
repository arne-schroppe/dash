module Language.Spot.CodeGen.CodeGen (
  compile
) where

import Language.Spot.CodeGen.CodeGenState
import Language.Spot.IR.Ast
import Language.Spot.IR.Tac
import Language.Spot.VM.Bits
import Language.Spot.VM.Types

import Control.Monad.State
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Control.Exception.Base




compile :: Expr -> ([[Tac]], ConstTable, SymbolNameList)
compile ast = (getInstructions result, getCTable result, getSymNames result) --todo reverse most of this
  where result = execState (addStartFunction ast) emptyCode

addStartFunction e = do
  beginFunction []
  code <- compileExpression e
  setFunctionCode 0 (code ++ [Tac_ret])
  endFunction



compileExpression e = case e of
  LitNumber n       -> compileLitNumber n
  LitSymbol s vals  -> compileLitSymbol s vals
  FunCall name args -> compileFunCall name args
  LocalBinding (Binding name expr) body -> compileLocalBinding name expr body
  Var a             -> compileVar a
  Match e pats      -> compileMatch e pats
  a -> error $ "Can't compile: " ++ show a

compileLitNumber n = do
  r <- resultReg
  return [Tac_load_i r (fromIntegral n)]

compileLitSymbol s []   = do
  newId <- addSymbolName s
  r <- resultReg
  return [Tac_load_s r newId]
compileLitSymbol s args = do
  newAddr <- encodeDataSymbol s args
  r <- resultReg
  return [Tac_load_sd r newAddr]

compileFunCall (Var "add") (op1:op2:[]) = -- do we really need opcodes for math stuff? How about built-in functions?
  compileMathFunc Tac_add op1 op2
compileFunCall (Var "sub") (op1:op2:[]) =
  compileMathFunc Tac_sub op1 op2
compileFunCall (Var funName) args = do
  resReg <- resultReg
  fr <- regContainingVar funName -- TODO it would be more efficient if we would simply load_f in here
  (code, nfr) <- ensureContinuousRegisters fr
  argRegs <- replicateM (length args) reserveReg
  let regsAndArgs = zip argRegs args
  argCode <- forM regsAndArgs (\(aReg, arg) -> do
    evalArgument arg aReg)
  return $ code ++
           (concat argCode) ++
           [ Tac_call resReg nfr (fromIntegral $ length args) ]
compileFunCall _ _ = error "Unknown function"

-- The registers for args must come immediately after the one for the
-- function address. Here we're making sure that that holds true.
ensureContinuousRegisters funcReg = do
  nextRegister <- peekReg
  if (nextRegister /= (funcReg + 1)) then do
    newFr <- reserveReg
    let code = [ Tac_move newFr funcReg ]
    return (code, newFr)
  else
    return ([], funcReg)

compileLocalBinding name (FunDef args expr) body = do
  r <- reserveReg
  funAddr <- compileFunction args expr
  addVar name r
  let code = [Tac_load_f r funAddr]
  bodyCode <- compileExpression body
  return $ code ++ bodyCode
compileLocalBinding name expr body = do
  r <- reserveReg
  pushResultReg r
  exprCode <- compileExpression expr
  popResultReg
  addVar name r
  bodyCode <- compileExpression body
  return $ exprCode ++
           bodyCode

compileFunction args expr = do
  funAddr <- beginFunction args
  code <- compileExpression expr
  setFunctionCode funAddr (code ++ [Tac_ret])
  endFunction
  return funAddr

compileVar a = do
  r <- resultReg
  r1 <- regContainingVar a
  return [ Tac_move r r1 ]

compileMathFunc mf op1 op2 = do
  r <- resultReg
  regsAndCode <- forM [op1, op2] (\arg ->
    case arg of
      Var n -> do
        reg <- regContainingVar n
        return (reg, [])
      a -> do
        reg <- reserveReg
        code <- evalArgument arg reg
        return (reg, code))
  let argRegs = map fst regsAndCode
  let varCode = map snd regsAndCode
  return $ (concat varCode) ++
           [ mf r (argRegs !! 0) (argRegs !! 1) ]


compileMatch expr patsAndExprs = do
  let numPats = length patsAndExprs
  (matchVars, matchDataAddr) <- storeMatchPattern $ map fst patsAndExprs
  matchStartCode <- createMatchCall expr matchDataAddr
  exprCodes <- mapM (uncurry compileSubExpression) (zip matchVars $ map snd patsAndExprs)

  let (exprJmpTargets, contJmpTargets) = unzip $ map (calcJumpTargets numPats exprCodes) [0..(numPats - 1)]
  let jmpTableCodes = map (singletonList . Tac_jmp) exprJmpTargets
  let bodyCodes = map (uncurry (++)) $ zip exprCodes $ map (singletonList . Tac_jmp) contJmpTargets

  let maxMatchVars = foldl (\a b -> max a (length b)) 0 matchVars
  mapM (const reserveReg) [1..maxMatchVars]
  -- TODO reserve maxMatchVars registers, since we can't use them afterwards

  return $ matchStartCode ++
           (concat jmpTableCodes) ++
           (concat bodyCodes)
  where
    compileSubExpression args expr = do
      pushSubContext
      addArguments args
      code <- compileExpression expr
      popSubContext
      return code

    storeMatchPattern ps = do
      -- matchVarsAndPatterns <- mapM encodePattern ps

      -- TODO the following function is a complete train wreck
      (_, matchVars, encodedPatterns) <- foldM (\(nextMV, accVars, accPats) p -> do
        (vars, encoded) <- encodePattern p nextMV
        return (nextMV + (fromIntegral $ length vars), accVars ++ [vars], accPats ++ [encoded])) (0, [], []) ps  -- TODO get that O(n*m) out and make it more clear what this does

      let encoded = encMatchHeader (fromIntegral $ length ps) : encodedPatterns
      constAddr <- addConstants encoded
      return (matchVars, constAddr)

    createMatchCall expr matchDataAddr = do
      subjReg <- reserveReg
      argCode <- evalArgument expr subjReg
      patReg <- reserveReg
      return $ argCode ++
               [ Tac_load_i patReg matchDataAddr
               , Tac_match subjReg patReg (patReg + 1) ]

    calcJumpTargets numPats exprCodes idx =
      let (pre, post) = splitAt idx exprCodes in
      let jmpToFirstExpr = (numPats - idx - 1) in
      let jmpToActualExpr = foldl (\acc ls -> acc + 1 + length ls) 0 pre in
      let exprJmpTarget = jmpToFirstExpr + jmpToActualExpr in

      -- contJmpTarget: relative address of code after the match body
      let lenRemainingResultExprs = foldl ( flip $ (+) . length) 0 (tail post) in
      let lenExtraJmps = (numPats - idx) - 1 in -- for every remaining result expr we need to add one jump expr
      let contJmpTarget = lenRemainingResultExprs + lenExtraJmps in

      (fromIntegral exprJmpTarget, fromIntegral contJmpTarget)

    singletonList = (:[])



-- for match vars:
-- keep track of vars for each expression
-- reserve maximum var number of registers
-- when evaluating expr, do so in a local context with those registers

encodePattern pat nextMatchVar =
  case pat of
    PatNumber n -> return ([], (encNumber $ fromIntegral n))
    PatSymbol s [] -> do sid <- addSymbolName s
                         return $ ([], encSymbol sid)
    PatSymbol s params -> do
                  (vars, addr) <- encodePatternDataSymbol s params nextMatchVar
                  return (vars, encDataSymbol addr)
    PatVar n -> return $ ([n], encMatchVar nextMatchVar)

evalArgument (Var n) targetReg = do
  vr <- regContainingVar n -- This is wasteful. In most cases, we'll just reserve two registers per var
  if (vr /= targetReg) then do return [ Tac_move targetReg vr ]
  else return []
evalArgument arg r = do
  pushResultReg r
  code <- compileExpression arg
  popResultReg
  return code


-- TODO unify these two functions!
encodePatternDataSymbol s args nextMatchVar = do
  symId <- addSymbolName s
  let symHeader = encDataSymbolHeader symId (fromIntegral $ length args)
  (_, vars, entries) <- foldM (\(nextMV, accVars, pats) p -> do
    (vars, encoded) <- encodePattern p nextMV
    return (nextMV + (fromIntegral $ length vars), accVars ++ vars, pats ++ [encoded])) (nextMatchVar, [], []) args  -- TODO get that O(n*m) out and make it more clear what this does
  let symEntry = symHeader : entries
  addr <- addConstants symEntry
  return (vars, addr)

encodeDataSymbol s args = do
  symId <- addSymbolName s
  let symHeader = encDataSymbolHeader symId (fromIntegral $ length args)
  encodedArgs <- mapM encodeAstValue args
  let symEntry = symHeader : encodedArgs
  addConstants symEntry


-- TODO fix naming, we have too much encDataSymbol and encodeDataSymbol overlap
encodeAstValue (LitNumber n) = return $ encNumber $ fromIntegral n
encodeAstValue (LitSymbol s args) = do
                          addr <- encodeDataSymbol s args
                          return $ encDataSymbol addr
encodeAstValue _ = error "can't encode symbol"



