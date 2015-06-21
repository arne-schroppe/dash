module Language.Dash.CodeGen.CodeGen (
  compile
) where

import           Data.Foldable
import           Data.Maybe                         (catMaybes)
import           Language.Dash.CodeGen.CodeGenState
import           Language.Dash.IR.Data
import           Language.Dash.IR.Nst
import           Language.Dash.IR.Tac
import           Control.Monad.State


-- TODO explain what the code generator does and how it does it !




-- TODO when there is more time, do dataflow analysis to reuse registers

-- TODO 'atom' could be misleading. Rename to 'atomExpr' or something like that

-- TODO after refactoring the VM, we need to change everything that is called
-- something with 'register' to something like 'localVar' or so. Otherwise
-- the uninitiated observer might think that we don't know what a register is.


compile :: NstExpr -> ConstTable -> SymbolNameList -> ([[Tac]], ConstTable, SymbolNameList)
compile expr cTable symlist =
  let result = execState (compileFunc [] [] expr "" False) emptyCompEnv in
  (toList (instructions result), cTable, symlist)


compileFunc :: [String] -> [String] -> NstExpr -> String -> Bool -> CodeGenState Int
compileFunc freeVars params expr name shouldAddHeader = do
  funAddr <- beginFunction freeVars params
  -- we add the name already here for recursion
  addCompileTimeConst name $ CTConstLambda funAddr
  funcCode <- compileExpr expr

  if shouldAddHeader then do
    let arity = length freeVars + length params
    let funcCode' = (Tac_fun_header arity) : funcCode
    endFunction funAddr funcCode'
  else
    endFunction funAddr funcCode

  addCompileTimeConst name $ CTConstLambda funAddr -- Have to re-add to outer scope    TODO this sucks
  return funAddr


compileExpr :: NstExpr -> CodeGenState [Tac]
compileExpr expr = case expr of
  NLet var atom body -> compileLet var atom body
  NAtom a -> do
          code <- compileAtom 0 a "" True
          return $ code ++ [Tac_ret 0]


compileAtom :: Reg -> NstAtomicExpr -> String -> Bool -> CodeGenState [Tac]
compileAtom reg atom name isResultValue = case atom of
  NNumber n -> do
          addCompileTimeConst name $ CTConstNumber (fromIntegral n)
          return [Tac_load_i reg (fromIntegral n)]
  NPlainSymbol sid -> do
          addCompileTimeConst name $ CTConstPlainSymbol sid
          return [Tac_load_ps reg sid]
  NCompoundSymbol False cAddr -> do
          -- addCompileTimeConst name $ CConstCompoundSymbol cAddr
          return [Tac_load_cs reg cAddr] -- TODO codeConstant?
  NPrimOp primop -> compilePrimOp primop reg
  NLambda [] params expr -> do
          funAddr <- compileFunc [] params expr name True
          compileLoadLambda reg funAddr isResultValue
  NLambda freeVars params expr -> compileClosure reg freeVars params expr name
  NFunCall funVar args -> do
          argInstrs <- mapM (uncurry compileSetArg) $ zipWithIndex args
          callInstr <- compileCallInstr reg funVar (length args) isResultValue
          return $ argInstrs ++ callInstr
  NVar var -> case var of
          NLocalVar _ _          -> moveVarToReg var reg
          NFunParam _            -> moveVarToReg var reg
          NDynamicFreeVar _      -> moveVarToReg var reg
          NConstantFreeVar vname -> compileConstantFreeVar reg vname isResultValue
          x -> error $ "Internal compiler error: Unexpected variable type: " ++ show x
  NMatch maxCaptures subject patternAddr branches ->
          compileMatch reg subject maxCaptures patternAddr branches isResultValue
  NPartAp funVar args -> do
          argInstrs <- mapM (uncurry compileSetArg) $ zipWithIndex args
          rFun <- getReg funVar
          let numArgs = length args
          let partApInst = [Tac_part_ap reg rFun numArgs]
          return $ argInstrs ++ partApInst
  x -> error $ "Unable to compile " ++ (show x)
  where
    moveVarToReg :: NstVar -> Reg -> CodeGenState [Tac]
    moveVarToReg var dest = do
              r <- getReg var
              return [Tac_move dest r]



compileCallInstr :: Reg -> NstVar -> Int -> Bool -> CodeGenState [Tac]
compileCallInstr reg funVar numArgs isResultValue = do
          rFun <- getReg funVar
          direct <- isDirectCallReg rFun
          -- TODO maybe the normalizer should already resolve what is a call to a known function and what isn't?
          let instr = case (direct, isResultValue) of
                  (True, False)  -> [Tac_call reg rFun numArgs]
                  (True, True)   -> [Tac_tail_call rFun numArgs]
                  (False, False) -> [Tac_gen_ap reg rFun numArgs]
                  (False, True)  -> [Tac_tail_gen_ap reg rFun numArgs]
          return instr


compilePrimOp :: NstPrimOp -> Reg -> CodeGenState [Tac]
compilePrimOp primop reg = case primop of
  NPrimOpAdd a b -> compileBinaryPrimOp Tac_add a b
  NPrimOpSub a b -> compileBinaryPrimOp Tac_sub a b
  NPrimOpMul a b -> compileBinaryPrimOp Tac_mul a b
  NPrimOpDiv a b -> compileBinaryPrimOp Tac_div a b
  NPrimOpEq a b -> compileBinaryPrimOp Tac_eq a b
  where
    compileBinaryPrimOp op a b = do
          ra <- getReg a
          rb <- getReg b
          return [op reg ra rb]

compileConstantFreeVar :: Reg -> String -> Bool -> CodeGenState [Tac]
compileConstantFreeVar reg name isResultValue = do
  compConst <- getCompileTimeConstInOuterScope name
  case compConst of
          CTConstNumber n -> return [Tac_load_i reg (fromIntegral n)] -- how about storing the constant in const table and simply load_c it here?
          CTConstPlainSymbol symId -> return [Tac_load_ps reg symId]
          -- CConstCompoundSymbol ConstAddr
          CTConstLambda funAddr -> compileLoadLambda reg funAddr isResultValue
          _ -> error "compileConstantFreeVar"


compileLoadLambda :: Reg -> Int -> Bool -> CodeGenState [Tac]
compileLoadLambda reg funAddr isResultValue = do
  let ldFunAddr = [Tac_load_f reg funAddr]
  if isResultValue then
      return $ ldFunAddr ++ [Tac_part_ap reg reg 0]
  else
      return $ ldFunAddr


compileLet :: NstVar -> NstAtomicExpr -> NstExpr -> CodeGenState [Tac]
compileLet tmpVar atom body =
  case tmpVar of
    NLocalVar _ name -> compileLet' name
    _                -> compileLet' ""
  where
    compileLet' :: String -> CodeGenState [Tac]
    compileLet' name = do
      let callDirect = canBeCalledDirectly atom
      rTmp <- getReg tmpVar
      when callDirect $ addDirectCallReg rTmp
      bindLocalVar name rTmp
      comp1 <- compileAtom rTmp atom name False
      comp2 <- compileExpr body
      return $ comp1 ++ comp2


-- This determines whether we'll use Tac_call or Tac_gen_ap later
canBeCalledDirectly :: NstAtomicExpr -> Bool
canBeCalledDirectly atom = case atom of
  NVar (NConstantFreeVar _) -> True    -- this is a function in a surrounding context     -- TODO we actually don't know whether this is a function at all!
  NLambda [] _ _ -> True               -- this is a function inside this function's context
  _ -> False


compileClosure :: Reg -> [String] -> [String] -> NstExpr -> String -> CodeGenState [Tac]
compileClosure reg freeVars params expr name = do
  funAddr <- compileFunc freeVars params expr name True
  -- TODO optimize argInstrs by using last parameter in set_arg
  argInstrsMaybes <- mapM (uncurry $ compileClosureArg name) $ zipWithIndex freeVars
  let argInstrs = catMaybes argInstrsMaybes
  -- Since free vars are always the first n vars of a compiled function, storing
  -- a closure is the same as partial application
  let makeClosureInstr = [Tac_load_f reg funAddr,
                          Tac_part_ap reg reg (length freeVars)]
  selfRefInstrs <- createSelfRefInstrsIfNeeded reg
  return $ argInstrs ++ makeClosureInstr ++ selfRefInstrs


compileClosureArg :: String -> String -> Int -> CodeGenState (Maybe Tac)
compileClosureArg clName argName argIndex =
  if argName == clName
    then (setSelfReferenceSlot argIndex) >> return Nothing
    else compileSetArgN argName argIndex >>= return . Just


createSelfRefInstrsIfNeeded :: Reg -> CodeGenState [Tac]
createSelfRefInstrsIfNeeded clReg = do
  scope <- getScope
  case selfReferenceSlot scope of
    Nothing -> return []
    Just index -> return [Tac_set_cl_val clReg clReg index]


-- Every branch in the match-expression has been converted to a lambda by the normalizer. (TODO inline these branches!)
compileMatch :: Reg -> NstVar -> Int -> Int -> [([String], NstVar)] -> Bool -> CodeGenState [Tac]
compileMatch reg subject maxCaptures patternAddr branches isResultValue = do
  let branchCaptures = map fst branches
  let branchLambdaVars = map snd branches -- the variables containing lambdas to call
  subjR <- getReg subject
  -- TODO use the next free register instead of hardcoded value
  let instrsPerBranch = 3 -- load args, call lambda, jump out
  let handledBranches = [0 .. (length branchLambdaVars) - 1]
  let remainingBranches = reverse handledBranches

  -- We are using the fact that registers are used linearly from smallest to largest.
  -- That way we can use the next n registers temporarily to capture match variables
  -- TODO Make sure that this actually works and doesn't create false results
  let captureStartReg = 29 - maxCaptures + 1 -- reg + 1
  let jumpTable = map (\(remaining, handled) ->
                      -- Jump 1 instr for each remaining entry in jump table
                      Tac_jmp (1 * remaining + instrsPerBranch * handled)) $
                      zip remainingBranches handledBranches
  -- We are using reg as a temporary register here
  let matchCode = [Tac_load_addr 30 patternAddr, Tac_match subjR 30 captureStartReg]
  compiledBranches <- forM (zip3 remainingBranches branchCaptures branchLambdaVars) $
                              \ (remaining, capturedVars, funVar) -> do
                                      let loadArgInstr = compileMatchBranchLoadArg captureStartReg capturedVars
                                      callInstr <- compileCallInstr reg funVar (length capturedVars) isResultValue
                                      return $ [loadArgInstr] ++ callInstr ++ [Tac_jmp (remaining * instrsPerBranch)]
  let body = Prelude.concat compiledBranches
  return $ matchCode ++ jumpTable ++ body


-- The lambda that represents a match-branch takes its captured values as arguments.
-- Here we create the set_arg instruction to load the captures. They are stored linearly
-- in the registers, starting at captureStartReg, so we just need a single set_arg instruction.
-- Note that we might end up with zero arguments.
compileMatchBranchLoadArg :: Reg -> [String] -> Tac
compileMatchBranchLoadArg captureStartReg capturedVars =
  let numCaptures = (max 0 $ (length capturedVars) - 1) in
  Tac_set_arg 0 captureStartReg numCaptures


compileSetArg :: NstVar -> Int -> CodeGenState Tac
compileSetArg var arg = do
  rVar <- getReg var
  return $ Tac_set_arg arg rVar 0


compileSetArgN :: String -> Int -> CodeGenState Tac
compileSetArgN name arg = do
  rVar <- getRegByName name
  return $ Tac_set_arg arg rVar 0




