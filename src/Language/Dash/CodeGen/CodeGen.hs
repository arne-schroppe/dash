module Language.Dash.CodeGen.CodeGen (
  compile
) where

import           Control.Monad.State
import           Data.Foldable
import           Data.Maybe                         (catMaybes)
import           Language.Dash.CodeGen.CodeGenState
import           Language.Dash.CodeGen.Limits
import           Language.Dash.IR.Data
import           Language.Dash.IR.Nst
import           Language.Dash.IR.Tac


-- TODO explain what the code generator does and how it does it !

-- TODO when there is more time, do dataflow analysis to reuse registers

-- TODO 'atom' could be misleading. Rename to 'atomExpr' or something like that


compile :: NstExpr -> ConstTable -> SymbolNameList -> ([[Tac]], ConstTable, SymbolNameList)
compile expr cTable symlist =
  let result = execState (compileFunc [] [] expr "" False) emptyCompEnv in
  (toList (instructions result), cTable, symlist)


-- TODO can we get rid of the shouldAddHeader parameter?
compileFunc :: [Name] -> [Name] -> NstExpr -> Name -> Bool -> CodeGenState FuncAddr
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

  addCompileTimeConst name $ CTConstLambda funAddr -- Have to re-add to outer scope
  return funAddr


compileExpr :: NstExpr -> CodeGenState [Tac]
compileExpr expr = case expr of
  NLet var atom body -> compileLet var atom body
  NAtom a -> do
          code <- compileAtom 0 a "" True
          return $ code ++ [Tac_ret 0]


compileAtom :: Reg -> NstAtomicExpr -> Name -> Bool -> CodeGenState [Tac]
compileAtom reg atom name isResultValue = case atom of
  NNumber n -> do
          when (n < 0 || n > maxInteger) $ error "Integer literal out of bounds"
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
          compileLoadLambda reg funAddr
  NLambda freeVars params expr ->
          compileClosure reg freeVars params expr name
  NMatchBranch freeVars matchedVars expr -> do
          -- free vars are applied directly in match branches, so we can compile them without creating a closure on the heap
          funAddr <- compileFunc freeVars matchedVars expr "" True
          compileLoadLambda reg funAddr
  NFunAp funVar args -> do
          argInstrs <- mapM (uncurry compileSetArg) $ zipWithIndex args
          callInstr <- compileCallInstr reg funVar (length args) isResultValue
          return $ argInstrs ++ callInstr
  NVar var -> case var of
          NLocalVar _            -> moveVarToReg var reg
          NFunParam _            -> moveVarToReg var reg
          NDynamicFreeVar _      -> moveVarToReg var reg
          NConstantFreeVar vname -> compileConstantFreeVar reg vname
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

compileConstantFreeVar :: Reg -> Name -> CodeGenState [Tac]
compileConstantFreeVar reg name = do
  compConst <- getCompileTimeConstInSurroundingScopes name
  case compConst of
          CTConstNumber n -> return [Tac_load_i reg (fromIntegral n)] -- how about storing the constant in const table and simply load_c it here?
          CTConstPlainSymbol symId -> return [Tac_load_ps reg symId]
          -- CConstCompoundSymbol ConstAddr
          CTConstLambda funAddr -> compileLoadLambda reg funAddr
          _ -> error "compileConstantFreeVar"


compileLoadLambda :: Reg -> FuncAddr -> CodeGenState [Tac]
compileLoadLambda reg funAddr = do
  let ldFunAddr = [Tac_load_f reg funAddr]
  return $ ldFunAddr


compileLet :: NstVar -> NstAtomicExpr -> NstExpr -> CodeGenState [Tac]
compileLet tmpVar atom body =
  case tmpVar of
    NLocalVar name   -> compileLet' name
    _                -> compileLet' ""    -- TODO we need names for this
  where
    compileLet' :: String -> CodeGenState [Tac]
    compileLet' name = do
      rTmp <- newReg
      bindLocalVar name rTmp
      let callDirect = canBeCalledDirectly atom
      when callDirect $ addDirectCallReg rTmp
      comp1 <- compileAtom rTmp atom name False
      comp2 <- compileExpr body
      return $ comp1 ++ comp2


-- This determines whether we'll use Tac_call or Tac_gen_ap later
canBeCalledDirectly :: NstAtomicExpr -> Bool
canBeCalledDirectly atom = case atom of
  -- TODO make sure that this is actually a function (in general, if we can determine that something isn't callable, emit an error or warning)
  NVar (NConstantFreeVar _) -> True    -- this is supposed to be a function in a surrounding context. If it isn't, calling this will result in a runtime exception
  NLambda [] _ _ -> True               -- this is a function inside this function's context
  _ -> False


compileClosure :: Reg -> [Name] -> [Name] -> NstExpr -> Name -> CodeGenState [Tac]
compileClosure reg freeVars params expr name = do
  funAddr <- compileFunc freeVars params expr name True
  -- TODO optimize argInstrs by using last parameter in set_arg (i.e. if we have arguments
  -- in consecutive registers, we can emit a single instruction for them)

  argInstrs <- compileClosureArgs name freeVars
  -- Since free vars are always the first n vars of a compiled function, creating
  -- a closure is the same as partial application
  let makeClosureInstr = [Tac_load_f reg funAddr,
                          Tac_part_ap reg reg (length freeVars)]
  selfRefInstrs <- createSelfRefInstrsIfNeeded reg
  return $ argInstrs ++ makeClosureInstr ++ selfRefInstrs


compileClosureArgs :: Name -> [Name] -> CodeGenState [Tac]
compileClosureArgs name freeVars = do
  argInstrsMaybes <- mapM (uncurry $ compileClosureArg name) $ zipWithIndex freeVars
  return $ catMaybes argInstrsMaybes
  where
    compileClosureArg :: String -> String -> Int -> CodeGenState (Maybe Tac)
    compileClosureArg clName argName argIndex =
      if argName == clName
        then (setSelfReferenceSlot argIndex) >> return Nothing
        else compileSetArgN argName argIndex >>= return . Just

compileSetArg :: NstVar -> Int -> CodeGenState Tac
compileSetArg var arg = do
  rVar <- getReg var
  return $ Tac_set_arg arg rVar 0

compileSetArgN :: Name -> Int -> CodeGenState Tac
compileSetArgN name arg = do
  rVar <- getRegByName name
  return $ Tac_set_arg arg rVar 0

-- If a closure has a reference to itself, it needs itself as a free variable.
-- This function checks if that is the case and emits instructions to set
-- a refernce to the closure inside the closure.
createSelfRefInstrsIfNeeded :: Reg -> CodeGenState [Tac]
createSelfRefInstrsIfNeeded clReg = do
  selfRef <- getSelfReference
  case selfRef of
    Nothing -> return []
    Just index -> return [Tac_set_cl_val clReg clReg index]


-- Every branch in the match-expression has been converted to a lambda by the normalizer.
compileMatch :: Reg -> NstVar -> Int -> ConstAddr -> [([Name], [Name], NstVar)] -> Bool -> CodeGenState [Tac]
compileMatch resultReg subject maxCaptures patternAddr branches isResultValue = do
  -- TODO it would be much more efficient if we wouldn't store free variables of the match-
  -- branch lambdas on the heap first but would call them directly. We know that those
  -- lambdas can't escape the local context.
  let matchBranchVars = map (\ (_, _, a) -> a) branches -- the variables containing matchbranches to call
  subjR <- getReg subject
  let handledBranches = [0 .. (length matchBranchVars) - 1]
  let remainingBranches = reverse handledBranches

  -- We are using the fact that registers are used linearly from smallest to largest.
  -- That way we can use the next n registers temporarily to capture match variables
  -- TODO Make sure that this actually works and doesn't create false results
  -- TODO use the next free register instead of hardcoded value
  -- TODO the following line might overwrite already used registers and we have no means of checking that limit right now
  let captureStartReg = mkReg $ (maxRegisters - 2) - maxCaptures + 1 -- reg + 1
  compiledBranches <- forM branches $
                              \ (freeVars, capturedVars, funVar) -> do
                                      loadArgInstrs <- compileMatchBranchLoadArg captureStartReg freeVars capturedVars
                                      callInstr <- compileCallInstr resultReg funVar (length capturedVars + length freeVars) isResultValue
                                      return $ loadArgInstrs ++ callInstr

  -- The next three bindings give us two lists: a list of the count of instructions that 
  -- comes before the match-branch call code for branch n and a list of the instruction 
  -- count for remaining branches. So if the branch-call blocks for a match with three 
  -- branches have the lengths 4, 7, and 3, we'd get:
  -- numRemainingBranchInstrs = [10, 3, 0]
  -- numHandledBranchInstrs = [0, 4, 11]
  let branchInstrCount = map (\ index -> let (pre, rest) = splitAt index compiledBranches in
                                         let numRemaining = (length matchBranchVars) - 1 - index in
                                         let numHandled = index in
                                         let numMissingInstructionsPerBranch = 1 in -- we'll add the jump out instruction later
                                         let numRemainingInstrs = (length (Prelude.concat rest)) + numMissingInstructionsPerBranch * numRemaining in
                                         let numHandledInstrs = (length (Prelude.concat pre)) + numMissingInstructionsPerBranch * numHandled in
                                         (numHandledInstrs, numRemainingInstrs))
                                 [0 .. (length matchBranchVars)]
  let numRemainingBranchInstrs = tail $ map snd branchInstrCount
  let numHandledBranchInstrs = init $ map fst branchInstrCount

  -- instructions for jumping out of the code that calls a match-branch and to the remaining code
  let jumpOutInstrs = map (\ numRemaining -> [Tac_jmp $ numRemaining + 1]) numRemainingBranchInstrs
  let completeCompiledBranches = map ( \ (a, b) -> a ++ b ) $ zip compiledBranches jumpOutInstrs

  -- compile jump table
  -- the match instruction calls the following instruction at position n if branch n matched. So
  -- we place a jump table directly after the match instruction, where every entry is exactly one
  -- instruction (which then jumps to the code that calls the branch instruction)
  let jumpInTable = map (\(remaining, numHandled) ->
                      let jumpTableEntrySize = 1 in
                      -- jump over remaining jump-table entries and then over match-branches we're done with
                      Tac_jmp (jumpTableEntrySize * remaining + numHandled)) $
                      zip remainingBranches numHandledBranchInstrs
  let addrTempReg = mkReg $ maxRegisters - 1

  -- compile match call
  let matchCode = [Tac_load_addr addrTempReg patternAddr, 
                   Tac_match subjR addrTempReg captureStartReg]
  let body = Prelude.concat completeCompiledBranches
  return $ matchCode ++ jumpInTable ++ body


-- The lambda that represents a match-branch takes its captured values as arguments.
-- Here we create the set_arg instruction to load the captures. They are stored linearly
-- in the registers, starting at captureStartReg, so we just need a single set_arg instruction.
-- Note that we might end up with zero arguments.
compileMatchBranchLoadArg :: Reg -> [Name] -> [a] -> CodeGenState [Tac]
compileMatchBranchLoadArg captureStartReg freeVars capturedVars = do
  freeArgInstrs <- compileClosureArgs "" freeVars
  let numCaptures = (max 0 $ (length capturedVars) - 1)
  let capturedArgsStart = length freeVars -- TODO make sure that freeVars can't contain duplicates? (it shouldn't be possible)
  let capturedArgInstrs = if length capturedVars > 0 then
                              [Tac_set_arg capturedArgsStart captureStartReg numCaptures]
                          else
                              []
  return $ freeArgInstrs ++ capturedArgInstrs

