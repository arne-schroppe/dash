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
import           Language.Dash.IR.Opcode


-- TODO explain what the code generator does and how it does it !

-- TODO when there is more time, do dataflow analysis to reuse registers

-- TODO 'atom' could be misleading. Rename to 'atomExpr' or something like that


compile :: NstExpr
        -> ConstTable
        -> SymbolNameList
        -> ([[Opcode]], ConstTable, SymbolNameList)
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
    let funcCode' = OpcFunHeader arity : funcCode
    endFunction funAddr funcCode'
  else
    endFunction funAddr funcCode

  addCompileTimeConst name $ CTConstLambda funAddr -- Have to re-add to outer scope
  return funAddr


compileExpr :: NstExpr -> CodeGenState [Opcode]
compileExpr expr =
  case expr of
    NLet var atom body ->
      compileLet var atom body
    NAtom a -> do
      code <- compileAtom 0 a "" True
      return $ code ++ [OpcRet 0]


compileAtom :: Reg -> NstAtomicExpr -> Name -> Bool -> CodeGenState [Opcode]
compileAtom reg atom name isResultValue = case atom of
  NNumber n -> do
      when (n < 0 || n > maxInteger) $ error "Integer literal out of bounds"
      addCompileTimeConst name $ CTConstNumber (fromIntegral n)
      return [OpcLoadI reg (fromIntegral n)]
  NPlainSymbol sid -> do
      addCompileTimeConst name $ CTConstPlainSymbol sid
      return [OpcLoadPS reg sid]
  NCompoundSymbol False cAddr ->
      -- addCompileTimeConst name $ CConstCompoundSymbol cAddr
      return [OpcLoadCS reg cAddr] -- TODO codeConstant?
  NPrimOp primop ->
      compilePrimOp primop reg
  NLambda [] params expr -> do
      funAddr <- compileFunc [] params expr name True
      compileLoadLambda reg funAddr
  NLambda freeVars params expr ->
      compileClosure reg freeVars params expr name
  NMatchBranch freeVars matchedVars expr -> do
      -- free vars are applied directly in match branches, so we can compile them
      -- without creating a closure on the heap
      funAddr <- compileFunc freeVars matchedVars expr "" True
      compileLoadLambda reg funAddr
  NFunAp funVar args -> do
      argInstrs <- mapM (uncurry compileSetArg) $ zipWithIndex args
      callInstr <- compileCallInstr reg funVar (length args) isResultValue
      return $ argInstrs ++ callInstr
  NVarExpr var ->
      case var of
        NVar _ NLocalVar     -> moveVarToReg var reg
        NVar _ NFunParam     -> moveVarToReg var reg
        NVar _ NFreeVar      -> moveVarToReg var reg
        NVar vname NConstant -> compileConstantFreeVar reg vname
        x -> error $ "Internal compiler error: Unexpected variable type: " ++ show x
  NMatch maxCaptures subject patternAddr branches ->
      compileMatch reg subject maxCaptures patternAddr branches isResultValue
  NPartAp funVar args -> do
      argInstrs <- mapM (uncurry compileSetArg) $ zipWithIndex args
      rFun <- getReg funVar
      let numArgs = length args
      let partApInst = [OpcPartAp reg rFun numArgs]
      return $ argInstrs ++ partApInst
  x ->
      error $ "Unable to compile " ++ show x
  where
    moveVarToReg :: NstVar -> Reg -> CodeGenState [Opcode]
    moveVarToReg var dest = do
      r <- getReg var
      return [OpcMove dest r]


compileCallInstr :: Reg -> NstVar -> Int -> Bool -> CodeGenState [Opcode]
compileCallInstr reg funVar numArgs isResultValue = do
  rFun <- getReg funVar
  direct <- isDirectCallReg rFun
  -- TODO maybe the normalizer should already resolve what is a call to a known
  -- function and what isn't?
  let instr = case (direct, isResultValue) of
          (True, False)  -> [OpcCall reg rFun numArgs]
          (True, True)   -> [OpcTailCall rFun numArgs]
          (False, False) -> [OpcGenAp reg rFun numArgs]
          (False, True)  -> [OpcTailGenAp reg rFun numArgs]
  return instr


compilePrimOp :: NstPrimOp -> Reg -> CodeGenState [Opcode]
compilePrimOp primop reg = case primop of
  NPrimOpAdd a b -> compileBinaryPrimOp OpcAdd a b
  NPrimOpSub a b -> compileBinaryPrimOp OpcSub a b
  NPrimOpMul a b -> compileBinaryPrimOp OpcMul a b
  NPrimOpDiv a b -> compileBinaryPrimOp OpcDiv a b
  NPrimOpEq a b  -> compileBinaryPrimOp OpcEq  a b
  where
    compileBinaryPrimOp op a b = do
      ra <- getReg a
      rb <- getReg b
      return [op reg ra rb]


compileConstantFreeVar :: Reg -> Name -> CodeGenState [Opcode]
compileConstantFreeVar reg name = do
  compConst <- getCompileTimeConstInSurroundingScopes name
  case compConst of
    -- TODO how about storing the constant in const table and simply load_c it here?
    CTConstNumber n          -> return [OpcLoadI reg (fromIntegral n)]
    CTConstPlainSymbol symId -> return [OpcLoadPS reg symId]
    -- CConstCompoundSymbol ConstAddr
    CTConstLambda funAddr    -> compileLoadLambda reg funAddr
    _ -> error "compileConstantFreeVar"


compileLoadLambda :: Reg -> FuncAddr -> CodeGenState [Opcode]
compileLoadLambda reg funAddr =
  return [OpcLoadF reg funAddr]


compileLet :: NstVar -> NstAtomicExpr -> NstExpr -> CodeGenState [Opcode]
compileLet tmpVar atom body =
  case tmpVar of
    NVar name NLocalVar -> compileLet' name
    _                   -> compileLet' ""    -- TODO we need names for this
  where
    compileLet' :: String -> CodeGenState [Opcode]
    compileLet' name = do
      rTmp <- newReg
      bindVar name rTmp
      let callDirect = canBeCalledDirectly atom
      when callDirect $ addDirectCallReg rTmp
      comp1 <- compileAtom rTmp atom name False
      comp2 <- compileExpr body
      return $ comp1 ++ comp2


-- This determines whether we'll use call or gen_ap later
canBeCalledDirectly :: NstAtomicExpr -> Bool
canBeCalledDirectly atom =
  case atom of
    -- TODO make sure that this is actually a function (in general, if we can determine
    -- that something isn't callable, emit an error or warning)

    -- this is supposed to be a function in a surrounding context. If it isn't, calling
    -- this will result in a runtime exception
    NVarExpr (NVar _ NConstant) -> True

    -- this is a function inside this function's context
    NLambda [] _ _ -> True
    _ -> False


compileClosure :: Reg -> [Name] -> [Name] -> NstExpr -> Name -> CodeGenState [Opcode]
compileClosure reg freeVars params expr name = do
  funAddr <- compileFunc freeVars params expr name True
  -- TODO optimize argInstrs by using last parameter in set_arg (i.e. if we have arguments
  -- in consecutive registers, we can emit a single instruction for them)

  argInstrs <- compileClosureArgs name freeVars
  -- Since free vars are always the first n vars of a compiled function, creating
  -- a closure is the same as partial application
  let makeClosureInstr =
          [ OpcLoadF reg funAddr
          , OpcPartAp reg reg (length freeVars)]
  selfRefInstrs <- createSelfRefInstrsIfNeeded reg
  return $ argInstrs ++ makeClosureInstr ++ selfRefInstrs


compileClosureArgs :: Name -> [Name] -> CodeGenState [Opcode]
compileClosureArgs name freeVars = do
  argInstrsMaybes <- mapM (uncurry $ compileClosureArg name) $ zipWithIndex freeVars
  return $ catMaybes argInstrsMaybes
  where
    compileClosureArg :: String -> String -> Int -> CodeGenState (Maybe Opcode)
    compileClosureArg clName argName argIndex =
      if argName == clName
        then setSelfReferenceSlot argIndex >> return Nothing
        else liftM Just $ compileSetArgN argName argIndex


compileSetArg :: NstVar -> Int -> CodeGenState Opcode
compileSetArg var arg = do
  rVar <- getReg var
  return $ OpcSetArg arg rVar 0


compileSetArgN :: Name -> Int -> CodeGenState Opcode
compileSetArgN name arg = do
  rVar <- getRegByName name
  return $ OpcSetArg arg rVar 0


-- If a closure has a reference to itself, it needs itself as a free variable.
-- This function checks if that is the case and emits instructions to set
-- a refernce to the closure inside the closure.
createSelfRefInstrsIfNeeded :: Reg -> CodeGenState [Opcode]
createSelfRefInstrsIfNeeded clReg = do
  selfRef <- getSelfReference
  case selfRef of
    Nothing -> return []
    Just index -> return [OpcSetClVal clReg clReg index]


-- Every branch in the match-expression has been converted to a lambda by the normalizer.
compileMatch :: Reg
             -> NstVar
             -> Int
             -> ConstAddr
             -> [([Name], [Name], NstVar)]
             -> Bool
             -> CodeGenState [Opcode]
compileMatch resultReg subject maxCaptures patternAddr branches isResultValue = do
  -- the variables containing matchbranches to call
  let matchBranchVars = map (\ (_, _, a) -> a) branches
  subjR <- getReg subject
  let handledBranches = [0 .. length matchBranchVars - 1]
  let remainingBranches = reverse handledBranches
  captureStartReg <- newReg -- TODO return register to pool after we're done with it
  -- TODO reserve maxCaptures vars (so that we can check that we don't run out of
  -- registers) and return them to pool afterwards
  compiledBranches <- forM branches $
    \ (freeVars, capturedVars, funVar) -> do
        loadArgInstrs <- compileMatchBranchLoadArg captureStartReg freeVars capturedVars
        callInstr <- compileCallInstr
                         resultReg
                         funVar
                         (length capturedVars + length freeVars)
                         isResultValue
        return $ loadArgInstrs ++ callInstr

  -- The next three bindings give us two lists: a list of the count of instructions that
  -- comes before the match-branch call code for branch n and a list of the instruction
  -- count for remaining branches. So if the branch-call blocks for a match with three
  -- branches have the lengths 4, 7, and 3, we'd get:
  -- numRemainingBranchInstrs = [10, 3, 0]
  -- numHandledBranchInstrs = [0, 4, 11]
  let branchInstrCount = map (
          \ index -> let (pre, rest) = splitAt index compiledBranches in
                     let numRemaining = length matchBranchVars - 1 - index in
                     let numHandled = index in
                     -- we'll add the jump out instruction later
                     let numMissingInstructionsPerBranch = 1 in
                     let numRemainingInstrs = length (Prelude.concat rest)
                                              + numMissingInstructionsPerBranch
                                              * numRemaining in
                     let numHandledInstrs = length (Prelude.concat pre)
                                            + numMissingInstructionsPerBranch
                                            * numHandled in
                     (numHandledInstrs, numRemainingInstrs))
          [0 .. (length matchBranchVars)]
  let numRemainingBranchInstrs = tail $ map snd branchInstrCount
  let numHandledBranchInstrs = init $ map fst branchInstrCount

  -- instructions for jumping out of the code that calls a match-branch
  -- and to the remaining code
  let jumpOutInstrs = map (\ numRemaining -> [OpcJmp $ numRemaining + 1])
                          numRemainingBranchInstrs
  let completeCompiledBranches = zipWith (++) compiledBranches jumpOutInstrs

  -- compile jump table
  -- the match instruction calls the following instruction at position n if branch n
  -- matched. So we place a jump table directly after the match instruction, where every
  -- entry is exactly one instruction (which then jumps to the code that calls the branch
  -- instruction)
  let jumpInTable = map (\(remaining, numHandled) ->
                      let jumpTableEntrySize = 1 in
                      -- jump over remaining jump-table entries and then over match-
                      -- branches we're done with
                      OpcJmp (jumpTableEntrySize * remaining + numHandled)) $
                      zip remainingBranches numHandledBranchInstrs

  -- after match has started, we can reuse the reg holding the address for captured vars
  let addrTempReg = captureStartReg

  -- compile match call
  let matchCode = [OpcLoadAddr addrTempReg patternAddr,
                   OpcMatch subjR addrTempReg captureStartReg]
  let body = Prelude.concat completeCompiledBranches
  return $ matchCode ++ jumpInTable ++ body


-- The lambda that represents a match-branch takes its captured values as arguments.
-- Here we create the set_arg instruction to load the captures. They are stored linearly
-- in the registers, starting at captureStartReg, so we just need a single set_arg
-- instruction.
-- Note that we might end up with zero arguments.
compileMatchBranchLoadArg :: Reg -> [Name] -> [a] -> CodeGenState [Opcode]
compileMatchBranchLoadArg captureStartReg freeVars capturedVars = do
  freeArgInstrs <- compileClosureArgs "" freeVars
  let numCaptures = max 0 $ length capturedVars - 1

  -- TODO make sure that freeVars can't contain duplicates? (it shouldn't be possible)
  let capturedArgsStart = length freeVars
  let capturedArgInstrs =
          if not (null capturedVars)
            then [OpcSetArg capturedArgsStart captureStartReg numCaptures]
            else []
  return $ freeArgInstrs ++ capturedArgInstrs

