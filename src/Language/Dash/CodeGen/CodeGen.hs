module Language.Dash.CodeGen.CodeGen (
  compile
) where

import           Control.Monad
import           Control.Monad.Except                     (runExceptT,
                                                           throwError)
import           Control.Monad.Identity                   (runIdentity)
import           Control.Monad.State.Strict
import           Data.Foldable
import           Data.List                                (transpose)
import           Data.Maybe                               (catMaybes)
import           Language.Dash.BuiltIn.BuiltInDefinitions
import           Language.Dash.CodeGen.CodeGenState
import           Language.Dash.Error.Error                (CompilationError (..))
import           Language.Dash.IR.Data
import           Language.Dash.IR.Nst
import           Language.Dash.IR.Opcode
import           Language.Dash.Limits


-- TODO explain what the code generator does and how it does it !

-- TODO when there is more time, do dataflow analysis to reuse registers



compile :: NstExpr
        -> ConstTable
        -> SymbolNameList
        -> Either CompilationError ([EncodedFunction], ConstTable, SymbolNameList)
compile expr cTable symlist =
  let resultOrError = runIdentity $ runExceptT $ execStateT (compileCompilationUnit expr)
                                                            (makeCompState cTable symlist)
  in
  extractResults <$> resultOrError
  where
    extractResults result =
        (map EncodedFunction $ toList (instructions result), constTable result, symbolNames result)


compileCompilationUnit :: NstExpr -> CodeGen ()
compileCompilationUnit expr = do
  funAddr <- beginFunction [] []
  addBuiltInFunctions
  funcCode <- compileExpr expr
  -- Don't add func header for wrapping function
  endFunction funAddr funcCode


addBuiltInFunctions :: CodeGen ()
addBuiltInFunctions =
  void $ forM builtInFunctions $ \ (name, arity, code) ->
          addBuiltInFunction name arity code


addBuiltInFunction :: Name -> Int -> [Opcode] -> CodeGen ()
addBuiltInFunction name bifArity code = do
  -- we're using some fake param names here because that's what beginFunction expects
  let params = map (\ (s, i) -> s ++ show i) $
                       zip (replicate bifArity "p") [0..bifArity]
  funAddr <- beginFunction [] params
  let arity = length params
  let funcCode' = OpcFunHeader arity : code
  endFunction funAddr funcCode'
  addCompileTimeConst name $ CTConstLambda funAddr -- Have to re-add to outer scope
  return ()



compileFunc :: [Name] -> [Name] -> NstExpr -> Name -> CodeGen FuncAddr
compileFunc freeVars params expr name = do
  funAddr <- beginFunction freeVars params
  -- we add the name already here for recursion
  addCompileTimeConst name $ CTConstLambda funAddr
  funcCode <- compileExpr expr

  let arity = length freeVars + length params
  let funcCode' = OpcFunHeader arity : funcCode
  endFunction funAddr funcCode'

  addCompileTimeConst name $ CTConstLambda funAddr -- Have to re-add to outer scope
  return funAddr


compileExpr :: NstExpr -> CodeGen [Opcode]
compileExpr expr =
  case expr of
    NLet var atom body ->
      compileLet var atom body
    NAtom a -> do
      code <- compileAtom 0 a "" True
      return $ code ++ [OpcRet 0]


compileAtom :: Reg -> NstAtomicExpr -> Name -> Bool -> CodeGen [Opcode]
compileAtom reg atom name isResultValue = case atom of
  NNumber n -> do
      when (n < minInteger || n > maxInteger) $
              throwError $ InternalCompilerError "Integer literal out of bounds"
      addCompileTimeConst name $ CTConstNumber $ fromIntegral n
      return [OpcLoadI reg n]
  NPlainSymbol sid -> do
      addCompileTimeConst name $ CTConstPlainSymbol sid
      return [OpcLoadPS reg sid]
  NCompoundSymbol [] cAddr ->
      -- addCompileTimeConst name $ CConstCompoundSymbol cAddr
      return [OpcLoadCS reg cAddr] -- TODO codeConstant?
  NCompoundSymbol dynamicFields cAddr ->
      compileDynamicSymbol reg dynamicFields cAddr
  NString strAddr ->
      return [OpcLoadStr reg strAddr]
  NPrimOp primop ->
      compilePrimOp primop reg
  NLambda [] params expr -> do
      funAddr <- compileFunc [] params expr name
      compileLoadLambda reg funAddr
  NLambda freeVars params expr ->
      compileClosure reg freeVars params expr name
  NMatchBranch freeVars matchedVars expr -> do
      -- free vars are applied directly in match branches, so we can compile them
      -- without creating a closure on the heap
      funAddr <- compileFunc freeVars matchedVars expr ""
      compileLoadLambda reg funAddr
  NFunAp funVar args ->
      compileFunAp reg funVar args isResultValue
  NVarExpr var ->
      case var of
        NVar _ NLocalVar     -> moveVarToReg var reg
        NVar _ NFunParam     -> moveVarToReg var reg
        NVar _ NFreeVar      -> moveVarToReg var reg
        NVar vname NConstant -> compileConstantFreeVar reg vname
        x -> throwError $ InternalCompilerError $ "Unexpected variable type: " ++ show x
  NMatch maxCaptures subject patternAddr branches ->
      compileMatch reg subject maxCaptures patternAddr branches isResultValue
  NPartAp funVar args -> do
      argInstrs <- mapM (uncurry compileSetArg) $ zipWithIndex args
      rFun <- getReg funVar
      let numArgs = length args
      let partApInst = [OpcPartAp reg rFun numArgs]
      return $ argInstrs ++ partApInst
  NModule fields ->
      compileModule reg fields
  NFieldLookup modVar symVar -> do
      modReg <- getReg modVar
      symReg <- getReg symVar
      return [OpcGetField reg modReg symReg]
  where
    moveVarToReg :: NstVar -> Reg -> CodeGen [Opcode]
    moveVarToReg var dest = do
      r <- getReg var
      return [OpcMove dest r]


compileFunAp :: Reg -> NstVar -> [NstVar] -> Bool -> CodeGen [Opcode]
compileFunAp reg funVar args isResultValue = do
  argInstrs <- mapM (uncurry compileSetArg) $ zipWithIndex args
  callInstr <- compileCallInstr reg funVar (length args) isResultValue
  return $ argInstrs ++ callInstr


compileCallInstr :: Reg -> NstVar -> Int -> Bool -> CodeGen [Opcode]
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


compileDynamicSymbol :: Reg -> [(Int, NstVar)] -> ConstAddr -> CodeGen [Opcode]
compileDynamicSymbol reg dynamicFields cAddr = do
  -- We're writing to a temp reg first because otherwise we might overwrite arguments
  -- in register 0 before we can apply them.
  -- TODO find out if this is the only place that can happen!
  tempReg <- newReg
  let loadConstCode = [ OpcLoadCS tempReg cAddr
                      , OpcCopySym tempReg tempReg] -- copy to heap and place new heap address in reg
  modifyCode <- forM dynamicFields $ \ (index, var) -> do
                        varReg <- getReg var
                        return $ OpcSetSymField tempReg varReg index
  let moveCode = [OpcMove reg tempReg]
  return $ loadConstCode ++ modifyCode ++ moveCode


compilePrimOp :: NstPrimOp -> Reg -> CodeGen [Opcode]
compilePrimOp primop reg = case primop of
  NPrimOpAdd a b -> compileBinaryPrimOp OpcAdd a b
  NPrimOpSub a b -> compileBinaryPrimOp OpcSub a b
  NPrimOpMul a b -> compileBinaryPrimOp OpcMul a b
  NPrimOpDiv a b -> compileBinaryPrimOp OpcDiv a b
  NPrimOpEq a b  -> compileBinaryPrimOp OpcEq  a b
  NPrimOpLessThan a b    -> compileBinaryPrimOp OpcLT a b
  NPrimOpGreaterThan a b -> compileBinaryPrimOp OpcGT a b
  NPrimOpOr a b  -> compileBinaryPrimOp OpcOr a b
  NPrimOpAnd a b -> compileBinaryPrimOp OpcAnd a b
  NPrimOpNot a   -> compileUnaryPrimOp OpcNot a
  where
    compileUnaryPrimOp op a = do
      ra <- getReg a
      return [op reg ra]

    compileBinaryPrimOp op a b = do
      ra <- getReg a
      rb <- getReg b
      return [op reg ra rb]


compileConstantFreeVar :: Reg -> Name -> CodeGen [Opcode]
compileConstantFreeVar reg name = do
  compConst <- getCompileTimeConstInSurroundingScopes name
  case compConst of
    -- TODO how about storing the constant in const table and simply load_c it here?
    CTConstNumber n          -> return [OpcLoadI reg $ fromIntegral n]
    CTConstPlainSymbol symId -> return [OpcLoadPS reg symId]
    -- CConstCompoundSymbol ConstAddr
    CTConstLambda funAddr    -> compileLoadLambda reg funAddr
    _ -> throwError $ InternalCompilerError "Unexpected compile time constant"


compileLoadLambda :: Reg -> FuncAddr -> CodeGen [Opcode]
compileLoadLambda reg funAddr =
  return [OpcLoadF reg funAddr]


compileLet :: NstVar -> NstAtomicExpr -> NstExpr -> CodeGen [Opcode]
compileLet tmpVar atom body =
  case tmpVar of
    NVar name NLocalVar -> compileLet' name
    _                   -> compileLet' ""    -- TODO we need names for this
  where
    compileLet' :: String -> CodeGen [Opcode]
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


compileClosure :: Reg -> [Name] -> [Name] -> NstExpr -> Name -> CodeGen [Opcode]
compileClosure reg freeVars params expr name = do
  funAddr <- compileFunc freeVars params expr name
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


compileClosureArgs :: Name -> [Name] -> CodeGen [Opcode]
compileClosureArgs name freeVars = do
  argInstrsMaybes <- mapM (uncurry $ compileClosureArg name) $ zipWithIndex freeVars
  return $ catMaybes argInstrsMaybes
  where
    compileClosureArg :: String -> String -> Int -> CodeGen (Maybe Opcode)
    compileClosureArg clName argName argIndex =
      if argName == clName
        then setSelfReferenceSlot argIndex >> return Nothing
        else liftM Just $ compileSetNamedArg argName argIndex


compileSetArg :: NstVar -> Int -> CodeGen Opcode
compileSetArg var arg = do
  rVar <- getReg var
  return $ OpcSetArg arg rVar 0


compileSetNamedArg :: Name -> Int -> CodeGen Opcode
compileSetNamedArg name arg = do
  rVar <- getRegByName name
  return $ OpcSetArg arg rVar 0


-- If a closure has a reference to itself, it needs itself as a free variable.
-- This function checks if that is the case and emits instructions to set
-- a refernce to the closure inside the closure.
createSelfRefInstrsIfNeeded :: Reg -> CodeGen [Opcode]
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
             -> CodeGen [Opcode]
compileMatch resultReg subject _{- maxCaptures -} patternAddr branches isResultValue = do
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
  -- count for remaining branches. So if the branch-call-blocks for a match with three
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
compileMatchBranchLoadArg :: Reg -> [Name] -> [a] -> CodeGen [Opcode]
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



compileModule :: Reg -> [(SymId, String, NstAtomicExpr)] -> CodeGen [Opcode]
compileModule resultReg fields = do
  -- TODO scan through fields for functions. create placeholders for them
  -- then scan for literals and add them as CTConsts.
  -- then add a new scope with bindings for all that. then compile
  -- TODO and obviousl we can only do that in normalization, so that's where we
  -- should create this
  let (accessSymbols, names, exprs) = unzip3 fields
  let fieldAccessors = map CPlainSymbol accessSymbols
  fieldConsts <- zipWithM encodeConstantLiteral exprs names
  let cFields = Prelude.concat $ transpose [fieldAccessors, fieldConsts]
  modId <- newModuleIdentifier
  modAddr <- encodeOpaqueSymbol modId moduleOwner cFields
  return [OpcLoadOS resultReg modAddr]

encodeConstantLiteral :: NstAtomicExpr -> Name -> CodeGen Constant
encodeConstantLiteral field name =
  case field of
    NNumber n      -> return $ CNumber n
    NPlainSymbol s -> return $ CPlainSymbol s
    NCompoundSymbol _ addr ->
                      return $ CCompoundSymbolRef addr
    NLambda [] params body -> do
                      fAddr <- compileFunc [] params body name
                      return $ CFunction fAddr
    NLambda {}     -> throwError $ InternalCompilerError "Sorry, can't compile modules with closures yet"
    _ -> throwError $ CodeError $ "Unexpected module field: " ++ show field



encodeOpaqueSymbol :: SymId -> SymId -> [Constant] -> CodeGen ConstAddr
encodeOpaqueSymbol symId owner fields =
  let oSym = COpaqueSymbol symId owner fields in
  addConstant oSym

