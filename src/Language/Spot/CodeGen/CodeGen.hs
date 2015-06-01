module Language.Spot.CodeGen.CodeGen (
  compile
) where

import Language.Spot.IR.Norm
import Language.Spot.IR.Tac
import Language.Spot.VM.Types

import Control.Monad.State
import qualified Data.Sequence as Seq
import Data.Foldable
import Control.Applicative
import Data.List (delete)
import Data.Maybe (catMaybes)

import Debug.Trace

import qualified Data.Sequence as Seq
import qualified Data.Map as Map

-- TODO when there is more time, do dataflow analysis to reuse registers

-- TODO 'atom' could be misleading. Rename to 'atomExpr' or something like that

-- TODO after refactoring the VM, we need to change everything that is called
-- something with 'register' to something like 'localVar' or so. Otherwise
-- the uninitiated observer might think that we don't know what a register is.



compile :: NormExpr -> ConstTable -> SymbolNameList -> ([[Tac Reg]], ConstTable, SymbolNameList)
compile expr cTable symlist =
  let result = execState (compileFunc [] [] expr "") emptyCompState in
  (toList (instructions result), cTable, symlist)


compileFunc freeVars params expr name = do
  funAddr <- beginFunction freeVars params
  -- we add the name already here for recursion
  addCompileTimeConst name $ CTConstLambda funAddr -- TODO this shouldn't really be in here
  funcCode <- compileExpr expr
  endFunction funAddr funcCode
  addCompileTimeConst name $ CTConstLambda funAddr -- Have to re-add to outer scope    TODO this sucks
  return funAddr

compileExpr expr = case expr of
  NLet var atom body -> compileLet var atom body
  NAtom a -> do
          code <- compileAtom 0 a "" True
          return $ code ++ [Tac_ret 0]


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
  NPrimOp (NPrimOpAdd a b) -> do
          ra <- getReg a
          rb <- getReg b
          return [Tac_add reg ra rb]
  NPrimOp (NPrimOpSub a b) -> do
          ra <- getReg a
          rb <- getReg b
          return [Tac_sub reg ra rb]
  NLambda [] params expr -> do
          funAddr <- compileFunc [] params expr name
          compileLoadLambda reg funAddr isResultValue
  NLambda freeVars params expr -> compileClosure reg freeVars params expr name
  NFunCall funVar args -> do
          argInstrs <- mapM (uncurry compileSetArg) $ zipWithIndex args
          callInstr <- compileCallInstr reg funVar args isResultValue
          return $ argInstrs ++ callInstr
  NVar var -> case var of
          NLocalVar varId _     -> moveVarToReg var reg
          NFunParam name        -> moveVarToReg var reg
          NDynamicFreeVar name  -> moveVarToReg var reg
          NConstantFreeVar name -> compileConstantFreeVar reg name isResultValue
          x -> error $ "Internal compiler error: Unexpected variable type: " ++ show x
  -- TODO unify order of arguments
  NMatch maxCaptures subject patternAddr branches ->
          compileMatch reg subject maxCaptures patternAddr branches isResultValue
  x -> error $ "Unable to compile " ++ (show x)

moveVarToReg var reg = do
          r <- getReg var
          return [Tac_move reg r]


compileCallInstr reg funVar args isResultValue = do
          rFun <- getReg funVar
          direct <- isDirectCallReg rFun
          let instr = case (direct, isResultValue) of
                  (True, False)  -> [Tac_call reg rFun (length args)]
                  (True, True)   -> [Tac_tail_call rFun (length args)]
                  (False, False) -> [Tac_call_cl reg rFun (length args)]
                  (False, True)  -> [Tac_tail_call_cl rFun (length args)]
          return instr

compileConstantFreeVar :: Reg -> String -> Bool -> State CompState [Tac Reg]
compileConstantFreeVar reg name isResultValue = do
  compConst <- getCompileTimeConstInOuterScope name
  case compConst of
          CTConstNumber n -> return [Tac_load_i reg (fromIntegral n)] -- how about storing the constant in const table and simply load_c it here?
          CTConstPlainSymbol symId -> return [Tac_load_ps reg symId]
          -- CConstCompoundSymbol ConstAddr
          CTConstLambda funAddr -> compileLoadLambda reg funAddr isResultValue

compileLoadLambda reg funAddr isResultValue = do
  let ldFunAddr = [Tac_load_f reg funAddr]
  if isResultValue then
      return $ ldFunAddr ++ [Tac_make_cl reg reg 0]
  else
      return $ ldFunAddr

compileLet tmpVar@(NLocalVar tmpId name) atom body =
  compileLet' tmpVar atom name body

compileLet tmpVar atom body =
  compileLet' tmpVar atom "" body

compileLet' tmpVar atom name body = do
  let callDirect = canBeCalledDirectly atom
  rTmp <- getReg tmpVar
  when callDirect $ addDirectCallReg rTmp
  comp1 <- compileAtom rTmp atom name False
  comp2 <- compileExpr body
  return $ comp1 ++ comp2

-- This determines whether we'll use Tac_call or Tac_call_cl later
canBeCalledDirectly atom = case atom of
  NVar (NConstantFreeVar _) -> True -- TODO we need a function tag for this case
  NLambda ([]) _ _ -> True
  _ -> False


compileClosure reg freeVars params expr name = do
  funAddr <- compileFunc freeVars params expr name
  -- TODO optimize argInstrs by using last parameter in set_arg
  argInstrsMaybes <- mapM (uncurry $ compileClosureArg name) $ zipWithIndex freeVars
  let argInstrs = catMaybes argInstrsMaybes
  let makeClosureInstr = [Tac_load_f reg funAddr, Tac_make_cl reg reg (length freeVars)]
  selfRefInstrs <- createSelfRefInstrsIfNeeded reg
  return $ argInstrs ++ makeClosureInstr ++ selfRefInstrs

compileClosureArg clName argName argIndex =
  if argName == clName
    then (setSelfReferenceSlot argIndex) >> return Nothing
    else compileSetArgN argName argIndex >>= return . Just

createSelfRefInstrsIfNeeded :: Reg -> State CompState [Tac Reg]
createSelfRefInstrsIfNeeded clReg = do
  scope <- getScope
  case selfReferenceSlot scope of
    Nothing -> return []
    Just index -> return [Tac_set_cl_arg clReg clReg index]


compileMatch reg subject maxCaptures patternAddr branches isResultValue = do
  let branchMatchedVars = map fst branches
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
  compiledBranches <- forM (zip3 remainingBranches branchMatchedVars branchLambdaVars) $
                              \ (remaining, matchedVars, funVar) -> do
                                      let loadArgInstr = compileMatchBranchLoadArg captureStartReg matchedVars
                                      callInstr <- compileCallInstr reg funVar matchedVars isResultValue
                                      return $ [loadArgInstr] ++ callInstr ++ [Tac_jmp (remaining * instrsPerBranch)]
  let body = Prelude.concat compiledBranches
  return $ matchCode ++ jumpTable ++ body


compileMatchBranchLoadArg startReg matchedVars =
  Tac_set_arg 0 startReg (max 0 $ (length matchedVars) - 1)


compileSetArg var arg = do
  rVar <- getReg var
  return $ Tac_set_arg arg rVar 0

compileSetArgN name arg = do
  rVar <- getRegByName name
  return $ Tac_set_arg arg rVar 0


zipWithIndex l = zip l [0..(length l)]


----- State -----
-- TODO put this into a separate file

data CompState = CompState {
                   instructions :: Seq.Seq [Tac Reg]
                 , scopes :: [CompScope]
                 }

emptyCompState = CompState {
                   instructions = Seq.fromList []
                 , scopes = []
                 }

data CompScope = CompScope {
                   functionParams :: Map.Map String Int
                 , freeVariables :: Map.Map String Int
                 , forwardDeclaredLambdas :: [String]
                 , selfReferenceSlot :: Maybe Int

                 -- these are all the registers that hold function values which
                 -- can be called directly with Tac_call. Everything else is
                 -- called with Tac_call_cl
                 , directCallRegs :: [Int]

                 , compileTimeConstants :: Map.Map String CompileTimeConstant
                 }

makeScope fps freeVars = CompScope {
               functionParams = fps
             , freeVariables = freeVars
             , forwardDeclaredLambdas = []
             , selfReferenceSlot = Nothing
             , directCallRegs = []
             , compileTimeConstants = Map.empty
             }

data CompileTimeConstant =
    CTConstNumber VMWord
  | CTConstPlainSymbol SymId
  | CTConstCompoundSymbol ConstAddr
  | CTConstLambda FunAddr


beginFunction freeVars params = do
  state <- get
  let localFreeVars = Map.fromList (zipWithIndex freeVars)
  let paramBindings = Map.fromList (zipWithIndex params)
  let newScope = makeScope paramBindings localFreeVars
  put $ state { scopes = newScope : (scopes state) }
  addr <- addPlaceholderFunction
  return addr

endFunction funAddr code = do
  state <- get
  let instrs = instructions state
  let instrs' = Seq.update funAddr code instrs
  put $ state { scopes = (tail $ scopes state),
                instructions = instrs' }

numParameters :: State CompState Int
numParameters = do
  localParams <- gets $ functionParams.head.scopes
  return $ Map.size localParams

param :: String -> State CompState (Maybe Int)
param name = do
  localParams <- gets $ functionParams.head.scopes
  let res = Map.lookup name localParams
  return res


numFreeVars :: State CompState Int
numFreeVars = do
  localFreeVars <- gets $ freeVariables.head.scopes
  return $ Map.size localFreeVars

freeVar :: String -> State CompState (Maybe Int)
freeVar name = do
  localFreeVars <- gets $ freeVariables.head.scopes
  let res = Map.lookup name localFreeVars
  return res


addPlaceholderFunction = do
  state <- get
  let instrs = instructions state
  let nextFunAddr = Seq.length instrs
  let instrs' = instrs Seq.|> []
  put $ state { instructions = instrs' }
  return nextFunAddr

getRegByName :: String -> State CompState Int
getRegByName name = do
  lookup <- getRegN name
  -- TODO code duplication, handling of register offsets for free vars should be handled in one place only
  case lookup of
    Just index -> return index
    Nothing -> error $ "Unknown identifier " ++ name
  where getRegN name = do
  -- TODO make this less ugly
          p <- param name
          case p of
                  Just i -> return $ Just i
                  Nothing -> do
                          f <- freeVar name
                          numParams <- numParameters
                          return $ (+) <$> f <*> Just numParams

getReg :: NormVar -> State CompState Int
getReg (NConstantFreeVar _) = error "Compiler error"

getReg (NFunParam name) = do
  lookup <- param name
  case lookup of
          Just index -> return index
          Nothing -> error $ "Unknown parameter: " ++ name

-- When calling a closure, the first n registers are formal arguments
-- and the next m registers are closed-over variables
-- TODO document this fact somewhere visible
getReg (NDynamicFreeVar name) = do
  numParams <- numParameters
  lookup <- freeVar name
  case lookup of
          Just index -> return $ numParams + index
          Nothing -> error $ "Unknown free var: " ++ name

getReg (NLocalVar tmpVar name) = do
  numFree <- numFreeVars
  numParams <- numParameters
  return $ numFree + numParams + tmpVar

getReg (NRecursiveVar name) = error "test" -- TODO delete this


isDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  return $ Prelude.elem reg dCallRegs

addDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  let dCallRegs' = reg : dCallRegs
  putScope $ scope { directCallRegs = dCallRegs' }

setSelfReferenceSlot index = do
  scope <- getScope
  putScope $ scope { selfReferenceSlot = Just index }

resetSelfReferenceSlot = do
  scope <- getScope
  putScope $ scope { selfReferenceSlot = Nothing }

getScope = do
  state <- get
  return $ head (scopes state)

putScope s = do
  state <- get
  put $ state { scopes = s : (tail $ scopes state) }


addCompileTimeConst "" _ = return ()
addCompileTimeConst name c = do
  scope <- getScope
  let consts = compileTimeConstants scope
  let consts' = Map.insert name c consts
  putScope $ scope { compileTimeConstants = consts' }

-- This retrieves values for NConstantFreeVar. Those are never inside the current scope
getCompileTimeConstInOuterScope :: String -> State CompState CompileTimeConstant
getCompileTimeConstInOuterScope name = do
  scps <- gets scopes
  getCompConst name scps
  where
    getCompConst name [] = error $ "Compiler error: no compile time constant named '" ++ name ++ "'"
    getCompConst name scps = do
      let consts = compileTimeConstants $ head scps
      case Map.lookup name consts of
        Just c -> return c
        Nothing -> getCompConst name $ tail scps




