module Language.Spot.CodeGen.CodeGen2 (
  compile
) where

import Language.Spot.IR.Norm
import Language.Spot.IR.Tac

import Control.Monad.State
import qualified Data.Sequence as Seq
import Data.Foldable
import Control.Applicative

import Debug.Trace

import qualified Data.Sequence as Seq
import qualified Data.Map as Map

-- TODO when there is more time, do dataflow analysis to reuse registers



compile :: NormExpr -> ConstTable -> SymbolNameList -> ([[Tac Reg]], ConstTable, SymbolNameList)
compile expr _ symlist =
  let result = execState (compileFunc [] [] expr) emptyCompState in
  (toList (instructions result), dataTable result, symlist)


compileFunc freeVars params expr = do
  funAddr <- beginFunction freeVars params
  funcCode <- compileExpr expr
  endFunction funAddr funcCode
  return funAddr

compileExpr expr = case expr of
  NLet var atom body -> compileLet var atom body
  NAtom a -> do
          code <- compileAtom 0 a True
          return $ code ++ [Tac_ret 0]


compileAtom reg atom isResultValue = case atom of
  NNumber n -> return [Tac_load_i reg (fromIntegral n)]
  NPlainSymbol sid -> return [Tac_load_ps reg sid]
  NPrimOp (NPrimOpAdd a b) -> do
          ra <- getReg a
          rb <- getReg b
          return [Tac_add reg ra rb]
  NPrimOp (NPrimOpSub a b) -> do
          ra <- getReg a
          rb <- getReg b
          return [Tac_sub reg ra rb]
  NLambda [] params expr -> do
          funAddr <- compileFunc [] params expr
          if isResultValue then
              return [Tac_load_f reg funAddr, Tac_make_cl reg reg 0]
          else
              return [Tac_load_f reg funAddr]
  NLambda freeVars params expr -> compileClosure reg freeVars params expr
  NFunCall funVar args -> do
          argInstrs <- mapM (uncurry compileSetArg) $ zip [0..(length args)] args
          rFun <- getReg funVar
          direct <- isDirectCallReg rFun
          if direct then do
            let callInstr = [Tac_call reg rFun (length args)]
            return $ argInstrs ++ callInstr
          else do
            let callInstr = [Tac_call_cl reg rFun (length args)]
            return $ argInstrs ++ callInstr
  NVar var -> case var of
    -- TODO add constant free var. Resolve to whatever it is in outer context
    NLocalVar varId _ -> do
          rt <- getReg var
          return [Tac_move reg rt]
    NFunParam name -> do
          regIndex <- getReg var
          return [Tac_move reg regIndex]
    _ -> error "fail"
  x -> error $ "Unable to compile " ++ (show x)

compileLet tmpVar atom body = do
  let callDirect = canBeCalledDirectly atom
  rTmp <- getReg tmpVar
  when callDirect $ addDirectCallReg rTmp
  comp1 <- compileAtom rTmp atom False
  comp2 <- compileExpr body
  return $ comp1 ++ comp2

-- This determines whether we'll use Tac_call or Tac_call_cl later
canBeCalledDirectly atom = case atom of
  NVar (NConstantFreeVar _) -> True
  NLambda ([]) _ _ -> True
  _ -> False

compileClosure reg freeVars params expr = do
  funAddr <- compileFunc freeVars params expr
  argInstrs <- mapM (uncurry compileSetArgN) $ zip [0..(length freeVars)] freeVars
  -- TODO use the next free register instead of hardcoded value
  let makeClosureInstr = [ Tac_load_f 31 funAddr, Tac_make_cl reg 31 (length freeVars)]
  return $ argInstrs ++ makeClosureInstr


compileSetArg arg var = do
  rVar <- getReg var
  return $ Tac_set_arg arg rVar 0

compileSetArgN arg name = do
  rVar <- getRegByName name
  return $ Tac_set_arg arg rVar 0



----- State -----

-- TODO introduce a proper symtable
data CompState = CompState {
                   instructions :: Seq.Seq [Tac Reg]
                 , dataTable :: ConstTable -- rename ConstTable to DataTable
                 , scopes :: [CompScope]
                 }

emptyCompState = CompState {
                   instructions = Seq.fromList []
                 , dataTable = []
                 , scopes = []
                 }

data CompScope = CompScope {
                   functionParams :: Map.Map String Int
                 , freeVariables :: Map.Map String Int

                 -- these are all the registers that hold function values which
                 -- can be called directly with Tac_call. Everything else is 
                 -- called with Tac_call_cl
                 , directCallRegs :: [Int]
                 }

makeScope fps freeVars = CompScope {
               functionParams = fps
             , freeVariables = freeVars
             , directCallRegs = []
             }

beginFunction freeVars params = do
  state <- get
  let localFreeVars = Map.fromList (zip freeVars [0..(length freeVars)])
  let paramBindings = Map.fromList (zip params [0..(length params)])
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
  case lookup of
    Just index -> return index
    Nothing -> error $ "Unknown identifier " ++ name
  where getRegN name = do
          p <- param name
          case p of
                  Just i -> return $ Just i
                  Nothing -> do
                          f <- freeVar name
                          return f

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

isDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  return $ Prelude.elem reg dCallRegs

addDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  let dCallRegs' = reg : dCallRegs
  putScope $ scope { directCallRegs = dCallRegs' }

getScope = do
  state <- get
  return $ head (scopes state)

putScope s = do
  state <- get
  put $ state { scopes = s : (tail $ scopes state) }









{-

compile :: Expr -> ([[Tac Reg]], ConstTable, SymbolNameList)
compile ast =
        let (result, finalState) = runState (compileExpression ast 0) emptyCode in
        let allFuncsncs = (instructions result) Seq.<| (otherFunctions result) in
        let result1 = map assignRegisters (toList allFuncs) in
        (result1, getConstantTable finalState, getSymbolNames finalState)

data CompValue =
    CVResult
  | CVVar String
  | CVTempVar Int
  | CVFunArg Int
  deriving (Show, Eq)

data CompilationResult = CompilationResult {
    instructions :: [Tac CompValue]
  , otherFunctions :: Seq.Seq [Tac CompValue]
  , usedTempVars :: Int
  -- , freeVariables :: [String]
  }
  deriving (Show)

emptyResult = CompilationResult { instructions = []
                                , otherFunctions = Seq.fromList []
                                , usedTempVars = 0
                                -- , freeVariables = [] 
                                }

compileExpression :: Expr -> Int -> State Code CompilationResult
compileExpression expr usedTempVars = case expr of
  LitNumber n         -> compileLitNumber n usedTempVars
  LitSymbol name args -> compileLitSymbol name args usedTempVars
  FunCall name args   -> compileFunCall name args usedTempVars
  LocalBinding (Binding name boundExpr) bodyExpr ->
                         compileLocalBinding name boundExpr bodyExpr usedTempVars
  Var a               -> compileVar a usedTempVars
{-
  Match e pats      -> compileMatch e pats
  FunDef params expr -> compileLambda params expr
-}
  a -> error $ "Can't compile: " ++ show a

compileLitNumber n utv = return $ emptyResult {
                              instructions = [Tac_load_i CVResult (tw n)],
                              usedTempVars = utv }

compileLitSymbol name [] utv = do
  symId <- addSymbolName name
  return $ emptyResult { instructions = [Tac_load_ps CVResult symId], usedTempVars = utv }
compileLitSymbol name args utv = do
  c <- createConstant $ LitSymbol name args
  cAddr <- addConstant c
  return $ emptyResult { instructions = [Tac_load_cs CVResult cAddr], usedTempVars = utv }


-- TODO when compiling fun calls: We need to store somewhere, whether we can call a function
-- directly, or whether it is (potentially) a closure
compileFunCall (Var "add") [op1, op2] utv = compileMathFunCall Tac_add op1 op2 utv
compileFunCall (Var "sub") [op1, op2] utv = compileMathFunCall Tac_sub op1 op2 utv
compileFunCall (Var name) args utv        = do
  argCode <- compileFunctionCallArgs args utv
  let instrs = argCode ++ [Tac_call CVResult (CVVar name)
  return $ emptyResult { instructions = instrs,
                         usedTempVars = utv + (length args) }

compileMathFunCall mf op1 op2 utv = do
  (v1, res1) <- compileArgument op1 utv
  (v2, res2) <- compileArgument op2 (usedTempVars res1)
  let finalInstrs = (instructions res1) ++ (instructions res2) ++ [mf CVResult v1 v2]
  return $ emptyResult { instructions = finalInstrs,
                         usedTempVars = usedTempVars res2 }

compileArgument (Var v) utv =
  return (CVVar v, emptyResult { usedTempVars = utv })
compileArgument e utv = do
  exprResult <- compileExpression e utv
  let resultTempVar = (usedTempVars exprResult) + 1
  let instrs' = convertResultToTemp resultTempVar (instructions exprResult)
  return $ (CVTempVar resultTempVar,
            exprResult { instructions = instrs', usedTempVars = resultTempVar } )

compileFunctionCallArgs args utv = do
  argResult <- foldM (uncurry compileCallArg) emptyResult (zip args [0..(length args)])
  where
    compileCallArg expr reg = do
      (argV, argResult 

compileLocalBinding name (FunDef params expr) bodyExpr utv = do
  funcResult <- compileFunction params expr
  funcAddr <- reserveFunAddr
  let bindingTempVar = utv + 1
  let combinedOtherFunctions = (otherFunctions funcResult) Seq.|> (instructions funcResult)
  let bindingInstrs = [ Tac_load_f (CVTempVar bindingTempVar) funcAddr ]
  bodyRes <- compileExpression bodyExpr bindingTempVar
  let bodyInstrs = convertVarToTemp name bindingTempVar (instructions bodyRes)
  return $ emptyResult { instructions = bindingInstrs ++ bodyInstrs,
                         usedTempVars = usedTempVars bodyRes,
                         otherFunctions = combinedOtherFunctions }
compileLocalBinding name boundExpr bodyExpr utv = do
  boundRes <- compileExpression boundExpr utv
  let bindingTempVar = (usedTempVars boundRes) + 1
  let bindingInstrs = convertResultToTemp bindingTempVar (instructions boundRes)
  bodyRes <- compileExpression bodyExpr bindingTempVar
  let bodyInstrs = convertVarToTemp name bindingTempVar (instructions bodyRes)
  return $ emptyResult { instructions = bindingInstrs ++ bodyInstrs,
                         usedTempVars = usedTempVars bodyRes  }  -- TODO also return other data


compileVar a utv = return $ emptyResult { instructions = [Tac_move CVResult (CVVar a)], usedTempVars = utv }

compileFunction params expr = do
  let numParams = length params
  exprRes <- compileExpression expr numParams
  exprInstrs <- foldM varToTemp (instructions exprRes) (zip [0..numParams] params)
  return $ emptyResult { instructions = exprInstrs } -- TODO add free vars
  where
    varToTemp instrs (n, param) =
      return $ convertVarToTemp param n instrs


convertVarToTemp varName tempNum instrs =
  mapTac (convertOne varName tempNum) instrs
  where
    convertOne name tempVar v = case v of
      CVVar n | n == name -> CVTempVar tempVar
      a                   -> a

convertResultToTemp :: Int -> [Tac CompValue] -> [Tac CompValue]
convertResultToTemp tempVar tacs =
  mapTac (convResult tempVar) tacs
  where
    convResult tempVar value = case value of
      CVResult -> CVTempVar tempVar
      a -> a



toWord32 = fromIntegral -- TODO add some range checks here
tw = toWord32


createConstant litC =
  case litC of
    LitNumber n -> return $ CNumber n
    LitSymbol s [] -> do
                sid <- addSymbolName s
                return $ CPlainSymbol sid
    LitSymbol s args -> do
                symId <- addSymbolName s
                encodedArgs <- mapM createConstant args
                return $ CCompoundSymbol symId encodedArgs


assignRegisters :: [Tac CompValue] -> [Tac Reg]
assignRegisters codeWithoutRegs =
  mapTac assignReg codeWithoutRegs
  where
    assignReg :: CompValue -> Reg
    assignReg cv = case cv of
      CVResult -> 0
      CVTempVar 0 -> error "Compilation error: Temp var 0"
      CVTempVar n -> n
      _ -> error "unable to convert value"


mapTac :: (a -> b) -> [Tac a] -> [Tac b]
mapTac f tacs = map (mapSingleTac f) tacs
  where
    mapSingleTac :: (a -> b) -> Tac a -> Tac b
    mapSingleTac f tac = case tac of
      Tac_load_i var v       -> Tac_load_i (f var) v
      Tac_load_addr var addr -> Tac_load_addr (f var) addr
      Tac_load_f var fAddr   -> Tac_load_f (f var) fAddr
      Tac_load_ps var symId  -> Tac_load_ps (f var) symId
      Tac_load_cs var cAddr  -> Tac_load_cs (f var) cAddr
      Tac_load_c var cAddr   -> Tac_load_c (f var) cAddr
      Tac_add v1 v2 v3       -> Tac_add (f v1) (f v2) (f v3)
      Tac_sub v1 v2 v3       -> Tac_sub (f v1) (f v2) (f v3)
      Tac_move v1 v2         -> Tac_move (f v1) (f v2)
      Tac_call v1 v2 n       -> Tac_call (f v1) (f v2) n
      Tac_call_cl v1 v2 n    -> Tac_call_cl (f v1) (f v2) n
      Tac_make_cl v1 v2 n    -> Tac_make_cl (f v1) (f v2) n
      Tac_jmp n              -> Tac_jmp n
      Tac_match v1 v2 v3     -> Tac_match (f v1) (f v2) (f v3)
      Tac_ret                -> Tac_ret
-}
