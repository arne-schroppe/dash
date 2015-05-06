module Language.Spot.CodeGen.CodeGen2 (
  compile
) where

import Language.Spot.CodeGen.CodeGenState
import Language.Spot.IR.Norm
import Language.Spot.IR.Tac

import Control.Monad.State
import qualified Data.Sequence as Seq
import Data.Foldable

import Debug.Trace

import qualified Data.Sequence as Seq
import qualified Data.Map as Map

-- TODO when there is more time, do dataflow analysis to reuse registers



compile :: NormExpr -> ConstTable -> SymbolNameList -> ([[Tac Reg]], ConstTable, SymbolNameList)
compile expr _ symlist =
  let result = execState (compileFunc [] expr) emptyCompState in
  (toList (instructions result), dataTable result, symlist)


compileFunc params expr = do
  let funcCode = compileExpr expr
  addFunction funcCode

compileExpr expr = case expr of
  NLet var atom body -> compileLet var atom body
  NAtom a -> (compileAtom 0 a) ++ [Tac_ret 0]


compileAtom reg atom = case atom of
  NNumber n -> [Tac_load_i reg (fromIntegral n)]
  NPlainSymbol sid -> [Tac_load_ps reg sid]
  NPrimOp (NPrimOpAdd a b) -> [Tac_add reg (r a) (r b)]
  NPrimOp (NPrimOpSub a b) -> [Tac_sub reg (r a) (r b)]
  NResultVar t -> [Tac_move reg (r t)]
  NLambda freeVars params expr -> [Tac_load_f reg 9999]
  x -> error $ "Unable to compile " ++ (show x)

compileLet tmpVar atom body =
  let comp1 = compileAtom (r tmpVar) atom in -- TODO compileAtom reg atom in
  let comp2 = compileExpr body in
  comp1 ++ comp2


r (NVar tmpVar) = tmpVar




----- State -----

data CompState = CompState {
                   instructions :: Seq.Seq [Tac Reg]
                 , dataTable :: ConstTable -- rename ConstTable to DataTable
                 }

emptyCompState = CompState {
                   instructions = Seq.fromList []
                 , dataTable = []
                 }

addFunction code = do
  state <- get
  let instrs' = (instructions state) Seq.|> code
  put $ state { instructions = instrs' }














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
