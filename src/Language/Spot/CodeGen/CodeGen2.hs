module Language.Spot.CodeGen.CodeGen2 (
  compile
) where

import Language.Spot.CodeGen.CodeGenState
import Language.Spot.IR.Ast
import Language.Spot.IR.Tac

import Control.Monad.State

import Debug.Trace

compile :: Expr -> ([[Tac Reg]], ConstTable, SymbolNameList)
compile ast =
        let (result, finalState) = runState (compileExpression ast) emptyCode in
        let result1 = assignRegisters result in
        ([result1], getConstantTable finalState, getSymbolNames finalState)

data CompValue = 
    CVResult
  | CVVar String
  | CVTempVar Int
  deriving (Show, Eq)

data CompilationResult = CompilationResult {
    instructions :: [Tac CompValue]
  , usedTempVars :: Int
  , freeVariables :: [String]
  }
  deriving (Show)

emptyResult = CompilationResult { instructions = [], usedTempVars = 0, freeVariables = [] }

compileExpression :: Expr -> State Code CompilationResult
compileExpression expr = case expr of
  LitNumber n         -> compileLitNumber n
  LitSymbol name args -> compileLitSymbol name args
  FunCall name args   -> compileFunCall name args
  LocalBinding (Binding name boundExpr) bodyExpr ->
                         compileLocalBinding name boundExpr bodyExpr
  Var a               -> compileVar a
{-
  Match e pats      -> compileMatch e pats
  FunDef params expr -> compileLambda params expr
-}
  a -> error $ "Can't compile: " ++ show a

compileLitNumber n = return $ emptyResult { instructions = [Tac_load_i CVResult (tw n)] }

compileLitSymbol name [] = do
  symId <- addSymbolName name
  return $ emptyResult { instructions = [Tac_load_ps CVResult symId] }
compileLitSymbol name args = do
  c <- createConstant $ LitSymbol name args
  cAddr <- addConstant c
  return $ emptyResult { instructions = [Tac_load_cs CVResult cAddr] }


compileFunCall (Var "add") [op1, op2] = compileMathFunCall Tac_add op1 op2
compileFunCall (Var "sub") [op1, op2] = compileMathFunCall Tac_sub op1 op2

compileMathFunCall mf op1 op2 = do
  (v1, res1) <- compileArgument op1
  (v2, res2) <- compileArgument op2
  let instrs2' = increaseTempVars (usedTempVars res1) (instructions res2)
  let v2' = increaseTempVar (usedTempVars res1) v2
  let finalInstrs = (instructions res1) ++ instrs2' ++ [mf CVResult v1 v2']
  let r = emptyResult { instructions = finalInstrs,
                         usedTempVars = (usedTempVars res1) + (usedTempVars res2) }
  return $ trace (show r) r

compileArgument (Var v) =
  return (CVVar v, emptyResult)
compileArgument e = do
  exprResult <- compileExpression e
  let resultTempVar = (usedTempVars exprResult) + 1
  let instrs' = convertResultToTemp resultTempVar (instructions exprResult)
  return $ (CVTempVar resultTempVar,
            exprResult { instructions = instrs', usedTempVars = resultTempVar } )

compileLocalBinding name boundExpr bodyExpr = do
  boundRes <- compileExpression boundExpr
  let bindingTempVar = usedTempVars boundRes + 1
  let bindingInstrs = convertResultToTemp bindingTempVar (instructions boundRes)
  bodyRes <- compileExpression bodyExpr
  let bodyInstrs = increaseTempVars bindingTempVar (instructions boundRes)
  let bodyInstrs1 = mapTac (convVarToTemp name bindingTempVar) bodyInstrs
  return $ emptyResult { instructions = bindingInstrs ++ bodyInstrs1 }  -- TODO also return other data
  where
    convVarToTemp name tempVar v = case v of
      CVVar n | n == name -> CVTempVar tempVar
      a -> a
  
  -- (freeVars, code) <- compileExpression bodyExpr

compileVar a = return $ emptyResult { instructions = [Tac_move CVResult (CVVar a)] }

increaseTempVars n tacs =
  mapTac (increaseTempVar n) tacs

increaseTempVar minTempVar v = case v of
  CVTempVar n -> CVTempVar $ minTempVar + n
  a -> a

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


assignRegisters :: CompilationResult -> [Tac Reg]
assignRegisters compResult =
  let codeWithoutRegs = instructions compResult in
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

