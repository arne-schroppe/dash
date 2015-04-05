module Language.Spot.CodeGen.CodeGen2 (
  compile
) where

import Language.Spot.CodeGen.CodeGenState
import Language.Spot.IR.Ast
import Language.Spot.IR.Tac

import Control.Monad.State

compile :: Expr -> ([[Tac Reg]], ConstTable, SymbolNameList)
compile ast =
        let (result, finalState) = runState (compileExpression ast) emptyCode in
        let result1 = assignRegisters result in
        ([result1], getConstantTable finalState, getSymbolNames finalState)

data CompValue = 
    CVResult
  | CVVar String
  | CVTempVar Int

data CompilationResult = CompilationResult {
    code :: [Tac CompValue]
  , usedTempVars :: Int
  , freeVariables :: [String]
  }

emptyResult = CompilationResult { code = [], usedTempVars = 0, freeVariables = [] }

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

compileLitNumber n = return $ emptyResult { code = [Tac_load_i CVResult (tw n)] }

compileLitSymbol name [] = do
  symId <- addSymbolName name
  return $ emptyResult { code = [Tac_load_ps CVResult symId] }
compileLitSymbol name args = do
  c <- createConstant $ LitSymbol name args
  cAddr <- addConstant c
  return $ emptyResult { code = [Tac_load_cs CVResult cAddr] }


compileFunCall (Var "add") [LitNumber op1, LitNumber op2] = do
  let code = [ Tac_load_i (CVTempVar 1) (tw op1),
               Tac_load_i (CVTempVar 2) (tw op2),
               Tac_add CVResult (CVTempVar 1) (CVTempVar 2)]
  return $ emptyResult { code = code, usedTempVars = 2 }
compileFunCall (Var "sub") [LitNumber op1, LitNumber op2] = do
  let code = [ Tac_load_i (CVTempVar 1) (tw op1),
               Tac_load_i (CVTempVar 2) (tw op2),
               Tac_sub CVResult (CVTempVar 1) (CVTempVar 2)]
  return $ emptyResult { code = code, usedTempVars = 2 }

compileLocalBinding name boundExpr bodyExpr = do
  boundRes <- compileExpression boundExpr
  let bindingTempVar = usedTempVars boundRes + 1
  let bindingTacs = convertResultToTemp bindingTempVar (code boundRes)
  bodyRes <- compileExpression bodyExpr
  let bodyTacs = mapTac (incTempVar (bindingTempVar + 1)) (code bodyRes)
  let bodyTacs1 = mapTac (convVarToTemp name bindingTempVar) bodyTacs
  return $ emptyResult { code = bindingTacs ++ bodyTacs1 }  -- TODO also return other data
  where
    incTempVar minTempVar v = case v of
      CVTempVar n -> CVTempVar $ minTempVar + n
      a -> a
    convVarToTemp name tempVar v = case v of
      CVVar n | n == name -> CVTempVar tempVar
      a -> a
  
  -- (freeVars, code) <- compileExpression bodyExpr

compileVar a = return $ emptyResult { code = [Tac_move CVResult (CVVar a)] }

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
  let codeWithoutRegs = code compResult in
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

