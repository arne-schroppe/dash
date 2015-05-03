module Language.Spot.CodeGen.Normalization where

import Language.Spot.IR.Ast
import Language.Spot.IR.Anf
import Language.Spot.IR.Tac
import Control.Monad.State

import Data.List
import qualified Data.Map as Map

-- TODO use named variables?


normalize :: Expr -> (NormExpr, ConstTable, SymbolNameList)
normalize expr =
  let (result, finalState) = runState (normalizeExpr expr) emptyNormState in
  (result, [], getSymbolNames finalState)

normalizeExpr :: Expr -> State NormState NormExpr
normalizeExpr expr = case expr of
  FunCall funExpr args -> normalizeFunCall funExpr args
  LocalBinding (Binding name boundExpr) restExpr ->
        normalizeNamedLet name boundExpr restExpr
  a -> do
    normalizedAtom <- normalizeAtomicExpr expr
    return $ NAtom normalizedAtom

normalizeAtomicExpr :: Expr -> State NormState NormAtomicExpr
normalizeAtomicExpr expr = case expr of
  LitNumber n -> normalizeNumber n
  LitSymbol sid args -> normalizeSymbol sid args
  Var name -> normalizeVar name
  Lambda params bodyExpr -> normalizeLambda params bodyExpr
  Match matchedExpr patterns -> normalizeMatch matchedExpr patterns
  x -> error $ "Unable to normalize " ++ (show x)

normalizeNamedLet name boundExpr restExpr = do
  tmpVar <- newTempVar
  addBinding name tmpVar
  normalizeLet (NTempVar tmpVar) boundExpr restExpr

normalizeLet var boundExpr restExpr = do
  normalizedBoundExpr <- normalizeAtomicExpr boundExpr
  normalizedRestExpr <- normalizeExpr restExpr
  return $ NLet var normalizedBoundExpr normalizedRestExpr

normalizeNumber n = return (NNumber n)

normalizeSymbol sid [] = do
  symId <- addSymbolName sid
  return (NPlainSymbol symId)
normalizeSymbol sid args = error "Can't normalize this symbol"

normalizeVar name = do
  return $ NVar name

normalizeLambda params bodyExpr = do
  normalizedBody <- normalizeExpr bodyExpr
  let freeVars = []
  return $ NLambda freeVars params normalizedBody


-- TODO allow for other cases than just named functions
normalizeFunCall (Var "add") [a, b] =
  normalizeMathPrimOp NPrimOpAdd a b

normalizeFunCall (Var "sub") [a, b] =
  normalizeMathPrimOp NPrimOpSub a b



normalizeFunCall funExpr args = do
  normFunExpr <- normalizeExpr funExpr
  nameExpr normFunExpr $ \ funVar ->
          normalizeExprList args $ \ normArgs -> do
                  return $ NAtom $ NFunCall $ funVar : normArgs

normalizeMathPrimOp mathPrimOp a b = do
  aExpr <- normalizeExpr a
  nameExpr aExpr ( \ aVar -> do
          bExpr <- normalizeExpr b
          nameExpr bExpr ( \ bVar ->
                  return $ NAtom (NPrimOp $ mathPrimOp aVar bVar) ))

normalizeExprList exprList k =
  normalizeExprList' exprList [] k
  where
    normalizeExprList' [] acc k = do
      expr <- k $ reverse acc
      return expr
    normalizeExprList' exprList acc k = do
      let hd = head exprList
      normExpr <- normalizeExpr hd
      nameExpr normExpr $ \ var ->
        normalizeExprList' (tail exprList) (var : acc) k

nameExpr expr k = case expr of
  NAtom (NVar name) -> do
    bnds <- gets bindings
    if Map.member name bnds then do
      let Just varId = Map.lookup name bnds
      bodyExpr <- k (NTempVar varId)
      return bodyExpr
    else
      letBind (NVar name) k
  NAtom aExpr ->
    letBind aExpr k
  NLet v boundExpr bodyExpr -> do
    expr <- nameExpr bodyExpr k
    return $ NLet v boundExpr expr
  where
    letBind aExpr k = do
      tmpVar <- newTempVar
      let var = NTempVar tmpVar
      bodyExpr <- k var
      return $ NLet var aExpr bodyExpr


normalizeMatch matchedExpr patterns = do
  normalizedPatterns <- forM patterns $
      \(pattern, expr) -> do
          normExpr <- normalizeExpr expr
          return (pattern, normExpr)
  tmpVar <- newTempVar
  return $ NNumber 0
{-
  return $ NLet (NTempVar tmpVar) (NNumber 0) $
           NAtom $ NMatch 0 (NTempVar tmpVar) normalizedPatterns
-}
  -- TODO create something like normalize-name to normalize matched expr


data NormState = NormState {
  tempVarCounter :: Int
, symbolNames :: Map.Map String SymId
, bindings :: Map.Map String Int
}

emptyNormState = NormState {
  tempVarCounter = 0
, symbolNames = Map.empty
, bindings = Map.empty
}


newTempVar :: State NormState Int
newTempVar = do
  state <- get
  let tmpVar = tempVarCounter state
  let nextTmpVar = (tempVarCounter state) + 1
  put $ state { tempVarCounter = nextTmpVar }
  return tmpVar


-- TODO copied this from CodeGenState, delete it there
addSymbolName :: String -> State NormState SymId
addSymbolName s = do
  state <- get
  let syms = symbolNames state
  if Map.member s syms then
    return $ syms Map.! s
  else do
    let nextId = Map.size syms
    let syms' = Map.insert s nextId syms
    put $ state { symbolNames = syms' }
    return nextId

getSymbolNames :: NormState -> SymbolNameList
getSymbolNames = map fst . sortBy (\a b -> compare (snd a) (snd b)) . Map.toList . symbolNames


addBinding :: String -> Int -> State NormState ()
addBinding name tmpVarId = do
  state <- get
  let bindings' = Map.insert name tmpVarId (bindings state)
  put $ state { bindings = bindings' }

