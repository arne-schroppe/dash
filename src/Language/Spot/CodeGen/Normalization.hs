module Language.Spot.CodeGen.Normalization where

import Language.Spot.IR.Ast
import Language.Spot.IR.Anf
import Language.Spot.IR.Tac
import Control.Monad.State

import Data.List
import qualified Data.Map as Map



type Cont = NormAtomicExpr -> State NormState NormExpr

normalize :: Expr -> (NormExpr, ConstTable, SymbolNameList)
normalize expr =
  let (result, finalState) = runState (normalizeExpr expr) emptyNormState in
  (result, [], getSymbolNames finalState)


normalizeExpr :: Expr -> State NormState NormExpr
normalizeExpr expr = case expr of
  LocalBinding (Binding name boundExpr) restExpr ->
    nameExpr boundExpr $ \ (NTempVar var) -> do
      addBinding name var
      rest <- normalizeExpr restExpr
      return $ rest
  _ -> do
    atomizeExpr expr $ return . NAtom


atomizeExpr :: Expr -> Cont -> State NormState NormExpr
atomizeExpr expr k = case expr of
  FunCall funExpr args -> normalizeFunCall funExpr args k
  LitNumber n -> normalizeNumber n k
  LitSymbol sid args -> normalizeSymbol sid args k
  Var name -> normalizeVar name k
  Lambda params bodyExpr -> normalizeLambda params bodyExpr k
  Match matchedExpr patterns -> normalizeMatch matchedExpr patterns k
  LocalBinding (Binding name boundExpr) restExpr -> -- inner local binding ! (i.e. let a = let b = 2 in 1 + b)
    atomizeExpr boundExpr $ \ aExpr -> do
      tmpVar <- newTempVar
      let var = NTempVar tmpVar
      addBinding name tmpVar
      atomizeExpr restExpr $ \ boundExpr -> do
        rest <- k boundExpr
        return $ NLet var aExpr rest
  x -> error $ "Unable to normalize " ++ (show x)


normalizeNumber n k = k (NNumber n)


normalizeSymbol sid [] k = do
  symId <- addSymbolName sid
  k (NPlainSymbol symId)

normalizeSymbol sid args k = error "Can't normalize this symbol"


normalizeVar name k = do
  k $ NVar name


normalizeLambda params bodyExpr k = do
  normalizedBody <- normalizeExpr bodyExpr
  let freeVars = []
  k $ NLambda freeVars params normalizedBody


normalizeFunCall (Var "add") [a, b] k =
  normalizeMathPrimOp NPrimOpAdd a b k

normalizeFunCall (Var "sub") [a, b] k =
  normalizeMathPrimOp NPrimOpSub a b k

normalizeFunCall funExpr args k = do
  nameExpr funExpr $ \ funVar ->
          normalizeExprList args $ \ normArgs ->
                  k $ NFunCall $ funVar : normArgs

normalizeMathPrimOp mathPrimOp a b k = do
  normalizeExprList [a, b] $ \ [aVar, bVar] ->
      k $ NPrimOp $ mathPrimOp aVar bVar


normalizeMatch matchedExpr patterns k = do
  normalizedPatterns <- forM patterns $
      \(pattern, expr) -> do
          normExpr <- normalizeExpr expr
          return (pattern, normExpr)
  tmpVar <- newTempVar
  k $ NNumber 0




----- Normalization helper functions -----

normalizeExprList exprList k =
  normalizeExprList' exprList [] k
  where
    normalizeExprList' [] acc k = do
      expr <- k $ reverse acc
      return expr
    normalizeExprList' exprList acc k = do
      let hd = head exprList
      nameExpr hd $ \ var -> do
        restExpr <- normalizeExprList' (tail exprList) (var : acc) k
        return restExpr

nameExpr expr k = case expr of
  Var name -> do
    bnds <- gets bindings
    if Map.member name bnds then do
      let Just varId = Map.lookup name bnds
      bodyExpr <- k (NTempVar varId)
      return bodyExpr
    else
      letBind expr k
  _ -> letBind expr k
  where
    letBind e k = do
      atomizeExpr e $ \ aExpr -> do
        tmpVar <- newTempVar
        let var = NTempVar tmpVar
        restExpr <- k var
        return $ NLet var aExpr restExpr




----- State -----

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

