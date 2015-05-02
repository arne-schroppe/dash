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
        normalizeLet (NNamedVar name) boundExpr restExpr
  Match matchedExpr patterns -> normalizeMatch matchedExpr patterns
  a -> do
    normalizedAtom <- normalizeAtomicExpr expr
    return $ NAtom normalizedAtom

normalizeAtomicExpr :: Expr -> State NormState NormAtomicExpr
normalizeAtomicExpr expr = case expr of
  LitNumber n -> normalizeNumber n
  LitSymbol sid args -> normalizeSymbol sid args
  Var name -> normalizeVar name
  Lambda params bodyExpr -> normalizeLambda params bodyExpr
  x -> error $ "Unable to normalize " ++ (show x)

normalizeLet var boundExpr restExpr = do
  normalizedBoundExpr <- normalizeAtomicExpr boundExpr
  normalizedRestExpr <- normalizeExpr restExpr
  return $ NLet var normalizedBoundExpr normalizedRestExpr

normalizeNumber n = return (NNumber n)

normalizeSymbol sid [] = do
  symId <- addSymbolName sid
  return (NPlainSymbol symId)
normalizeSymbol sid args = error "Can't normalize this symbol"

normalizeVar name = return $ NVar $ NNamedVar name

normalizeLambda params bodyExpr = do
  normalizedBody <- normalizeExpr bodyExpr
  let freeVars = []
  return $ NLambda freeVars params normalizedBody


-- TODO allow for other cases than just named functions
normalizeFunCall (Var name) args =
  normalizeNamedFun name args


normalizeNamedFun "add" [a, b] =
  normalizeMathPrimOp NPrimOpAdd a b

normalizeNamedFun "sub" [a, b] =
  normalizeMathPrimOp NPrimOpSub a b

normalizeNamedFun name args =
  normalizeExprList args $ \ normArgs -> NAtom $ NFunCall $ (NNamedVar name) : normArgs

normalizeMathPrimOp mathPrimOp a b = do
  aExpr <- normalizeExpr a
  nameExpr aExpr ( \ aVar -> do
          bExpr <- normalizeExpr b
          nameExpr bExpr ( \ bVar ->
                  return $ NAtom (NPrimOp $ mathPrimOp aVar bVar) ))

normalizeExprList exprList k =
  normalizeExprList' exprList [] k
  where
    normalizeExprList' [] acc k = return $ k $ reverse acc
    normalizeExprList' exprList acc k = do
      let hd = head exprList
      normExpr <- normalizeExpr hd
      nameExpr normExpr $ \ var ->
        normalizeExprList' (tail exprList) (var : acc) k

nameExpr expr k = case expr of
  NMatch _ _ _ -> error "Non-atomic"
  NAtom aExpr -> do
    tmpVar <- newTempVar
    let var = NTempVar tmpVar
    bodyExpr <- k var
    return $ NLet var aExpr bodyExpr
  NLet v boundExpr bodyExpr -> do
    expr <- nameExpr bodyExpr k
    return $ NLet v boundExpr expr

normalizeMatch matchedExpr patterns = do
  normalizedPatterns <- forM patterns $
      \(pattern, expr) -> do
          normExpr <- normalizeExpr expr
          return (pattern, normExpr)
  tmpVar <- newTempVar
  return $ NLet (NTempVar tmpVar) (NNumber 0) $
           NMatch 0 (NTempVar tmpVar) normalizedPatterns
  -- TODO create something like normalize-name to normalize matched expr


data NormState = NormState {
  tempVarCounter :: Int
, symbolNames :: Map.Map String SymId
}

emptyNormState = NormState {
  tempVarCounter = 0
, symbolNames = Map.empty
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


