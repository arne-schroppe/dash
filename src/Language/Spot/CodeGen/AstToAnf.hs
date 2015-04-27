module Language.Spot.CodeGen.AstToAnf where

import Language.Spot.IR.Ast
import Language.Spot.IR.Anf
import Language.Spot.IR.Tac
import Control.Monad.State

import Data.List
import qualified Data.Map as Map

-- TODO use named variables?



normalize :: Expr -> (AnfExpr, ConstTable, SymbolNameList)
normalize expr =
  let (result, finalState) = runState (normalizeExpr expr) emptyNormData in
  (result, [], getSymbolNames finalState)

normalizeExpr :: Expr -> State NormData AnfExpr
normalizeExpr expr = case expr of
  FunCall funExpr args -> normalizeFunCall funExpr args
  LocalBinding (Binding name boundExpr) restExpr ->
        normalizeLet (AnfNamedVar name) boundExpr restExpr
  Match matchedExpr patterns -> normalizeMatch matchedExpr patterns
  a -> do
    normalizedAtom <- normalizeAtomicExpr expr
    return $ AnfAtom normalizedAtom

normalizeAtomicExpr :: Expr -> State NormData AnfAtomicExpr
normalizeAtomicExpr expr = case expr of
  LitNumber n -> normalizeNumber n
  LitSymbol sid args -> normalizeSymbol sid args
  Var name -> normalizeVar name
  Lambda params bodyExpr -> normalizeLambda params bodyExpr
  x -> error $ "Unable to normalize " ++ (show x)

normalizeLet var boundExpr restExpr = do
  normalizedBoundExpr <- normalizeAtomicExpr boundExpr
  normalizedRestExpr <- normalizeExpr restExpr
  return $ AnfLet var normalizedBoundExpr normalizedRestExpr

normalizeNumber n = return (AnfNumber n)

normalizeSymbol sid [] = do
  symId <- addSymbolName sid
  return (AnfPlainSymbol symId)
normalizeSymbol sid args = error "Can't normalize this symbol"

normalizeVar name = return $ AnfVar $ AnfNamedVar name

normalizeLambda params bodyExpr = do
  normalizedBody <- normalizeExpr bodyExpr
  let freeVars = []
  return $ AnfLambda freeVars params normalizedBody



normalizeFunCall (Var name) args =
  normalizeNamedFun name args

normalizeNamedFun "add" [LitNumber a, LitNumber b] =
  let norm = AnfLet (AnfTempVar 1) (AnfNumber a) $
             AnfLet (AnfTempVar 2) (AnfNumber b) $
             (AnfPrimOp $ AnfPrimOpAdd (AnfTempVar 1) (AnfTempVar 2))
  in
  return norm

normalizeNamedFun "sub" [LitNumber a, LitNumber b] =
  let norm = AnfLet (AnfTempVar 1) (AnfNumber a) $
             AnfLet (AnfTempVar 2) (AnfNumber b) $
             (AnfPrimOp $ AnfPrimOpSub (AnfTempVar 1) (AnfTempVar 2))
  in
  return norm


normalizeMatch a b = error "Fail"
{-
normalizeMatch matchedExpr patterns = do
  normalizedPatterns <- forM patterns $
      \(pattern, expr) -> do
          normExpr <- normalizeExpr expr
          return (pattern, normExpr)
  return $ AnfMatch (AnfNumber 0) normalizedPatterns
  -- TODO create something like normalize-name to normalize matched expr
-}


isAtomic :: Expr -> Bool
isAtomic expr = case expr of
  LitNumber _ -> True
  LitString _ -> True
  LitSymbol _ _ -> True
  Var _ -> True
  Lambda _ _ -> True
  _ -> False


data NormData = NormData {
  tempVarCounter :: Int
, symbolNames :: Map.Map String SymId
}

emptyNormData = NormData {
  tempVarCounter = 0
, symbolNames = Map.empty
}


newTempVar :: State NormData Int
newTempVar = do
  state <- get
  let nextTmpVar = (tempVarCounter state) + 1
  put $ state { tempVarCounter = nextTmpVar }
  return nextTmpVar


-- TODO copied this from CodeGenState, delete it there
addSymbolName :: String -> State NormData SymId
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

getSymbolNames :: NormData -> SymbolNameList
getSymbolNames = map fst . sortBy (\a b -> compare (snd a) (snd b)) . Map.toList . symbolNames
