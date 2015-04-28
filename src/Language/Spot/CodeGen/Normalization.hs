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


-- TODO prevent code duplication, allow for other functions
normalizeNamedFun "add" [LitNumber a, LitNumber b] =
  normalizeMathPrimOp NPrimOpAdd a b

normalizeNamedFun "sub" [LitNumber a, LitNumber b] =
  normalizeMathPrimOp NPrimOpSub a b

normalizeMathPrimOp mathPrimOp a b = do
  tmpVar1 <- newTempVar
  tmpVar2 <- newTempVar
  let norm = NLet (NTempVar tmpVar1) (NNumber a) $
             NLet (NTempVar tmpVar2) (NNumber b) $
             (NPrimOp $ mathPrimOp (NTempVar tmpVar1) (NTempVar tmpVar2))
  return norm

normalizeMatch matchedExpr patterns = do
  normalizedPatterns <- forM patterns $
      \(pattern, expr) -> do
          normExpr <- normalizeExpr expr
          return (pattern, normExpr)
  return $ NMatch (NNumber 0) normalizedPatterns
  -- TODO create something like normalize-name to normalize matched expr


isAtomic :: Expr -> Bool
isAtomic expr = case expr of
  LitNumber _ -> True
  LitString _ -> True
  LitSymbol _ _ -> True
  Var _ -> True
  Lambda _ _ -> True
  _ -> False


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
  let nextTmpVar = (tempVarCounter state) + 1
  put $ state { tempVarCounter = nextTmpVar }
  return nextTmpVar


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


