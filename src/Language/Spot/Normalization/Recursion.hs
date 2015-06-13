module Language.Spot.Normalization.Recursion (
  resolveRecursion
) where

import           Control.Monad.State                   hiding (state)
import           Data.List
import           Language.Spot.IR.Nst

-- State

-- TODO pull up new extra vars (if not resolved in that scope)
-- avoid duplicate free vars by adding all current freevars when entering scope

data RecursionEnv = RecursionEnv {
  lambdaStack   :: [(String, NstVar)]

  -- 'extra free vars' are the additional free variables that are created when
  -- resolving recursion that involves closures or unknown functions
, extraFreeVars :: [[String]]
}


emptyRecursionEnv :: RecursionEnv
emptyRecursionEnv = RecursionEnv {
  lambdaStack = []
, extraFreeVars = []
}


type RecursionState a = State RecursionEnv a


resolveRecursion :: NstExpr -> NstExpr
resolveRecursion normExpr = evalState (resolveRecExpr normExpr) emptyRecursionEnv


resolveRecExpr :: NstExpr -> RecursionState NstExpr
resolveRecExpr normExpr = case normExpr of
  NLet var atom expr -> resolveRecLet var atom expr
  NAtom atom -> do
    recAtom <- resolveRecAtom atom ""
    return $ NAtom recAtom


resolveRecLet :: NstVar -> NstAtomicExpr -> NstExpr -> RecursionState NstExpr
resolveRecLet var atom expr = do
  resAtom <- resolveRecAtom atom (localVarName var)
  resExpr <- resolveRecExpr expr
  return $ NLet var resAtom resExpr


localVarName :: NstVar -> String
localVarName (NLocalVar _ name) = name
localVarName _ = error "Internal compiler error: Something else than a local var was let-bound"


resolveRecAtom :: NstAtomicExpr -> String -> RecursionState NstAtomicExpr
resolveRecAtom atom name = case atom of
  -- Invariant: Recursive vars are always let-bound  (TODO loosen this restriction later)
  NLambda freeVars params expr -> resolveRecLambda freeVars params expr name
  NVar (NRecursiveVar vname) -> do
    var <- resolveRecVar vname
    return $ NVar var
  a -> return a


resolveRecLambda :: [String] -> [String] -> NstExpr -> String -> RecursionState NstAtomicExpr
resolveRecLambda freeVars params expr name = do
  pushLambdaScope freeVars name
  resolvedBody <- resolveRecExpr expr
  extraFree <- gets extraFreeVars
  let extra = head extraFree
  popLambdaScope
  return $ NLambda (freeVars ++ extra) params resolvedBody


resolveRecVar :: String -> RecursionState NstVar
resolveRecVar name = do
  state <- get
  let maybeVar = findName name (lambdaStack state)
  case maybeVar of
    Nothing -> error $ "Internal compiler error: Can't resolve recursive use of " ++ name
    Just v@(NConstantFreeVar _)    -> return v
    Just v@(NDynamicFreeVar vname) -> do
      addExtraFreeVar vname
      return v
    _ -> error "resolveRecVar"


findName :: String -> [(String, a)] -> Maybe a
findName _ []             = Nothing
findName name ((n, v):ns) =
  if n == name then Just v
               else findName name ns


addExtraFreeVar :: String -> RecursionState ()
addExtraFreeVar name = do
  state <- get
  let extra = extraFreeVars state
  let thisFreeVars = head $ extra
  when (not $ name `elem` thisFreeVars) $ do
          let free' = name : thisFreeVars
          let state' = state { extraFreeVars = free' : (tail extra) }
          put state'

-- TODO this doesn't work for mutual recursion

-- The first case (for a lambda that isn't let-bound) is just a placeholder, so that we
-- can easily pop the scope later. No recursive var will resolve to it anyway
pushLambdaScope :: [String] -> String -> RecursionState ()
pushLambdaScope _ "" = pushLambdaScope' "$$$invalid$$$" (NConstantFreeVar "$$$invalid$$$")
pushLambdaScope [] n = pushLambdaScope' n (NConstantFreeVar n)
pushLambdaScope _  n = pushLambdaScope' n (NDynamicFreeVar n)


pushLambdaScope' :: String -> NstVar -> RecursionState ()
pushLambdaScope' name var = do
  state <- get
  let newStack = (name, var) : (lambdaStack state)
  let newExtraFreeVars = [] : (extraFreeVars state)
  put $ state { lambdaStack = newStack, extraFreeVars = newExtraFreeVars }


popLambdaScope :: RecursionState ()
popLambdaScope = do
  state <- get
  let lst = lambdaStack state
  let efv = extraFreeVars state
  let newStack = tail lst
  let (nextScopeName, _) = head lst
  let newExtraFreeVars = pullUpUnresolvedFreeVars nextScopeName efv
  put $ state { lambdaStack = newStack, extraFreeVars = newExtraFreeVars }


pullUpUnresolvedFreeVars :: String -> [[String]] -> [[String]]
pullUpUnresolvedFreeVars nextScopeName efvStack =
  -- pull up all new free vars that are not resolved by this scope
  let tailEfv = tail efvStack in
  let cleanedEfv = delete nextScopeName $ head efvStack in
  let nextScopeEfv = head $ tailEfv in
  let nextScopeAllEfv = union cleanedEfv nextScopeEfv in
  nextScopeAllEfv : (tail tailEfv)


