module Language.Dash.Normalization.Recursion (
  resolveRecursion
) where

import           Control.Monad.State  hiding (state)
import           Data.List
import           Language.Dash.IR.Nst

-- State


data RecursionEnv = RecursionEnv {
  contexts :: [RecursionContext]
}


emptyRecursionEnv :: RecursionEnv
emptyRecursionEnv = RecursionEnv {
  contexts = []
}


data RecursionContext = RecursionContext {
  lambdaName   :: String
, lambdaVar    :: NstVar

  -- 'extra free vars' are the additional free variables that are created when
  -- resolving recursion that involves closures or unknown functions
, extraFreeVars :: [String]
}

emptyRecursionContext :: String -> NstVar -> RecursionContext
emptyRecursionContext lamName lamVar = RecursionContext {
  lambdaName = lamName
, lambdaVar  = lamVar
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
localVarName _ = error "Internal compiler error: Something other than a local var was let-bound"


type LambdaCtor = [String] -> [String] -> NstExpr -> NstAtomicExpr

resolveRecAtom :: NstAtomicExpr -> String -> RecursionState NstAtomicExpr
resolveRecAtom atom name = case atom of
  -- Invariant: Recursive vars are always let-bound  (TODO loosen this restriction later)
  NLambda freeVars params expr -> resolveRecLambda NLambda freeVars params expr name
  NMatchBranch freeVars params expr -> resolveRecLambda NMatchBranch freeVars params expr ""
  NVar (NRecursiveVar vname) -> do
    var <- resolveRecVar vname
    return $ NVar var
  a -> return a


resolveRecLambda :: LambdaCtor -> [String] -> [String] -> NstExpr -> String -> RecursionState NstAtomicExpr
resolveRecLambda lambdaConstructor freeVars params expr name = do
  pushContext freeVars name
  resolvedBody <- resolveRecExpr expr
  context <- getContext
  let extraFree = extraFreeVars context
  popContext
  return $ lambdaConstructor (freeVars ++ extraFree) params resolvedBody


resolveRecVar :: String -> RecursionState NstVar
resolveRecVar name = do
  state <- get
  let maybeVar = findName name (contexts state)
  case maybeVar of
    Nothing -> error $ "Internal compiler error: Can't resolve recursive use of " ++ name
    Just v@(NConstantFreeVar _)    -> return v
    Just v@(NDynamicFreeVar vname) -> do
      addExtraFreeVar vname
      return v
    _ -> error "resolveRecVar"


findName :: String -> [RecursionContext] -> Maybe NstVar
findName _ []             = Nothing
findName name (context:cs) =
  if (lambdaName context) == name then Just $ lambdaVar context
  else findName name cs


addExtraFreeVar :: String -> RecursionState ()
addExtraFreeVar name = do
  context <- getContext
  let extra = extraFreeVars context
  when (not $ name `elem` extra) $ do
          let free' = name : extra
          modifyContext $ \ ctx -> ctx { extraFreeVars = free'  }

getContext :: RecursionState RecursionContext
getContext = do
  state <- get
  return $ head $ contexts state

modifyContext :: (RecursionContext -> RecursionContext) -> RecursionState ()
modifyContext f = do
  ctx <- getContext
  let ctx' = f ctx
  modify $ \ state -> state { contexts = ctx' : (tail $ contexts state) }

-- TODO this doesn't work for mutual recursion

-- The first case (for a lambda that isn't let-bound and thus is unnamed) is just a
-- placeholder, so that we can easily pop the context later. No recursive var will resolve
-- to it anyway.
pushContext :: [String] -> String -> RecursionState ()
pushContext _ "" = pushContext' "$$$invalid$$$" (NConstantFreeVar "$$$invalid$$$")
pushContext [] n = pushContext' n (NConstantFreeVar n)
pushContext _  n = pushContext' n (NDynamicFreeVar n)


pushContext' :: String -> NstVar -> RecursionState ()
pushContext' name var = do
  let newContext = emptyRecursionContext name var
  modify $ \ state -> state { contexts = newContext : (contexts state) }


popContext :: RecursionState ()
popContext = do
  state <- get
  let ctxs = contexts state
  let currentContext = head ctxs
  let nextContext = head $ tail ctxs

  let name = lambdaName currentContext
  let efv = extraFreeVars currentContext
  let nextEfv = extraFreeVars nextContext

  let nextEfv' = pullUpUnresolvedFreeVars name efv nextEfv
  let ctxs' = tail ctxs
  put $ state { contexts = ctxs' }
  modifyContext $ \ context -> context { extraFreeVars = nextEfv' }


pullUpUnresolvedFreeVars :: String -> [String] -> [String] -> [String]
pullUpUnresolvedFreeVars nextContextName currentEfv nextEfv =
  -- pull up all new free vars that are not resolved by this context
  let cleanedEfv = delete nextContextName $ currentEfv in
  let nextContextAllEfv = union cleanedEfv nextEfv in
  nextContextAllEfv


