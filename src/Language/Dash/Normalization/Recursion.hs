module Language.Dash.Normalization.Recursion (
  resolveRecursion
) where

import           Control.Monad.State   hiding (state)
import           Data.List
import qualified Data.Map              as Map
import           Language.Dash.IR.Data (ConstAddr)
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
  lambdaName     :: String
, lambdaVar      :: NstVar

  -- 'extra free vars' are the additional free variables that are created when
  -- resolving recursion that involves closures or unknown functions
, extraFreeVars  :: [String]
, freeVarsForVar :: Map.Map NstVar [String]
}

emptyRecursionContext :: String -> NstVar -> RecursionContext
emptyRecursionContext lamName lamVar = RecursionContext {
  lambdaName = lamName
, lambdaVar  = lamVar
, extraFreeVars = []
, freeVarsForVar = Map.empty
}

type RecursionState a = State RecursionEnv a


resolveRecursion :: NstExpr -> NstExpr
resolveRecursion normExpr = evalState (resolveRecExprInContext normExpr) emptyRecursionEnv

resolveRecExprInContext :: NstExpr -> RecursionState NstExpr
resolveRecExprInContext expr = do
  pushContext [] ""
  expr' <- resolveRecExpr expr
  popContext
  return expr'

resolveRecExpr :: NstExpr -> RecursionState NstExpr
resolveRecExpr normExpr = case normExpr of
  NLet var atom expr -> resolveRecLet var atom expr
  NAtom atom -> do
    (recAtom, _) <- resolveRecAtom atom ""
    return $ NAtom recAtom


resolveRecLet :: NstVar -> NstAtomicExpr -> NstExpr -> RecursionState NstExpr
resolveRecLet var atom expr = do
  (resAtom, freeVars) <- resolveRecAtom atom (localVarName var)
  setFreeVarsForLocalVar var freeVars
  resExpr <- resolveRecExpr expr
  return $ NLet var resAtom resExpr


localVarName :: NstVar -> String
localVarName (NLocalVar name) = name
localVarName _ = error "Internal compiler error: Something other than a local var was let-bound"


type LambdaCtor = [String] -> [String] -> NstExpr -> NstAtomicExpr

resolveRecAtom :: NstAtomicExpr -> String -> RecursionState (NstAtomicExpr, [String])
resolveRecAtom atom name = case atom of
  -- Invariant: Recursive vars are always let-bound  (TODO loosen this restriction later)
  NLambda freeVars params expr -> resolveRecLambda NLambda freeVars params expr name
  NMatchBranch freeVars params expr -> resolveRecLambda NMatchBranch freeVars params expr ""
  NMatch maxCapt resultVar addr branches -> resolveRecMatch maxCapt resultVar addr branches
  NVar (NRecursiveVar vname) -> do
    var <- resolveRecVar vname
    return $ (NVar var, [])
  a -> return (a, [])


resolveRecMatch :: Int -> NstVar -> ConstAddr -> [([String], [String], NstVar)] -> RecursionState (NstAtomicExpr, [String])
resolveRecMatch maxCapt resultVar addr branches = do
  newBranches <- forM branches ( \ (_, capturedVars, branchVar) -> do
                                   freeVars <- getFreeVarsForLocalVar branchVar
                                   return $ (freeVars, capturedVars, branchVar))
  return $ (NMatch maxCapt resultVar addr newBranches, [])

-- We return the free vars for this lambda so that match instructions coming later can know about them
-- (they need to know about free vars so that they can call match-branches correctly)
resolveRecLambda :: LambdaCtor -> [String] -> [String] -> NstExpr -> String -> RecursionState (NstAtomicExpr, [String])
resolveRecLambda lambdaConstructor freeVars params expr name = do
  pushContext freeVars name
  resolvedBody <- resolveRecExpr expr
  context <- getContext
  let extraFree = extraFreeVars context
  popContext
  let allFreeVars = freeVars ++ extraFree
  return $ (lambdaConstructor allFreeVars params resolvedBody, allFreeVars)


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


setFreeVarsForLocalVar :: NstVar -> [String] -> RecursionState ()
setFreeVarsForLocalVar var freeVars = do
  context <- getContext
  let freeVarMap = freeVarsForVar context
  modifyContext $ \ ctx -> ctx { freeVarsForVar = Map.insert var freeVars freeVarMap }

getFreeVarsForLocalVar :: NstVar -> RecursionState [String]
getFreeVarsForLocalVar var = do
  context <- getContext
  let freeVarMap = freeVarsForVar context
  case (Map.lookup var freeVarMap) of
      Nothing -> error $ "Internal compiler error: Unknown local var: " ++ (show var)
      Just freeVars -> return freeVars

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
pullUpUnresolvedFreeVars name currentEfv nextEfv =
  -- pull up all new free vars that are not resolved by this context
  let cleanedEfv = delete name currentEfv in
  let nextContextAllEfv = union cleanedEfv nextEfv in
  nextContextAllEfv


