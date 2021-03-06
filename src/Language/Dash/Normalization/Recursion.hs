module Language.Dash.Normalization.Recursion (
  resolveRecursion
) where

import           Control.Monad.Except         (ExceptT (..), runExceptT,
                                               throwError)
import           Control.Monad.Identity       (Identity (..), runIdentity)
import           Control.Monad.State.Strict   hiding (state)
import           Data.List
import qualified Data.Map                     as Map
import           Language.Dash.Error.Error
import           Language.Dash.IR.Data        (ConstAddr, Name, SymId)
import           Language.Dash.IR.Nst




resolveRecursion :: NstExpr -> Either CompilationError NstExpr
resolveRecursion normExpr =
  runIdentity $ runExceptT $ evalStateT
                               (resolveRecExprInContext normExpr)
                               emptyRecursionState


resolveRecExprInContext :: NstExpr -> Recursion NstExpr
resolveRecExprInContext expr = do
  pushContext [] ""
  expr' <- resolveRecExpr expr
  popContext
  return expr'


resolveRecExpr :: NstExpr -> Recursion NstExpr
resolveRecExpr normExpr = case normExpr of
  NLet var atom expr -> resolveRecLet var atom expr
  NDestructuringBind vars pat subjVar expr -> do -- resolveRecDestrBind vars pat subjVar expr
    -- TODO do we have to save the bound vars in a context?
    resExpr <- resolveRecExpr expr
    return $ NDestructuringBind vars pat subjVar resExpr
  NAtom atom -> do
    (recAtom, _) <- resolveRecAtom atom ""
    return $ NAtom recAtom


resolveRecLet :: NstVar -> NstAtomicExpr -> NstExpr -> Recursion NstExpr
resolveRecLet var atom expr = do
  name <- localVarName var
  (resAtom, freeVars) <- resolveRecAtom atom name
  setFreeVarsForLocalVar var freeVars
  resExpr <- resolveRecExpr expr
  return $ NLet var resAtom resExpr

{-
resolveRecDestrBind :: [NstVar] -> ConstAddr -> NstVar -> NstExpr -> Recursion NstExpr
resolveRecDestrBind vars pat subjVar expr = do
  names <- mapM localVarName vars
  (resAtom, freeVars) <- foldM foldResolveRecAtom (subj, []) names
  void $ forM vars $ \ n -> setFreeVarsForLocalVar n freeVars
  resExpr <- resolveRecExpr expr
  return $ NDestructuringBind vars pat resAtom resExpr
  where
    foldResolveRecAtom :: (NstAtomicExpr, [String]) -> Name -> Recursion (NstAtomicExpr, [String])
    foldResolveRecAtom (atom, existingFreeVars) boundName = do
      (resAtom, freeVars) <- resolveRecAtom atom boundName
      -- TODO avoid nub
      return (resAtom, nub $ existingFreeVars ++ freeVars)
-}

localVarName :: NstVar -> Recursion String
localVarName (NVar name NLocalVar) = return name
localVarName _ =
  throwError $ InternalCompilerError "Something other than a local var was let-bound"


type LambdaCtor = [String] -> [String] -> NstExpr -> NstAtomicExpr


resolveRecAtom :: NstAtomicExpr -> Name -> Recursion (NstAtomicExpr, [String])
resolveRecAtom atom name = case atom of
  -- Invariant: Recursive vars are always let-bound  (TODO loosen this restriction later)
  NLambda freeVars params expr ->
      resolveRecLambda NLambda freeVars params expr name
  NMatchBranch freeVars params expr ->
      resolveRecLambda NMatchBranch freeVars params expr ""
  NMatch maxCapt resultVar addr branches ->
      resolveRecMatch maxCapt resultVar addr branches
  NVarExpr (NVar vname NRecursiveVar) -> do
      var <- resolveRecVar vname
      return (NVarExpr var, [])
  NModule fields ->
      resolveRecModule fields
  a ->
      return (a, [])


resolveRecModule :: [(SymId, Name, NstAtomicExpr)] -> Recursion (NstAtomicExpr, [String])
resolveRecModule fields = do
  let namesAndExprs = map (\(_, n, e) -> (n, e)) fields
  resolved <- mapM (\(n, e) -> resolveRecAtom e n) namesAndExprs
  let freeVars = nub $ concatMap snd resolved
  let newFields = zipWith (\(sid, n, _) (e, _) -> (sid, n, e)) fields resolved
  return (NModule newFields, freeVars)




resolveRecMatch :: Int
                -> NstVar
                -> ConstAddr
                -> [([String], [String], NstVar)]
                -> Recursion (NstAtomicExpr, [String])
resolveRecMatch maxCapt resultVar addr branches = do
  newBranches <- forM branches ( \ (_, capturedVars, branchVar) -> do
                                   freeVars <- getFreeVarsForLocalVar branchVar
                                   return (freeVars, capturedVars, branchVar))
  return (NMatch maxCapt resultVar addr newBranches, [])


-- We return the free vars for this lambda so that match instructions coming later
-- can know about them. (they need to know about free vars so that they can call
-- match-branches correctly.)
resolveRecLambda :: LambdaCtor
                 -> [String]
                 -> [String]
                 -> NstExpr
                 -> String
                 -> Recursion (NstAtomicExpr, [String])
resolveRecLambda lambdaConstructor freeVars params expr name = do
  pushContext freeVars name
  resolvedBody <- resolveRecExpr expr
  context <- getContext
  let extraFree = extraFreeVars context
  popContext
  let allFreeVars = freeVars ++ extraFree
  return (lambdaConstructor allFreeVars params resolvedBody, allFreeVars)


resolveRecVar :: String -> Recursion NstVar
resolveRecVar name = do
  state <- get
  let maybeVar = findName name (contexts state)
  case maybeVar of
    Nothing ->
        throwError $ InternalCompilerError $ "Can't resolve recursive use of " ++ name
    Just v@(NVar _ NConstant) ->
        return v
    Just v@(NVar vname NFreeVar) -> do
        addExtraFreeVar vname
        return v
    _ ->
        throwError $ InternalCompilerError "Unexpected variable type"


findName :: String -> [RecursionContext] -> Maybe NstVar
findName _ []             = Nothing
findName name (context:cs) =
  if lambdaName context == name
    then Just $ lambdaVar context
    else findName name cs


addExtraFreeVar :: String -> Recursion ()
addExtraFreeVar name = do
  context <- getContext
  let extra = extraFreeVars context
  unless (name `elem` extra) $ do
          let free' = name : extra
          modifyContext $ \ ctx -> ctx { extraFreeVars = free'  }


setFreeVarsForLocalVar :: NstVar -> [String] -> Recursion ()
setFreeVarsForLocalVar var freeVars = do
  context <- getContext
  let freeVarMap = freeVarsForVar context
  modifyContext $ \ ctx -> ctx { freeVarsForVar = Map.insert var freeVars freeVarMap }


getFreeVarsForLocalVar :: NstVar -> Recursion [String]
getFreeVarsForLocalVar var = do
  context <- getContext
  let freeVarMap = freeVarsForVar context
  case Map.lookup var freeVarMap of
      Nothing -> throwError $ InternalCompilerError $ "Unknown local var: " ++ show var
      Just freeVars -> return freeVars


getContext :: Recursion RecursionContext
getContext = do
  state <- get
  return $ head $ contexts state


modifyContext :: (RecursionContext -> RecursionContext) -> Recursion ()
modifyContext f = do
  ctx <- getContext
  let ctx' = f ctx
  modify $ \ state -> state { contexts = ctx' : tail (contexts state) }


-- TODO this doesn't work for mutual recursion

-- The first case (for a lambda that isn't let-bound and thus is unnamed) is just a
-- placeholder, so that we can easily pop the context later. No recursive var will resolve
-- to it anyway.
pushContext :: [String] -> String -> Recursion ()
pushContext _ "" = pushContext' "$$$invalid$$$" (NVar "$$$invalid$$$" NConstant)
pushContext [] n = pushContext' n (NVar n NConstant)
pushContext _  n = pushContext' n (NVar n NFreeVar)


pushContext' :: String -> NstVar -> Recursion ()
pushContext' name var = do
  let newContext = emptyRecursionContext name var
  modify $ \ state -> state { contexts = newContext : contexts state }


popContext :: Recursion ()
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


type RecursionT a m = StateT RecursionState (ExceptT CompilationError m) a
type Recursion a = RecursionT a Identity


data RecursionState = RecursionState
  { contexts :: [RecursionContext]
  }


emptyRecursionState :: RecursionState
emptyRecursionState = RecursionState
  { contexts = []
  }


data RecursionContext = RecursionContext
  { lambdaName     :: String
  , lambdaVar      :: NstVar

  -- 'extra free vars' are the additional free variables that are created when
  -- resolving recursion that involves closures or unknown functions
  , extraFreeVars  :: [String]
  , freeVarsForVar :: Map.Map NstVar [String]
  }


emptyRecursionContext :: String -> NstVar -> RecursionContext
emptyRecursionContext lamName lamVar = RecursionContext
  { lambdaName = lamName
  , lambdaVar  = lamVar
  , extraFreeVars = []
  , freeVarsForVar = Map.empty
  }

