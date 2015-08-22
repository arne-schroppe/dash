module Language.Dash.Normalization.NormalizationState (
  Norm
, emptyNormState
, enterContext
, leaveContext
, addBinding
, addAlias
, hasBinding
, addDynamicVar
, lookupName
, newTempVar
, freeVariables
, addSymbolName
, getSymbolNames
, constTable
, addConstant
, arity
, addArity
) where


import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State.Strict               hiding (state)
import           Data.Function                            (on)
import           Data.List
import qualified Data.Map                                 as Map
import           Language.Dash.CodeGen.BuiltInDefinitions (builtInSymbols)
import           Language.Dash.Internal.Error             (CompilationError (..))
import           Language.Dash.IR.Data
import           Language.Dash.IR.Nst



type NormT a m = StateT NormState (ExceptT CompilationError m) a
type Norm a = NormT a Identity

data NormState = NormState
  { symbolNames    :: Map.Map String SymId
  , constTable     :: ConstTable -- TODO rename ConstTable to DataTable (or ConstPool)
  , contexts       :: [Context] -- head is current context

  -- TODO for this we *really* need unique names
  , arities        :: Map.Map String (Int, Int) -- (Num free vars, num formal params)
  , varNameCounter :: Int
  } deriving (Eq, Show)



emptyNormState :: NormState
emptyNormState = NormState
  { symbolNames = Map.fromList builtInSymbols
  , constTable = []
  , contexts = []
  , arities = Map.empty
  , varNameCounter = 0
  }


-- TODO rename to scope ? or environment? To something else at lease
data Context = Context
  { tempVarCounter :: Int
  , bindings       :: Map.Map String (NstVar, Bool) -- Bool indicates whether this is
                                                    -- a dynamic var or not
  , freeVars       :: [String]
  } deriving (Eq, Show)


emptyContext :: Context
emptyContext = Context
  { tempVarCounter = 0
  , bindings = Map.empty
  , freeVars = []
  }


newTempVar :: Norm NstVar
newTempVar = do
  name <- newName
  return $ NVar name NLocalVar
  where
    newName = do
      index <- gets varNameCounter
      modify $ \ env -> env { varNameCounter = index + 1 }
      return $ "$local" ++ show index

{-
A name lookup can have these outcomes:
1. It is a local temp var or fun param. In this case, just use LocalVar or FunParam
2. It is a variable from a surrounding context. In this case there are two possibilities:
2.1. It is a static value (i.e. a lambda or compound symbol with no free vars or
     any other constant expression, e.g. a number). In this case, add a new local var that
     has a ConstantFreeVar assigned to it. The code generator will resolve this directly.
2.2. It is a dynamic variable. In this case add it as a DynamicFreeVar. The code
     generator will assign a register for it and it will be passed into the function as an
     additional argument.
3. It is a recursive variable. Only lambdas and closures can be recursive. Return it as-is
   so that it can be resolved in the second stage of normalization, when recursion is
   resolved.
4. The name is unknown, which results in an error.
-}
lookupName :: String -> Norm NstVar
lookupName name = do
  state <- get
  let ctxs = contexts state
  let localContext = head ctxs
  case Map.lookup name (bindings localContext) of
    Just (var, _) -> return var
    Nothing -> do
      (var, isDynamic) <- lookupNameInContext name (tail ctxs)
      if isDynamic
        then do
          addDynamicVar name
          return $ NVar name NFreeVar
        else
          case var of
            NVar _ NRecursiveVar -> return var
            _ -> return $ NVar name NConstant


lookupNameInContext :: String -> [Context] -> Norm (NstVar, Bool)
lookupNameInContext name [] = throwError $ InternalCompilerError $ "Unknown variable \"" ++ name ++ "\""
lookupNameInContext name conts = do
  let binds = bindings $ head conts
  case Map.lookup name binds of
    Just bnd -> return bnd
    Nothing  -> lookupNameInContext name (tail conts)


enterContext :: [String] -> Norm ()
enterContext funParams = do
  pushContext emptyContext
  void $ forM funParams $ \ paramName ->
    -- Function params are always dynamic
    addBinding paramName (NVar paramName NFunParam, True)
  return ()


leaveContext :: Norm ()
leaveContext = popContext


context :: Norm Context
context = gets $ head.contexts


pushContext :: Context -> Norm ()
pushContext c = do
  state <- get
  put $ state { contexts = c : contexts state }


popContext :: Norm ()
popContext = do
  state <- get
  put $ state { contexts = tail.contexts $ state }


putContext :: Context -> Norm ()
putContext c = do
  state <- get
  put $ state { contexts = c : (tail.contexts $ state) }


-- TODO more like addDynamicVarUsage ??
addDynamicVar :: String -> Norm ()
addDynamicVar name = do
  con <- context
  let free = freeVars con
  when (name `notElem` free) $
          do let free' = name : free
             let con' = con { freeVars = free' }
             putContext con'


addBinding :: String -> (NstVar, Bool) -> Norm ()
addBinding "" _ = return ()
addBinding name bnd = do
  con <- context
  -- TODO handle this more gracefully
  when (name /= "_" && Map.member name (bindings con)) $
          throwError $ CodeError $ "Redefinition of '" ++ name ++ "'"
  -- TODO warn when shadowing bindings
  let bindings' = Map.insert name bnd (bindings con)
  putContext $ con { bindings = bindings' }

addAlias :: String -> String -> Norm ()
addAlias newName oldName = do
  con <- context
  -- TODO handle this more gracefully
  let maybeVar = Map.lookup oldName (bindings con)
  bindings' <- case maybeVar of
          Nothing -> throwError $ InternalCompilerError $ "Can't alias unknown variable '" ++ oldName ++ "'"
          Just v -> return $ Map.insert newName v (bindings con)
  putContext $ con { bindings = bindings' }



hasBinding :: String -> Norm Bool
hasBinding name = do
  con <- context
  return $ Map.member name (bindings con)


freeVariables :: Norm [String]
freeVariables = do
  con <- context
  return $ freeVars con


addArity :: String -> Int -> Int -> Norm ()
addArity "" _ _ = return ()
addArity funName numFreeVars ar = do
  env <- get
  let arities' = Map.insert funName (numFreeVars, ar) (arities env)
  put $ env { arities = arities' }


-- If we don't know the arity, we return Nothing here
arity :: NstVar -> Norm (Maybe (Int, Int))
arity var = do
  let vname = varName var
  env <- get
  return $ Map.lookup vname (arities env)


varName :: NstVar -> String
varName (NVar name _) = name


----- Symbols

addSymbolName :: String -> Norm SymId
addSymbolName s = do
  state <- get
  let syms = symbolNames state
  if Map.member s syms then
    return $ syms Map.! s
  else do
    let nextId = mkSymId $ Map.size syms
    let syms' = Map.insert s nextId syms
    put $ state { symbolNames = syms' }
    return nextId


getSymbolNames :: NormState -> SymbolNameList
getSymbolNames = map fst . sortBy (compare `on` snd) . Map.toList . symbolNames


--- Constants
-- TODO Split this into separate module? Together with constTable type ?

addConstant :: Constant -> Norm ConstAddr
addConstant c = do
  state <- get
  let cTable = constTable state
  let nextAddr = mkConstAddr $ length cTable
  let constTable' = cTable ++ [c] -- TODO can we cons + reverse?
  put $ state { constTable = constTable' }
  return nextAddr


