module Language.Spot.CodeGen.Normalization where

import           Control.Monad.State
import           Data.List
import qualified Data.Map              as Map
import           Language.Spot.IR.Ast
import           Language.Spot.IR.Norm
import           Language.Spot.IR.Tac


{-

# Normalization

This module normalizes the abstract syntax tree (Ast) generated by the parser into
a normalized form (Norm) that is easier for the code generator to compile. Mainly,
the normalized form assigns all intermediate results to a name (let-binding). For
example, the code

set-value (at list 2) (3 * 6)

would be normalized to something like

let temp1 = 2
let temp2 = at [list, temp1]
let temp3 = 3 * 6
set-value [temp2, temp3]

## Types of variables
The actual form doesn't generate string identifiers like "temp1", though. Instead
it uses the type NormVar. In this type, a LocalVar is one of the temporary generated
during normalization or an explicit local let-binding. A FunParam is a formal parameter
in a lambda.

The more interesting one's are DynamicFreeVar and ConstantFreeVar. A ConstantFreeVar is a
free variable in a lambda that is a constant known value in the surrounding scope. Lambdas
that are not closures (i.e. don't have any free variables) count as constant values too.
A ConstantFreeVar is resolved directly (by loading the function address or the constant)
and doesn't need any further support.

A DynamicFreeVar is a free variable that is not a compile time constant. These are later
added to a lambda as additional parameters. This module traces dynamic free variables and
mentions them explicitly for every `Norm.Lambda`.


## Recursion

This module works in two passes. The first pass is the normalization described earlier,
the second pass resolves recursion


-}


type Cont = NormAtomicExpr -> State NormState NormExpr

normalize :: Expr -> (NormExpr, ConstTable, SymbolNameList)
normalize expr =
  let (result, finalState) = runState (normalizeInContext expr) emptyNormState in
  let result' = resolveRecursion result in
  (result', constTable finalState, getSymbolNames finalState)

normalizeInContext :: Expr -> State NormState NormExpr
normalizeInContext expr = do
  enterContext []
  nExpr <- normalizeExpr expr
  leaveContext
  return nExpr


normalizeExpr :: Expr -> State NormState NormExpr
normalizeExpr expr = case expr of
  LocalBinding (Binding name boundExpr) restExpr ->
    nameExpr boundExpr name $ \ var -> do
      -- TODO use isDynamic here
      addBinding name (var, False)
      rest <- normalizeExpr restExpr
      return $ rest
  _ -> do
    atomizeExpr expr "" $ return . NAtom


atomizeExpr :: Expr -> String -> Cont -> State NormState NormExpr
atomizeExpr expr name k = case expr of
  FunCall funExpr args ->
          normalizeFunCall funExpr args k
  LitNumber n ->
          normalizeNumber n k
  LitSymbol sid args ->
          normalizeSymbol sid args k
  Var name ->
          normalizeVar name k
  Match matchedExpr patterns ->
          normalizeMatch matchedExpr patterns k
  Lambda params bodyExpr ->
          normalizeLambda params bodyExpr name k
  LocalBinding (Binding name boundExpr) restExpr -> -- inner local binding ! (i.e. let a = let b = 2 in 1 + b)
          atomizeExpr boundExpr name $ \ aExpr -> do
            var <- newTempVar name
            addBinding name (var, False)
            atomizeExpr restExpr "" $ \ boundExpr -> do
              rest <- k boundExpr
              return $ NLet var aExpr rest
  x -> error $ "Unable to normalize " ++ (show x)


normalizeNumber n k = k (NNumber n)

normalizeSymbol sid [] k = do
  symId <- addSymbolName sid
  k (NPlainSymbol symId)

normalizeSymbol sid args k = do
  encConst <- encodeConstant $ LitSymbol sid args
  cAddr <- addConstant encConst
  k (NCompoundSymbol False cAddr)

-- There are three cases:
--   - static symbols, which are completely in the sym table
--   - dynamic symbols, which have some dynamic elements. A template for these can be
--     generated in the sym table and then be copied and modified
--   - unknown dynamism. This happens when symbols include free vars. We need to
--     resolve at a later point whether the closed over var is static or dynamic
--     (or do we?)
{-
  normalizeExprList args $ \ normArgs ->
          k $ NFunCall $ funVar : normArgs
-}


-- This is only direct usage of a var (as a "return value")
normalizeVar name k = do
  var <- lookupName name
  k $ NVar var


normalizeLambda params bodyExpr name k = do
  enterContext params
  when (not $ null name) $ addBinding name (NRecursiveVar name, False) -- TODO we don't know whether this var is dynamic or not!
  normalizedBody <- normalizeExpr bodyExpr
  con <- context
  let free = freeVars con
  leaveContext
  pullUpFreeVars free
  k $ NLambda free params normalizedBody


normalizeFunCall (Var "add") [a, b] k =
  normalizeMathPrimOp NPrimOpAdd a b k

normalizeFunCall (Var "sub") [a, b] k =
  normalizeMathPrimOp NPrimOpSub a b k

normalizeFunCall funExpr args k = do
  nameExpr funExpr "" $ \ funVar ->
          normalizeExprList args $ \ normArgs ->
                  k $ NFunCall funVar normArgs

normalizeMathPrimOp mathPrimOp a b k = do
  normalizeExprList [a, b] $ \ [aVar, bVar] ->
      k $ NPrimOp $ mathPrimOp aVar bVar


normalizeMatch :: Expr -> [(Pattern, Expr)] -> Cont -> State NormState NormExpr
normalizeMatch matchedExpr patternsAndExpressions k = do
  matchedVarsAndEncodedPatterns <- forM (map fst patternsAndExpressions) $ encodeMatchPattern 0
  let matchedVars = map fst matchedVarsAndEncodedPatterns
  patternAddr <- addConstant $ CMatchData $ map snd matchedVarsAndEncodedPatterns
  let exprs = map snd patternsAndExpressions
  -- we wrap each match branch in a lambda. This way we can handle them easier in the codegenerator
  let maxMatchVars = maximum $ map length matchedVars
  let lambdaizedExprs = map (\ (params, expr) -> Lambda params expr) $ zip matchedVars exprs
  nameExpr matchedExpr "" $ \ subjVar ->
          normalizeExprList lambdaizedExprs $ \ branchVars -> do
                  let branches = zip matchedVars branchVars
                  k $ NMatch maxMatchVars subjVar patternAddr branches

-- Free variables in a used lambda which can't be resolved in our context need to become
-- free variables in our context
pullUpFreeVars freeVs =
  forM (reverse freeVs) $ \ name -> do
          hasB <- hasBinding name
          when (not hasB) $ addDynamicVar name



----- Normalization helper functions -----

-- TODO shouldn't this be called something like nameExprList ?
normalizeExprList exprList k =
  normalizeExprList' exprList [] k
  where
    normalizeExprList' [] acc k = do
      expr <- k $ reverse acc
      return expr
    normalizeExprList' exprList acc k = do
      let hd = head exprList
      nameExpr hd "" $ \ var -> do
        restExpr <- normalizeExprList' (tail exprList) (var : acc) k
        return restExpr


-- TODO rename this function to something more appropriate
nameExpr expr originalName k = case expr of
  -- Some variable can be used directly and don't need to be let-bound
  Var name -> do
    var <- lookupName name
    case var of
      -- Constant free vars are let-bound
      NConstantFreeVar n -> letBind expr k ""
      -- Recursive vars are also let-bound. Not strictly necessary, but easier later on  (TODO loosen this restriction)
      NRecursiveVar n -> letBind expr k ""
      -- All other vars are used directly (because they will be in a register later on)
      v -> do
            bodyExpr <- k v
            return bodyExpr
  -- Everything that is not a Var needs to be let-bound
  _ -> letBind expr k originalName
  where
    letBind e k n = do
      atomizeExpr e n $ \ aExpr -> do
        var <- newTempVar n
        restExpr <- k var
        return $ NLet var aExpr restExpr

{-
isAtomDynamic expr = case expr of
  NLambda (h:_) _ _ -> True
  NCompoundSymbol True _ -> True
  _ -> False
-}


----- State -----

data NormState = NormState {
  symbolNames :: Map.Map String SymId
, constTable  :: ConstTable -- rename ConstTable to DataTable
, contexts    :: [Context] -- head is current context
} deriving (Eq, Show)

emptyNormState = NormState {
  symbolNames = Map.empty
, constTable = []
, contexts = []
}

data Context = Context {
  tempVarCounter :: Int
, bindings       :: Map.Map String (NormVar, Bool) -- Bool indicates whether this is a dynamic var or not
, freeVars       :: [String]
} deriving (Eq, Show)

emptyContext = Context {
  tempVarCounter = 0
, bindings = Map.empty
, freeVars = []
}

{-
A name lookup can have these outcomes:
1. It is a local temp var or fun param. In this case, just use NLocalVar or NFunParam
2. It is a variable from a surrounding context. In this case there are two possibilities:
2.1. It is a static variable (i.e. a lambda or compound symbol with no free vars or
     another constant expression, e.g. a number). In this case, add a new temp that has a
     NConstantFreeVar assigned to it. The code generator will resolve this directly.
2.2. It is a dynamic variable. In this case add it as an NDynamicFreeVar. The code
     generator will assign a register for it.
3. It is a recursive variable. Only lambdas and closures can be recursive. Return it as-is
   so that it can be resolved later.
4. It is an unknown variable, which results in an error.
-}
lookupName name = do
  state <- get
  let ctxs = contexts state
  let localContext = head ctxs
  case Map.lookup name (bindings localContext)  of
    Just (var, _) -> return var
    Nothing -> do
           (var, isDynamic) <- lookupNameInContext name (tail ctxs)
           if isDynamic then do
             addDynamicVar name
             return $ NDynamicFreeVar name
           else case var of
             NRecursiveVar name -> return var
             _ -> return $ NConstantFreeVar name


lookupNameInContext name [] = error $ "Identifier " ++ name ++ " not found"
lookupNameInContext name conts = do
  let binds = bindings $ head conts
  case Map.lookup name binds of
    Just bnd -> return bnd
    Nothing  -> lookupNameInContext name (tail conts)

enterContext funParams = do
  pushContext emptyContext
  forM funParams $ \ paramName ->
    -- Function params are always dynamic
    addBinding paramName (NFunParam paramName, True)

leaveContext = do
  popContext

newTempVar :: String -> State NormState NormVar
newTempVar name = do
  con <- context
  let tmpVar = tempVarCounter con
  let nextTmpVar = tmpVar + 1
  putContext $ con { tempVarCounter = nextTmpVar }
  return (NLocalVar tmpVar name)


context = gets $ head.contexts

pushContext c = do
  state <- get
  put $ state { contexts = c : (contexts state) }

popContext = do
  state <- get
  put $ state { contexts = tail.contexts $ state }

putContext c = do
  state <- get
  put $ state { contexts = c : (tail.contexts $ state) }

-- TODO more like addDynamicVarUsage ??
addDynamicVar name = do
  con <- context
  let free = freeVars con
  if not (name `elem` free) then do
          let free' = name : free
          let con' = con { freeVars = free' }
          putContext con'
  else return ()

addBinding :: String -> (NormVar, Bool) -> State NormState ()
addBinding name bnd = do
  con <- context
  let bindings' = Map.insert name bnd (bindings con)
  putContext $ con { bindings = bindings' }

hasBinding name = do
  con <- context
  return $ Map.member name (bindings con)


--- Symbols

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


--- Constants
-- TODO Split this into separate module? Together with constTable type ?

addConstant :: Constant -> State NormState ConstAddr
addConstant c = do
  state <- get
  let cTable = constTable state
  let nextAddr = length cTable
  let constTable' = cTable ++ [c] -- TODO can we cons + reverse?
  put $ state { constTable = constTable' }
  return $ fromIntegral nextAddr


encodeConstant :: Expr -> State NormState Constant
encodeConstant v =
  case v of
    LitNumber n -> return $ CNumber n
    LitSymbol s [] -> do
                sid <- addSymbolName s
                return $ CPlainSymbol sid
    LitSymbol s args -> do
                symId <- addSymbolName s
                encodedArgs <- mapM encodeConstant args
                return $ CCompoundSymbol symId encodedArgs
    _ -> error $ "Can only encode constant symbols for now"


encodeMatchPattern nextMatchVar pat =
  case pat of
    PatNumber n -> return ([], (CNumber n))
    PatSymbol s [] -> do sid <- addSymbolName s
                         return $ ([], CPlainSymbol sid)
    PatSymbol s params -> do
                  symId <- addSymbolName s
                  (vars, pats) <- encodePatternCompoundSymbolArgs nextMatchVar params
                  return (vars, CCompoundSymbol symId pats)
    PatVar n -> return $ ([n], CMatchVar nextMatchVar)

-- TODO use inner state
encodePatternCompoundSymbolArgs nextMatchVar args = do
  (_, vars, entries) <- foldM (\(nextMV, accVars, pats) p -> do
    (vars, encoded) <- encodeMatchPattern nextMV p
    return (nextMV + (fromIntegral $ length vars), accVars ++ vars, pats ++ [encoded])
    ) (nextMatchVar, [], []) args  -- TODO get that O(n*m) out and make it more clear what this does
  return (vars, entries)




----- Recursion

-- TODO split this off into a separate module
-- TODO also move normalization state out into separate module


-- state

-- TODO pull up new extra vars (if not resolved in that scope)
-- avoid duplicate free vars by adding all current freevars when entering scope

data RecursionState = RecursionState {
    lambdaStack   :: [(String, NormVar)]
  , extraFreeVars :: [[String]]
}

emptyRecursionState = RecursionState {
    lambdaStack = []
  , extraFreeVars = []
}


resolveRecursion :: NormExpr -> NormExpr
resolveRecursion normExpr = evalState (resolveRecExpr normExpr) emptyRecursionState


resolveRecExpr normExpr = case normExpr of
  NLet var atom expr -> resolveRecLet var atom expr
  NAtom atom -> do
    recAtom <- resolveRecAtom atom ""
    return $ NAtom recAtom


resolveRecLet var atom expr = do
  resAtom <- resolveRecAtom atom (localVarName var)
  resExpr <- resolveRecExpr expr
  return $ NLet var resAtom resExpr

localVarName (NLocalVar _ name) = name
localVarName _ = error "Internal compiler error: Something else than a local var was let-bound"


resolveRecAtom atom name = case atom of
  -- Invariant: Recursive vars are always let-bound  (TODO loosen this restriction later)
  NLambda freeVars params expr -> resolveRecLambda freeVars params expr name
  NVar (NRecursiveVar name) -> do
    var <- resolveRecVar name
    return $ NVar var
  a -> return a

resolveRecLambda freeVars params expr name = do
  pushLambdaScope freeVars name
  resolvedBody <- resolveRecExpr expr
  extraFreeVars <- gets extraFreeVars
  let extra = head extraFreeVars
  popLambdaScope
  return $ NLambda (freeVars ++ extra) params resolvedBody

resolveRecVar name = do
  state <- get
  let maybeVar = findName name (lambdaStack state)
  case maybeVar of
    Nothing -> error $ "Internal compiler error: Can't resolve recursive use of " ++ name
    Just v@(NConstantFreeVar name) -> return v
    Just v@(NDynamicFreeVar name) -> do
      addExtraFreeVar name
      return v

findName name []        = Nothing
findName name ((n, v):ns) =
  if n == name then Just v
               else findName name ns


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
pushLambdaScope _ "" = pushLambdaScope' "$$$invalid$$$" (NConstantFreeVar "$$$invalid$$$")
pushLambdaScope [] n = pushLambdaScope' n (NConstantFreeVar n)
pushLambdaScope fs n = pushLambdaScope' n (NDynamicFreeVar n)

pushLambdaScope' name var = do
  state <- get
  let newStack = (name, var) : (lambdaStack state)
  let newExtraFreeVars = [] : (extraFreeVars state)
  put $ state { lambdaStack = newStack, extraFreeVars = newExtraFreeVars }

popLambdaScope = do
  state <- get
  let lst = lambdaStack state
  let efv = extraFreeVars state
  let newStack = tail lst
  let (nextScopeName, _) = head lst
  let newExtraFreeVars = pullUpUnresolvedFreeVars nextScopeName efv
  put $ state { lambdaStack = newStack, extraFreeVars = newExtraFreeVars }

pullUpUnresolvedFreeVars nextScopeName efvStack =
  -- pull up all new free vars that are not resolved by this scope
  let tailEfv = tail efvStack in
  let cleanedEfv = delete nextScopeName $ head efvStack in
  let nextScopeEfv = head $ tailEfv in
  let nextScopeAllEfv = union cleanedEfv nextScopeEfv in
  nextScopeAllEfv : (tail tailEfv)


