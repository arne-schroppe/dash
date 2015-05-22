module Language.Spot.CodeGen.Normalization where

import Language.Spot.IR.Ast
import Language.Spot.IR.Norm
import Language.Spot.IR.Tac
import Control.Monad.State

import Data.List
import qualified Data.Map as Map


{-

Normalization
~~~~~~~~~~~~~


There are 4 types of variables: Local temporary variables (these are the ones create by
normalization), function parameters (these will have a register inside a lambda), dynamic
free variables (these will turn a lambda into a closure and thus will also have their
unique register), and constant free variables (these are free variables that are known at
compile time and can thus be used immediately). Furthermore, free variables can occur in
two constructs, lambdas and compund symbols. The NLambda constructor lists all its free
variables, NCompoundSymbol has a boolean to indicate whether it is constant or dynamic.



TODO: Forget forward declaration for now
TODO: Forget mutual recursion for now


-}


type Cont = NormAtomicExpr -> State NormState NormExpr

normalize :: Expr -> (NormExpr, ConstTable, SymbolNameList)
normalize expr =
  let (result, finalState) = runState (normalizeInContext expr) emptyNormState in
  (result, constTable finalState, getSymbolNames finalState)



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

isDynamic expr = case expr of
  NLambda (h:[]) _ _ -> True
  NCompoundSymbol True _ -> True
  _ -> False


----- State -----

data NormState = NormState {
  symbolNames :: Map.Map String SymId
, constTable :: ConstTable -- rename ConstTable to DataTable
, contexts :: [Context] -- head is current context
} deriving (Eq, Show)

emptyNormState = NormState {
  symbolNames = Map.empty
, constTable = []
, contexts = []
}

data Context = Context {
  tempVarCounter :: Int
, bindings :: Map.Map String (NormVar, Bool) -- Bool indicates whether this is a dynamic var or not
, freeVars :: [String]
} deriving (Eq, Show)

emptyContext = Context {
  tempVarCounter = 0
, bindings = Map.empty
, freeVars = []
}

-- Name lookup can have X outcomes:
-- 1. It is a local temp var or fun param. In this case, just use NFunParam or NLocalVar
-- 2. It is a variable from an outer context. In this case there are two possibilities:
-- 2.1. It is a static variable (i.e. a lambda or compound symbol with no free vars or
--      another constant expression). In this case, add a new temp that has a 
--      NConstantFreeVar assigned to it. The code generator will resolve this directly.
-- 2.2. It is a dynamic variable. In this case add it as an NDynamicFreeVar. The code
--      generator will assign a register for it.
-- 3. It is an unknown variable, which results in an error.


lookupName name = do
  state <- get
  let ctxs = contexts state
  let localContext = head ctxs
  case Map.lookup name (bindings localContext)  of
    Just bnd -> return $ fst bnd
    Nothing -> do
           (var, isDynamic) <- lookupNameInContext name (tail ctxs)
           if isDynamic then do
             addDynamicVar name
             return $ NDynamicFreeVar name
           else
             return $ NConstantFreeVar name


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

