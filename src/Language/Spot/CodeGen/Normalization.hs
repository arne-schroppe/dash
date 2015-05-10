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
  let (result, finalState) = runState (normalizeLambda [] expr return) emptyNormState in
  (NAtom $ result, [], getSymbolNames finalState)


normalizeExpr :: Expr -> State NormState NormExpr
normalizeExpr expr = case expr of
  LocalBinding (Binding name boundExpr) restExpr ->
    nameExpr boundExpr $ \ var -> do
      -- TODO use isDynamic here
      addBinding name (var, False)
      rest <- normalizeExpr restExpr
      return $ rest
  _ -> do
    atomizeExpr expr $ return . NAtom


atomizeExpr :: Expr -> Cont -> State NormState NormExpr
atomizeExpr expr k = case expr of
  FunCall funExpr args -> normalizeFunCall funExpr args k
  LitNumber n -> normalizeNumber n k
  LitSymbol sid args -> normalizeSymbol sid args k
  Var name -> normalizeVar name k
  Lambda params bodyExpr -> normalizeLambda params bodyExpr k
  Match matchedExpr patterns -> normalizeMatch matchedExpr patterns k
  LocalBinding (Binding name boundExpr) restExpr -> -- inner local binding ! (i.e. let a = let b = 2 in 1 + b)
    atomizeExpr boundExpr $ \ aExpr -> do
      var <- newTempVar
      addBinding name (var, False)
      atomizeExpr restExpr $ \ boundExpr -> do
        rest <- k boundExpr
        return $ NLet var aExpr rest
  x -> error $ "Unable to normalize " ++ (show x)


normalizeNumber n k = k (NNumber n)

normalizeSymbol sid [] k = do
  symId <- addSymbolName sid
  k (NPlainSymbol symId)

normalizeSymbol sid args k = error "Can't normalize complex symbols yet"
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


normalizeLambda params bodyExpr k = do
  enterContext
  normalizedBody <- normalizeExpr bodyExpr
  leaveContext
  let freeVars = []
  k $ NLambda freeVars params normalizedBody


normalizeFunCall (Var "add") [a, b] k =
  normalizeMathPrimOp NPrimOpAdd a b k

normalizeFunCall (Var "sub") [a, b] k =
  normalizeMathPrimOp NPrimOpSub a b k

normalizeFunCall funExpr args k = do
  nameExpr funExpr $ \ funVar ->
          normalizeExprList args $ \ normArgs ->
                  k $ NFunCall funVar normArgs

normalizeMathPrimOp mathPrimOp a b k = do
  normalizeExprList [a, b] $ \ [aVar, bVar] ->
      k $ NPrimOp $ mathPrimOp aVar bVar


normalizeMatch matchedExpr patterns k = do
  normalizedPatterns <- forM patterns $
      \(pattern, expr) -> do
          normExpr <- normalizeExpr expr
          return (pattern, normExpr)
  k $ NNumber 0




----- Normalization helper functions -----

normalizeExprList exprList k =
  normalizeExprList' exprList [] k
  where
    normalizeExprList' [] acc k = do
      expr <- k $ reverse acc
      return expr
    normalizeExprList' exprList acc k = do
      let hd = head exprList
      nameExpr hd $ \ var -> do
        restExpr <- normalizeExprList' (tail exprList) (var : acc) k
        return restExpr


nameExpr expr k = case expr of
  -- Some variable can be used directly and don't need to be let-bound
  Var name -> do
    var <- lookupName name
    case var of
      -- Constant free vars are let-bound
      NConstantFreeVar n -> letBind expr k
      -- All other vars are used directly (because they will be in a register later on)
      v -> do
            bodyExpr <- k v
            return bodyExpr
  -- Everything that is not a Var needs to be let-bound
  _ -> letBind expr k
  where
    letBind e k = do
      atomizeExpr e $ \ aExpr -> do
        var <- newTempVar
        restExpr <- k var
        return $ NLet var aExpr restExpr

isDynamic expr = case expr of
  NLambda (h:[]) _ _ -> True
  -- NCompoundSymbol True _ _ -> True
  _ -> False


----- State -----

data NormState = NormState {
  symbolNames :: Map.Map String SymId
, contexts :: [Context] -- head is current context
}

emptyNormState = NormState {
  symbolNames = Map.empty
, contexts = []
}

data Context = Context {
  tempVarCounter :: Int
, bindings :: Map.Map String (NormVar, Bool) -- Bool indicates if this is a dynamic var
} deriving (Eq, Show)

emptyContext = Context {
  tempVarCounter = 0
, bindings = Map.empty
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
           if isDynamic then
             return $ NDynamicFreeVar name
           else
             return $ NConstantFreeVar name


lookupNameInContext name [] = error $ "Identifier " ++ name ++ " not found"
lookupNameInContext name conts = do
  let binds = bindings $ head conts
  case Map.lookup name binds of
    Just bnd -> return bnd
    Nothing  -> lookupNameInContext name (tail conts)

enterContext = do
  pushContext emptyContext

leaveContext = do
  popContext

newTempVar :: State NormState NormVar
newTempVar = do
  con <- context
  let tmpVar = tempVarCounter con
  let nextTmpVar = tmpVar + 1
  putContext $ con { tempVarCounter = nextTmpVar }
  return (NLocalVar tmpVar)


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


addBinding :: String -> (NormVar, Bool) -> State NormState ()
addBinding name bnd = do
  con <- context
  let bindings' = Map.insert name bnd (bindings con)
  putContext $ con { bindings = bindings' }


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


