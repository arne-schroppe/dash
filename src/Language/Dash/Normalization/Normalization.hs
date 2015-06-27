module Language.Dash.Normalization.Normalization (
  normalize
) where

import           Control.Monad.State hiding (state)
import           Language.Dash.IR.Ast
import           Language.Dash.IR.Data
import           Language.Dash.IR.Nst
import           Language.Dash.Normalization.NormalizationState
import           Language.Dash.Normalization.Recursion


{-

Normalization
~~~~~~~~~~~~~

This module normalizes the abstract syntax tree (Ast) generated by the parser into
a normalized form (Nst) that is easier for the code generator to compile. Mainly,
the normalized form assigns all intermediate results to a name (let-binding). For
example, the code

    read-position csv (row-length * 5 + 7)

would be normalized to something like

    let temp1 = 5
    let temp2 = (*) [row-length, temp1]
    let temp3 = 7
    let temp4 = (+) [temp2, temp3]
    read-position [csv, temp4]


Types of variables
~~~~~~~~~~~~~~~~~~

The actual form doesn't generate string identifiers like "temp1", though. Instead
it uses the type NstVar. In this type, a LocalVar is one of the temporary generated
during normalization or an explicit local let-binding. A FunParam is a formal parameter
in a lambda.

The more interesting one's are DynamicFreeVar and ConstantFreeVar. A ConstantFreeVar is a
free variable in a lambda that is a constant known value in the surrounding scope. Lambdas
that are not closures (i.e. don't have any free variables) count as constant values too.
A ConstantFreeVar is resolved directly (by loading the function address or the constant)
and doesn't need any further support.

A DynamicFreeVar is a free variable that is not a compile time constant. These are later
added to a lambda as additional parameters. This module traces dynamic free variables and
adds them explicitly to every `Nst.Lambda`.


Recursion
~~~~~~~~~

This module works in two passes. The first pass is the normalization described earlier,
the second pass resolves recursion. The resulting normalized code will not contain any
`Nst.RecursiveVar`


-}


type Cont = NstAtomicExpr -> NormState NstExpr
type VCont = NstVar -> NormState NstExpr

normalize :: Expr -> (NstExpr, ConstTable, SymbolNameList)
normalize expr =
  let (result, finalState) = runState (normalizeInContext expr) emptyNormEnv in
  let result' = resolveRecursion result in
  (result', constTable finalState, getSymbolNames finalState)

normalizeInContext :: Expr -> NormState NstExpr
normalizeInContext expr = do
  enterContext []
  nExpr <- normalizeExpr expr
  leaveContext
  return nExpr


normalizeExpr :: Expr -> NormState NstExpr
normalizeExpr expr = case expr of
  LocalBinding (Binding name boundExpr) restExpr ->
    nameExpr boundExpr name $ \ _ -> do
      rest <- normalizeExpr restExpr
      return $ rest
  _ -> do
    atomizeExpr expr "" $ return . NAtom


atomizeExpr :: Expr -> String -> Cont -> NormState NstExpr
atomizeExpr expr name k = case expr of
  FunAp funExpr args ->
          normalizeFunAp funExpr args k
  LitNumber n ->
          normalizeNumber n k
  LitSymbol sname args ->
          normalizeSymbol sname args k
  Var vname ->
          normalizeVar vname k
  Match matchedExpr patterns ->
          normalizeMatch matchedExpr patterns k
  Lambda params bodyExpr ->
          normalizeLambda params bodyExpr name k
  LocalBinding (Binding bname boundExpr) restExpr ->
          -- This case is only for inner local bindings, i.e. let a = let b = 2 in 1 + b
          -- (So in that example "let b = ..." is the inner local binding
          atomizeExpr boundExpr bname $ \ aExpr -> do
            var <- newTempVar bname
            addBinding bname (var, False)
            atomizeExpr restExpr "" $ \ normBoundExpr -> do
              rest <- k normBoundExpr
              return $ NLet var aExpr rest
  x -> error $ "Unable to normalize " ++ (show x)


normalizeNumber :: Int -> Cont -> NormState NstExpr
normalizeNumber n k = k (NNumber n)


normalizeSymbol :: String -> [Expr] -> Cont -> NormState NstExpr
normalizeSymbol sname [] k = do
  symId <- addSymbolName sname
  k (NPlainSymbol symId)

normalizeSymbol sname args k = do
  encConst <- encodeConstant $ LitSymbol sname args
  cAddr <- addConstant encConst
  k (NCompoundSymbol False cAddr)


-- This is only direct usage of a var (as a "return value")
normalizeVar :: String -> Cont -> NormState NstExpr
normalizeVar name k = do
  var <- lookupName name
  k $ NVar var


normalizeLambda :: [String] -> Expr -> String -> Cont -> NormState NstExpr
normalizeLambda params bodyExpr name k = do
  enterContext params
  addBinding name (NRecursiveVar name, False) -- TODO we don't know whether this var is dynamic or not!
  -- TODO add arity for recursive var?
  normalizedBody <- normalizeExpr bodyExpr
  freeVars <- freeVariables
  leaveContext
  pullUpFreeVars freeVars
  addArity name (length freeVars) (length params)
  k $ NLambda freeVars params normalizedBody


-- TODO throw an error if the thing being called is obviously not callable
-- TODO it gets a bit confusing in which cases we expect a closure and where we expect a simple function
normalizeFunAp :: Expr -> [Expr] -> Cont -> NormState NstExpr
normalizeFunAp funExpr args k = case (funExpr, args) of
  (Var "+", [a, b]) -> normalizeBinaryPrimOp NPrimOpAdd a b
  (Var "-", [a, b]) -> normalizeBinaryPrimOp NPrimOpSub a b
  (Var "*", [a, b]) -> normalizeBinaryPrimOp NPrimOpMul a b
  (Var "/", [a, b]) -> normalizeBinaryPrimOp NPrimOpDiv a b
  (Var "==", [a, b]) -> normalizeBinaryPrimOp NPrimOpEq a b
  _ -> do nameExpr funExpr "" $ \ funVar -> do
            maybeAr <- arity funVar
            case maybeAr of
              Nothing -> applyUnknownFunction funVar
              Just (numFree, ar) -> applyKnownFunction funVar numFree ar
  where
    normalizeBinaryPrimOp :: (NstVar -> NstVar -> NstPrimOp) -> Expr -> Expr -> NormState NstExpr
    normalizeBinaryPrimOp primOp a b = do
      normalizeExprList [a, b] $ \ [aVar, bVar] ->
          k $ NPrimOp $ primOp aVar bVar

    applyUnknownFunction :: NstVar -> NormState NstExpr
    applyUnknownFunction funVar =
      do normalizeExprList args $ \ normArgs ->
          k $ NFunAp funVar normArgs

    applyKnownFunction :: NstVar -> Int -> Int -> NormState NstExpr
    applyKnownFunction funVar numFreeVars funArity =
      let numArgs = length args in
      -- saturated call
      if numArgs == funArity then do
        normalizeExprList args $ \ normArgs ->
            k $ NFunAp funVar normArgs
      -- under-saturated call
      else if numArgs < funArity then
        -- We already know at this point, that this *must* be a non-closure
        if numFreeVars > 0 then error "Internal compiler error, trying to do static partial application of closure" 
        else
          normalizeExprList args $ \ normArgs ->
              k $ NPartAp funVar normArgs
      -- over-saturated call
      else do
        -- TODO should we also check for closures here? can we even have known closures?
        let (knownFunArgs, remainingArgs) = splitAt funArity args
        normalizeExprList knownFunArgs $ \ normKnownFunArgs -> do
            knownFunResult <- newTempVar ""
            let apKnownFun = NFunAp funVar normKnownFunArgs
            normalizeExprList remainingArgs $ \ normRemArgs -> do
                -- application of the result of the previous application of a known function
                rest <- k $ NFunAp knownFunResult normRemArgs
                return $ NLet knownFunResult apKnownFun rest


normalizeMatch :: Expr -> [(Pattern, Expr)] -> Cont -> NormState NstExpr
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



----- Normalization helper functions -----


-- Free variables in a used lambda which can't be resolved in our context need to become
-- free variables in our context
pullUpFreeVars :: [String] -> NormState ()
pullUpFreeVars freeVars = do
  _ <- forM (reverse freeVars) $ \ name -> do
          hasB <- hasBinding name
          when (not hasB) $ addDynamicVar name
  return ()


-- TODO shouldn't this be called something like nameExprList ?
normalizeExprList :: [Expr] -> ([NstVar] -> NormState NstExpr) -> NormState NstExpr
normalizeExprList exprList k =
  normalizeExprList' exprList [] k
  where
    normalizeExprList' [] acc k' = do
      expr <- k' $ reverse acc
      return expr
    normalizeExprList' expLs acc k' = do
      let hd = head expLs
      nameExpr hd "" $ \ var -> do
        restExpr <- normalizeExprList' (tail expLs) (var : acc) k'
        return restExpr


-- TODO rename this function to something more appropriate
nameExpr :: Expr -> String -> VCont -> NormState NstExpr
nameExpr expr originalName k = case expr of
  -- Some variable can be used directly and don't need to be let-bound
  -- TODO what if we use a var several times, will it be bound several times? answer: yes it will. fix that!
  Var name -> do
    var <- lookupName name
    case var of
      -- Constant free vars are let-bound
      NConstantFreeVar _ -> letBind expr "" k
      -- Recursive vars are also let-bound. Not strictly necessary, but easier later on  (TODO loosen this restriction)
      NRecursiveVar _ -> letBind expr "" k
      -- All other vars are used directly (because they will be in a register later on)
      v -> do
            bodyExpr <- k v
            return bodyExpr
  -- Everything that is not a Var needs to be let-bound
  _ -> letBind expr originalName k
  where
    letBind :: Expr -> String -> VCont -> NormState NstExpr
    letBind expr' name k' = do
      atomizeExpr expr' name $ \ aExpr -> do
        var <- newTempVar name
        addBinding name (var, (isDynamic aExpr))
        restExpr <- k' var
        return $ NLet var aExpr restExpr


isDynamic :: NstAtomicExpr -> Bool
isDynamic aExpr =
  case aExpr of
    NNumber _ -> False
    NPlainSymbol _ -> False
    NString _ -> False
    NLambda [] _ _ -> False
    _ -> True




