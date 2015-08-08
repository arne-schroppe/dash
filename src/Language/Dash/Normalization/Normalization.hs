module Language.Dash.Normalization.Normalization (
  normalize
) where

import           Control.Monad.State                            hiding (state)
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
    nameExpr boundExpr name $ \ _ -> normalizeExpr restExpr
  _ ->
    atomizeExpr expr "" $ return . NAtom


atomizeExpr :: Expr -> String -> Cont -> NormState NstExpr
atomizeExpr expr name k = case expr of
  FunAp funExpr args ->
      normalizeFunAp funExpr args k
  LitNumber n ->
      normalizeNumber n k
  LitSymbol sname args ->
      normalizeSymbol sname args k
  LitString s ->
      normalizeString s k
  Var vname ->
      normalizeVar vname k
  Match matchedExpr patterns ->
      normalizeMatch matchedExpr patterns k
  Lambda params bodyExpr ->
      normalizeLambda params bodyExpr name k
  MatchBranch matchedVars bodyExpr ->
      normalizeMatchBranch matchedVars bodyExpr k
  LocalBinding (Binding bname boundExpr) restExpr ->
      -- This case is only for inner local bindings, i.e. let a = let b = 2 in 1 + b
      -- (So in that example "let b = ..." is the inner local binding)
      atomizeExpr boundExpr bname $ \ aExpr -> do
        let var = NVar bname NLocalVar
        addBinding bname (var, False)
        atomizeExpr restExpr "" $ \ normBoundExpr -> do
          rest <- k normBoundExpr
          return $ NLet var aExpr rest
  x -> error $ "Unable to normalize " ++ show x


normalizeNumber :: Int -> Cont -> NormState NstExpr
normalizeNumber n k = k (NNumber n)

normalizeString :: String -> Cont -> NormState NstExpr
normalizeString s k = do
  encString <- encodeConstantString s
  strAddr <- addConstant encString
  k (NString strAddr)

normalizeSymbol :: String -> [Expr] -> Cont -> NormState NstExpr
normalizeSymbol sname [] k = do
  symId <- addSymbolName sname
  k (NPlainSymbol symId)
normalizeSymbol sname args k =
  if (isDynamicLiteral $ LitSymbol sname args) then
    let indicesAndDynamicValues = indexedDynamicSymbolFields args in
    let indices = map fst indicesAndDynamicValues in
    let dynamicVars = map snd indicesAndDynamicValues in
    nameExprList dynamicVars $ \ freeVars -> do
      let zeroedFields = setZeroesAtIndices args indices
      encConst <- encodeConstantCompoundSymbol sname zeroedFields
      cAddr <- addConstant encConst
      let indicesAndVars = zip indices freeVars
      k $ NCompoundSymbol indicesAndVars cAddr
    -- letbind all dynamic values
    -- get indices of letbound values in symbol. Replace those with 0
    -- load constant
    -- setSymField of all these letbound vars (in codegen)
  else do
    encConst <- encodeConstantCompoundSymbol sname args
    cAddr <- addConstant encConst
    -- TODO get a list of all dynamic values and their positions. Then
    -- letbind the dynamic values. At the dynamic positions inside the
    -- symbol just set 0. Codegen will then take that const symbol, copy
    -- it to the heap, and set the letbound values at their respective
    -- positions. So change isDynamic::Bool to freeVars::[LocalVar, Index]
    -- New opcodes: LOAD_SYM heap_addr_reg sym_reg, SET_SYM_VAL heap_sym_reg val_reg
    -- We also need a new tag for heap symbols
    k (NCompoundSymbol [] cAddr)

-- Only dynamic values in the list and their index
indexedDynamicSymbolFields :: [Expr] -> [(Int, Expr)]
indexedDynamicSymbolFields fields =
  filter (isDynamicLiteral.snd) $ zipWithIndex fields

setZeroesAtIndices :: [Expr] -> [Int] -> [Expr]
setZeroesAtIndices fields indices = 
  map (\ (index, e) -> 
    if index `elem` indices then
      LitNumber 0
    else
      e) $
    zipWithIndex fields


isDynamicLiteral :: Expr -> Bool
isDynamicLiteral v =
  case v of
    LitNumber _ -> False
    LitSymbol _ [] -> False
    LitSymbol _ args -> any isDynamicLiteral args
    _ -> True



-- This is only direct usage of a var (as a "return value")
normalizeVar :: String -> Cont -> NormState NstExpr
normalizeVar name k = do
  var <- lookupName name
  k $ NVarExpr var


normalizeLambda :: [String] -> Expr -> String -> Cont -> NormState NstExpr
normalizeLambda params bodyExpr name k = do
  enterContext params
  -- TODO we don't know whether this var is dynamic or not!
  addBinding name (NVar name NRecursiveVar, False)
  -- TODO add arity for recursive var?
  normalizedBody <- normalizeExpr bodyExpr
  freeVars <- freeVariables
  leaveContext
  pullUpFreeVars freeVars
  addArity name (length freeVars) (length params)
  k $ NLambda freeVars params normalizedBody


normalizeMatchBranch :: [String] -> Expr -> Cont -> NormState NstExpr
normalizeMatchBranch matchedVars bodyExpr k = do
  enterContext matchedVars
  -- TODO add arity for recursive var?
  normalizedBody <- normalizeExpr bodyExpr
  freeVars <- freeVariables
  leaveContext
  pullUpFreeVars freeVars
  k $ NMatchBranch freeVars matchedVars normalizedBody


-- TODO throw an error if the thing being called is obviously not callable
-- TODO it gets a bit confusing in which cases we expect a closure and where we expect
-- a simple function
normalizeFunAp :: Expr -> [Expr] -> Cont -> NormState NstExpr
normalizeFunAp funExpr args k =
  case (funExpr, args) of
    (Var "+", [a, b])  -> normalizeBinaryPrimOp NPrimOpAdd a b
    (Var "-", [a, b])  -> normalizeBinaryPrimOp NPrimOpSub a b
    (Var "*", [a, b])  -> normalizeBinaryPrimOp NPrimOpMul a b
    (Var "/", [a, b])  -> normalizeBinaryPrimOp NPrimOpDiv a b
    (Var "<", [a, b])  -> normalizeBinaryPrimOp NPrimOpLessThan a b
    (Var ">", [a, b])  -> normalizeBinaryPrimOp NPrimOpGreaterThan a b
    (Var "==", [a, b]) -> normalizeBinaryPrimOp NPrimOpEq  a b
    -- TODO prevent user from defining a "string-length" function
    (Var "string-length", [a]) -> normalizeUnaryPrimOp NPrimOpStrLen a

    -- TODO name this anonymous function
    _ -> nameExpr funExpr "" $ \ funVar -> do
      maybeAr <- arity funVar
      case maybeAr of
        Nothing -> applyUnknownFunction funVar
        Just (numFree, ar) -> applyKnownFunction funVar numFree ar
  where
    normalizeBinaryPrimOp :: (NstVar -> NstVar -> NstPrimOp)
                          -> Expr
                          -> Expr
                          -> NormState NstExpr
    normalizeBinaryPrimOp primOp a b =
      nameExprList [a, b] $ \ [aVar, bVar] ->
          k $ NPrimOp $ primOp aVar bVar

    normalizeUnaryPrimOp :: (NstVar -> NstPrimOp)
                          -> Expr
                          -> NormState NstExpr
    normalizeUnaryPrimOp primOp a =
      nameExprList [a] $ \ [aVar] ->
          k $ NPrimOp $ primOp aVar

    applyUnknownFunction :: NstVar -> NormState NstExpr
    applyUnknownFunction funVar =
      nameExprList args $ \ normArgs ->
          k $ NFunAp funVar normArgs

    applyKnownFunction :: NstVar -> Int -> Int -> NormState NstExpr
    applyKnownFunction funVar numFreeVars funArity =
      let numArgs = length args in
      -- saturated call
      if numArgs == funArity then
        nameExprList args $ \ normArgs ->
            k $ NFunAp funVar normArgs
      -- under-saturated call
      else if numArgs < funArity then
        -- We already know at this point, that this *must* be a non-closure
        if numFreeVars > 0
          then error "Internal compiler error, trying to do static partial\
                     \ application of closure"
        else nameExprList args $ \ normArgs ->
               k $ NPartAp funVar normArgs
      -- over-saturated call
      else do -- numArgs > funArity
        let (knownFunArgs, remainingArgs) = splitAt funArity args
        nameExprList knownFunArgs $ \ normKnownFunArgs -> do
          knownFunResult <- newTempVar
          let apKnownFun = NFunAp funVar normKnownFunArgs
          nameExprList remainingArgs $ \ normRemArgs -> do
            -- The previous function application should have resulted in a new function
            -- , which we're applying here
            rest <- k $ NFunAp knownFunResult normRemArgs
            return $ NLet knownFunResult apKnownFun rest


normalizeMatch :: Expr -> [(Pattern, Expr)] -> Cont -> NormState NstExpr
normalizeMatch matchedExpr patternsAndExpressions k = do
  matchedVarsAndEncodedPatterns <- forM (map fst patternsAndExpressions) $
                                        encodeMatchPattern 0
  let matchedVars = map fst matchedVarsAndEncodedPatterns
  patternAddr <- addConstant $ CMatchData $ map snd matchedVarsAndEncodedPatterns
  let exprs = map snd patternsAndExpressions
  let maxMatchVars = maximum $ map length matchedVars
  -- we wrap each match branch in a lambda. This way we can handle them easier in the
  -- code generator. But the lambda has a different constructor, called MatchBranch,
  -- which helps us in optimizing the compiled code (free variables in matchBranches are
  -- not pushed as a closure on the heap, for example).
  nameExpr matchedExpr "" $ \ subjVar ->
    let matchBranches = zipWith MatchBranch matchedVars exprs in
    nameExprList matchBranches $ \ branchVars -> do
      -- for now we're only inserting empty lists instead of free vars. The
      -- recursion module will insert the actual free vars of each match
      -- branch, because only that module has full knowledge of them
      let freeVars = replicate (length branchVars) []
      let branches = zip3 freeVars matchedVars branchVars
      k $ NMatch maxMatchVars subjVar patternAddr branches


----- Normalization helper functions

-- Free variables in a closure used by us which can't be resolved in our context need to
-- become free variables in our context
pullUpFreeVars :: [String] -> NormState ()
pullUpFreeVars freeVars = do
  _ <- forM (reverse freeVars) $ \ name -> do
          hasB <- hasBinding name
          unless hasB $ addDynamicVar name
  return ()


nameExprList :: [Expr] -> ([NstVar] -> NormState NstExpr) -> NormState NstExpr
nameExprList exprList =
  nameExprList' exprList []
  where
    nameExprList' [] acc k' =
      k' $ reverse acc
    nameExprList' expLs acc k' =
      nameExpr (head expLs) "" $ \ var ->
        nameExprList' (tail expLs) (var : acc) k'


nameExpr :: Expr -> String -> VCont -> NormState NstExpr
nameExpr expr originalName k = case expr of
  -- Some variable can be used directly and don't need to be let-bound
  -- TODO what if we use a var several times, will it be bound several times? answer: yes it will. fix that!
  Var name -> do
    var <- lookupName name
    case var of
      -- Constant free vars are let-bound
      NVar _ NConstant -> letBind expr "" k
      -- Recursive vars are also let-bound. Not strictly necessary, but easier later on  (TODO loosen this restriction)
      NVar _ NRecursiveVar -> letBind expr "" k
      -- All other vars are used directly (because they will be in a register later on)
      v -> k v
  -- Everything that is not a Var needs to be let-bound
  _ -> letBind expr originalName k
  where
    letBind :: Expr -> String -> (NstVar -> NormState NstExpr) -> NormState NstExpr
    letBind expr' name k' =
      atomizeExpr expr' name $ \ aExpr -> do
        var <- if null name then newTempVar else return $ NVar name NLocalVar
        addBinding name (var, isDynamic aExpr)
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

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex values = zip [0..(length values)] values


-- Encoding

encodeConstantCompoundSymbol :: Name -> [Expr] -> NormState Constant
encodeConstantCompoundSymbol symName symArgs = do
  symId <- addSymbolName symName
  encodedArgs <- mapM encodeConstantLiteral symArgs
  return $ CCompoundSymbol symId encodedArgs

encodeConstantString :: String -> NormState Constant
encodeConstantString str = do
  return $ CString str

encodeConstantLiteral :: Expr -> NormState Constant
encodeConstantLiteral v =
  case v of
    LitNumber n ->
        return $ CNumber n
    LitSymbol s [] -> do
        sid <- addSymbolName s
        return $ CPlainSymbol sid
    LitSymbol s args ->
        encodeConstantCompoundSymbol s args
    _ ->
        error "Expected a literal"


encodeMatchPattern :: Int -> Pattern -> NormState ([String], Constant)
encodeMatchPattern nextMatchVar pat =
  case pat of
    PatNumber n ->
        return ([], CNumber n)
    PatSymbol s [] -> do
        sid <- addSymbolName s
        return ([], CPlainSymbol sid)
    PatSymbol s params -> do
        symId <- addSymbolName s
        (vars, pats) <- encodePatternCompoundSymbolArgs nextMatchVar params
        return (vars, CCompoundSymbol symId pats)
    PatVar n ->
        return ([n], CMatchVar nextMatchVar)
    PatWildcard ->
        return (["_"], CMatchVar nextMatchVar) -- TODO be a bit more sophisticated here
                                               -- and don't encode this as a var that is
                                               -- passed to the match branch

-- TODO use inner state ?
encodePatternCompoundSymbolArgs :: Int -> [Pattern] -> NormState ([String], [Constant])
encodePatternCompoundSymbolArgs nextMatchVar args = do
  (_, vars, entries) <- foldM (\(nextMV, accVars, pats) p -> do
    (vars, encoded) <- encodeMatchPattern nextMV p
    return (nextMV + fromIntegral (length vars), accVars ++ vars, pats ++ [encoded])
    ) (nextMatchVar, [], []) args  -- TODO get that O(n*m) out and make it more clear
                                   -- what this does
  return (vars, entries)



