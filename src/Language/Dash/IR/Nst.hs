module Language.Dash.IR.Nst (
  NstExpr (..)
, NstAtomicExpr (..)
, NstVar (..)
, NstVarType (..)
, NstPrimOp (..)
) where


import           Language.Dash.IR.Data (ConstAddr, Name, SymId, symIdToInt)

-- NST, the Normalized Syntax Tree


data NstExpr =
    NAtom NstAtomicExpr
  | NLet NstVar NstAtomicExpr NstExpr
  deriving (Eq, Ord)


data NstAtomicExpr =
    NNumber Int
  | NPlainSymbol SymId
  | NCompoundSymbol [(Int, NstVar)] ConstAddr  -- IsDynamic SymbolAddr
  | NString ConstAddr
  | NVarExpr NstVar
  | NLambda [Name] [Name] NstExpr -- FreeVars FormalParams Body
  | NMatchBranch [Name] [Name] NstExpr -- FreeVars FormalParams Body
  | NPrimOp NstPrimOp
  | NPartAp NstVar [NstVar] -- partial application. Func var, arguments
  | NFunAp NstVar [NstVar]
  | NModule [(SymId, Name, NstAtomicExpr)]
  | NModuleLookup NstVar NstVar  -- module, symbol
  | NMatch Int NstVar ConstAddr [([Name], [Name], NstVar)] -- MaxCaptures Subject
                                                           -- PatternAddr
                                                           -- [ MatchBranchFreeVars
                                                           -- , MatchedVars
                                                           -- , MatchBranchVar]
  deriving (Eq, Ord)


data NstVarType =
    NLocalVar
  | NFunParam
  | NFreeVar
  | NConstant -- Global constants, e.g. literals or named functions without free vars
  | NRecursiveVar
  deriving (Eq, Ord)


data NstVar = NVar Name NstVarType
  deriving (Eq, Ord)


-- TODO make it possible to hide the names of primops
data NstPrimOp =
    NPrimOpAdd NstVar NstVar
  | NPrimOpSub NstVar NstVar
  | NPrimOpMul NstVar NstVar
  | NPrimOpDiv NstVar NstVar
  | NPrimOpEq  NstVar NstVar
  | NPrimOpLessThan  NstVar NstVar
  | NPrimOpGreaterThan  NstVar NstVar
  | NPrimOpOr NstVar NstVar
  | NPrimOpAnd NstVar NstVar
  | NPrimOpNot NstVar
  deriving (Eq, Ord, Show)


instance Show NstExpr where
  show expr = case expr of
    NAtom atom -> "return " ++ show atom ++ "\n"
    NLet var atom rest -> show var ++ " <- " ++ show atom ++ "\n" ++ show rest


instance Show NstVarType where
  show v = case v of
    NLocalVar     -> "l"
    NFunParam     -> "p"
    NFreeVar      -> "f"
    NConstant     -> "g"
    NRecursiveVar -> "r"


instance Show NstVar where
  show (NVar name vartype) = show vartype ++ "'" ++ name ++ "'"


instance Show NstAtomicExpr where
  show atom = case atom of
    NNumber n            -> show n
    NPlainSymbol s       -> "sym #" ++ show s
    NCompoundSymbol free sa -> "sym " ++ (show free) ++ " @" ++ show sa
    NString strAddr      -> "str @" ++ show strAddr
    NVarExpr v           -> "var " ++ show v
    NLambda free params body ->
                            "λ f" ++ show free ++ " p" ++ show params
                            ++ " {\n" ++ show body ++ "}"
    NMatchBranch free matchedVars body ->
                            "mλ f" ++ show free ++ " m" ++ show matchedVars
                            ++ " {\n" ++ show body ++ "}"
    NPrimOp p            -> show p
    NPartAp v args       -> "pap " ++ show v ++ " " ++ show args
    NFunAp v args        -> "ap " ++ show v ++ " " ++ show args
    NMatch maxv subj pat body ->
                            "match (max " ++ show maxv ++ ") [" ++ show subj ++ "] @"
                            ++ show pat ++ " " ++ show body

    NModule fields       -> "module {\n" ++ concat (map (\(sym, name, field) -> name ++ 
                              "(" ++ (show $ symIdToInt sym) ++  "): " ++ show field) fields) ++ "}"
    NModuleLookup m f    -> (show m) ++ "." ++ (show f)

