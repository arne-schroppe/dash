module Language.Dash.IR.Nst (
  NstExpr (..)
, NstAtomicExpr (..)
, NstVar (..)
, NstVarType (..)
, NstPrimOp (..)
) where


import           Language.Dash.IR.Data (ConstAddr, Name, SymId)

-- NST, the Normalized Syntax Tree


data NstExpr =
    NAtom NstAtomicExpr
  | NLet NstVar NstAtomicExpr NstExpr
  deriving (Eq, Ord)



data NstAtomicExpr =
    NNumber Int
  | NPlainSymbol SymId
  | NCompoundSymbol Bool ConstAddr  -- IsDynamic SymbolAddr
  | NString Name
  | NVarExpr NstVar
  | NLambda [Name] [Name] NstExpr -- FreeVars FormalParams Body
  | NMatchBranch [Name] [Name] NstExpr -- FreeVars FormalParams Body
  | NPrimOp NstPrimOp
  | NPartAp NstVar [NstVar] -- partial application. Func var, arguments
  | NFunAp NstVar [NstVar]
  | NMatch Int NstVar ConstAddr [([Name], [Name], NstVar)] -- MaxCaptures Subject PatternAddr [MatchBranchFreeVars, MatchedVars, VarHoldingMatchBranch]
  deriving (Eq, Ord)

data NstVarType =
    NLocalVar
  | NFunParam
  | NFreeVar
  | NConstant -- Global constants, e.g. named functions without free variables or named literals
  | NRecursiveVar
  deriving (Eq, Ord)

data NstVar = NVar Name NstVarType
  deriving (Eq, Ord)

data NstPrimOp =
    NPrimOpAdd NstVar NstVar
  | NPrimOpSub NstVar NstVar
  | NPrimOpMul NstVar NstVar
  | NPrimOpDiv NstVar NstVar
  | NPrimOpEq  NstVar NstVar
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
    NCompoundSymbol b sa -> "sym @" ++ show sa ++ if b then " (dynamic)" else ""
    NString str          -> "\"" ++ str ++ "\""
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

