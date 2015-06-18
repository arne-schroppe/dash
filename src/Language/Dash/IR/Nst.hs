module Language.Dash.IR.Nst (
  NstExpr (..)
, NstAtomicExpr (..)
, NstVar (..)
, NstPrimOp (..)
) where


import           Language.Dash.IR.Data  (ConstAddr)

-- NST, the Normalized Syntax Tree

-- TODO Should every code branch in a match expression really be a closure?

data NstExpr =
    NAtom NstAtomicExpr
  | NLet NstVar NstAtomicExpr NstExpr
  deriving (Eq, Ord)

instance Show NstExpr where
  show expr = case expr of
    NAtom atom -> "return " ++ (show atom) ++ "\n"
    NLet var atom rest -> (show var) ++ " <- " ++ (show atom) ++ "\n" ++ (show rest)

type VarName = String
type ParamName = String

data NstAtomicExpr =
    NNumber Int
  | NPlainSymbol Int
  | NCompoundSymbol Bool ConstAddr  -- IsDynamic SymbolAddr
  | NString String
  | NVar NstVar -- This is only for returning a var as a result
  | NLambda [VarName] [ParamName] NstExpr  -- FreeVars FormalParams Body
  | NPrimOp NstPrimOp
  | NPartAp NstVar [NstVar] -- partial application. Func var, arguments
  | NFunCall NstVar [NstVar]
  | NMatch Int NstVar ConstAddr [([VarName], NstVar)] -- MaxCaptures Subject PatternAddr [MatchedVars, Var-That-Holds-Closure]
  deriving (Eq, Ord)


data NstVar =
    NLocalVar Int String
  | NFunParam String
  | NDynamicFreeVar String
  | NConstantFreeVar String -- We should rename this to StaticFreeVar
  | NRecursiveVar String
  deriving (Eq, Ord, Show)

data NstPrimOp =
    NPrimOpAdd NstVar NstVar
  | NPrimOpSub NstVar NstVar
  deriving (Eq, Ord, Show)



instance Show NstAtomicExpr where
  show atom = case atom of
    NNumber n -> show n
    NPlainSymbol s -> "sym id " ++ (show s)
    NCompoundSymbol b sa -> "sym @" ++ (show sa) ++ (if b then " (dynamic)" else "")
    NString str -> "\"" ++ str ++ "\""
    NVar v -> "var " ++ (show v)
    NLambda free params body -> "fun fr" ++ (show free) ++ " prms" ++ (show params) ++ " {\n" ++ (show body) ++ "} "
    NPrimOp p -> show p
    NPartAp v args -> "part-ap " ++ (show v) ++ " " ++ (show args)
    NFunCall v args -> "ap " ++ (show v) ++ " " ++ (show args)
    NMatch maxv subj pat body -> "match (max: " ++ (show maxv) ++ ") " ++ (show subj) ++ " @" ++ (show pat) ++ " " ++ (show body)
    

