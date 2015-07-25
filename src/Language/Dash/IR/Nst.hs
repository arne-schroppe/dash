module Language.Dash.IR.Nst (
  NstExpr (..)
, NstAtomicExpr (..)
, NstVar (..)
, NstPrimOp (..)
) where


import           Language.Dash.IR.Data  (ConstAddr, SymId, Name)

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
  | NVar NstVar -- This is only for returning a var as a result
  | NLambda [Name] [Name] NstExpr -- FreeVars FormalParams Body
  | NMatchBranch [Name] [Name] NstExpr -- FreeVars FormalParams Body
  | NPrimOp NstPrimOp
  | NPartAp NstVar [NstVar] -- partial application. Func var, arguments
  | NFunAp NstVar [NstVar]
  | NMatch Int NstVar ConstAddr [([Name], [Name], NstVar)] -- MaxCaptures Subject PatternAddr [MatchBranchFreeVars, MatchedVars, VarHoldingMatchBranch]
  deriving (Eq, Ord)


data NstVar =
    NLocalVar Name
  | NFunParam Name
  | NDynamicFreeVar Name
  | NConstantFreeVar Name -- We should rename this to StaticFreeVar
  | NRecursiveVar Name
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
    NAtom atom -> "return " ++ (show atom) ++ "\n"
    NLet var atom rest -> (show var) ++ " <- " ++ (show atom) ++ "\n" ++ (show rest)

instance Show NstVar where
  show v = case v of
    NLocalVar name -> "r" ++ " '" ++ name ++ "'"
    NFunParam name -> "p '" ++ name ++ "'"
    NDynamicFreeVar name -> "f '" ++ name ++ "'"
    NConstantFreeVar name -> "g '" ++ name ++ "'"
    NRecursiveVar name -> "r '" ++ name ++ "'"

instance Show NstAtomicExpr where
  show atom = case atom of
    NNumber n -> show n
    NPlainSymbol s -> "sym #" ++ (show s)
    NCompoundSymbol b sa -> "sym @" ++ (show sa) ++ (if b then " (dynamic)" else "")
    NString str -> "\"" ++ str ++ "\""
    NVar v -> "var " ++ (show v)
    NLambda free params body -> "λ f" ++ (show free) ++ " p" ++ (show params) ++ " {\n" ++ (show body) ++ "}"
    NMatchBranch free matchedVars body -> "mλ f" ++ (show free) ++ " m" ++ (show matchedVars) ++ " {\n" ++ (show body) ++ "}"
    NPrimOp p -> show p
    NPartAp v args -> "pap " ++ (show v) ++ " " ++ (show args)
    NFunAp v args -> "ap " ++ (show v) ++ " " ++ (show args)
    NMatch maxv subj pat body -> "match (max " ++ (show maxv) ++ ") [" ++ (show subj) ++ "] @" ++ (show pat) ++ " " ++ (show body)

