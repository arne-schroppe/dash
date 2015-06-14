module Language.Spot.IR.Nst (
  NstExpr (..)
, NstAtomicExpr (..)
, NstVar (..)
, NstPrimOp (..)
) where


import           Language.Spot.IR.Data  (ConstAddr)

-- NST, the Normalized Syntax Tree

-- TODO Should every code branch in a match expression really be a closure?

data NstExpr =
    NAtom NstAtomicExpr
  | NLet NstVar NstAtomicExpr NstExpr
  deriving (Eq, Ord, Show)

type VarName = String
type ParamName = String

data NstAtomicExpr =
    NNumber Int
  | NPlainSymbol Int
  | NCompoundSymbol Bool ConstAddr  -- IsDynamic SymbolAddr
  | NVar NstVar -- This is only for returning a var as a result
  | NLambda [VarName] [ParamName] NstExpr  -- FreeVars FormalParams Body
  | NPrimOp NstPrimOp
  | NPartAp NstVar [NstVar] Int -- partial application. Func var, arguments, arity
  | NFunCall NstVar [NstVar]
  | NMatch Int NstVar ConstAddr [([VarName], NstVar)] -- MaxCaptures Subject PatternAddr [MatchedVars, Var-That-Holds-Closure]
  deriving (Eq, Ord, Show)


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




