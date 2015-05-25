module Language.Spot.IR.Norm where

-- TODO this is not ANF anymore. Just call it Norm

import Language.Spot.VM.Types
import Language.Spot.IR.Ast

import Language.Spot.IR.Tac (ConstAddr)

-- How do we handle KnownFreeVars? Initial idea would be to add an initial function
-- placeholder for each toplevel function (to get an address) and to immediately
-- encode constants, and then use that information to replace NKnownFreeVar with its
-- respective value. For this to work we finally need a symbol table

-- Symbol table:
-- When entering a scope, scan through all definitions as explained above (known free
-- variables are not necessarily at the top-level). Note down the names and known
-- types but don't eval yet. This way we can also do mutual recursion.

data NormExpr =
    NAtom NormAtomicExpr
  | NLet NormVar NormAtomicExpr NormExpr
  deriving (Eq, Show)

data NormAtomicExpr =
    NNumber Int
  | NPlainSymbol Int
  | NCompoundSymbol Bool ConstAddr  -- IsDynamic SymbolAddr
  | NVar NormVar -- This is only for returning a var as a result
  | NLambda [String] [String] NormExpr  -- FreeVars FormalParams Body -- TODO free vars and formal params should appear in the same order as later in assembly!
  | NPrimOp NormPrimOp
  | NFunCall NormVar [NormVar]
  | NMatch Int NormVar ConstAddr [([String], NormVar)] -- MaxCaptures Subject PatternAddr [MatchedVars, Var-With-Lambda]
  deriving (Eq, Show)


data NormVar =
    NLocalVar Int String
  | NDynamicFreeVar String
  | NFunParam String
  | NConstantFreeVar String -- We should rename this to StaticFreeVar
  | NRecursiveVar String
  deriving (Eq, Show)

data NormPrimOp =
    NPrimOpAdd NormVar NormVar
  | NPrimOpSub NormVar NormVar
  deriving (Eq, Show)




