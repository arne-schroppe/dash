module Language.Spot.CodeGen.AstToAnf where

import Language.Spot.IR.Ast
import Language.Spot.IR.Anf
import Language.Spot.IR.Tac
import Language.Spot.CodeGen.CodeGenState
import Control.Monad.State

-- TODO use named variables?

normalize :: Expr -> (AnfExpr, ConstTable, SymbolNameList)
normalize expr =
  let (result, finalState) = runState (normalizeExpr expr) emptyCode in
  (result, getConstantTable finalState, getSymbolNames finalState)

normalizeExpr :: Expr -> State Code AnfExpr
normalizeExpr expr = case expr of
  FunCall funExpr args -> normalizeFunCall funExpr args
  LocalBinding (Binding name boundExpr) restExpr ->
        normalizeLet (AnfNamedVar name) boundExpr restExpr
  Match matchedExpr patterns -> normalizeMatch matchedExpr patterns
  a -> do
    normalizedAtom <- normalizeAtomicExpr expr
    return $ AnfAtom normalizedAtom

normalizeAtomicExpr :: Expr -> State Code AnfAtomicExpr
normalizeAtomicExpr expr = case expr of
  LitNumber n -> normalizeNumber n
  LitSymbol sid args -> normalizeSymbol sid args
  Var name -> normalizeVar name
  Lambda params bodyExpr -> normalizeLambda params bodyExpr
  x -> error $ "Unable to normalize " ++ (show x)

normalizeLet var boundExpr restExpr = do
  normalizedBoundExpr <- normalizeAtomicExpr boundExpr
  normalizedRestExpr <- normalizeExpr restExpr
  return $ AnfLet var normalizedBoundExpr normalizedRestExpr

normalizeNumber n = return (AnfNumber n)

normalizeSymbol sid [] = do
  symId <- addSymbolName sid
  return (AnfPlainSymbol symId)
normalizeSymbol sid args = error "Can't normalize this symbol"

normalizeVar name = return $ AnfVar $ AnfNamedVar name

normalizeLambda params bodyExpr = do
  normalizedBody <- normalizeExpr bodyExpr
  let freeVars = []
  return $ AnfLambda freeVars params normalizedBody



normalizeFunCall (Var name) args =
  normalizeNamedFun name args

normalizeNamedFun "add" [LitNumber a, LitNumber b] =
  let norm = AnfLet (AnfTempVar 1) (AnfNumber a) $
             AnfLet (AnfTempVar 2) (AnfNumber b) $
             (AnfPrimOp $ AnfPrimOpAdd (AnfTempVar 1) (AnfTempVar 2))
  in
  return norm

normalizeNamedFun "sub" [LitNumber a, LitNumber b] =
  let norm = AnfLet (AnfTempVar 1) (AnfNumber a) $
             AnfLet (AnfTempVar 2) (AnfNumber b) $
             (AnfPrimOp $ AnfPrimOpSub (AnfTempVar 1) (AnfTempVar 2))
  in
  return norm


normalizeMatch matchedExpr patterns = error "Fail match"
