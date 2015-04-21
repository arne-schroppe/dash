module Language.Spot.CodeGen.AstToAnf where

import Language.Spot.IR.Ast
import Language.Spot.IR.Anf

normalize :: Expr -> AnfExpr
normalize expr = case expr of
  LitNumber n -> AnfNumber n
  -- LitSymbol sid [] -> AnfPlainSymbol sid
  x -> error $ "Unable to normalize " ++ (show x)
