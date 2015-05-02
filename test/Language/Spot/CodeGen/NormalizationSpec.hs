module Language.Spot.CodeGen.NormalizationSpec where

import Language.Spot.CodeGen.Normalization
import Language.Spot.IR.Anf
import Language.Spot.IR.Ast

import Test.Hspec

pureNorm ast =
  let (norm, _, _) = normalize ast in
  norm

spec :: Spec
spec = do
  describe "Normalization" $ do

      it "normalizes a number directly" $ do
        let ast = LitNumber 3
        let norm = pureNorm ast
        norm `shouldBe` (NAtom $ NNumber 3)

      it "normalizes a simple symbol directly" $ do
        let ast = LitSymbol "Test" []
        let (norm, _, syms) = normalize ast
        (length syms) `shouldBe` 1
        syms `shouldBe` ["Test"]
        norm `shouldBe` (NAtom $ NPlainSymbol 0)

      it "splits a complex addition operation" $ do
        let ast = FunCall (Var "add") 
                      [(FunCall (Var "sub")
                          [LitNumber 2, LitNumber 3]),
                       LitNumber 4]
        let norm = pureNorm ast
        let expected =
                NLet (NTempVar 0) (NNumber 2) $
                NLet (NTempVar 1) (NNumber 3) $
                NLet (NTempVar 2) (NPrimOp $ NPrimOpSub (NTempVar 0) (NTempVar 1)) $
                NLet (NTempVar 3) (NNumber 4) $
                NAtom $ NPrimOp $ NPrimOpAdd (NTempVar 2) (NTempVar 3)
        norm `shouldBe` expected

      it "normalizes general function calls" $ do
        let ast = FunCall (Var "fun1") 
                      [(FunCall (Var "fun2")
                          [LitNumber 1, LitNumber 2]),
                       LitNumber 3,
                       (FunCall (Var "add")
                          [LitNumber 4, LitNumber 5]),
                       LitNumber 6]
        let norm = pureNorm ast
        let expected =
                NLet (NTempVar 0) (NNumber 1) $
                NLet (NTempVar 1) (NNumber 2) $
                NLet (NTempVar 2) (NFunCall [(NNamedVar "fun2"), (NTempVar 0), (NTempVar 1)]) $
                NLet (NTempVar 3) (NNumber 3) $
                NLet (NTempVar 4) (NNumber 4) $
                NLet (NTempVar 5) (NNumber 5) $
                NLet (NTempVar 6) (NPrimOp $ NPrimOpAdd (NTempVar 4) (NTempVar 5)) $
                NLet (NTempVar 7) (NNumber 6) $
                NAtom $ NFunCall [(NNamedVar "fun1"),
                                  (NTempVar 2),
                                  (NTempVar 3),
                                  (NTempVar 6),
                                  (NTempVar 7)]
        norm `shouldBe` expected
