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
