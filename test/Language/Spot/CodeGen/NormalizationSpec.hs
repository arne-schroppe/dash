module Language.Spot.CodeGen.NormalizationSpec where

import Language.Spot.CodeGen.Normalization
import Language.Spot.IR.Norm
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
                NLet (NVar 0) (NNumber 2) $
                NLet (NVar 1) (NNumber 3) $
                NLet (NVar 2) (NPrimOp $ NPrimOpSub (NVar 0) (NVar 1)) $
                NLet (NVar 3) (NNumber 4) $
                NAtom $ NPrimOp $ NPrimOpAdd (NVar 2) (NVar 3)
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
                NLet (NVar 0) (NFreeVar "fun1") $
                NLet (NVar 1) (NFreeVar "fun2") $
                NLet (NVar 2) (NNumber 1) $
                NLet (NVar 3) (NNumber 2) $
                NLet (NVar 4) (NFunCall [(NVar 1), (NVar 2), (NVar 3)]) $
                NLet (NVar 5) (NNumber 3) $
                NLet (NVar 6) (NNumber 4) $
                NLet (NVar 7) (NNumber 5) $
                NLet (NVar 8) (NPrimOp $ NPrimOpAdd (NVar 6) (NVar 7)) $
                NLet (NVar 9) (NNumber 6) $
                NAtom $ NFunCall [NVar 0,
                                  NVar 4,
                                  NVar 5,
                                  NVar 8,
                                  NVar 9]
        norm `shouldBe` expected

      it "normalizes a lambda call" $ do
        let ast = FunCall
                (Lambda ["a", "b"]
                    (LitNumber 5))
                [LitNumber 1, LitNumber 2]
        let norm = pureNorm ast
        let expected =
                NLet (NVar 0) (NLambda [] ["a", "b"] (NAtom $ NNumber 5)) $
                NLet (NVar 1) (NNumber 1) $
                NLet (NVar 2) (NNumber 2) $
                NAtom $ NFunCall [NVar 0, NVar 1, NVar 2]
        norm `shouldBe` expected

      it "reuses named variables" $ do
        let ast = LocalBinding (Binding "x" $ LitNumber 3) $
                  FunCall (Var "add") [Var "x", Var "x"]
        let norm = pureNorm ast
        let expected =
                NLet (NVar 0) (NNumber 3) $
                NAtom $ NPrimOp $ NPrimOpAdd (NVar 0) (NVar 0)
        norm `shouldBe` expected


      it "normalizes a returned lambda call" $ do
        let ast = LocalBinding (Binding "make-l" $ Lambda ["x"] $
                                Lambda ["y"] $ LitNumber 22 ) $
                  LocalBinding (Binding "l" $ FunCall (Var "make-l") [LitNumber 0]) $
                  FunCall (Var "l") [LitNumber 55]
        let norm = pureNorm ast
        let expected =
                NLet (NVar 0) (NLambda [] ["x"] $ NAtom $
                    NLambda [] ["y"] $ NAtom $ NNumber 22) $
                NLet (NVar 1) (NNumber 0) $
                NLet (NVar 2) (NFunCall [NVar 0, NVar 1]) $
                NLet (NVar 3) (NNumber 55) $
                NAtom $ NFunCall [NVar 2, NVar 3]
        norm `shouldBe` expected

      it "normalizes nested bindings" $ do
        let ast = LocalBinding (Binding "a" $
                    LocalBinding (Binding "b" $ LitNumber 22) $
                    FunCall (Var "add") [Var "b", LitNumber 4] ) $
                  FunCall (Var "sub") [Var "a", LitNumber 55]
        let norm = pureNorm ast
        let expected =
                NLet (NVar 1) (NNumber 4) $
                NLet (NVar 0) (NNumber 22) $
                NLet (NVar 2) (NPrimOp $ NPrimOpAdd (NVar 0) (NVar 1)) $
                NLet (NVar 3) (NNumber 55) $
                NAtom $ NPrimOp $ NPrimOpSub (NVar 2) (NVar 3)
        norm `shouldBe` expected

      it "vars in tail position are referenced by number" $ do
        let ast = LocalBinding (Binding "a" $ LitNumber 55) $
                  Var "a"
        let norm = pureNorm ast
        let expected =
                NLet (NVar 0) (NNumber 55) $
                NAtom $ NResultVar (NVar 0)
        norm `shouldBe` expected

-- TODO don't care about match for now
{-
      -- TODO all named vars should also be put in a temp var
      it "normalizes a match expression" $ do
        let ast = LocalBinding (Binding "a" $
                Match (LitNumber 101) [
                  (PatNumber 1, LitNumber 33),
                  (PatNumber 2, LitNumber 44)
                ]) $
                FunCall (Var "func") [Var "a"]
        let norm = pureNorm ast
        let expected =
                NLet (NTempVar 0) (NNumber 101) $
                NLet (NTempVar 1) (NMatch 0 (NTempVar 0) [
                  (PatNumber 1, NAtom $ NNumber 33),
                  (PatNumber 2, NAtom $ NNumber 44)
                ]) $
                NAtom $ NFunCall [NNamedVar "func", NTempVar 1]
        norm `shouldBe` expected
-}


