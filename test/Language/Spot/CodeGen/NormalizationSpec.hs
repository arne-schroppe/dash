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
                NLet (NLocalVar 0 "") (NNumber 2) $
                NLet (NLocalVar 1 "") (NNumber 3) $
                NLet (NLocalVar 2 "") (NPrimOp $ NPrimOpSub (NLocalVar 0 "") (NLocalVar 1 "")) $
                NLet (NLocalVar 3 "") (NNumber 4) $
                NAtom $ NPrimOp $ NPrimOpAdd (NLocalVar 2 "") (NLocalVar 3 "")
        norm `shouldBe` expected

      it "normalizes general function calls" $ do
        let dummyFunc = Lambda ["a"] $ LitNumber 0
        let ast = LocalBinding (Binding "fun1" dummyFunc) $
                  LocalBinding (Binding "fun2" dummyFunc) $
                  FunCall (Var "fun1") 
                      [(FunCall (Var "fun2")
                          [LitNumber 1, LitNumber 2]),
                       LitNumber 3,
                       (FunCall (Var "add")
                          [LitNumber 4, LitNumber 5]),
                       LitNumber 6]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar 0 "fun1") (NLambda [] ["a"] $ NAtom $ NNumber 0) $
                NLet (NLocalVar 1 "fun2") (NLambda [] ["a"] $ NAtom $ NNumber 0) $
                NLet (NLocalVar 2 "") (NNumber 1) $
                NLet (NLocalVar 3 "") (NNumber 2) $
                NLet (NLocalVar 4 "") (NFunCall (NLocalVar 1 "fun2") [(NLocalVar 2 ""), (NLocalVar 3 "")]) $
                NLet (NLocalVar 5 "") (NNumber 3) $
                NLet (NLocalVar 6 "") (NNumber 4) $
                NLet (NLocalVar 7 "") (NNumber 5) $
                NLet (NLocalVar 8 "") (NPrimOp $ NPrimOpAdd (NLocalVar 6 "") (NLocalVar 7 "")) $
                NLet (NLocalVar 9 "") (NNumber 6) $
                NAtom $ NFunCall (NLocalVar 0 "fun1") [
                                  NLocalVar 4 "",
                                  NLocalVar 5 "",
                                  NLocalVar 8 "",
                                  NLocalVar 9 ""]
        norm `shouldBe` expected

      it "normalizes a lambda call" $ do
        let ast = FunCall
                (Lambda ["a", "b"]
                    (LitNumber 5))
                [LitNumber 1, LitNumber 2]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar 0 "") (NLambda [] ["a", "b"] (NAtom $ NNumber 5)) $
                NLet (NLocalVar 1 "") (NNumber 1) $
                NLet (NLocalVar 2 "") (NNumber 2) $
                NAtom $ NFunCall (NLocalVar 0 "") [NLocalVar 1 "", NLocalVar 2 ""]
        norm `shouldBe` expected

      it "reuses named variables" $ do
        let ast = LocalBinding (Binding "x" $ LitNumber 3) $
                  FunCall (Var "add") [Var "x", Var "x"]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar 0 "x") (NNumber 3) $
                NAtom $ NPrimOp $ NPrimOpAdd (NLocalVar 0 "x") (NLocalVar 0 "x")
        norm `shouldBe` expected


      it "normalizes a returned lambda call" $ do
        let ast = LocalBinding (Binding "make-l" $ Lambda ["x"] $
                                Lambda ["y"] $ LitNumber 22 ) $
                  LocalBinding (Binding "l" $ FunCall (Var "make-l") [LitNumber 0]) $
                  FunCall (Var "l") [LitNumber 55]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar 0 "make-l") (NLambda [] ["x"] $ NAtom $
                    NLambda [] ["y"] $ NAtom $ NNumber 22) $
                NLet (NLocalVar 1 "") (NNumber 0) $
                NLet (NLocalVar 2 "l") (NFunCall (NLocalVar 0 "make-l") [NLocalVar 1 ""]) $
                NLet (NLocalVar 3 "") (NNumber 55) $
                NAtom $ NFunCall (NLocalVar 2 "l") [NLocalVar 3 ""]
        norm `shouldBe` expected

      it "normalizes nested bindings" $ do
        let ast = LocalBinding (Binding "a" $
                    LocalBinding (Binding "b" $ LitNumber 22) $
                    FunCall (Var "add") [Var "b", LitNumber 4] ) $
                  FunCall (Var "sub") [Var "a", LitNumber 55]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar 1 "") (NNumber 4) $
                NLet (NLocalVar 0 "b") (NNumber 22) $
                NLet (NLocalVar 2 "a") (NPrimOp $ NPrimOpAdd (NLocalVar 0 "b") (NLocalVar 1 "")) $
                NLet (NLocalVar 3 "") (NNumber 55) $
                NAtom $ NPrimOp $ NPrimOpSub (NLocalVar 2 "a") (NLocalVar 3 "")
        norm `shouldBe` expected

      it "vars in tail position are referenced by number" $ do
        let ast = LocalBinding (Binding "a" $ LitNumber 55) $
                  Var "a"
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar 0 "a") (NNumber 55) $
                NAtom $ NVar $ NLocalVar 0 "a"
        norm `shouldBe` expected


      it "identifies constant free variables" $ do
        let ast = LocalBinding (Binding "b" (LitNumber 4)) $
                     Lambda ["a"] $ FunCall (Var "add") [Var "a", Var "b"]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar 0 "b") (NNumber 4) $
                       NAtom $ NLambda [] ["a"] $
                         NLet (NLocalVar 0 "") (NVar $ NConstantFreeVar "b") $
                         NAtom $ NPrimOp $ NPrimOpAdd (NFunParam "a") (NLocalVar 0 "")
        norm `shouldBe` expected

      it "identifies dynamic free variables" $ do
        let ast = Lambda ["b"] $
                     Lambda ["a"] $ FunCall (Var "add") [Var "a", Var "b"]
        let norm = pureNorm ast
        let expected = NAtom $ NLambda [] ["b"] $
                       NAtom $ NLambda ["b"] ["a"] $
                         NAtom $ NPrimOp $ NPrimOpAdd (NFunParam "a") (NDynamicFreeVar "b")
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


