module Language.Dash.Normalization.NormalizationSpec where

import           Language.Dash.IR.Ast
import           Language.Dash.IR.Data
import           Language.Dash.IR.Nst
import           Language.Dash.IR.Tac
import           Language.Dash.Normalization.Normalization
import           Test.Hspec

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
        let ast = FunCall (Var "+")
                      [(FunCall (Var "-")
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
        let ast = LocalBinding (Binding "fun1" $ Lambda ["a", "b", "c", "d"] $ LitNumber 0) $
                  LocalBinding (Binding "fun2" $ Lambda ["a", "b"] $ LitNumber 0) $
                  FunCall (Var "fun1")
                      [(FunCall (Var "fun2")
                          [LitNumber 1, LitNumber 2]),
                       LitNumber 3,
                       (FunCall (Var "+")
                          [LitNumber 4, LitNumber 5]),
                       LitNumber 6]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar 0 "fun1") (NLambda [] ["a", "b", "c", "d"] $ NAtom $ NNumber 0) $
                NLet (NLocalVar 1 "fun2") (NLambda [] ["a", "b"] $ NAtom $ NNumber 0) $
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
                  FunCall (Var "+") [Var "x", Var "x"]
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
                    FunCall (Var "+") [Var "b", LitNumber 4] ) $
                  FunCall (Var "-") [Var "a", LitNumber 55]
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
                     Lambda ["a"] $ FunCall (Var "+") [Var "a", Var "b"]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar 0 "b") (NNumber 4) $
                       NAtom $ NLambda [] ["a"] $
                         NLet (NLocalVar 0 "") (NVar $ NConstantFreeVar "b") $
                         NAtom $ NPrimOp $ NPrimOpAdd (NFunParam "a") (NLocalVar 0 "")
        norm `shouldBe` expected

      it "identifies dynamic free variables" $ do
        let ast = Lambda ["b"] $
                     Lambda ["a"] $ FunCall (Var "+") [Var "a", Var "b"]
        let norm = pureNorm ast
        let expected = NAtom $ NLambda [] ["b"] $
                       NAtom $ NLambda ["b"] ["a"] $
                         NAtom $ NPrimOp $ NPrimOpAdd (NFunParam "a") (NDynamicFreeVar "b")
        norm `shouldBe` expected

      it "identifies nested closures" $ do
        let ast = Lambda ["a"] $
                  Lambda ["b"] $
                  Lambda ["c"] $
                  Lambda ["d"] $ FunCall (Var "+") [Var "a", Var "b"]
        let norm = pureNorm ast
        let expected = NAtom $ NLambda [] ["a"] $
                       NAtom $ NLambda ["a"] ["b"] $
                       NAtom $ NLambda ["b", "a"] ["c"] $
                       NAtom $ NLambda ["b", "a"] ["d"] $
                        NAtom $ NPrimOp $ NPrimOpAdd (NDynamicFreeVar "a") (NDynamicFreeVar "b")
        norm `shouldBe` expected


      it "normalizes match-bodies to lambdas" $ do
        let ast = Match (LitNumber 2) [
                    (PatNumber 1, LitNumber 33),
                    (PatNumber 2, LitNumber 44)
                  ]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar 0 "") (NNumber 2) $
                       NLet (NLocalVar 1 "") (NLambda [] [] $ NAtom $ NNumber 33) $
                       NLet (NLocalVar 2 "") (NLambda [] [] $ NAtom $ NNumber 44) $
                       NAtom $ NMatch 0 (NLocalVar 0 "") 0 [([], NLocalVar 1 ""), ([], NLocalVar 2 "")]
        norm `shouldBe` expected


      it "captures constant free variables in match bodies" $ do
        let ast = LocalBinding (Binding "a" $ LitNumber 77) $
                  Match (LitNumber 2) [
                    (PatNumber 1, Var "a"),
                    (PatNumber 2, LitNumber 44)
                  ]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar 0 "a") (NNumber 77) $
                       NLet (NLocalVar 1 "") (NNumber 2) $
                       NLet (NLocalVar 2 "") (NLambda [] [] $ NAtom $ NVar $ NConstantFreeVar "a") $
                       NLet (NLocalVar 3 "") (NLambda [] [] $ NAtom $ NNumber 44) $
                       NAtom $ NMatch 0 (NLocalVar 1 "") 0 [([], NLocalVar 2 ""), ([], NLocalVar 3 "")]
        norm `shouldBe` expected

      it "captures dynamic free variables in match bodies" $ do
        let ast = Lambda ["a"] $
                  Match (LitNumber 2) [
                    (PatNumber 1, Var "a"),
                    (PatNumber 2, LitNumber 44)
                  ]
        let norm = pureNorm ast
        let expected = NAtom $ NLambda [] ["a"] $
                       NLet (NLocalVar 0 "") (NNumber 2) $
                       NLet (NLocalVar 1 "") (NLambda ["a"] [] $ NAtom $ NVar $ NDynamicFreeVar "a") $
                       NLet (NLocalVar 2 "") (NLambda [] [] $ NAtom $ NNumber 44) $
                       NAtom $ NMatch 0 (NLocalVar 0 "") 0 [([], NLocalVar 1 ""), ([], NLocalVar 2 "")]
        norm `shouldBe` expected

      it "handles vars in patterns as lambda parameters" $ do
        let ast = Match (LitNumber 2) [
                    (PatVar "n", Var "n"),
                    (PatVar "m", Var "m")
                  ]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar 0 "") (NNumber 2) $
                       NLet (NLocalVar 1 "") (NLambda [] ["n"] $ NAtom $ NVar $ NFunParam "n") $
                       NLet (NLocalVar 2 "") (NLambda [] ["m"] $ NAtom $ NVar $ NFunParam "m") $
                       NAtom $ NMatch 1 (NLocalVar 0 "") 0 [(["n"], NLocalVar 1 ""), (["m"], NLocalVar 2 "")]
        norm `shouldBe` expected


      it "identifies the maximum number of captures" $ do
        let ast = Match (LitNumber 2) [
                    (PatSymbol "y" [PatVar "n", PatVar "o", PatVar "p"], Var "n"),
                    (PatSymbol "x" [PatVar "m", PatVar "l"], Var "m")
                  ]
        let (norm, ctable, _) = normalize ast
        let expectedCTable = [ CMatchData [
                               CCompoundSymbol 0 [CMatchVar 0, CMatchVar 1, CMatchVar 2],
                               CCompoundSymbol 1 [CMatchVar 0, CMatchVar 1]
                             ]]
        let expected = NLet (NLocalVar 0 "") (NNumber 2) $
                       NLet (NLocalVar 1 "") (NLambda [] ["n", "o", "p"] $ NAtom $ NVar $ NFunParam "n") $
                       NLet (NLocalVar 2 "") (NLambda [] ["m", "l"] $ NAtom $ NVar $ NFunParam "m") $
                       NAtom $ NMatch 3 (NLocalVar 0 "") 0 [(["n", "o", "p"], NLocalVar 1 ""), (["m", "l"], NLocalVar 2 "")]
        ctable `shouldBe` expectedCTable
        norm `shouldBe` expected


      it "resolves recursive use of an identifier" $ do
        let ast = LocalBinding (Binding "fun" $
                    Lambda ["a"] $ FunCall (Var "fun") [FunCall (Var "+") [Var "a", LitNumber 1]]) $
                  FunCall (Var "fun") [LitNumber 10]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar 0 "fun") (NLambda [] ["a"] $
                         NLet (NLocalVar 0 "") (NVar $ NConstantFreeVar "fun") $
                         NLet (NLocalVar 1 "") (NNumber 1) $
                         NLet (NLocalVar 2 "") (NPrimOp $ NPrimOpAdd (NFunParam "a") (NLocalVar 1 "")) $
                         NAtom $ NFunCall (NLocalVar 0 "") [NLocalVar 2 ""]) $
                       NLet (NLocalVar 1 "") (NNumber 10) $
                       NAtom $ NFunCall (NLocalVar 0 "fun") [NLocalVar 1 ""]
        norm `shouldBe` expected


      -- TODO forget about this for now
      it "resolves recursive use of an identifier in a closure" $ do
        let ast = LocalBinding (Binding "outer" $ Lambda ["b"] $
                    LocalBinding (Binding "fun" $
                      Lambda ["a"] $ FunCall (Var "fun") [FunCall (Var "+") [Var "a", Var "b"]]) $
                    FunCall (Var "fun") [LitNumber 10]) $
                  FunCall (Var "outer") [LitNumber 2]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar 0 "outer") (NLambda [] ["b"] $
                         NLet (NLocalVar 0 "fun") (NLambda ["b", "fun"] ["a"] $
                           -- recursive use of "fun" will call the same closure again
                           NLet (NLocalVar 0 "") (NVar $ NDynamicFreeVar "fun") $
                           NLet (NLocalVar 1 "") (NPrimOp $ NPrimOpAdd (NFunParam "a") (NDynamicFreeVar "b")) $
                           NAtom $ NFunCall (NLocalVar 0 "") [NLocalVar 1 ""]) $
                         NLet (NLocalVar 1 "") (NNumber 10) $
                         NAtom $ NFunCall (NLocalVar 0 "fun") [NLocalVar 1 ""]) $
                       NLet (NLocalVar 1 "") (NNumber 2) $
                       NAtom $ NFunCall (NLocalVar 0 "outer") [NLocalVar 1 ""]
        norm `shouldBe` expected

      it "pulls up new free variables into outer scopes" $ do
        let ast = LocalBinding (Binding "outer" $ Lambda ["b"] $
                    LocalBinding (Binding "fun" $ Lambda ["a"] $
                      LocalBinding (Binding "inner" $ Lambda ["x"] $
                        FunCall (Var "fun") [FunCall (Var "+") [Var "a", Var "b"]]) $
                      FunCall (Var "inner") [LitNumber 0]) $
                    FunCall (Var "fun") [LitNumber 10]) $
                  FunCall (Var "outer") [LitNumber 2]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar 0 "outer") (NLambda [] ["b"] $
                         NLet (NLocalVar 0 "fun") (NLambda ["b", "fun"] ["a"] $
                           NLet (NLocalVar 0 "inner") (NLambda ["b", "a", "fun"] ["x"] $
                             NLet (NLocalVar 0 "") (NVar $ NDynamicFreeVar "fun") $
                             NLet (NLocalVar 1 "") (NPrimOp $ NPrimOpAdd (NDynamicFreeVar "a") (NDynamicFreeVar "b")) $
                             NAtom $ NFunCall (NLocalVar 0 "") [NLocalVar 1 ""]) $
                           NLet (NLocalVar 1 "") (NNumber 0) $
                           NAtom $ NFunCall (NLocalVar 0 "inner") [NLocalVar 1 ""]) $
                         NLet (NLocalVar 1 "") (NNumber 10) $
                         NAtom $ NFunCall (NLocalVar 0 "fun") [NLocalVar 1 ""]) $
                       NLet (NLocalVar 1 "") (NNumber 2) $
                       NAtom $ NFunCall (NLocalVar 0 "outer") [NLocalVar 1 ""]
        norm `shouldBe` expected



      it "does not oversaturate a call to a known function" $ do
        let ast = LocalBinding (Binding "fun" $ Lambda ["a", "b"] $
                    Lambda ["c"] $ Lambda ["d", "e", "f"] $ LitNumber 42) $
                  FunCall (Var "fun") [LitNumber 1, LitNumber 2,
                                       LitNumber 33,
                                       LitNumber 444, LitNumber 555, LitNumber 666]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar 0 "fun") (NLambda [] ["a", "b"] $
                         NAtom $ (NLambda [] ["c"] $ NAtom $ (NLambda [] ["d", "e", "f"] $
                            NAtom $ (NNumber 42)))) $
                       NLet (NLocalVar 1 "") (NNumber 1) $
                       NLet (NLocalVar 2 "") (NNumber 2) $

                       NLet (NLocalVar 4 "") (NNumber 33) $
                       NLet (NLocalVar 5 "") (NNumber 444) $
                       NLet (NLocalVar 6 "") (NNumber 555) $
                       NLet (NLocalVar 7 "") (NNumber 666) $
                       NLet (NLocalVar 3 "") (NFunCall (NLocalVar 0 "fun") [NLocalVar 1 "", NLocalVar 2 ""]) $

                       -- The two returned functions are no known functions anymore, so generic apply needs to
                       -- deal with them at runtime
                       NAtom $ NFunCall (NLocalVar 3 "") [NLocalVar 4 "", NLocalVar 5 "", NLocalVar 6 "", NLocalVar 7 ""]
        norm `shouldBe` expected

{-
      -- TODO fix this (by giving all temp vars a name)
      it "does not oversaturate a call to a known anonymous function" $ do
        let ast = FunCall (Lambda ["a", "b"] $ Lambda ["c"] $ Lambda ["d", "e", "f"] $ LitNumber 42) $
                      [ LitNumber 1, LitNumber 2,
                        LitNumber 33,
                        LitNumber 444, LitNumber 555, LitNumber 666]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar 0 "") (NLambda [] ["a", "b"] $
                         NAtom $ (NLambda [] ["c"] $ NAtom $ (NLambda [] ["d", "e", "f"] $
                            NAtom $ (NNumber 42)))) $
                       NLet (NLocalVar 1 "") (NNumber 1) $
                       NLet (NLocalVar 2 "") (NNumber 2) $

                       NLet (NLocalVar 4 "") (NNumber 33) $
                       NLet (NLocalVar 5 "") (NNumber 444) $
                       NLet (NLocalVar 6 "") (NNumber 555) $
                       NLet (NLocalVar 7 "") (NNumber 666) $
                       NLet (NLocalVar 3 "") (NFunCall (NLocalVar 0 "") [NLocalVar 1 "", NLocalVar 2 ""]) $

                       -- The two returned functions are no known functions anymore, so generic apply needs to
                       -- deal with them at runtime
                       NAtom $ NFunCall (NLocalVar 3 "") [NLocalVar 4 "", NLocalVar 5 "", NLocalVar 6 "", NLocalVar 7 ""]
        norm `shouldBe` expected
-}

      it "identifies an under-saturated call to a known function" $ do
        let ast = LocalBinding (Binding "fun" $ Lambda ["a", "b", "c"] $
                                                LitNumber 42) $
                  FunCall (Var "fun") [LitNumber 1, LitNumber 2]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar 0 "fun") (NLambda [] ["a", "b", "c"] $
                            NAtom $ (NNumber 42)) $
                       NLet (NLocalVar 1 "") (NNumber 1) $
                       NLet (NLocalVar 2 "") (NNumber 2) $
                       NAtom $ NPartAp (NLocalVar 0 "fun") [NLocalVar 1 "", NLocalVar 2 ""]
        norm `shouldBe` expected


{-
 -    TODO test a lambda that becomes a closure only because of a recursive use of another closure
      it "resolves nested recursive lambdas that become closures by using recursion" $ do
        let code = "
              \ val outer (m) = \n\
              \   val counter (acc) = \n\
              \     val next = sub acc m \n\
              \   counter 9 \n\
              \ outer 3"
-}

      -- TODO a recursive closure which changes it's context (is that possible?)
