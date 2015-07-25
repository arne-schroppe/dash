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

-- local var name
lvn n = "$local" ++ (show n)

spec :: Spec
spec = do
  describe "Normalization" $ do

      it "normalizes a number directly" $ do
        let ast = LitNumber 3
        let norm = pureNorm ast
        norm `shouldBe` (NAtom $ NNumber 3)

      it "normalizes a simple symbol directly" $ do
        let builtInSymbols = ["false", "true"]
        let numBuiltInSymbols = length builtInSymbols
        let ast = LitSymbol "Test" []
        let (norm, _, syms) = normalize ast
        (length syms) `shouldBe` (numBuiltInSymbols + 1)
        syms `shouldBe` (builtInSymbols ++ ["Test"])
        norm `shouldBe` (NAtom $ NPlainSymbol $ mkSymId (numBuiltInSymbols + 0))

      it "splits a complex addition operation" $ do
        let ast = FunAp (Var "+")
                      [(FunAp (Var "-")
                          [LitNumber 2, LitNumber 3]),
                       LitNumber 4]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar $ lvn 0) (NNumber 2) $
                NLet (NLocalVar $ lvn 1) (NNumber 3) $
                NLet (NLocalVar $ lvn 2) (NPrimOp $ NPrimOpSub (NLocalVar $ lvn 0)  (NLocalVar $ lvn 1)) $
                NLet (NLocalVar $ lvn 3) (NNumber 4) $
                NAtom $ NPrimOp $ NPrimOpAdd (NLocalVar $ lvn 2) (NLocalVar $ lvn 3)
        norm `shouldBe` expected

      it "normalizes general function calls" $ do
        let ast = LocalBinding (Binding "fun1" $ Lambda ["a", "b", "c", "d"] $ LitNumber 0) $
                  LocalBinding (Binding "fun2" $ Lambda ["a", "b"] $ LitNumber 0) $
                  FunAp (Var "fun1")
                      [(FunAp (Var "fun2")
                          [LitNumber 1, LitNumber 2]),
                       LitNumber 3,
                       (FunAp (Var "+")
                          [LitNumber 4, LitNumber 5]),
                       LitNumber 6]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar "fun1") (NLambda [] ["a", "b", "c", "d"] $ NAtom $ NNumber 0) $
                NLet (NLocalVar "fun2") (NLambda [] ["a", "b"] $ NAtom $ NNumber 0) $
                NLet (NLocalVar $ lvn 0) (NNumber 1) $
                NLet (NLocalVar $ lvn 1) (NNumber 2) $
                NLet (NLocalVar $ lvn 2) (NFunAp (NLocalVar "fun2") [(NLocalVar $ lvn 0), (NLocalVar $ lvn 1)]) $
                NLet (NLocalVar $ lvn 3) (NNumber 3) $
                NLet (NLocalVar $ lvn 4) (NNumber 4) $
                NLet (NLocalVar $ lvn 5) (NNumber 5) $
                NLet (NLocalVar $ lvn 6) (NPrimOp $ NPrimOpAdd (NLocalVar $ lvn 4) (NLocalVar $ lvn 5)) $
                NLet (NLocalVar $ lvn 7) (NNumber 6) $
                NAtom $ NFunAp (NLocalVar "fun1") [
                                  NLocalVar $ lvn 2,
                                  NLocalVar $ lvn 3,
                                  NLocalVar $ lvn 6,
                                  NLocalVar $ lvn 7]
        norm `shouldBe` expected

      it "normalizes a lambda call" $ do
        let ast = FunAp
                (Lambda ["a", "b"]
                    (LitNumber 5))
                [LitNumber 1, LitNumber 2]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar $ lvn 0) (NLambda [] ["a", "b"] (NAtom $ NNumber 5)) $
                NLet (NLocalVar $ lvn 1) (NNumber 1) $
                NLet (NLocalVar $ lvn 2) (NNumber 2) $
                NAtom $ NFunAp (NLocalVar $ lvn 0) [NLocalVar $ lvn 1, NLocalVar $ lvn 2]
        norm `shouldBe` expected

      it "reuses named variables" $ do
        let ast = LocalBinding (Binding "x" $ LitNumber 3) $
                  FunAp (Var "+") [Var "x", Var "x"]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar "x") (NNumber 3) $
                NAtom $ NPrimOp $ NPrimOpAdd (NLocalVar "x") (NLocalVar "x")
        norm `shouldBe` expected


      it "normalizes a returned lambda call" $ do
        let ast = LocalBinding (Binding "make-l" $ Lambda ["x"] $
                                Lambda ["y"] $ LitNumber 22 ) $
                  LocalBinding (Binding "l" $ FunAp (Var "make-l") [LitNumber 0]) $
                  FunAp (Var "l") [LitNumber 55]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar "make-l") (NLambda [] ["x"] $ NAtom $
                    NLambda [] ["y"] $ NAtom $ NNumber 22) $
                NLet (NLocalVar $ lvn 0) (NNumber 0) $
                NLet (NLocalVar "l") (NFunAp (NLocalVar "make-l") [NLocalVar $ lvn 0]) $
                NLet (NLocalVar $ lvn 1) (NNumber 55) $
                NAtom $ NFunAp (NLocalVar "l") [NLocalVar $ lvn 1]
        norm `shouldBe` expected

      it "normalizes nested bindings" $ do
        let ast = LocalBinding (Binding "a" $
                    LocalBinding (Binding "b" $ LitNumber 22) $
                    FunAp (Var "+") [Var "b", LitNumber 4] ) $
                  FunAp (Var "-") [Var "a", LitNumber 55]
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar $ lvn 0) (NNumber 4) $
                NLet (NLocalVar "b") (NNumber 22) $
                NLet (NLocalVar "a") (NPrimOp $ NPrimOpAdd (NLocalVar "b") (NLocalVar $ lvn 0)) $
                NLet (NLocalVar $ lvn 1) (NNumber 55) $
                NAtom $ NPrimOp $ NPrimOpSub (NLocalVar "a") (NLocalVar $ lvn 1)
        norm `shouldBe` expected

      it "vars in tail position are referenced by number" $ do
        let ast = LocalBinding (Binding "a" $ LitNumber 55) $
                  Var "a"
        let norm = pureNorm ast
        let expected =
                NLet (NLocalVar "a") (NNumber 55) $
                NAtom $ NVar $ NLocalVar "a"
        norm `shouldBe` expected


      it "identifies constant free variables" $ do
        let ast = LocalBinding (Binding "b" (LitNumber 4)) $
                     Lambda ["a"] $ FunAp (Var "+") [Var "a", Var "b"]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar "b") (NNumber 4) $
                       NAtom $ NLambda [] ["a"] $
                         NLet (NLocalVar $ lvn 0) (NVar $ NConstantFreeVar "b") $
                         NAtom $ NPrimOp $ NPrimOpAdd (NFunParam "a") (NLocalVar $ lvn 0)
        norm `shouldBe` expected

      it "identifies dynamic free variables" $ do
        let ast = Lambda ["b"] $
                     Lambda ["a"] $ FunAp (Var "+") [Var "a", Var "b"]
        let norm = pureNorm ast
        let expected = NAtom $ NLambda [] ["b"] $
                       NAtom $ NLambda ["b"] ["a"] $
                         NAtom $ NPrimOp $ NPrimOpAdd (NFunParam "a") (NDynamicFreeVar "b")
        norm `shouldBe` expected

      it "identifies nested closures" $ do
        let ast = Lambda ["a"] $
                  Lambda ["b"] $
                  Lambda ["c"] $
                  Lambda ["d"] $ FunAp (Var "+") [Var "a", Var "b"]
        let norm = pureNorm ast
        let expected = NAtom $ NLambda [] ["a"] $
                       NAtom $ NLambda ["a"] ["b"] $
                       NAtom $ NLambda ["b", "a"] ["c"] $
                       NAtom $ NLambda ["b", "a"] ["d"] $
                        NAtom $ NPrimOp $ NPrimOpAdd (NDynamicFreeVar "a") (NDynamicFreeVar "b")
        norm `shouldBe` expected


      it "normalizes match-bodies to match-branches" $ do
        let ast = Match (LitNumber 2) [
                    (PatNumber 1, LitNumber 33),
                    (PatNumber 2, LitNumber 44)
                  ]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar $ lvn 0) (NNumber 2) $
                       NLet (NLocalVar $ lvn 1) (NMatchBranch [] [] $ NAtom $ NNumber 33) $
                       NLet (NLocalVar $ lvn 2) (NMatchBranch [] [] $ NAtom $ NNumber 44) $
                       NAtom $ NMatch 0 (NLocalVar $ lvn 0) (mkConstAddr 0) 
                              [ ([], [], NLocalVar $ lvn 1)
                              , ([], [], NLocalVar $ lvn 2)]
        norm `shouldBe` expected


      it "captures constant free variables in match bodies" $ do
        let ast = LocalBinding (Binding "a" $ LitNumber 77) $
                  Match (LitNumber 2) [
                    (PatNumber 1, Var "a"),
                    (PatNumber 2, LitNumber 44)
                  ]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar "a") (NNumber 77) $
                       NLet (NLocalVar $ lvn 0) (NNumber 2) $
                       NLet (NLocalVar $ lvn 1) (NMatchBranch [] [] $ NAtom $ NVar $ NConstantFreeVar "a") $
                       NLet (NLocalVar $ lvn 2) (NMatchBranch [] [] $ NAtom $ NNumber 44) $
                       NAtom $ NMatch 0 (NLocalVar $ lvn 0) (mkConstAddr 0)
                               [ ([], [], NLocalVar $ lvn 1)
                               , ([], [], NLocalVar $ lvn 2)]
        norm `shouldBe` expected

      it "captures dynamic free variables in match bodies" $ do
        let ast = Lambda ["a"] $
                  Match (LitNumber 2) [
                    (PatNumber 1, Var "a"),
                    (PatNumber 2, LitNumber 44)
                  ]
        let norm = pureNorm ast
        let expected = NAtom $ NLambda [] ["a"] $
                       NLet (NLocalVar $ lvn 0) (NNumber 2) $
                       NLet (NLocalVar $ lvn 1) (NMatchBranch ["a"] [] $ NAtom $ NVar $ NDynamicFreeVar "a") $
                       NLet (NLocalVar $ lvn 2) (NMatchBranch [] [] $ NAtom $ NNumber 44) $
                       NAtom $ NMatch 0 (NLocalVar $ lvn 0) (mkConstAddr 0)
                               [ (["a"], [], NLocalVar $ lvn 1)
                               , ([], [], NLocalVar $ lvn 2)]
        norm `shouldBe` expected

      it "handles vars in patterns as lambda parameters" $ do
        let ast = Match (LitNumber 2) [
                    (PatVar "n", Var "n"),
                    (PatVar "m", Var "m")
                  ]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar $ lvn 0) (NNumber 2) $
                       NLet (NLocalVar $ lvn 1) (NMatchBranch [] ["n"] $ NAtom $ NVar $ NFunParam "n") $
                       NLet (NLocalVar $ lvn 2) (NMatchBranch [] ["m"] $ NAtom $ NVar $ NFunParam "m") $
                       NAtom $ NMatch 1 (NLocalVar $ lvn 0) (mkConstAddr 0) 
                               [ ([], ["n"], NLocalVar $ lvn 1)
                               , ([], ["m"], NLocalVar $ lvn 2)]
        norm `shouldBe` expected


      it "identifies the maximum number of captures" $ do
        let ast = Match (LitNumber 2) [
                    (PatSymbol "y" [PatVar "n", PatVar "o", PatVar "p"], Var "n"),
                    (PatSymbol "x" [PatVar "m", PatVar "l"], Var "m")
                  ]
        let (norm, ctable, _) = normalize ast
        let expectedCTable = [ CMatchData [
                               CCompoundSymbol (mkSymId 2) [CMatchVar 0, CMatchVar 1, CMatchVar 2],
                               CCompoundSymbol (mkSymId 3) [CMatchVar 0, CMatchVar 1]
                             ]]
        let expected = NLet (NLocalVar $ lvn 0) (NNumber 2) $
                       NLet (NLocalVar $ lvn 1) (NMatchBranch [] ["n", "o", "p"] $ NAtom $ NVar $ NFunParam "n") $
                       NLet (NLocalVar $ lvn 2) (NMatchBranch [] ["m", "l"] $ NAtom $ NVar $ NFunParam "m") $
                       NAtom $ NMatch 3 (NLocalVar $ lvn 0) (mkConstAddr 0) 
                               [ ([], ["n", "o", "p"], NLocalVar $ lvn 1)
                               , ([], ["m", "l"], NLocalVar $ lvn 2)]
        ctable `shouldBe` expectedCTable
        norm `shouldBe` expected


      it "resolves recursive use of an identifier" $ do
        let ast = LocalBinding (Binding "fun" $
                    Lambda ["a"] $ FunAp (Var "fun") [FunAp (Var "+") [Var "a", LitNumber 1]]) $
                  FunAp (Var "fun") [LitNumber 10]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar "fun") (NLambda [] ["a"] $
                         NLet (NLocalVar $ lvn 0) (NVar $ NConstantFreeVar "fun") $
                         NLet (NLocalVar $ lvn 1) (NNumber 1) $
                         NLet (NLocalVar $ lvn 2) (NPrimOp $ NPrimOpAdd (NFunParam "a") (NLocalVar $ lvn 1)) $
                         NAtom $ NFunAp (NLocalVar $ lvn 0) [NLocalVar $ lvn 2]) $
                       NLet (NLocalVar $ lvn 3) (NNumber 10) $
                       NAtom $ NFunAp (NLocalVar "fun") [NLocalVar $ lvn 3]
        norm `shouldBe` expected


      -- TODO forget about this for now
      it "resolves recursive use of an identifier in a closure" $ do
        let ast = LocalBinding (Binding "outer" $ Lambda ["b"] $
                    LocalBinding (Binding "fun" $
                      Lambda ["a"] $ FunAp (Var "fun") [FunAp (Var "+") [Var "a", Var "b"]]) $
                    FunAp (Var "fun") [LitNumber 10]) $
                  FunAp (Var "outer") [LitNumber 2]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar "outer") (NLambda [] ["b"] $
                         NLet (NLocalVar "fun") (NLambda ["b", "fun"] ["a"] $
                           -- recursive use of "fun" will call the same closure again
                           NLet (NLocalVar $ lvn 0) (NVar $ NDynamicFreeVar "fun") $
                           NLet (NLocalVar $ lvn 1) (NPrimOp $ NPrimOpAdd (NFunParam "a") (NDynamicFreeVar "b")) $
                           NAtom $ NFunAp (NLocalVar $ lvn 0) [NLocalVar $ lvn 1]) $
                         NLet (NLocalVar $ lvn 2) (NNumber 10) $
                         NAtom $ NFunAp (NLocalVar "fun") [NLocalVar $ lvn 2]) $
                       NLet (NLocalVar $ lvn 3) (NNumber 2) $
                       NAtom $ NFunAp (NLocalVar "outer") [NLocalVar $ lvn 3]
        norm `shouldBe` expected

      it "pulls up new free variables into outer scopes" $ do
        let ast = LocalBinding (Binding "outer" $ Lambda ["b"] $
                    LocalBinding (Binding "fun" $ Lambda ["a"] $
                      LocalBinding (Binding "inner" $ Lambda ["x"] $
                        FunAp (Var "fun") [FunAp (Var "+") [Var "a", Var "b"]]) $
                      FunAp (Var "inner") [LitNumber 0]) $
                    FunAp (Var "fun") [LitNumber 10]) $
                  FunAp (Var "outer") [LitNumber 2]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar "outer") (NLambda [] ["b"] $
                         NLet (NLocalVar "fun") (NLambda ["b", "fun"] ["a"] $
                           NLet (NLocalVar "inner") (NLambda ["b", "a", "fun"] ["x"] $
                             NLet (NLocalVar $ lvn 0) (NVar $ NDynamicFreeVar "fun") $
                             NLet (NLocalVar $ lvn 1) (NPrimOp $ NPrimOpAdd (NDynamicFreeVar "a") (NDynamicFreeVar "b")) $
                             NAtom $ NFunAp (NLocalVar $ lvn 0) [NLocalVar $ lvn 1]) $
                           NLet (NLocalVar $ lvn 2) (NNumber 0) $
                           NAtom $ NFunAp (NLocalVar "inner") [NLocalVar $ lvn 2]) $
                         NLet (NLocalVar $ lvn 3) (NNumber 10) $
                         NAtom $ NFunAp (NLocalVar "fun") [NLocalVar $ lvn 3]) $
                       NLet (NLocalVar $ lvn 4) (NNumber 2) $
                       NAtom $ NFunAp (NLocalVar "outer") [NLocalVar $ lvn 4]
        norm `shouldBe` expected



      it "does not oversaturate a call to a known function" $ do
        let ast = LocalBinding (Binding "fun" $ Lambda ["a", "b"] $
                    Lambda ["c"] $ Lambda ["d", "e", "f"] $ LitNumber 42) $
                  FunAp (Var "fun") [LitNumber 1, LitNumber 2,
                                       LitNumber 33,
                                       LitNumber 444, LitNumber 555, LitNumber 666]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar "fun") (NLambda [] ["a", "b"] $
                         NAtom $ (NLambda [] ["c"] $ NAtom $ (NLambda [] ["d", "e", "f"] $
                            NAtom $ (NNumber 42)))) $
                       NLet (NLocalVar $ lvn 0) (NNumber 1) $
                       NLet (NLocalVar $ lvn 1) (NNumber 2) $

                       NLet (NLocalVar $ lvn 3) (NNumber 33) $
                       NLet (NLocalVar $ lvn 4) (NNumber 444) $
                       NLet (NLocalVar $ lvn 5) (NNumber 555) $
                       NLet (NLocalVar $ lvn 6) (NNumber 666) $
                       NLet (NLocalVar $ lvn 2) (NFunAp (NLocalVar "fun") [NLocalVar $ lvn 0, NLocalVar $ lvn 1]) $

                       -- The two returned functions are no known functions anymore, so generic apply needs to
                       -- deal with them at runtime
                       NAtom $ NFunAp (NLocalVar $ lvn 2) [ NLocalVar $ lvn 3
                                                            , NLocalVar $ lvn 4
                                                            , NLocalVar $ lvn 5
                                                            , NLocalVar $ lvn 6]
        norm `shouldBe` expected

{-
      -- TODO fix this (by giving all temp vars a name)
      it "does not oversaturate a call to a known anonymous function" $ do
        let ast = FunAp (Lambda ["a", "b"] $ Lambda ["c"] $ Lambda ["d", "e", "f"] $ LitNumber 42) $
                      [ LitNumber 1, LitNumber 2,
                        LitNumber 33,
                        LitNumber 444, LitNumber 555, LitNumber 666]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar "") (NLambda [] ["a", "b"] $
                         NAtom $ (NLambda [] ["c"] $ NAtom $ (NLambda [] ["d", "e", "f"] $
                            NAtom $ (NNumber 42)))) $
                       NLet (NLocalVar "") (NNumber 1) $
                       NLet (NLocalVar "") (NNumber 2) $

                       NLet (NLocalVar "") (NNumber 33) $
                       NLet (NLocalVar "") (NNumber 444) $
                       NLet (NLocalVar "") (NNumber 555) $
                       NLet (NLocalVar "") (NNumber 666) $
                       NLet (NLocalVar "") (NFunAp (NLocalVar "") [NLocalVar "", NLocalVar ""]) $

                       -- The two returned functions are no known functions anymore, so generic apply needs to
                       -- deal with them at runtime
                       NAtom $ NFunAp (NLocalVar "") [NLocalVar "", NLocalVar "", NLocalVar "", NLocalVar ""]
        norm `shouldBe` expected
-}

      it "identifies an under-saturated call to a known function" $ do
        let ast = LocalBinding (Binding "fun" $ Lambda ["a", "b", "c"] $
                                                LitNumber 42) $
                  FunAp (Var "fun") [LitNumber 1, LitNumber 2]
        let norm = pureNorm ast
        let expected = NLet (NLocalVar "fun") (NLambda [] ["a", "b", "c"] $
                            NAtom $ (NNumber 42)) $
                       NLet (NLocalVar $ lvn 0) (NNumber 1) $
                       NLet (NLocalVar $ lvn 1) (NNumber 2) $
                       NAtom $ NPartAp (NLocalVar "fun") [NLocalVar $ lvn 0, NLocalVar $ lvn 1]
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


      it "has :true and :false as built-in symbols" $ do
        let ast = LocalBinding (Binding "x" $ LitSymbol "a" []) $
                  LocalBinding (Binding "y" $ LitSymbol "b" []) $
                  LocalBinding (Binding "t" $ LitSymbol "true" []) $
                  LocalBinding (Binding "f" $ LitSymbol "false" []) $
                  LitNumber 0
        let norm = pureNorm ast
        let expected = NLet (NLocalVar "x") (NPlainSymbol $ mkSymId 2) $
                       NLet (NLocalVar "y") (NPlainSymbol $ mkSymId 3) $
                       NLet (NLocalVar "t") (NPlainSymbol $ mkSymId 1) $
                       NLet (NLocalVar "f") (NPlainSymbol $ mkSymId 0) $
                       NAtom $ NNumber 0
        norm `shouldBe` expected
