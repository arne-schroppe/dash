module Language.Dash.Normalization.NormalizationSpec where

import           Control.Applicative
import           Language.Dash.CodeGen.BuiltInDefinitions
import           Language.Dash.CodeGen.BuiltInDefinitions  (builtInSymbols)
import           Language.Dash.Internal.Error
import           Language.Dash.IR.Ast
import           Language.Dash.IR.Data
import           Language.Dash.IR.Nst
import           Language.Dash.Normalization.Normalization
import           Test.Hspec

pureNorm :: Expr -> Either CompilationError NstExpr
pureNorm ast =
  let resultOrError = normalize ast in
  (\ (norm, _, _) -> norm) <$> resultOrError

normAll :: Expr -> (Either CompilationError NstExpr, Either CompilationError ConstTable, Either CompilationError SymbolNameList)
normAll ast =
  let resultOrError = normalize ast in
  case resultOrError of
    Left err -> (Left err, Left err, Left err)
    Right (norm, ctable, syms) -> (Right norm, Right ctable, Right syms)

minUserSym :: Int
minUserSym = length builtInSymbols

shouldBeRight :: (Show a, Eq a) => Either CompilationError a -> a -> Expectation
shouldBeRight a b = a `shouldBe` Right b

-- local var name
lvn n = "$local" ++ (show n)

spec :: Spec
spec = do
  describe "Normalization" $ do

      it "normalizes a number directly" $ do
        let ast = LitNumber 3
        let norm = pureNorm ast
        norm `shouldBeRight` (NAtom $ NNumber 3)

      it "normalizes a simple symbol directly" $ do
        let numBuiltInSymbols = length builtInSymbols
        let ast = LitSymbol "Test" []
        let (norm, _, syms) = normAll ast
        (length <$> syms) `shouldBeRight` (numBuiltInSymbols + 1)
        syms `shouldBeRight` ((map fst builtInSymbols) ++ ["Test"])
        norm `shouldBeRight` (NAtom $ NPlainSymbol $ mkSymId (numBuiltInSymbols + 0))

      it "splits a complex addition operation" $ do
        let ast = FunAp (Var "+")
                      [(FunAp (Var "-")
                          [LitNumber 2, LitNumber 3]),
                       LitNumber 4]
        let norm = pureNorm ast
        let expected =
                NLet (NVar (lvn 0) NLocalVar) (NNumber 2) $
                NLet (NVar (lvn 1) NLocalVar) (NNumber 3) $
                NLet (NVar (lvn 2) NLocalVar) (NPrimOp $ NPrimOpSub (NVar (lvn 0) NLocalVar)  (NVar (lvn 1) NLocalVar)) $
                NLet (NVar (lvn 3) NLocalVar) (NNumber 4) $
                NAtom $ NPrimOp $ NPrimOpAdd (NVar (lvn 2) NLocalVar) (NVar (lvn 3) NLocalVar)
        norm `shouldBeRight` expected

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
                NLet (NVar "fun1" NLocalVar) (NLambda [] ["a", "b", "c", "d"] $ NAtom $ NNumber 0) $
                NLet (NVar "fun2" NLocalVar) (NLambda [] ["a", "b"] $ NAtom $ NNumber 0) $
                NLet (NVar (lvn 0) NLocalVar) (NNumber 1) $
                NLet (NVar (lvn 1) NLocalVar) (NNumber 2) $
                NLet (NVar (lvn 2) NLocalVar) (NFunAp (NVar "fun2" NLocalVar) [(NVar (lvn 0) NLocalVar), (NVar (lvn 1) NLocalVar)]) $
                NLet (NVar (lvn 3) NLocalVar) (NNumber 3) $
                NLet (NVar (lvn 4) NLocalVar) (NNumber 4) $
                NLet (NVar (lvn 5) NLocalVar) (NNumber 5) $
                NLet (NVar (lvn 6) NLocalVar) (NPrimOp $ NPrimOpAdd (NVar (lvn 4) NLocalVar) (NVar (lvn 5) NLocalVar)) $
                NLet (NVar (lvn 7) NLocalVar) (NNumber 6) $
                NAtom $ NFunAp (NVar "fun1" NLocalVar) [
                                  NVar (lvn 2) NLocalVar,
                                  NVar (lvn 3) NLocalVar,
                                  NVar (lvn 6) NLocalVar,
                                  NVar (lvn 7) NLocalVar]
        norm `shouldBeRight` expected

      it "normalizes a lambda call" $ do
        let ast = FunAp
                (Lambda ["a", "b"]
                    (LitNumber 5))
                [LitNumber 1, LitNumber 2]
        let norm = pureNorm ast
        let expected =
                NLet (NVar (lvn 0) NLocalVar) (NLambda [] ["a", "b"] (NAtom $ NNumber 5)) $
                NLet (NVar (lvn 1) NLocalVar) (NNumber 1) $
                NLet (NVar (lvn 2) NLocalVar) (NNumber 2) $
                NAtom $ NFunAp (NVar (lvn 0) NLocalVar) [NVar (lvn 1) NLocalVar, NVar (lvn 2) NLocalVar]
        norm `shouldBeRight` expected

      it "reuses named variables" $ do
        let ast = LocalBinding (Binding "x" $ LitNumber 3) $
                  FunAp (Var "+") [Var "x", Var "x"]
        let norm = pureNorm ast
        let expected =
                NLet (NVar "x" NLocalVar) (NNumber 3) $
                NAtom $ NPrimOp $ NPrimOpAdd (NVar "x" NLocalVar) (NVar "x" NLocalVar)
        norm `shouldBeRight` expected


      it "normalizes a returned lambda call" $ do
        let ast = LocalBinding (Binding "make-l" $ Lambda ["x"] $
                                Lambda ["y"] $ LitNumber 22 ) $
                  LocalBinding (Binding "l" $ FunAp (Var "make-l") [LitNumber 0]) $
                  FunAp (Var "l") [LitNumber 55]
        let norm = pureNorm ast
        let expected =
                NLet (NVar "make-l" NLocalVar) (NLambda [] ["x"] $ NAtom $
                    NLambda [] ["y"] $ NAtom $ NNumber 22) $
                NLet (NVar (lvn 0) NLocalVar) (NNumber 0) $
                NLet (NVar "l" NLocalVar) (NFunAp (NVar "make-l" NLocalVar) [NVar (lvn 0) NLocalVar]) $
                NLet (NVar (lvn 1) NLocalVar) (NNumber 55) $
                NAtom $ NFunAp (NVar "l" NLocalVar) [NVar (lvn 1) NLocalVar]
        norm `shouldBeRight` expected

      it "normalizes nested bindings" $ do
        let ast = LocalBinding (Binding "a" $
                    LocalBinding (Binding "b" $ LitNumber 22) $
                    FunAp (Var "+") [Var "b", LitNumber 4] ) $
                  FunAp (Var "-") [Var "a", LitNumber 55]
        let norm = pureNorm ast
        let expected =
                NLet (NVar (lvn 0) NLocalVar) (NNumber 4) $
                NLet (NVar "b" NLocalVar) (NNumber 22) $
                NLet (NVar "a" NLocalVar) (NPrimOp $ NPrimOpAdd (NVar "b" NLocalVar) (NVar (lvn 0) NLocalVar)) $
                NLet (NVar (lvn 1) NLocalVar) (NNumber 55) $
                NAtom $ NPrimOp $ NPrimOpSub (NVar "a" NLocalVar) (NVar (lvn 1) NLocalVar)
        norm `shouldBeRight` expected

      it "vars in tail position are referenced by number" $ do
        let ast = LocalBinding (Binding "a" $ LitNumber 55) $
                  Var "a"
        let norm = pureNorm ast
        let expected =
                NLet (NVar "a" NLocalVar) (NNumber 55) $
                NAtom $ NVarExpr $ NVar "a" NLocalVar
        norm `shouldBeRight` expected


      it "identifies constant free variables" $ do
        let ast = LocalBinding (Binding "b" (LitNumber 4)) $
                     Lambda ["a"] $ FunAp (Var "+") [Var "a", Var "b"]
        let norm = pureNorm ast
        let expected = NLet (NVar "b" NLocalVar) (NNumber 4) $
                       NAtom $ NLambda [] ["a"] $
                         NLet (NVar (lvn 0) NLocalVar) (NVarExpr $ NVar "b" NConstant) $
                         NAtom $ NPrimOp $ NPrimOpAdd (NVar "a" NFunParam) (NVar (lvn 0) NLocalVar)
        norm `shouldBeRight` expected

      it "identifies dynamic free variables" $ do
        let ast = Lambda ["b"] $
                     Lambda ["a"] $ FunAp (Var "+") [Var "a", Var "b"]
        let norm = pureNorm ast
        let expected = NAtom $ NLambda [] ["b"] $
                       NAtom $ NLambda ["b"] ["a"] $
                         NAtom $ NPrimOp $ NPrimOpAdd (NVar "a" NFunParam) (NVar "b" NFreeVar)
        norm `shouldBeRight` expected

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
                        NAtom $ NPrimOp $ NPrimOpAdd (NVar "a" NFreeVar) (NVar "b" NFreeVar)
        norm `shouldBeRight` expected


      it "normalizes match-bodies to match-branches" $ do
        let ast = Match (LitNumber 2) [
                    (PatNumber 1, LitNumber 33),
                    (PatNumber 2, LitNumber 44)
                  ]
        let norm = pureNorm ast
        let expected = NLet (NVar (lvn 0) NLocalVar) (NNumber 2) $
                       NLet (NVar (lvn 1) NLocalVar) (NMatchBranch [] [] $ NAtom $ NNumber 33) $
                       NLet (NVar (lvn 2) NLocalVar) (NMatchBranch [] [] $ NAtom $ NNumber 44) $
                       NAtom $ NMatch 0 (NVar (lvn 0) NLocalVar) (mkConstAddr 0)
                              [ ([], [], NVar (lvn 1) NLocalVar)
                              , ([], [], NVar (lvn 2) NLocalVar)]
        norm `shouldBeRight` expected


      it "captures constant free variables in match bodies" $ do
        let ast = LocalBinding (Binding "a" $ LitNumber 77) $
                  Match (LitNumber 2) [
                    (PatNumber 1, Var "a"),
                    (PatNumber 2, LitNumber 44)
                  ]
        let norm = pureNorm ast
        let expected = NLet (NVar "a" NLocalVar) (NNumber 77) $
                       NLet (NVar (lvn 0) NLocalVar) (NNumber 2) $
                       NLet (NVar (lvn 1) NLocalVar) (NMatchBranch [] [] $ NAtom $ NVarExpr $ NVar "a" NConstant) $
                       NLet (NVar (lvn 2) NLocalVar) (NMatchBranch [] [] $ NAtom $ NNumber 44) $
                       NAtom $ NMatch 0 (NVar (lvn 0) NLocalVar) (mkConstAddr 0)
                               [ ([], [], NVar (lvn 1) NLocalVar)
                               , ([], [], NVar (lvn 2) NLocalVar)]
        norm `shouldBeRight` expected

      it "captures dynamic free variables in match bodies" $ do
        let ast = Lambda ["a"] $
                  Match (LitNumber 2) [
                    (PatNumber 1, Var "a"),
                    (PatNumber 2, LitNumber 44)
                  ]
        let norm = pureNorm ast
        let expected = NAtom $ NLambda [] ["a"] $
                       NLet (NVar (lvn 0) NLocalVar) (NNumber 2) $
                       NLet (NVar (lvn 1) NLocalVar) (NMatchBranch ["a"] [] $ NAtom $ NVarExpr $ NVar "a" NFreeVar) $
                       NLet (NVar (lvn 2) NLocalVar) (NMatchBranch [] [] $ NAtom $ NNumber 44) $
                       NAtom $ NMatch 0 (NVar (lvn 0) NLocalVar) (mkConstAddr 0)
                               [ (["a"], [], NVar (lvn 1) NLocalVar)
                               , ([], [], NVar (lvn 2) NLocalVar)]
        norm `shouldBeRight` expected

      it "handles vars in patterns as lambda parameters" $ do
        let ast = Match (LitNumber 2) [
                    (PatVar "n", Var "n"),
                    (PatVar "m", Var "m")
                  ]
        let norm = pureNorm ast
        let expected = NLet (NVar (lvn 0) NLocalVar) (NNumber 2) $
                       NLet (NVar (lvn 1) NLocalVar) (NMatchBranch [] ["n"] $ NAtom $ NVarExpr $ NVar "n" NFunParam) $
                       NLet (NVar (lvn 2) NLocalVar) (NMatchBranch [] ["m"] $ NAtom $ NVarExpr $ NVar "m" NFunParam) $
                       NAtom $ NMatch 1 (NVar (lvn 0) NLocalVar) (mkConstAddr 0)
                               [ ([], ["n"], NVar (lvn 1) NLocalVar)
                               , ([], ["m"], NVar (lvn 2) NLocalVar)]
        norm `shouldBeRight` expected


      it "identifies the maximum number of captures" $ do
        let ast = Match (LitNumber 2) [
                    (PatSymbol "y" [PatVar "n", PatVar "o", PatVar "p"], Var "n"),
                    (PatSymbol "x" [PatVar "m", PatVar "l"], Var "m")
                  ]
        let (norm, ctable, _) = normAll ast
        let expectedCTable = [ CMatchData [
                               CCompoundSymbol (mkSymId minUserSym) [CMatchVar 0, CMatchVar 1, CMatchVar 2],
                               CCompoundSymbol (mkSymId (minUserSym + 1)) [CMatchVar 0, CMatchVar 1]
                             ]]
        let expected = NLet (NVar (lvn 0) NLocalVar) (NNumber 2) $
                       NLet (NVar (lvn 1) NLocalVar) (NMatchBranch [] ["n", "o", "p"] $ NAtom $ NVarExpr $ NVar "n" NFunParam) $
                       NLet (NVar (lvn 2) NLocalVar) (NMatchBranch [] ["m", "l"] $ NAtom $ NVarExpr $ NVar "m" NFunParam) $
                       NAtom $ NMatch 3 (NVar (lvn 0) NLocalVar) (mkConstAddr 0)
                               [ ([], ["n", "o", "p"], NVar (lvn 1) NLocalVar)
                               , ([], ["m", "l"], NVar (lvn 2) NLocalVar)]
        ctable `shouldBeRight` expectedCTable
        norm `shouldBeRight` expected


      it "resolves recursive use of an identifier" $ do
        let ast = LocalBinding (Binding "fun" $
                    Lambda ["a"] $ FunAp (Var "fun") [FunAp (Var "+") [Var "a", LitNumber 1]]) $
                  FunAp (Var "fun") [LitNumber 10]
        let norm = pureNorm ast
        let expected = NLet (NVar "fun" NLocalVar) (NLambda [] ["a"] $
                         NLet (NVar (lvn 0) NLocalVar) (NVarExpr $ NVar "fun" NConstant) $
                         NLet (NVar (lvn 1) NLocalVar) (NNumber 1) $
                         NLet (NVar (lvn 2) NLocalVar) (NPrimOp $ NPrimOpAdd (NVar "a" NFunParam) (NVar (lvn 1) NLocalVar)) $
                         NAtom $ NFunAp (NVar (lvn 0) NLocalVar) [NVar (lvn 2) NLocalVar]) $
                       NLet (NVar (lvn 3) NLocalVar) (NNumber 10) $
                       NAtom $ NFunAp (NVar "fun" NLocalVar) [NVar (lvn 3) NLocalVar]
        norm `shouldBeRight` expected


      -- TODO forget about this for now
      it "resolves recursive use of an identifier in a closure" $ do
        let ast = LocalBinding (Binding "outer" $ Lambda ["b"] $
                    LocalBinding (Binding "fun" $
                      Lambda ["a"] $ FunAp (Var "fun") [FunAp (Var "+") [Var "a", Var "b"]]) $
                    FunAp (Var "fun") [LitNumber 10]) $
                  FunAp (Var "outer") [LitNumber 2]
        let norm = pureNorm ast
        let expected = NLet (NVar "outer" NLocalVar) (NLambda [] ["b"] $
                         NLet (NVar "fun" NLocalVar) (NLambda ["b", "fun"] ["a"] $
                           -- recursive use of "fun" will call the same closure again
                           NLet (NVar (lvn 0) NLocalVar) (NVarExpr $ NVar "fun" NFreeVar) $
                           NLet (NVar (lvn 1) NLocalVar) (NPrimOp $ NPrimOpAdd (NVar "a" NFunParam) (NVar "b" NFreeVar)) $
                           NAtom $ NFunAp (NVar (lvn 0) NLocalVar) [NVar (lvn 1) NLocalVar]) $
                         NLet (NVar (lvn 2) NLocalVar) (NNumber 10) $
                         NAtom $ NFunAp (NVar "fun" NLocalVar) [NVar (lvn 2) NLocalVar]) $
                       NLet (NVar (lvn 3) NLocalVar) (NNumber 2) $
                       NAtom $ NFunAp (NVar "outer" NLocalVar) [NVar (lvn 3) NLocalVar]
        norm `shouldBeRight` expected

      it "pulls up new free variables into outer scopes" $ do
        let ast = LocalBinding (Binding "outer" $ Lambda ["b"] $
                    LocalBinding (Binding "fun" $ Lambda ["a"] $
                      LocalBinding (Binding "inner" $ Lambda ["x"] $
                        FunAp (Var "fun") [FunAp (Var "+") [Var "a", Var "b"]]) $
                      FunAp (Var "inner") [LitNumber 0]) $
                    FunAp (Var "fun") [LitNumber 10]) $
                  FunAp (Var "outer") [LitNumber 2]
        let norm = pureNorm ast
        let expected = NLet (NVar "outer" NLocalVar) (NLambda [] ["b"] $
                         NLet (NVar "fun" NLocalVar) (NLambda ["b", "fun"] ["a"] $
                           NLet (NVar "inner" NLocalVar) (NLambda ["b", "a", "fun"] ["x"] $
                             NLet (NVar (lvn 0) NLocalVar) (NVarExpr $ NVar "fun" NFreeVar) $
                             NLet (NVar (lvn 1) NLocalVar) (NPrimOp $ NPrimOpAdd (NVar "a" NFreeVar) (NVar "b" NFreeVar)) $
                             NAtom $ NFunAp (NVar (lvn 0) NLocalVar) [NVar (lvn 1) NLocalVar]) $
                           NLet (NVar (lvn 2) NLocalVar) (NNumber 0) $
                           NAtom $ NFunAp (NVar "inner" NLocalVar) [NVar (lvn 2) NLocalVar]) $
                         NLet (NVar (lvn 3) NLocalVar) (NNumber 10) $
                         NAtom $ NFunAp (NVar "fun" NLocalVar) [NVar (lvn 3) NLocalVar]) $
                       NLet (NVar (lvn 4) NLocalVar) (NNumber 2) $
                       NAtom $ NFunAp (NVar "outer" NLocalVar) [NVar (lvn 4) NLocalVar]
        norm `shouldBeRight` expected



      it "does not oversaturate a call to a known function" $ do
        let ast = LocalBinding (Binding "fun" $ Lambda ["a", "b"] $
                    Lambda ["c"] $ Lambda ["d", "e", "f"] $ LitNumber 42) $
                  FunAp (Var "fun") [LitNumber 1, LitNumber 2,
                                       LitNumber 33,
                                       LitNumber 444, LitNumber 555, LitNumber 666]
        let norm = pureNorm ast
        let expected = NLet (NVar "fun" NLocalVar) (NLambda [] ["a", "b"] $
                         NAtom $ (NLambda [] ["c"] $ NAtom $ (NLambda [] ["d", "e", "f"] $
                            NAtom $ (NNumber 42)))) $
                       NLet (NVar (lvn 0) NLocalVar) (NNumber 1) $
                       NLet (NVar (lvn 1) NLocalVar) (NNumber 2) $

                       NLet (NVar (lvn 3) NLocalVar) (NNumber 33) $
                       NLet (NVar (lvn 4) NLocalVar) (NNumber 444) $
                       NLet (NVar (lvn 5) NLocalVar) (NNumber 555) $
                       NLet (NVar (lvn 6) NLocalVar) (NNumber 666) $
                       NLet (NVar (lvn 2) NLocalVar) (NFunAp (NVar "fun" NLocalVar) [NVar (lvn 0) NLocalVar, NVar (lvn 1) NLocalVar]) $

                       -- The two returned functions are no known functions anymore, so generic apply needs to
                       -- deal with them at runtime
                       NAtom $ NFunAp (NVar (lvn 2) NLocalVar) [ NVar (lvn 3) NLocalVar
                                                            , NVar (lvn 4) NLocalVar
                                                            , NVar (lvn 5) NLocalVar
                                                            , NVar (lvn 6) NLocalVar]
        norm `shouldBeRight` expected


{- TODO fix this
      it "does not oversaturate a call to a known anonymous function" $ do
        let ast = FunAp (Lambda ["a", "b"] $ Lambda ["c"] $ Lambda ["d", "e", "f"] $ LitNumber 42) $
                      [ LitNumber 1, LitNumber 2,
                        LitNumber 33,
                        LitNumber 444, LitNumber 555, LitNumber 666]
        let norm = pureNorm ast
        let expected = NLet (NVar (lvn 0) NLocalVar) (NLambda [] ["a", "b"] $
                         NAtom $ (NLambda [] ["c"] $ NAtom $ (NLambda [] ["d", "e", "f"] $
                            NAtom $ (NNumber 42)))) $
                       NLet (NVar (lvn 1) NLocalVar) (NNumber 1) $
                       NLet (NVar (lvn 2) NLocalVar) (NNumber 2) $

                       NLet (NVar (lvn 3) NLocalVar) (NNumber 33) $
                       NLet (NVar (lvn 4) NLocalVar) (NNumber 444) $
                       NLet (NVar (lvn 5) NLocalVar) (NNumber 555) $
                       NLet (NVar (lvn 6) NLocalVar) (NNumber 666) $
                       NLet (NVar (lvn 7) NLocalVar) (NFunAp (NVar (lvn 0) NLocalVar) [NVar (lvn 1) NLocalVar, NVar (lvn 2) NLocalVar]) $

                       -- The two returned functions are no known functions anymore, so generic apply needs to
                       -- deal with them at runtime
                       NAtom $ NFunAp (NVar (lvn 7) NLocalVar) [NVar (lvn 4) NLocalVar, NVar (lvn 5) NLocalVar, NVar (lvn 6) NLocalVar, NVar (lvn 7) NLocalVar]
        norm `shouldBeRight` expected
-}

      it "identifies an under-saturated call to a known function" $ do
        let ast = LocalBinding (Binding "fun" $ Lambda ["a", "b", "c"] $
                                                LitNumber 42) $
                  FunAp (Var "fun") [LitNumber 1, LitNumber 2]
        let norm = pureNorm ast
        let expected = NLet (NVar "fun" NLocalVar) (NLambda [] ["a", "b", "c"] $
                            NAtom $ (NNumber 42)) $
                       NLet (NVar (lvn 0) NLocalVar) (NNumber 1) $
                       NLet (NVar (lvn 1) NLocalVar) (NNumber 2) $
                       NAtom $ NPartAp (NVar "fun" NLocalVar) [NVar (lvn 0) NLocalVar, NVar (lvn 1) NLocalVar]
        norm `shouldBeRight` expected


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
                  LocalBinding (Binding "t" $ LitSymbol trueSymbolName []) $
                  LocalBinding (Binding "f" $ LitSymbol falseSymbolName []) $
                  LitNumber 0
        let norm = pureNorm ast
        let expected = NLet (NVar "x" NLocalVar) (NPlainSymbol $ mkSymId minUserSym) $
                       NLet (NVar "y" NLocalVar) (NPlainSymbol $ mkSymId (minUserSym + 1)) $
                       NLet (NVar "t" NLocalVar) (NPlainSymbol $ mkSymId 1) $
                       NLet (NVar "f" NLocalVar) (NPlainSymbol $ mkSymId 0) $
                       NAtom $ NNumber 0
        norm `shouldBeRight` expected


      it "normalizes a module call" $ do
        let numBuiltInSymbols = length builtInSymbols
        let ast = LocalBinding (Binding "my-mod" $
                    Module [Binding "num" $ LitNumber 3]) $
                  FunAp (Qualified "my-mod" (Var "num")) [LitNumber 10, LitNumber 11]
        let norm = pureNorm ast
        let numFieldSym = mkSymId minUserSym
        let expected = NLet (NVar "my-mod" NLocalVar) (NModule [(numFieldSym, "num", NNumber 3)]) $
                       NLet (NVar (lvn 0) NLocalVar) (NPlainSymbol numFieldSym) $
                       NLet (NVar (lvn 1) NLocalVar) (NModuleLookup (NVar "my-mod" NLocalVar) (NVar (lvn 0) NLocalVar)) $
                       NLet (NVar (lvn 2) NLocalVar) (NNumber 10) $
                       NLet (NVar (lvn 3) NLocalVar) (NNumber 11) $
                       NAtom $ NFunAp (NVar (lvn 1) NLocalVar) [NVar (lvn 2) NLocalVar, NVar (lvn 3) NLocalVar]
        let (norm, _, syms) = normAll ast
        (length <$> syms) `shouldBeRight` (numBuiltInSymbols + 1)
        syms `shouldBeRight` ((map fst builtInSymbols) ++ ["num"])
        norm `shouldBeRight` expected


      it "normalizes a recursive module call" $ do
        let numBuiltInSymbols = length builtInSymbols
        let ast = LocalBinding (Binding "my-mod" $
                    Module [Binding "func" $ Lambda ["a"] $ FunAp (Var "func") [LitNumber 2] ]) $
                  FunAp (Qualified "my-mod" (Var "func")) [LitNumber 10]
        let norm = pureNorm ast
        let numFieldSym = mkSymId minUserSym
        let expected = NLet (NVar "my-mod" NLocalVar) (NModule [(numFieldSym, "func",
                               NLambda [] ["a"] $
                                NLet (NVar (lvn 0) NLocalVar) (NVarExpr $ NVar "func" NConstant) $
                                NLet (NVar (lvn 1) NLocalVar) (NNumber 2) $
                                NAtom $ NFunAp (NVar (lvn 0) NLocalVar) [NVar (lvn 1) NLocalVar])]) $
                       NLet (NVar (lvn 2) NLocalVar) (NPlainSymbol numFieldSym) $
                       NLet (NVar (lvn 3) NLocalVar) (NModuleLookup (NVar "my-mod" NLocalVar) (NVar (lvn 2) NLocalVar)) $
                       NLet (NVar (lvn 4) NLocalVar) (NNumber 10) $
                       NAtom $ NFunAp (NVar (lvn 3) NLocalVar) [NVar (lvn 4) NLocalVar]
        let (norm, _, syms) = normAll ast
        (length <$> syms) `shouldBeRight` (numBuiltInSymbols + 1)
        syms `shouldBeRight` ((map fst builtInSymbols) ++ ["func"])
        norm `shouldBeRight` expected

      -- TODO mutual recursion in module

