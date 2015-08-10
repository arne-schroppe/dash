module Language.Dash.Parser.ParserSpec where

import           Language.Dash.IR.Ast
import           Language.Dash.Constants
import           Language.Dash.Parser.Lexer  as L
import           Language.Dash.Parser.Parser
import           Test.Hspec

import           Debug.Trace

parse_string :: String -> Expr
parse_string = parse . L.lex

spec :: Spec
spec = do
  describe "Parser" $ do

    it "parses a symbol" $ do
      parse_string ":dash" `shouldBe` LitSymbol "dash" []


    it "parses an anonymous function" $ do
      parse_string ".\\ a = add a 1" `shouldBe`
        (Lambda ["a"] $
          FunAp (Var "add") [Var "a", LitNumber 1])

    it "parses if-then-else as match" $ do
      parse_string "if a then b else c" `shouldBe`
        (Match (Var "a") [(PatSymbol trueSymbolId [], Var "b"), (PatSymbol falseSymbolId [], Var "c")])

    it "parses an empty list" $ do
      parse_string "[]" `shouldBe`
        LitSymbol listEmptySymbolId []

    it "parses a list" $ do
      parse_string "[1, a, 2]" `shouldBe`
        LitSymbol listConsSymbolId [LitNumber 1,
            LitSymbol listConsSymbolId [Var "a",
            LitSymbol listConsSymbolId [LitNumber 2,
            LitSymbol listEmptySymbolId []]]]

    -- TODO it should be possible to "call" action2 without a parameter
    it "parses a do-expression" $ do
      let source = " do maybe begin   \n\
                   \   action1 1 2    \n\
                   \   a <- action2 0   \n\
                   \   action3 a      \n\
                   \   return b       \n\
                   \ end"
      let parsed = parse_string source
      parsed `shouldBe` (FunAp (Var "maybe-bind") [
                            FunAp (Var "action1") [LitNumber 1, LitNumber 2],
                            (Lambda ["_"] $
                                FunAp (Var "maybe-bind") [
                                    FunAp (Var "action2") [LitNumber 0],
                                    (Lambda ["a"] $ 
                                        FunAp (Var "maybe-bind") [
                                           FunAp (Var "action3") [Var "a"],
                                           (Lambda ["_"] $
                                              FunAp (Var "maybe-return") [Var "b"])])])])


    it "parses a do-expression 2" $ do
      let source = " do maybe begin   \n\
                   \   action1 1 2    \n\
                   \   a <- return 0  \n\
                   \   action3 a      \n\
                   \   return b       \n\
                   \ end"
      let parsed = parse_string source
      parsed `shouldBe` (FunAp (Var "maybe-bind") [
                            FunAp (Var "action1") [LitNumber 1, LitNumber 2],
                            (Lambda ["_"] $
                                FunAp (Var "maybe-bind") [
                                    FunAp (Var "maybe-return") [LitNumber 0],
                                    (Lambda ["a"] $ 
                                        FunAp (Var "maybe-bind") [
                                           FunAp (Var "action3") [Var "a"],
                                           (Lambda ["_"] $
                                              FunAp (Var "maybe-return") [Var "b"])])])])


    it "parses a do-expression 3" $ do
      let source = " do maybe begin   \n\
                   \   action1 1 2    \n\
                   \   a <- action2   \n\
                   \   action3 a      \n\
                   \   return b       \n\
                   \ end"
      let parsed = parse_string source
      parsed `shouldBe` (FunAp (Var "maybe-bind") [
                            FunAp (Var "action1") [LitNumber 1, LitNumber 2],
                            (Lambda ["_"] $
                                FunAp (Var "maybe-bind") [
                                    Var "action2",
                                    (Lambda ["a"] $ 
                                        FunAp (Var "maybe-bind") [
                                           FunAp (Var "action3") [Var "a"],
                                           (Lambda ["_"] $
                                              FunAp (Var "maybe-return") [Var "b"])])])])



