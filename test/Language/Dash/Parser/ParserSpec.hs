module Language.Dash.Parser.ParserSpec where

import           Language.Dash.IR.Ast
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
        (Match (Var "a") [(PatSymbol "true" [], Var "b"), (PatSymbol "false" [], Var "c")])
