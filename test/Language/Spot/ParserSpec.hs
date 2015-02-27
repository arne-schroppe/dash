module Language.Spot.ParserSpec where

import Language.Spot.Parser
import Language.Spot.Lexer as L
import Language.Spot.Ast

import Test.Hspec
import Debug.Trace

parse_string :: String -> Expr
parse_string = parse . L.lex

spec :: Spec
spec = do
  describe "Parser" $ do

    it "parses a symbol" $ do
      parse_string ":spot" `shouldBe` LitSymbol "spot" []

