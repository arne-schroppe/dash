module LexerSpec where

import Parsing.Lexer as Lexer

import Test.Hspec


spec :: Spec
spec = do
  describe "Lexer" $ do

    it "lexes a single newline" $ do
      Lexer.lex "\n" `shouldBe` [EOL]

    it "combines several newlines into one token" $ do
      Lexer.lex "\n\n\n\n\n" `shouldBe` [EOL]

--    it "ignores whitespace before EOF" $ do
--      (Lexer.lex "  \n \n      \n  \n") `shouldBe` [EOL, EOF]
