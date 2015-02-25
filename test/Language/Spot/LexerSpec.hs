module Language.Spot.LexerSpec where

import Language.Spot.Lexer as L

import Test.Hspec


spec :: Spec
spec = do
  describe "Lexer" $ do

    it "lexes a single newline" $ do
      L.lex "\n" `shouldBe` [EOL]

    it "combines several newlines into one token" $ do
      L.lex "\n\n\n\n\n" `shouldBe` [EOL]

--    it "ignores whitespace before EOF" $ do
--      (Lexer.lex "  \n \n      \n  \n") `shouldBe` [EOL, EOF]
