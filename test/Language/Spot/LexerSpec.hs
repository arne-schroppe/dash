module Language.Spot.LexerSpec where

import Language.Spot.Lexer as L

import Test.Hspec


spec :: Spec
spec = do
  describe "Lexer" $ do

    it "lexes a single newline" $ do
      L.lex "\n" `shouldBe` [EOL]

    it "combines several newlines into one token" $ do
      L.lex "\n\r\r\n\n\n\r\n" `shouldBe` [EOL]

    it "lexes a symbol" $ do
      L.lex " :spot " `shouldBe` [Symbol "spot"]

    it "lexes an end-of-line comment" $ do
      L.lex ":a -- :ignored \n :b " `shouldBe` [Symbol "a", EOL, Symbol "b"]

    it "lexes an identifier" $ do
      L.lex " id " `shouldBe` [Id "id"]

--    it "ignores whitespace before EOF" $ do
--      (Lexer.lex "  \n \n      \n  \n") `shouldBe` [EOL, EOF]
