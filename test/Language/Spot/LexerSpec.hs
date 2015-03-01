module Language.Spot.LexerSpec where

import Language.Spot.Lexer as L

import Test.Hspec


spec :: Spec
spec = do
  describe "Lexer" $ do

    it "lexes a single newline" $ do
      L.lex "\n" `shouldBe` [TEOL]

    it "combines several newlines into one token" $ do
      L.lex "\n\r\r\n\n\n\r\n" `shouldBe` [TEOL]

    it "lexes a symbol" $ do
      L.lex " :spot " `shouldBe` [TSymbol "spot", TEOL]

    it "lexes an end-of-line comment" $ do
      L.lex ":a -- :ignored \n :b " `shouldBe` [TSymbol "a", TEOL, TSymbol "b", TEOL]

    it "lexes an identifier" $ do
      L.lex " id " `shouldBe` [TId "id", TEOL]


    it "always inserts an eol at the end" $ do
      L.lex "0\n1" `shouldBe` [TInt 0, TEOL, TInt 1, TEOL]

    it "doesn't insert an eol at the end if there already is one" $ do
      L.lex "0\n1\n" `shouldBe` [TInt 0, TEOL, TInt 1, TEOL]

--    it "ignores whitespace before EOF" $ do
--      (Lexer.lex "  \n \n      \n  \n") `shouldBe` [EOL, EOF]
