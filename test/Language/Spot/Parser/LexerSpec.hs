module Language.Spot.Parser.LexerSpec where

import Language.Spot.Parser.Lexer as L

import Test.Hspec


spec :: Spec
spec = do
  describe "Lexer" $ do

    context "lexing whitespace and newlines" $ do

      it "lexes a single newline" $ do
        L.lex "\n" `shouldBe` [TEOL]

      it "always inserts an eol at the end" $ do
        L.lex "0\n1" `shouldBe` [TInt 0, TEOL, TInt 1, TEOL]

      it "doesn't insert an eol at the end if there already is one" $ do
        L.lex "0\n1\n" `shouldBe` [TInt 0, TEOL, TInt 1, TEOL]

      it "ignores whitespace before EOL" $ do
        L.lex "  \n \n  \t \t    \n  \n   " `shouldBe` [TEOL]

      it "lexes pure whitespace" $ do
        L.lex "  \t   \t " `shouldBe` [TEOL]

      it "combines several newlines into one token" $ do
        L.lex "\n\r\r\n\n\n\r\n" `shouldBe` [TEOL]

    context "lexing comments" $ do

      it "lexes an end-of-line comment" $ do
        L.lex ":a -- :ignored \n :b " `shouldBe` [TSymbol "a", TEOL, TSymbol "b", TEOL]

      it "lexes a delimiet comment" $ do
        L.lex ":a /-- :ignored --/ :b" `shouldBe` [TSymbol "a", TSymbol "b", TEOL]

      it "doesn't insert newlines inside a delimited comments" $ do
        L.lex ":a /-- \n \n :ignored \n --/ :b" `shouldBe` [TSymbol "a", TSymbol "b", TEOL]

      it "doesn't insert additional newlines around delimited comments" $ do
        L.lex ":a \n /-- \n \n :ignored \n --/ \n\n :b" `shouldBe` [TSymbol "a", TEOL, TSymbol "b", TEOL]

      it "doesn't insert additional newlines around several delimited comments" $ do
        L.lex ":a \n /-- bla --/ \n /-- \n \n :ignored \n --/ \n\n /-- \n\n\n --/ \n /-- bla --/ \n :b" `shouldBe` 
          [TSymbol "a", TEOL, TSymbol "b", TEOL]

      it "doesn't insert EOL if a delimited comment spans several lines but isn't surrounded by EOL" $ do
        L.lex ":a /-- \n --/ :b" `shouldBe` [TSymbol "a", TSymbol "b", TEOL]


    it "lexes a symbol" $ do
      L.lex " :spot " `shouldBe` [TSymbol "spot", TEOL]

    it "lexes a symbol containing hyphens" $ do
      L.lex " :finn-and-jake " `shouldBe` [TSymbol "finn-and-jake", TEOL]

    it "lexes an identifier" $ do
      L.lex " id " `shouldBe` [TId "id", TEOL]

    it "lexes an identifier containing hyphens" $ do
      L.lex "finn-and-jake" `shouldBe` [TId "finn-and-jake", TEOL]


