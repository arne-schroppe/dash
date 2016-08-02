module Language.Dash.Parser.LexerSpec where

import           Language.Dash.Error.Error  (CompilationError (..))
import           Language.Dash.Parser.Lexer as L
import           Test.Hspec

shouldBeRight :: (Show a, Eq a) => Either CompilationError a -> a -> Expectation
shouldBeRight a b = a `shouldBe` Right b

spec :: Spec
spec = do
  describe "Lexer" $ do

    context "lexing whitespace and newlines" $ do

      it "lexes a single newline" $ do
        L.lex "\n" `shouldBeRight` [TEOL, TEOF]

      it "always inserts an eol at the end" $ do
        L.lex "0\n1" `shouldBeRight` [TInt 0, TEOL, TInt 1, TEOL, TEOF]

      it "doesn't insert an eol at the end if there already is one" $ do
        L.lex "0\n1\n" `shouldBeRight` [TInt 0, TEOL, TInt 1, TEOL, TEOF]

      it "ignores whitespace before EOL" $ do
        L.lex "  \n \n  \t \t    \n  \n   " `shouldBeRight` [TEOL, TEOF]

      it "lexes pure whitespace" $ do
        L.lex "  \t   \t " `shouldBeRight` [TEOL, TEOF]

      it "combines several newlines into one token" $ do
        L.lex "\n\r\r\n\n\n\r\n" `shouldBeRight` [TEOL, TEOF]

      it "lexes a newline sequence" $ do
        L.lex "\"dash\\n\"" `shouldBeRight` [TString "dash\n", TEOL, TEOF]

    context "lexing comments" $ do

      it "lexes an end-of-line comment" $ do
        L.lex ":a -- :ignored \n :b " `shouldBeRight` [TSymbol "a", TEOL, TSymbol "b", TEOL, TEOF]

      it "lexes a delimiet comment" $ do
        L.lex ":a /-- :ignored --/ :b" `shouldBeRight` [TSymbol "a", TSymbol "b", TEOL, TEOF]

      it "doesn't insert newlines inside a delimited comments" $ do
        L.lex ":a /-- \n \n :ignored \n --/ :b" `shouldBeRight` [TSymbol "a", TSymbol "b", TEOL, TEOF]

      it "doesn't insert additional newlines around delimited comments" $ do
        L.lex ":a \n /-- \n \n :ignored \n --/ \n\n :b" `shouldBeRight` [TSymbol "a", TEOL, TSymbol "b", TEOL, TEOF]

      it "doesn't insert additional newlines around several delimited comments" $ do
        L.lex ":a \n /-- bla --/ \n /-- \n \n :ignored \n --/ \n\n /-- \n\n\n --/ \n /-- bla --/ \n :b" `shouldBeRight`
          [TSymbol "a", TEOL, TSymbol "b", TEOL, TEOF]

      it "doesn't insert EOL if a delimited comment spans several lines but isn't surrounded by EOL" $ do
        L.lex ":a /-- \n --/ :b" `shouldBeRight` [TSymbol "a", TSymbol "b", TEOL, TEOF]

      it "doesn't eat content in between delimited comments" $ do
        L.lex "/--\n--/\n :hello\n /--\n--/\n :goodbye" `shouldBeRight` [TEOL, TSymbol "hello", TEOL, TSymbol "goodbye", TEOL, TEOF]


    it "lexes a symbol" $ do
      L.lex " :dash " `shouldBeRight` [TSymbol "dash", TEOL, TEOF]

    it "lexes a symbol containing hyphens" $ do
      L.lex " :finn-and-jake " `shouldBeRight` [TSymbol "finn-and-jake", TEOL, TEOF]

    it "lexes an identifier" $ do
      L.lex " id " `shouldBeRight` [TId "id", TEOL, TEOF]

    it "lexes an identifier containing hyphens" $ do
      L.lex "finn-and-jake" `shouldBeRight` [TId "finn-and-jake", TEOL, TEOF]


