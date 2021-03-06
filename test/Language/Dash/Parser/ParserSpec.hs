module Language.Dash.Parser.ParserSpec where

import           Language.Dash.BuiltIn.BuiltInDefinitions
import           Language.Dash.Error.Error                (CompilationError (..))
import           Language.Dash.IR.Ast
import           Language.Dash.Parser.Lexer               as L
import           Language.Dash.Parser.Parser
import           Test.Hspec


shouldBeRight :: (Show a, Eq a) => Either CompilationError a -> a -> Expectation
shouldBeRight a b = a `shouldBe` Right b


-- TODO `Var "_"` should probably be `Wildcard`

parseString :: String -> Either CompilationError Expr
parseString str = do
  lexed <- L.lex str
  parse lexed

spec :: Spec
spec = do
  describe "Parser" $ do

    it "parses a symbol" $ do
      parseString ":dash" `shouldBeRight` LitSymbol "dash" []

    it "parses nothing" $ do
      parseString "" `shouldBeRight` LitSymbol "ok" []

    it "parses a single binding" $ do
      parseString "a = 1" `shouldBeRight`
        (LocalBinding (Binding "a" $ LitNumber 1) (LitSymbol "ok" []))

    it "parses a single function" $ do
      parseString "f a = a" `shouldBeRight`
        (LocalBinding (Binding "f" $ Lambda [Var "a"] (Var "a")) (LitSymbol "ok" []))

    it "parses an anonymous function" $ do
      parseString "a b -> add a b" `shouldBeRight`
        (Lambda [Var "a", Var "b"] $
          FunAp (Var "add") [Var "a", Var "b"])

    it "parses an anonymous function with one argument" $ do
      parseString "a -> add a 1" `shouldBeRight`
        (Lambda [Var "a"] $
          FunAp (Var "add") [Var "a", LitNumber 1])

    it "parses if-then-else as match" $ do
      parseString "if a then b else c" `shouldBeRight`
        (Match (Var "a") [(LitSymbol trueSymbolName [], Var "b"), (LitSymbol falseSymbolName [], Var "c")])

    it "parses an empty list" $ do
      parseString "[]" `shouldBeRight`
        LitSymbol listEmptySymbolName []


    it "parses a list" $ do
      parseString "[1, a, 2]" `shouldBeRight`
        LitSymbol listConsSymbolName [LitNumber 1,
            LitSymbol listConsSymbolName [Var "a",
            LitSymbol listConsSymbolName [LitNumber 2,
            LitSymbol listEmptySymbolName []]]]

    it "parses a record" $ do
      parseString "{ a = 1, b = \"two\" }" `shouldBeRight`
        LitSymbol recordSymbolName [LitSymbol "a" [], LitNumber 1, LitSymbol "b" [], LitString "two"]


    -- TODO it should be possible to "call" action2 without a parameter
    it "parses a do-expression" $ do
      let source = " do maybe with   \n\
                   \   action1 1 2    \n\
                   \   a <- action2 0   \n\
                   \   action3 a      \n\
                   \   return b       \n\
                   \ end"
      let parsed = parseString source
      parsed `shouldBeRight` (FunAp (Qualified "maybe" $ Var "bind") [
                            FunAp (Var "action1") [LitNumber 1, LitNumber 2],
                            (Lambda [Var "_"] $
                                FunAp (Qualified "maybe" $ Var "bind") [
                                    FunAp (Var "action2") [LitNumber 0],
                                    (Lambda [Var "a"] $
                                        FunAp (Qualified "maybe" $ Var "bind") [
                                           FunAp (Var "action3") [Var "a"],
                                           (Lambda [Var "_"] $
                                              FunAp (Qualified "maybe" $ Var "return") [Var "b"])])])])


    it "parses an assigned return statement" $ do
      let source = " do maybe with   \n\
                   \   action1 1 2    \n\
                   \   a <- return 0  \n\
                   \   action3 a      \n\
                   \   return b       \n\
                   \ end"
      let parsed = parseString source
      parsed `shouldBeRight` (FunAp (Qualified "maybe" $ Var "bind") [
                            FunAp (Var "action1") [LitNumber 1, LitNumber 2],
                            (Lambda [Var "_"] $
                                FunAp (Qualified "maybe" $ Var "bind") [
                                    FunAp (Qualified "maybe" $ Var "return") [LitNumber 0],
                                    (Lambda [Var "a"] $
                                        FunAp (Qualified "maybe" $ Var "bind") [
                                           FunAp (Var "action3") [Var "a"],
                                           (Lambda [Var "_"] $
                                              FunAp (Qualified "maybe" $ Var "return") [Var "b"])])])])


    it "parses an assignment from a variable" $ do
      let source = " do maybe with    \n\
                   \   a <- action2   \n\
                   \   action3 a      \n\
                   \   return b       \n\
                   \ end"
      let parsed = parseString source
      parsed `shouldBeRight` (FunAp (Qualified "maybe" $ Var "bind") [
                            Var "action2",
                            (Lambda [Var "a"] $
                                FunAp (Qualified "maybe" $ Var "bind") [
                                   FunAp (Var "action3") [Var "a"],
                                   (Lambda [Var "_"] $
                                      FunAp (Qualified "maybe" $ Var "return") [Var "b"])])])



    it "parses a binding before an assignment" $ do
      let source = " do maybe with    \n\
                   \   action1 1 2    \n\
                   \   x = 3          \n\
                   \   a <- action2   \n\
                   \   action3 a      \n\
                   \   return b       \n\
                   \ end"
      let parsed = parseString source
      parsed `shouldBeRight` (FunAp (Qualified "maybe" $ Var "bind") [
                            FunAp (Var "action1") [LitNumber 1, LitNumber 2],
                            (Lambda [Var "_"] $
                                FunAp (Qualified "maybe" $ Var "bind") [
                                    LocalBinding (Binding "x" $ LitNumber 3) (Var "action2"),
                                    (Lambda [Var "a"] $
                                        FunAp (Qualified "maybe" $ Var "bind") [
                                           FunAp (Var "action3") [Var "a"],
                                           (Lambda [Var "_"] $
                                              FunAp (Qualified "maybe" $ Var "return") [Var "b"])])])])


    it "parses a binding before a return" $ do
      let source = " do maybe with    \n\
                   \   action1 1 2    \n\
                   \   x = 3          \n\
                   \   return b       \n\
                   \ end"
      let parsed = parseString source
      parsed `shouldBeRight` (FunAp (Qualified "maybe" $ Var "bind") [
                            FunAp (Var "action1") [LitNumber 1, LitNumber 2],
                            (Lambda [Var "_"] $
                                LocalBinding (Binding "x" $ LitNumber 3) $
                                FunAp (Qualified "maybe" $ Var "return") [Var "b"])])


    it "inserts parentheses around interpolated strings" $ do
      let source = " print_line \"a \\(b) c\"\n"
      let parsed = parseString source
      parsed `shouldBeRight` (FunAp (Var "print_line") [
                            FunAp (Var bifStringConcatName) [
                              FunAp (Var bifStringConcatName) [
                                LitString "a ",
                                FunAp (Var bifToStringName) [Var "b"]],
                              LitString " c"]])


