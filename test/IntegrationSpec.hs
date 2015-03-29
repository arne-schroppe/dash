module IntegrationSpec where

import Test.Hspec

-- This is mainly a test of the code generator. But it is an integration test because
-- we don't care about the instructions it churns out, as long as everything behaves as expected.

import Language.Spot.API
import Language.Spot.VM.Bits

import Numeric


spec :: Spec
spec = do
  describe "Spot" $ do

{-

    it "evaluates an integer" $ do
      let result = run "4815"
      result `shouldReturn` VMNumber 4815

    it "evaluates a symbol" $ do
      let result = run ":spot"
      result `shouldReturn` VMSymbol "spot" []

    it "applies built-in add function" $ do
      let result = run "add 2 3"
      result `shouldReturn` VMNumber 5

    it "applies built-in subtract function" $ do
      let result = run "sub 7 3"
      result `shouldReturn` VMNumber 4

    it "interprets a symbol with values" $ do
      let result = run ":sym 2 3"
      result `shouldReturn` VMSymbol "sym" [VMNumber 2, VMNumber 3]

    it "stores a value in a variable" $ do
      let result = run " val a = 4\n\
                       \ a"
      result `shouldReturn` VMNumber 4

    it "uses local bindings in function call" $ do
      let code = " val a = 4 \n\
                 \ val b = 7 \n\
                 \ add a b"
      let result = run code
      result `shouldReturn` VMNumber 11

    it "applies a custom function" $ do
      let code = " val add-two (a) = {\n\
                 \   add 2 a \n\
                 \ } \n\
                 \ add-two 5"
      let result = run code
      result `shouldReturn` VMNumber 7

    it "applies a local variable to a custom function" $ do
      let code = " val add-two (a) = {\n\
                 \   add 2 a \n\
                 \ } \n\
                 \ val x = 10 \n\
                 \ val y = 5 \n\
                 \ add-two y"
      let result = run code
      result `shouldReturn` VMNumber 7

    it "matches a value against a single number" $ do
      let code = " match 1 with {\n\
                 \   1 -> :one \n\
                 \ }"
      let result = run code
      result `shouldReturn` VMSymbol "one" []

    it "matches a value against numbers" $ do
      let code = " match 7 with {\n\
                 \   1 -> :one \n\
                 \   2 -> :two \n\
                 \   3 -> :three \n\
                 \   4 -> :four \n\
                 \   5 -> :five \n\
                 \   6 -> :six \n\
                 \   7 -> :seven \n\
                 \   8 -> :eight \n\
                 \ }"
      let result = run code
      result `shouldReturn` VMSymbol "seven" []

    it "matches a value against symbols" $ do
      let code = " match :two with {\n\
                 \   :one -> 1 \n\
                 \   :two -> 2 \n\
                 \ }"
      let result = run code
      result `shouldReturn` VMNumber 2

    it "matches a value against numbers inside a function" $ do
      let code = " val check (n) = { \n\
                 \   match n with { \n\
                 \     1 -> :one \n\
                 \     2 -> :two \n\
                 \ } \n\
                 \ } \n\
                 \ check 2"
      let result = run code
      result `shouldReturn` VMSymbol "two" []


    it "binds an identifier in a match pattern" $ do
      let code = " match 2 with { \n\
                 \   1 -> :one \n\
                 \   n -> add 5 n \n\
                 \ }"
      let result = run code
      result `shouldReturn` VMNumber 7


    it "matches a symbol with data" $ do
      let code =  " match (:test 4 8 15) with { \n\
                  \ :test 1 2 3 -> 1 \n\
                  \ :test 4 8 15 -> 2 \n\
                  \ :test 99 100 101 -> 3 \n\
                  \ }"
      let result = run code
      result `shouldReturn` VMNumber 2


    it "binds a value inside a symbol" $ do
      let code =  " match (:test 4 8 15) with { \n\
                  \ :test 1 2 3 -> 1 \n\
                  \ :test 4 n m -> add n m \n\
                  \ :test 99 100 101 -> 3 \n\
                  \ }"
      let result = run code
      result `shouldReturn` VMNumber 23

-}


    it "binds a value inside a nested symbol" $ do
      let code =  " match (:test 4 (:inner 8) 15) with { \n\
                  \ :test 4 (:wrong n) m -> 1 \n\
                  \ :test 4 (:inner n) m -> add n m \n\
                  \ :test 5 (:inner n) m -> 3 \n\
                  \ }"

      -- putStrLn $ show $ toAsm code
      -- let cTable = extractConstTable code
      -- putStrLn $ foldl (++) "" $ map (\n -> showHex n "\n") cTable
      let result = run code
      result `shouldReturn` VMNumber 23


{-
What's missing:

- Matching
- Closures
- Currying
- Creating symbols

TODO: Functions need a runtime tag!

-}

{-

    it "supports nested closures" $ do
      let result = run "\
      \ val outside = 16 \n\
      \ val make-adder-maker (x) = {\n\
      \   val (y) = {\n\
      \     val (z) = {\n\
      \       add (add x (add z y)) outside \n\
      \ }\n\
      \ }\n\
      \ }\n\
      \ ((make-adder-maker 4) 8) 15"
      result `shouldReturn` VMNumber 43
-}

    {- TODO
      val make-sym (i) = 
        :sym i

      make-sym 4
    -}

