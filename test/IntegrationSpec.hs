module IntegrationSpec where

import           Language.Dash.API
import           Language.Dash.BuiltIn.BuiltInDefinitions
import           Language.Dash.Error.Error
import           Language.Dash.VM.DataEncoding
import           Numeric
import           Test.Hspec

-- This is mainly a test of the code generator. But it is an integration test because
-- we don't care about the instructions it churns out, as long as everything behaves as expected.

shouldReturnRight :: (Show a, Eq a) => IO (Either CompilationError a) -> a -> Expectation
shouldReturnRight action val = action `shouldReturn` Right val

spec :: Spec
spec = do
  describe "Dash" $ do


    it "evaluates an integer" $ do
      let result = run "4815"
      result `shouldReturnRight` VMNumber 4815

    it "evaluates a symbol" $ do
      let result = run ":dash"
      result `shouldReturnRight` VMSymbol "dash" []

    it "evaluates a string" $ do
      let result = run "\"dash!\""
      result `shouldReturnRight` VMString "dash!"

    it "applies built-in add function" $ do
      let result = run "2 + 3"
      result `shouldReturnRight` VMNumber 5

    it "applies built-in subtract function" $ do
      let result = run "7 - 3"
      result `shouldReturnRight` VMNumber 4

    it "applies built-in multiply function" $ do
      let result = run "7 * 3"
      result `shouldReturnRight` VMNumber 21

    it "applies built-in divide function" $ do
      let result = run "11 / 3"
      result `shouldReturnRight` VMNumber 3

    it "stores a value in a variable" $ do
      let result = run " a = 4\n\
                       \ a"
      result `shouldReturnRight` VMNumber 4

    it "uses local bindings in function call" $ do
      let code = " a = 4 \n\
                 \ b = 7 \n\
                 \ a + b"
      let result = run code
      result `shouldReturnRight` VMNumber 11

    it "applies a custom function" $ do
      let code = " add-two a = \n\
                 \   2 + a \n\
                 \ \n\
                 \ add-two 5"
      let result = run code
      result `shouldReturnRight` VMNumber 7

    it "applies a local variable to a custom function" $ do
      let code = " add-two a = \n\
                 \   2 + a \n\
                 \ \n\
                 \ x = 10 \n\
                 \ y = 5 \n\
                 \ add-two y"
      let result = run code
      result `shouldReturnRight` VMNumber 7

    it "does a generic application of a function" $ do
      let code = "\
      \ my-sub a b = a - b \n\
      \ apply f = \n\
      \   f 123 3 \n\
      \ apply my-sub"
      let result = run code
      result `shouldReturnRight` VMNumber 120

    it "returns a simple lambda" $ do
      let code =  " make-adder x = \n\
                  \   .\\ y = 22 + y \n\
                  \ \n\
                  \ adder = make-adder :nil \n\
                  \ adder 55"
      let result = run code
      result `shouldReturnRight` VMNumber 77



    it "optimizes tail calls" $ do
      let code = "\
      \ counter acc = \n\
      \   next = acc + 1 \n\
      \   match next with\n\
      \     1000 -> 43   \n\
      \     x -> counter x \n\
      \   end \n\
      \ y = counter 1 \n\
      \ y "
      let result = run code
      result `shouldReturnRight` VMNumber 43




    context "when using recursion" $ do

            it "handles nested self-recursion" $ do
              let code = "\
              \ counter acc = \n\
              \   next = acc - 1 \n\
              \   match next with\n\
              \     0 -> 43   \n\
              \     x -> counter x \n\
              \   end \n\
              \ counter 5"
              let result = run code
              result `shouldReturnRight` VMNumber 43


            it "handles nested self-recursion of closure" $ do
              -- We add the dummy closure so that it is the closure at memory index 0.
              -- This way we know that the inner use of `counter` is not simply using
              -- an uninitialized value.
              let code = "\
              \ outer m res = \n\
              \   dummy-closure y = \n\
              \     res + y    \n\
              \   counter acc = \n\
              \     next = acc - m \n\
              \     match next with\n\
              \       0 -> res   \n\
              \       x -> counter x \n\
              \     end \n\
              \   counter 9 \n\
              \ outer 3 995"
              let result = run code
              result `shouldReturnRight` VMNumber 995

    context "when using closures" $ do

            it "returns a closure with a dynamic variable" $ do
              let code =  " make-sub x = \n\
                          \   .\\ y = x - y \n\
                          \ \n\
                          \ subtractor = make-sub 55 \n\
                          \ subtractor 4"
              let result = run code
              result `shouldReturnRight` VMNumber 51

            it "captures a constant number" $ do
              let code =  " c = 30 \n\
                          \ make-sub x = \n\
                          \   .\\ y = c - y \n\
                          \ \n\
                          \ subtractor = make-sub 10 \n\
                          \ subtractor 4"
              let result = run code
              result `shouldReturnRight` VMNumber 26

            it "captures a constant plain symbol" $ do
              let code =  " ps = :my-symbol \n\
                          \ make-sym x = \n\
                          \   .\\ y = ps \n\
                          \ \n\
                          \ symbolicator = make-sym 44 \n\
                          \ symbolicator 55"
              let result = run code
              result `shouldReturnRight` VMSymbol "my-symbol" []

            it "captures a constant function" $ do
              let code =  " subsub a b = a - b \n\
                          \ make-sub x = \n\
                          \   .\\ y = subsub x y \n\
                          \ \n\
                          \ subtractor = make-sub 10 \n\
                          \ subtractor 4"
              let result = run code
              result `shouldReturnRight` VMNumber 6

            it "captures several dynamic values" $ do
              let code =  " make-sub x y z w = \n\
                          \   .\\ a = (z - y) - (x - a)\n\
                          \ \n\
                          \ test = make-sub 33 55 99 160 \n\
                          \ test 24"
              let result = run code
              result `shouldReturnRight` VMNumber ( (99 - 55) - (33 - 24) ) -- result: 35

            it "supports nested closures" $ do
              let code = "\
              \ outside = 1623 \n\
              \ make-adder-maker x = \n\
              \   .\\ y = \n\
              \     .\\ z = \n\
              \       (x + (z + y)) + outside \n\
              \ \n\
              \ ((make-adder-maker 9) 80) 150"
              let result = run code
              result `shouldReturnRight` VMNumber 1862



    context "when using currying" $ do

            it "applies a known curried function" $ do
              let code = "\
              \ my-sub a b = a - b \n\
              \ curry = my-sub 123  \n\
              \ curry 3"
              let result = run code
              result `shouldReturnRight` VMNumber 120

            it "applies an unknown curried closure" $ do
              let code = "\
              \ get-cl x = \n\
              \   .\\ a b = a - b \n\
              \ apply f = \n\
              \   curry = f 123  \n\
              \   curry 3 \n\
              \ cl = get-cl 0 \n\
              \ apply cl"
              let result = run code
              result `shouldReturnRight` VMNumber 120

            it "applies an unknown curried function" $ do
              let code = "\
              \ my-sub a b = a - b \n\
              \ apply f = \n\
              \   curry = f 123  \n\
              \   curry 3 \n\
              \ apply my-sub"
              let result = run code
              result `shouldReturnRight` VMNumber 120

            it "applies an over-saturated call to a known function" $ do
              let code = "\
              \ f a = .\\ b c = (a + b) - c \n\
              \ f 54 67 13"
              let result = run code
              result `shouldReturnRight` VMNumber 108

            it "applies an oversaturated call to an unknown closure" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = ((f + e) - (c + d)) + (a + b) \n\
              \ res = fun 1 2 3 4 5 6 \n\
              \ res"
              let result = run code
              result `shouldReturnRight` VMNumber 7

            it "applies an oversaturated tail-call to an unknown closure" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = ((f + e) - (c + d)) + (a + b) \n\
              \ fun 1 2 3 4 5 6"
              let result = run code
              result `shouldReturnRight` VMNumber 7

            it "applies an oversaturated call to an unknown function" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = :success \n\
              \ res = fun 1 2 3 4 5 6 \n\
              \ res"
              let result = run code
              result `shouldReturnRight` VMSymbol "success" []

            it "applies an oversaturated tail-call to an unknown function" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = :success \n\
              \ fun 1 2 3 4 5 6"
              let result = run code
              result `shouldReturnRight` VMSymbol "success" []

            it "applies result of a function application" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = 77 \n\
              \ (((fun 1) 2 3) 4 5) 6"
              let result = run code
              result `shouldReturnRight` VMNumber 77

            it "applies result of a closure application" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = ((f + e) - (c + d)) + (a + b) \n\
              \ (((fun 1) 2 3) 4 5) 6"
              let result = run code
              result `shouldReturnRight` VMNumber 7

            it "applies a partial application of a closure application" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d = .\\ e f = ((f + e) - (c + d)) + (a + b) \n\
              \ fpart = fun 1 2 3 4 5 \n\
              \ fpart 6"
              let result = run code
              result `shouldReturnRight` VMNumber 7


            it "applies a partial application of a function application" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d = .\\ e f = :success \n\
              \ fpart = fun 1 2 3 4 5 \n\
              \ fpart 6"
              let result = run code
              result `shouldReturnRight` VMSymbol "success" []


    context "when using compound symbols" $ do

            it "interprets a compound symbol" $ do
              let result = run ":sym 2 3"
              result `shouldReturnRight` VMSymbol "sym" [VMNumber 2, VMNumber 3]

            it "creates a symbol at runtime" $ do
              let code = "\
              \ fun a =   \n\
              \   :sym a \n\
              \ fun 7"
              let result = run code
              result `shouldReturnRight` VMSymbol "sym" [VMNumber 7]

            it "creates a nested symbol at runtime" $ do
              let code = "\
              \ fun a =   \n\
              \   :sym 1 (:sym2 a) 3 \n\
              \ fun 2"
              let result = run code
              result `shouldReturnRight` VMSymbol "sym" [VMNumber 1, VMSymbol "sym2" [VMNumber 2], VMNumber 3]


    context "when matching" $ do

            it "matches a value against a single number" $ do
              let code = " match 1 with\n\
                         \   1 -> :one \n\
                         \ end"
              let result = run code
              result `shouldReturnRight` VMSymbol "one" []

            it "matches a value against a negative number" $ do
              let code = " a = 3 \n\
                         \ match -a with\n\
                         \   -3 -> :three \n\
                         \ end"
              let result = run code
              result `shouldReturnRight` VMSymbol "three" []

            it "matches a value against numbers" $ do
              let code = " match 7 with\n\
                         \   1 -> :one \n\
                         \   2 -> :two \n\
                         \   3 -> :three \n\
                         \   4 -> :four \n\
                         \   5 -> :five \n\
                         \   6 -> :six \n\
                         \   7 -> :seven \n\
                         \   8 -> :eight \n\
                         \ end"
              let result = run code
              result `shouldReturnRight` VMSymbol "seven" []

            it "matches a value against symbols" $ do
              let code = " match :two with\n\
                         \   :one -> 1 \n\
                         \   :two -> 2 \n\
                         \ end"
              let result = run code
              result `shouldReturnRight` VMNumber 2

            it "matches a value against numbers inside a function" $ do
              let code = " check n = \n\
                         \   match n with \n\
                         \     1 -> :one \n\
                         \     2 -> :two \n\
                         \   end \n\
                         \ \n\
                         \ check 2"
              let result = run code
              result `shouldReturnRight` VMSymbol "two" []


            it "binds an identifier in a match pattern" $ do
              let code = " match 2 with \n\
                         \   1 -> :one \n\
                         \   n -> 5 + n \n\
                         \ end"
              let result = run code
              result `shouldReturnRight` VMNumber 7


            it "matches a compound symbol" $ do
              let code =  " match :test 4 8 15 with \n\
                          \ :test 1 2 3 -> 1 \n\
                          \ :test 4 8 15 -> 2 \n\
                          \ :test 99 100 101 -> 3 \n\
                          \ end"
              let result = run code
              result `shouldReturnRight` VMNumber 2


            it "binds a value inside a symbol" $ do
              let code =  " match :test 4 8 15 with \n\
                          \ :test 1 2 3 -> 1 \n\
                          \ :test 4 n m -> n + m \n\
                          \ :test 99 100 101 -> 3 \n\
                          \ end"
              let result = run code
              result `shouldReturnRight` VMNumber 23


            it "binds a value inside a nested symbol" $ do
              let code =  " match :test 4 (:inner 8) 15 with \n\
                          \ :test 4 (:wrong n) m -> 1 \n\
                          \ :test 4 (:inner n) m -> n + m \n\
                          \ :test 4 (:wrong n) m -> 1 \n\
                          \ end"
              let result = run code
              result `shouldReturnRight` VMNumber 23


            it "uses wildcards in a match" $ do
              let code =  " match :test 3 4 with \n\
                          \ :test _ 4 _ -> 22 \n\
                          \ :test 4     -> 33 \n\
                          \ :test _ 4   -> 44 \n\
                          \ end"
              let result = run code
              result `shouldReturnRight` VMNumber 44

            it "correctly applies free variables" $ do
              let code =  " run a b c d = \n\
                          \   match 2 with \n\
                          \     1 -> 22 \n\
                          \     2 -> (a + b) + (c + d) \n\
                          \     3 -> 44 \n\
                          \   end \n\
                          \ run 4 12 34 55"
              let result = run code
              result `shouldReturnRight` VMNumber 105


            it "binds a value inside a tuple" $ do
              let code =  " match (4, 8, 15) with \n\
                          \ (1, 2, 3) -> 1 \n\
                          \ (4, n, m) -> n + m \n\
                          \ (99, 100, 101) -> 3 \n\
                          \ end"
              let result = run code
              result `shouldReturnRight` VMNumber 23

    context "when using modules" $ do

      it "calls a function in a module" $ do
        let code = " mod = module                  \n\
                   \   func a = a + 12             \n\
                   \ end                           \n\
                   \ mod.func 7"
        let result = run code
        result `shouldReturnRight` VMNumber 19

      it "calls a function in a module self-recursively" $ do
        let code = " mod = module                  \n\
                   \   func a =                    \n\
                   \       if a == 0               \n\
                   \           then 10             \n\
                   \           else func (a - 1)   \n\
                   \ end                           \n\
                   \ mod.func 3"
        let result = run code
        result `shouldReturnRight` VMNumber 10


    it "resolves closed over vars in match-branches" $ do
      let code = " fib n =                       \n\
                 \   n' = n - 1                  \n\
                 \   n'' = n - 2                 \n\
                 \   match n with               \n\
                 \     0 -> 0                    \n\
                 \     1 -> 1                    \n\
                 \     x -> (fib n') + (fib n'') \n\
                 \   end                         \n\
                 \   fib 13"
      let result = run code
      result `shouldReturnRight` VMNumber 233


    it "has an equality operator" $ do
      let code = " eq = :sym == :sym     \n\
                 \ match eq with        \n\
                 \   :false -> 33        \n\
                 \   :true  -> 55        \n\
                 \ end"
      let result = run code
      result `shouldReturnRight` VMNumber 55

    it "determines equality between numbers" $ do
      let code = "2 == 2"
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "has less-than operator" $ do
      let code = "1 < 2"
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "has greater-than operator" $ do
      let code = "3 > 2"
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "less-than-equal operator with true result" $ do
      let code = "1 <= 2"
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "less-than-equal operator with true result 2" $ do
      let code = "2 <= 2"
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "less-than-equal operator with false result" $ do
      let code = "3 <= 2"
      let result = run code
      result `shouldReturnRight` VMSymbol "false" []

    it "greater-than-equal operator with true result" $ do
      let code = "3 >= 2"
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "greater-than-equal operator with true result 2" $ do
      let code = "2 >= 2"
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "greater-than-equal operator with false result" $ do
      let code = "1 >= 2"
      let result = run code
      result `shouldReturnRight` VMSymbol "false" []


    it "boolean 'or' with true result" $ do
      let code = ":false || :true"
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "boolean 'or' with false result" $ do
      let code = ":false || :false"
      let result = run code
      result `shouldReturnRight` VMSymbol "false" []

    it "boolean 'and' with true result" $ do
      let code = ":true && :true"
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "boolean 'and' with false result" $ do
      let code = ":true && :false"
      let result = run code
      result `shouldReturnRight` VMSymbol "false" []

    it "boolean 'not'" $ do
      let code = "! :false"
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    -- TODO should this work without parentheses?
    it "determines equality between compound symbols" $ do
      let code = "(:test 1 2 :three) == (:test 1 2 :three)"
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "determines equality between strings" $ do
      let code = "\"test\" == \"test\""
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "determines equality between empty strings" $ do
      let code = "\"\" == \"\""
      let result = run code
      result `shouldReturnRight` VMSymbol "true" []

    it "has correct precedence for math operators" $ do
      let code = "12 + 6 / 2 - 3 * 2"
      let result = run code
      result `shouldReturnRight` VMNumber 9


    it "has if-then-else" $ do
      let code = " if :sym == :no-sym then  \n\
                 \   77                  \n\
                 \ else                  \n\
                 \   99"
      let result = run code
      result `shouldReturnRight` VMNumber 99


    it "has tuples" $ do
      let code = "(1, 2, :sym)"
      let result = run code
      result `shouldReturnRight` VMSymbol tupleSymbolName [VMNumber 1, VMNumber 2, VMSymbol "sym" []]


    it "has lists" $ do
      let code = "[1, 2, :sym]"
      let result = run code
      result `shouldReturnRight` VMSymbol listConsSymbolName [VMNumber 1,
                              VMSymbol listConsSymbolName [ VMNumber 2,
                              VMSymbol listConsSymbolName [ VMSymbol "sym" [],
                              VMSymbol listEmptySymbolName []]]]


    it "matches an empty list" $ do
      let code = " ls = []      \n\
                 \ match ls with        \n\
                 \   [1, 2] -> :a \n\
                 \   [1, 2, 3] -> :b        \n\
                 \   [] -> :c        \n\
                 \ end"
      let result = run code
      result `shouldReturnRight` VMSymbol "c" []


    it "matches an exact list" $ do
      let code = " ls = [1, 2, 3]      \n\
                 \ match ls with        \n\
                 \   [1, 2] -> :a \n\
                 \   [1, 2, 3] -> :b        \n\
                 \ end"
      let result = run code
      result `shouldReturnRight` VMSymbol "b" []


    it "matches a list's tail" $ do
      let code = " ls = [1, 2, 3, 4]      \n\
                 \ match ls with        \n\
                 \   [1, 2] -> :a \n\
                 \   [1, 2 | tl] -> tl   \n\
                 \ end"
      let result = run code
      result `shouldReturnRight` VMSymbol listConsSymbolName [VMNumber 3,
                                VMSymbol listConsSymbolName [VMNumber 4,
                                VMSymbol listEmptySymbolName []]]


    it "matches a list's tail with a nested pattern" $ do
      let code = " ls = [1, 2, 3, 4, 5]      \n\
                 \ match ls with        \n\
                 \   [1, 2] -> :a \n\
                 \   [1 | [2 | [ 3 | tl]]] -> tl        \n\
                 \ end"
      let result = run code
      result `shouldReturnRight` VMSymbol listConsSymbolName [VMNumber 4,
                                VMSymbol listConsSymbolName [VMNumber 5,
                                VMSymbol listEmptySymbolName []]]

    it "cons a list" $ do
      let code = " tail = [3, 4] \n\
                 \ [1, 2 | tail]"
      let result = run code
      result `shouldReturnRight` VMSymbol listConsSymbolName [VMNumber 1,
                                VMSymbol listConsSymbolName [VMNumber 2,
                                VMSymbol listConsSymbolName [VMNumber 3,
                                VMSymbol listConsSymbolName [VMNumber 4,
                                VMSymbol listEmptySymbolName []]]]]

    it "has negative numbers" $ do
      let code = " 0 - 7 + 3"
      let result = run code
      result `shouldReturnRight` VMNumber (-4)

    it "has a prefix minus operator" $ do
      let code = " a = 7 \n\
                 \ b = 13 \n\
                 \ -a - -b"
      let result = run code
      result `shouldReturnRight` VMNumber 6

    it "knows the length of a string" $ do
      let code = "string-length \"1234567\""
      let result = run code
      result `shouldReturnRight` VMNumber 7

    it "concatenates strings" $ do
      let code =  " s1 = \"ab\" \n\
                  \ s3 = \"ef\" \n\
                  \ s1 ++ \"cd\" ++ s3"
      let result = run code
      result `shouldReturnRight` VMString "abcdef"

    it "creates a sub-strings" $ do
      let code =  " s1 = \"abcdefghijklmn\" \n\
                  \ sub-string 2 5 s1"
      let result = run code
      result `shouldReturnRight` VMString "cdefg"


    it "converts a string to a number" $ do
      let code =  " s = \"4815\" \n\
                  \ to-number s"
      let result = run code
      result `shouldReturnRight` VMNumber 4815

    it "converts a number to a string" $ do
      let code =  " n = 4815 \n\
                  \ to-string n"
      let result = run code
      result `shouldReturnRight` VMString "4815"

    context "regression tests" $ do

      it "compiles variable assignment" $ do
        let code = " a = 4 \n\
                   \ b = a \n\
                   \ b"
        let result = run code
        result `shouldReturnRight` VMNumber 4

      it "compiles variable assignment in inner binding" $ do
        let code = " a = 4 \n\
                   \ b = \n\
                   \   c = a \n\
                   \   c   \n\
                   \ b"
        let result = run code
        result `shouldReturnRight` VMNumber 4

      it "runs code that starts with a delimited comment" $ do
        let code = " /-- \n\
                   \ --/ \n\
                   \ test a = a \n\
                   \ /-- \n\
                   \ --/ \n\
                   \ test 3 \n\
                   \ "
        let result = run code
        result `shouldReturnRight` VMNumber 3



