module IntegrationSpec where

import           Test.Hspec
import           Language.Dash.API
import           Language.Dash.Constants
import           Language.Dash.VM.DataEncoding
import           Numeric

-- This is mainly a test of the code generator. But it is an integration test because
-- we don't care about the instructions it churns out, as long as everything behaves as expected.


spec :: Spec
spec = do
  describe "Dash" $ do


    it "evaluates an integer" $ do
      let result = run "4815"
      result `shouldReturn` VMNumber 4815

    it "evaluates a symbol" $ do
      let result = run ":dash"
      result `shouldReturn` VMSymbol "dash" []

    it "applies built-in add function" $ do
      let result = run "2 + 3"
      result `shouldReturn` VMNumber 5

    it "applies built-in subtract function" $ do
      let result = run "7 - 3"
      result `shouldReturn` VMNumber 4

    it "applies built-in multiply function" $ do
      let result = run "7 * 3"
      result `shouldReturn` VMNumber 21

    it "applies built-in divide function" $ do
      let result = run "11 / 3"
      result `shouldReturn` VMNumber 3

    it "stores a value in a variable" $ do
      let result = run " a = 4\n\
                       \ a"
      result `shouldReturn` VMNumber 4

    it "uses local bindings in function call" $ do
      let code = " a = 4 \n\
                 \ b = 7 \n\
                 \ a + b"
      let result = run code
      result `shouldReturn` VMNumber 11

    it "applies a custom function" $ do
      let code = " add-two a = \n\
                 \   2 + a \n\
                 \ \n\
                 \ add-two 5"
      let result = run code
      result `shouldReturn` VMNumber 7

    it "applies a local variable to a custom function" $ do
      let code = " add-two a = \n\
                 \   2 + a \n\
                 \ \n\
                 \ x = 10 \n\
                 \ y = 5 \n\
                 \ add-two y"
      let result = run code
      result `shouldReturn` VMNumber 7

    it "does a generic application of a function" $ do
      let code = "\
      \ my-sub a b = a - b \n\
      \ apply f = \n\
      \   f 123 3 \n\
      \ apply my-sub"
      let result = run code
      result `shouldReturn` VMNumber 120

    it "returns a simple lambda" $ do
      let code =  " make-adder x = \n\
                  \   .\\ y = 22 + y \n\
                  \ \n\
                  \ adder = make-adder :nil \n\
                  \ adder 55"
      let result = run code
      result `shouldReturn` VMNumber 77



    it "optimizes tail calls" $ do
      let code = "\
      \ counter acc = \n\
      \   next = acc + 1 \n\
      \   match next begin\n\
      \     1000 -> 43   \n\
      \     x -> counter x \n\
      \   end \n\
      \ y = counter 1 \n\
      \ y "
      let result = run code
      result `shouldReturn` VMNumber 43




    context "when using recursion" $ do

            it "handles nested self-recursion" $ do
              let code = "\
              \ counter acc = \n\
              \   next = acc - 1 \n\
              \   match next begin\n\
              \     0 -> 43   \n\
              \     x -> counter x \n\
              \   end \n\
              \ counter 5"
              let result = run code
              result `shouldReturn` VMNumber 43


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
              \     match next begin\n\
              \       0 -> res   \n\
              \       x -> counter x \n\
              \     end \n\
              \   counter 9 \n\
              \ outer 3 995"
              let result = run code
              result `shouldReturn` VMNumber 995

{- Note: Mutual recursion will only be possible in the top level of a module (and thus without closures)
            it "handles mutual recursion of lambdas" $ do
              let code = "\
              \ val check (a) = \n\
              \   match a begin\n\
              \     10 -> 43   \n\
              \     x -> add-one a \n\
              \   end \n\
              \ val add-one (v) = \n\
              \   val x = v + 1 \n\
              \   check x \n\
              \ check 0"
              let result = run code
              result `shouldReturn` VMNumber 43

            it "handles mutual recursion of closures" $ do
              let code = "\
              \ val make-checker (step res) = \n\
              \   val check (a) = \n\
              \     match a begin \n\
              \       10 -> res   \n\
              \       x -> add-n a \n\
              \     end \n\
              \   val add-n (v) = \n\
              \     val x = add v step \n\
              \     check x \n\
              \   check \n\
              \ val checker = make-checker 2 999 \n\
              \ checker 0"
              let result = run code
              result `shouldReturn` VMNumber 999

-}

    context "when using closures" $ do

            it "returns a closure with a dynamic variable" $ do
              let code =  " make-sub x = \n\
                          \   .\\ y = x - y \n\
                          \ \n\
                          \ subtractor = make-sub 55 \n\
                          \ subtractor 4"
              let result = run code
              result `shouldReturn` VMNumber 51

            it "captures a constant number" $ do
              let code =  " c = 30 \n\
                          \ make-sub x = \n\
                          \   .\\ y = c - y \n\
                          \ \n\
                          \ subtractor = make-sub 10 \n\
                          \ subtractor 4"
              let result = run code
              result `shouldReturn` VMNumber 26

            it "captures a constant plain symbol" $ do
              let code =  " ps = :my-symbol \n\
                          \ make-sym x = \n\
                          \   .\\ y = ps \n\
                          \ \n\
                          \ symbolicator = make-sym 44 \n\
                          \ symbolicator 55"
              let result = run code
              result `shouldReturn` VMSymbol "my-symbol" []

            it "captures a constant function" $ do
              let code =  " subsub a b = a - b \n\
                          \ make-sub x = \n\
                          \   .\\ y = subsub x y \n\
                          \ \n\
                          \ subtractor = make-sub 10 \n\
                          \ subtractor 4"
              let result = run code
              result `shouldReturn` VMNumber 6

            it "captures several dynamic values" $ do
              let code =  " make-sub x y z w = \n\
                          \   .\\ a = (z - y) - (x - a)\n\
                          \ \n\
                          \ test = make-sub 33 55 99 160 \n\
                          \ test 24"
              let result = run code
              result `shouldReturn` VMNumber ( (99 - 55) - (33 - 24) ) -- result: 35

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
              result `shouldReturn` VMNumber 1862



    context "when using currying" $ do

            it "applies a known curried function" $ do
              let code = "\
              \ my-sub a b = a - b \n\
              \ curry = my-sub 123  \n\
              \ curry 3"
              let result = run code
              result `shouldReturn` VMNumber 120

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
              result `shouldReturn` VMNumber 120

            it "applies an unknown curried function" $ do
              let code = "\
              \ my-sub a b = a - b \n\
              \ apply f = \n\
              \   curry = f 123  \n\
              \   curry 3 \n\
              \ apply my-sub"
              let result = run code
              result `shouldReturn` VMNumber 120

            it "applies an over-saturated call to a known function" $ do
              let code = "\
              \ f a = .\\ b c = (a + b) - c \n\
              \ f 54 67 13"
              let result = run code
              result `shouldReturn` VMNumber 108

            it "applies an oversaturated call to an unknown closure" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = ((f + e) - (c + d)) + (a + b) \n\
              \ res = fun 1 2 3 4 5 6 \n\
              \ res"
              let result = run code
              result `shouldReturn` VMNumber 7

            it "applies an oversaturated tail-call to an unknown closure" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = ((f + e) - (c + d)) + (a + b) \n\
              \ fun 1 2 3 4 5 6"
              let result = run code
              result `shouldReturn` VMNumber 7

            it "applies an oversaturated call to an unknown function" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = :success \n\
              \ res = fun 1 2 3 4 5 6 \n\
              \ res"
              let result = run code
              result `shouldReturn` VMSymbol "success" []

            it "applies an oversaturated tail-call to an unknown function" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = :success \n\
              \ fun 1 2 3 4 5 6"
              let result = run code
              result `shouldReturn` VMSymbol "success" []

            it "applies result of a function application" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = 77 \n\
              \ (((fun 1) 2 3) 4 5) 6"
              let result = run code
              result `shouldReturn` VMNumber 77

            it "applies result of a closure application" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d e = .\\ f = ((f + e) - (c + d)) + (a + b) \n\
              \ (((fun 1) 2 3) 4 5) 6"
              let result = run code
              result `shouldReturn` VMNumber 7

            it "applies a partial application of a closure application" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d = .\\ e f = ((f + e) - (c + d)) + (a + b) \n\
              \ fpart = fun 1 2 3 4 5 \n\
              \ fpart 6"
              let result = run code
              result `shouldReturn` VMNumber 7


            it "applies a partial application of a function application" $ do
              let code = "\
              \ fun a =   \n\
              \   .\\ b c = .\\ d = .\\ e f = :success \n\
              \ fpart = fun 1 2 3 4 5 \n\
              \ fpart 6"
              let result = run code
              result `shouldReturn` VMSymbol "success" []


    context "when using compound symbols" $ do

            it "interprets a compound symbol" $ do
              let result = run ":sym 2 3"
              result `shouldReturn` VMSymbol "sym" [VMNumber 2, VMNumber 3]

            it "creates a symbol at runtime" $ do
              let code = "\
              \ fun a =   \n\
              \   :sym a \n\
              \ fun 7"
              let result = run code
              result `shouldReturn` VMSymbol "sym" [VMNumber 7]

            it "creates a nested symbol at runtime" $ do
              let code = "\
              \ fun a =   \n\
              \   :sym 1 (:sym2 a) 3 \n\
              \ fun 2"
              let result = run code
              result `shouldReturn` VMSymbol "sym" [VMNumber 1, VMSymbol "sym2" [VMNumber 2], VMNumber 3]


    context "when matching" $ do

            it "matches a value against a single number" $ do
              let code = " match 1 begin\n\
                         \   1 -> :one \n\
                         \ end"
              let result = run code
              result `shouldReturn` VMSymbol "one" []

            it "matches a value against a negative number" $ do
              let code = " a = 3 \n\
                         \ match -a begin\n\
                         \   -3 -> :three \n\
                         \ end"
              let result = run code
              result `shouldReturn` VMSymbol "three" []

            it "matches a value against numbers" $ do
              let code = " match 7 begin\n\
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
              result `shouldReturn` VMSymbol "seven" []

            it "matches a value against symbols" $ do
              let code = " match :two begin\n\
                         \   :one -> 1 \n\
                         \   :two -> 2 \n\
                         \ end"
              let result = run code
              result `shouldReturn` VMNumber 2

            it "matches a value against numbers inside a function" $ do
              let code = " check n = \n\
                         \   match n begin \n\
                         \     1 -> :one \n\
                         \     2 -> :two \n\
                         \   end \n\
                         \ \n\
                         \ check 2"
              let result = run code
              result `shouldReturn` VMSymbol "two" []


            it "binds an identifier in a match pattern" $ do
              let code = " match 2 begin \n\
                         \   1 -> :one \n\
                         \   n -> 5 + n \n\
                         \ end"
              let result = run code
              result `shouldReturn` VMNumber 7


            it "matches a compound symbol" $ do
              let code =  " match :test 4 8 15 begin \n\
                          \ :test 1 2 3 -> 1 \n\
                          \ :test 4 8 15 -> 2 \n\
                          \ :test 99 100 101 -> 3 \n\
                          \ end"
              let result = run code
              result `shouldReturn` VMNumber 2


            it "binds a value inside a symbol" $ do
              let code =  " match :test 4 8 15 begin \n\
                          \ :test 1 2 3 -> 1 \n\
                          \ :test 4 n m -> n + m \n\
                          \ :test 99 100 101 -> 3 \n\
                          \ end"
              let result = run code
              result `shouldReturn` VMNumber 23


            it "binds a value inside a nested symbol" $ do
              let code =  " match :test 4 (:inner 8) 15 begin \n\
                          \ :test 4 (:wrong n) m -> 1 \n\
                          \ :test 4 (:inner n) m -> n + m \n\
                          \ :test 4 (:wrong n) m -> 1 \n\
                          \ end"
              let result = run code
              result `shouldReturn` VMNumber 23


            it "uses wildcards in a match" $ do
              let code =  " match :test 3 4 begin \n\
                          \ :test _ 4 _ -> 22 \n\
                          \ :test 4     -> 33 \n\
                          \ :test _ 4   -> 44 \n\
                          \ end"
              let result = run code
              result `shouldReturn` VMNumber 44

            it "correctly applies free variables" $ do
              let code =  " run a b c d = \n\
                          \   match 2 begin \n\
                          \     1 -> 22 \n\
                          \     2 -> (a + b) + (c + d) \n\
                          \     3 -> 44 \n\
                          \   end \n\
                          \ run 4 12 34 55"
              let result = run code
              result `shouldReturn` VMNumber 105


            it "binds a value inside a tuple" $ do
              let code =  " match (4, 8, 15) begin \n\
                          \ (1, 2, 3) -> 1 \n\
                          \ (4, n, m) -> n + m \n\
                          \ (99, 100, 101) -> 3 \n\
                          \ end"
              let result = run code
              result `shouldReturn` VMNumber 23



    it "resolves closed over vars in match-branches" $ do
      let code = " fib n =                       \n\
                 \   n' = n - 1                  \n\
                 \   n'' = n - 2                 \n\
                 \   match n begin               \n\
                 \     0 -> 0                    \n\
                 \     1 -> 1                    \n\
                 \     x -> (fib n') + (fib n'') \n\
                 \   end                         \n\
                 \   fib 13"
      let result = run code
      result `shouldReturn` VMNumber 233


    it "has an equality operator" $ do
      let code = " eq = :sym == :sym     \n\
                 \ match eq begin        \n\
                 \   :false -> 33        \n\
                 \   :true  -> 55        \n\
                 \ end"
      let result = run code
      result `shouldReturn` VMNumber 55

    it "has correct precedence for math operators" $ do
      let code = "12 + 6 / 2 - 3 * 2"
      let result = run code
      result `shouldReturn` VMNumber 9


    it "has if-then-else" $ do
      let code = " if :sym == :no-sym then  \n\
                 \   77                  \n\
                 \ else                  \n\
                 \   99"
      let result = run code
      result `shouldReturn` VMNumber 99


    it "has tuples" $ do
      let code = "(1, 2, :sym)"
      let result = run code
      result `shouldReturn` VMSymbol tupleSymbolId [VMNumber 1, VMNumber 2, VMSymbol "sym" []]


    it "has lists" $ do
      let code = "[1, 2, :sym]"
      let result = run code
      result `shouldReturn` VMSymbol listConsSymbolId [VMNumber 1,
                              VMSymbol listConsSymbolId [ VMNumber 2,
                              VMSymbol listConsSymbolId [ VMSymbol "sym" [], 
                              VMSymbol listEmptySymbolId []]]]


    it "matches an empty list" $ do
      let code = " ls = []      \n\
                 \ match ls begin        \n\
                 \   [1, 2] -> :a \n\
                 \   [1, 2, 3] -> :b        \n\
                 \   [] -> :c        \n\
                 \ end"
      let result = run code
      result `shouldReturn` VMSymbol "c" []


    it "matches an exact list" $ do
      let code = " ls = [1, 2, 3]      \n\
                 \ match ls begin        \n\
                 \   [1, 2] -> :a \n\
                 \   [1, 2, 3] -> :b        \n\
                 \ end"
      let result = run code
      result `shouldReturn` VMSymbol "b" []


    it "matches a list's tail" $ do
      let code = " ls = [1, 2, 3, 4]      \n\
                 \ match ls begin        \n\
                 \   [1, 2] -> :a \n\
                 \   [1, 2 | tl] -> tl   \n\
                 \ end"
      let result = run code
      result `shouldReturn` VMSymbol listConsSymbolId [VMNumber 3,
                                VMSymbol listConsSymbolId [VMNumber 4,
                                VMSymbol listEmptySymbolId []]]

    it "matches a list's tail with a nested pattern" $ do
      let code = " ls = [1, 2, 3, 4, 5]      \n\
                 \ match ls begin        \n\
                 \   [1, 2] -> :a \n\
                 \   [1 | [2 | [ 3 | tl]]] -> tl        \n\
                 \ end"
      let result = run code
      result `shouldReturn` VMSymbol listConsSymbolId [VMNumber 4,
                                VMSymbol listConsSymbolId [VMNumber 5,
                                VMSymbol listEmptySymbolId []]]

    it "has negative numbers" $ do
      let code = " 0 - 7 + 3"
      let result = run code
      result `shouldReturn` VMNumber (-4)

    it "has a prefix minus operator" $ do
      let code = " a = 7 \n\
                 \ b = 13 \n\
                 \ -a - -b"
      let result = run code
      result `shouldReturn` VMNumber 6

{-
What's missing:

immediate goals:
K Refactoring
x Proper error handling (Either result)
K Negative integers
K General clean up. Try with real code samples! fix everything that doesn't work. Clean up code.
K TODO make it impossible to reassign values (d'oh!)
K Limits (integers, num local vars, num arguments, num symbols, etc)
- Put all compiler errors into a common error modules
- unify naming, e.g. fun/func, get vs. no prefix, etc
K Use better types (Reg as member of Num typeclass)
- Lots and lots of cleanup. Unified names and parameter orders and smart constructors, etc

K operator precedence / limited set of operators
K negative numbers (with bias)
- inequality operators
- strings (string concatenation, to-string)
- runtime errors
- garbage collection


missing language features:
K Closures
K Recursion (also mutual recursion)
K Tail call optimisation (isResultValue, add new opcodes)
K Currying (also with underscore ?)
K Over-saturated calls!
K Change order of free vars and formal parameters in function code so that we can use partial application as currying
K Equality operator (needs built-in :true and :false symbols)
K if-then-else
K pattern wildcard (can be used multiple times)
K More math operators
K infix operators
x inlining of match branches (provide map for arguments, shift base reg, transform norm, then inline)
x if a closure doesn't escape the current context, apply free variables directly
- Strings
- Garbage collection
K Creating symbols (symbol arity is known statically)
K tuples
K Lists
- Static modules
- I/O

- maps/dictionaries as built-in type (because that's fairly useful)
  - under the hood these could be somewhat similar to modules, maybe. Or modules can't be modified at runtime,
    but then we're not very dynamic
  - map functions: hasKeys, merge (for entitas)

- Change tags and runtime representation of values
- Garbage collection
- Reducing the amount of created garbage


- Live editing would be really nice

- Add license header everywhere!
- Also add Haskell documentation header everywhere

- A proper number type (big decimal?)


After release ?

- Dynamic modules

- off-site syntax
- Mutual recursion in module top-level
- Multiple files
- Operator precedence with arbitrary operators
- Debugging. Stack traces (or breadcrumbs?)

- private/secret symbols

- concurrency!

- a REPL

- Clean up compiler errors. Have a variant with all errors and warnings and emit those. Render when printing


- Can we do partial compilation? Per module? Or just a single function or value in a repl?

- Refactor VM
  - bytecode
  - stack

- Matching the same var multiple times (e.g.  :test a 4 a -> :something ... only works if symbol is e.g. :test "a" 4 "a")
- Faster, optimized match patterns (reduce number of comparisons)
- Uniquely name vars in frontend, i.e. data-flow analysis
K Prevent duplicate var names in function definition (unless it's for pattern matching? No, that would be useless)


-}

