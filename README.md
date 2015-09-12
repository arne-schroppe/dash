
# Dash

Dash is a purely functional, strictly evaluated, and dynamically typed programming language.

It is in the earliest of early alpha stages.


# Usage

Dash currently only consists of the Dash interpreter. Use
```
dash hello.ds
```
to run a file called `hello.ds`. There is no repl yet.



# Syntax


```

-- This is how you define a value:
my-string = "Hello, Dash!"
some-number = 1234
-- (Dash doesn't understand floating point numbers yet, only integers)


-- A fairly useful kind of value in Dash is the symbol:
job = :engineer

-- A symbol can also carry data:
employee-of-the-month = :employee "bob" :engineer 46


-- Dash also has built-in syntax for lists and tuples:
my-list = ["unicorns", :confetti, 1234]
a-tuple = ("robots", "lazers", :more-confetti)


-- A function definition looks similar to a value definition:
celsius-to-kelvin c = c + 273

-- This is actually just a more compact version of this:
celsius-to-kelvin = .\ c = c + 273

/-- That `.\ ... = ...` construct is a lambda expression. If you squint
your eyes, the .\ looks a bit like the greek letter lambda. --/


/-- More complex functions typically need pattern matching to "break down"
complex data. Pattern matching is done with the `match-with-end` construct: --/

job-title e =
  match e with
    :employee name title age  -> title
    _ -> :error "This doesn't look like a proper employee"
  end

-- You can also just write '_' for values you're not interested in:
age e =
  match e with
    :employee _ _ age  -> age
    _ -> :error "This doesn't look like a proper employee"
  end


-- There is also special syntax for pattern-matching lists:

split-list ls =
  match ls with
    [first|rest] -> (first, rest)
    _ -> :error "Unexpected value"
  end

add-first-three ls =
  match ls with
    [first, second, third | rest] -> (first + second + third, rest)
    _ -> :error "Not the kind of list we expected"
  end


-- There is also an if-then-else expression with the usual semantics:

should-i-write-this-down num-data-items =
  if num-data-items < 7
    then "I can keep this in my head"
    else "Wait, I'll need to write that down"



/-- Dash has native support for currying. That means that if you apply fewer
arguments to a function than is required (this is called "partial application"),
you get a new function. That's useful in many cases: --/

between a b n =
  n >= a && n <= b

numbers = [4, 7, -12, 90, 0]

clamped-numbers = filter (between 0 10) numbers

/-- `filter` is a built-in function that checks each value in a list against
the function it receives as it's first argument. The function we're using in
this case is the partially applied function `between 0 10`. With currying and 
partial application we can avoid using the somewhat clunky lambda expressions in
a lot of cases. --/


/-- A programming language wouldn't be of much use if it couldn't communicate with
the user. In Dash, input/output is somewhat separated from "normal" code. To talk
to the user, we use a do-expression:
--/

do io with
  io.print-ln "What is your name?"
  name <- io.read-ln
  io.print-ln ("Hello, " ++ name)
end

/-- For smaller programs you'll typically have a single i/o action at the end
of your file. But you can also create auxilliary i/o actions: --/

ask-name =
  do io with
    io.print "What is your name?"
    name <- io.read-ln
  end

do io with
  name <- ask-name
  io.print-ln ("Hello, " ++ name)
end

-- (That is the same example as before, just split into several io actions)

/-- One important detail is that i/o code is separate from "normal" code in Dash. That
means that if you'd use `io.print-ln` or any other i/o action somewhere deep down in
a function, it will not start printing things on the screen. Why is that? It's because
all those io things don't do anything directly. Instead they *describe* what should
be done. So `io.print-ln "Hello"` doesn't write the text "Hello" on the screen, but
returns a value that says "this is an action that, when executed, should print the
text "Hello"'. So how are i/o actions executed then? They're executed by returning
them to the environment! The last value in a file is the file's result. If that result
is a number or a string or another simple value, that value is printed on the screen.
But if the value is an i/o action, the action is executed (and afterwards Dash
continues with the code following the i/o action). So any i/o action needs to be
directly or indirectly returned as the last value in a file. --/


/-- You might also have been wondering what that dot-syntax is, e.g. `io.read-ln`.
That is Dash's module lookup syntax: --/

temp = module
  celsius-kelvin-diff = 273

  celsius-to-kelvin c =
    c + celsius-kelvin-diff

  kelvin-to-celsius k =
    k - celsius-kelvin-diff
end


very-very-cold = temp.kelvin-to-celsius 34


/-- This also means that there's nothing special about the do-syntax. It's only
the built-in io module that has some special powers. But you can create you own modules
for do-expressions. They just need to provide two functions at the very least, `bind`
and `return`: --/

maybe = module
  bind a next =
    match a with
      :some x -> next x
      :none   -> none
    end

  return x = :some x
end

/-- Those two functions turn the module into a monad. There are many good tutorials
elsewhere on the internet that explain what a monad is and what they're good for --/


```

# Built-in functions

There is currently no standard library. Actually you can't even import or include other
Dash-files, so your entire code needs to be in one file. There are, however, a couple
of built-in functions:

Operators (ordered by precedence):

    ||
    &&
    ==, <, >, <=, >=
    +, - (binary), ++
    *, /
    - (unary), !


Functions:

  - `string-length ls`
  - `sub-string start len s`
  - `to-number x`
  - `to-string x`
  - `head ls`
  - `tail ls`
  - `map f ls`
  - `foldr f z ls`
  - `concatenate a b`
  - `reverse l`
  - `filter f ls`
  - `length ls`
  - `sequence m ms`
  - `m-map m action ls`


The `io`-module:

  - `bind io-action next`
  - `return a`
  - `read-ln`
  - `print a`
  - `print-ln a`


# Examples

See the `examples` folder for examples how to use Dash



