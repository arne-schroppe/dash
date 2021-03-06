
# dash

dash is a purely functional, strictly evaluated, and dynamically typed programming language.

It is in an early alpha stage.


## Usage

You can use dash in two ways. If you want to get a first impression, you can start the REPL (read-eval-print loop) with
```
dash-repl
```

If you want to run a complete dash script, use
```
dash hello.ds
```


## Syntax


This is how you define a value:
```
my_string = "Hello, dash!"
some_number = 1234
```
(dash doesn't understand floating point numbers yet, only integers.)

You can include expressions inside strings with string interpolation:
```
winner = 5432
message = "The winning number is: \(winner)!"
```

A fairly useful kind of value in dash is the symbol. A symbol is simply a `:` followed by
an identifier:
```
job = :engineer
```

A symbol can also contain data:
```
employee_of_the_month = :employee<"bob", :engineer, 46>
```
The identifier of this symbol is `employee`. It contains three pieces of data:
The string `"bob"`, the atomic symbol `:engineer`, and the number `46`


dash has built-in syntax for lists, tuples, and records:
```
my_list = ["unicorns", :confetti, 1234]
a_tuple = ("robots", "lazers", :more_confetti)
a_record = { name = "cat", special_skill = :sleep }
```

A function definition looks similar to a value definition:
```
celsius_to_kelvin c = c + 273
```

This is actually just a more compact version of this:
```
celsius_to_kelvin = c -> c + 273
```

That `<params> -> <expr>` construct is a lambda expression (also known as an
anonymous function). You list the parameters before the arrow and an 
expression after it. Very simple!


More complex functions typically need pattern matching to "break down"
data. Pattern matching is done with the `match-with-end` expression:
```
employee_job_title e =
  match e with
    :employee<name, title, age> -> title
    _ -> :error<"This doesn't look like a proper employee">
  end
```

You can simply write `_` for values you're not interested in:
```
employee_age e =
  match e with
    :employee<_, _, age> -> age
    _ -> :error<"This doesn't look like a proper employee">
  end
```

And this is how you do pattern matching on lists:
```
split_head ls =
  match ls with
    [head | tail] -> (head, tail)
    _ -> :error<"Unexpected value">
  end

add_first_three ls =
  match ls with
    [first, second, third | _] -> first + second + third
    _ -> :error<"Not the kind of list we were expecting">
  end
```

There is also an `if-then-else` expression with the usual semantics:
```
abs n =
  if n < 0
    then -n
    else n
```



But let's talk about functions again. dash has native support for currying. That means
that if you apply fewer arguments to a function than is required (this is called "partial
application"), you get a new function which takes the remaining parameters. That's useful
in many cases:
```
is_between a b n =
  n >= a && n <= b

numbers = [4, 7, -12, 90, 0]

clamped_numbers = filter (is_between 0 10) numbers
```

`filter` is a built-in function that checks each value in a list against
the function it receives as its first argument. The function we're using in
this case is the partially applied function `between 0 10`. With currying and
partial application we can avoid using the somewhat clunky lambda expressions in
a lot of cases.


A programming language wouldn't be of much use if it couldn't communicate with
the user. In dash a `do`-expression is used for that:
```
do io with
  io.print "What is your name? "
  name <- io.read_line
  io.print_line "Hello, \(name)!"
end
```

For smaller programs you'll typically have a single i/o action at the end
of your file. But you can also create auxiliary i/o actions:
```
ask_name =
  do io with
    io.print "What is your name? "
    name <- io.read_line
    return name
  end

do io with
  name <- ask_name
  io.print_line "Hello, \(name)!"
end
```
(That is the same example as before, just split into several i/o actions)


One important detail is that i/o code is separate from "normal" code in dash. That
means that if you'd use `io.print_line` or any other i/o action somewhere deep down in
a function, it will not start printing things on the screen. Why is that? It's because
all those `io` things don't do anything directly. Instead they *describe* an action
to be performed. So `io.print_line "Hello"` doesn't write the text "Hello" on the screen, but
returns a value that says "this is an action that, when executed, prints the text 'Hello'".

So how are i/o actions executed then? They're executed by returning them to the environment.
The last value in a file is the file's result. If the result is a number or a string or
another simple value, the value is simply printed on the screen. But if the value is an i/o
action, the action is executed. So any i/o action needs to be directly or indirectly
returned as the last value in a file in order to have any effect.


You might also have been wondering what that dot-syntax is, e.g. `io.read_line`.
That is dash's module lookup syntax:
```
celsius_kelvin_diff = 273

temp = module

  celsius_to_kelvin c =
    c + celsius_kelvin_diff

  kelvin_to_celsius k =
    k - celsius_kelvin_diff

end

very_very_cold = temp.kelvin_to_celsius 34
```

This means that you can use `do`-expressions for lots of other things, not only
input/output. You can create your own modules for `do`-expressions. They just need to provide
two functions at the very least, `bind` and `return`:
```
maybe = module
  bind a next =
    match a with
      :some<x> -> next x
      :none    -> :none
    end

  return x = :some<x>
end
```

Those two functions turn the module into a monad. There are many good tutorials
elsewhere on the internet that explain what a monad is and what they're good for.


What else? Oh yeah, this is how you write comments:
```
-- this is a single-line comment

/-- and this
is a multi-line
comment --/
```


## Built-in functions

There is currently no standard library. Actually you can't even import or include other
dash-files, so your entire code needs to be in one file. There are, however, a couple
of built-in functions:

#### Operators (ordered by precedence)

    ||
    &&
    == < > <= >=
    + - (binary) ^+ ++
    * /
    - (unary) !

Note that `^+` is the string concatenation operator and `++` is the list concatenation
operator.

#### Functions

  - `string_length ls`
  - `sub_string start len s`
  - `to_number x`
  - `to_string x`
  - `head ls`
  - `tail ls`
  - `map f ls`
  - `foldr f z ls`
  - `reverse l`
  - `filter f ls`
  - `length ls`
  - `sequence m ms`
  - `m_map m action ls`


#### The `io`-module

  - `bind io_action next`
  - `return a`
  - `read_line`
  - `print a`
  - `print_line a`


## Examples

See the "examples" folder for examples of how to use dash



