# Mini tutorial for Roc's New Compiler

If you're feeling adventurous, you can try Roc's new bleeding-edge rewritten compiler. We're really excited about it!

Be warned, this is far from a polished experience! Expect missing features, bugs, and almost no documentation (everything on [roc-lang.org](https://www.roc-lang.org) is referring to the old compiler and the old design; we haven't updated any of it yet).

The purpose of this document is to give you a sense of how the revised language and new compiler work, since [roc-lang.org](https://www.roc-lang.org) is all about the old stuff.

By the way, if you want help, **the** best place to get it is [Roc Zulip](https://roc.zulipchat.com/) - you're welcome to ask any questions you have in [#beginners](https://roc.zulipchat.com/#narrow/channel/231634-beginners/).

With those disclaimers in mind, let's get into the adventure!

# Hello, World

First, [grab a nightly build of the new compiler](https://github.com/roc-lang/nightlies/releases). It'll have an executable named `roc` (or `roc.exe` on Windows). You'll want to [put that executable on your PATH] - you'll know it worked if you can run `roc version` in a terminal and see it print something like `Roc compiler version release-fast-123c5d78` (the hash at the very end there will be different from this one, as it changes with each nightly release).

Next, copy/paste this into a new file named `main.roc`: (yes, the long URL is correct - we'll discuss it later!)

```ruby
app [main!] { pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.6/2BfGn4M9uWJNhDVeMghGeXNVDFijMfPsmmVeo6M4QjKX.tar.zst" }

import pf.Stdout

main! = |_args| {
    Stdout.line!("Hello, World!")
    Ok({})
}
```

You can run this with:

```bash
$ roc main.roc
```

You should see this:

```
Hello, World!
```

Hooray! 

> Tip: If you don't provide a path to a .roc file, `roc` will default to "main.roc" - so since your Roc program is named main.roc, you can run your program by just running `roc` in the future.

## Imports

Let's look at that `main.roc` file, starting with the second line.

```
import pf.Stdout
```

This imports `Stdout` into scope, allowing us to call its `Stdout.line!` function later. The `pf.` means that `Stdout` can be found in the `pf` _package_, which we specified on the first line. 

Let's add another import right below it:

```
import pf.Stdin
```

Now we can also use things from `Stdin`. 

## The `main!` function

Let's take a look at `main!` next:

```ruby
main! = |_args| {
    Stdout.line!("Hello, World!")
}
```

This defines our program's entrypoint. The code inside the curly braces will run when we do `roc main.roc`.

## Pure and effectful functions

That `|_args| { ... }` is Roc syntax for an anonymous function with one argument, named `_args`. (If it had multiple arguments, 
it would look like `|arg1, arg2| { ... }` instead.) Functions in Roc are ordinary values, just
like numbers, strings, or booleans; we can pass them around, put them in collections, and use `=` to name them.

We named it `main!` (and not the traditional `main`) because it's a function that runs the 
[_side effect_](https://en.wikipedia.org/wiki/Side_effect_(computer_science)) of printing to
[stdout](https://en.wikipedia.org/wiki/Standard_streams#Standard_output_(stdout)). 
In Roc, we call functions that can run side effects _effectful functions_,  and by convention 
we always name them with a `!` at the end of their names. In contrast, 
[_pure functions_](https://en.wikipedia.org/wiki/Pure_function) don't have a `!` at the end of their names.

## Standard I/O

As you might guess, this prints the line `"Hello, World!"` to stdout:

```ruby
Stdout.line!("Hello, World!")
```

As you might also guess, `Stderr.line!` prints to stderr instead. (After adding an
`import pf.Stderr` above, of course!)

### Reading from stdin

We can make our program interactive by replacing `main!` with a mix of `Stdout` and `Stdin`:

```ruby
main! = || {
    Stdout.line!("What's your name?")
    name = Stdin.line!()
    Stdout.line!("Hello, ${name}!")
}
```

Just like with `main!`, the functions `Stdout.line!` and `Stdin.line!` end in an `!` because they're effectful functions:
- `Stdout.line!` has the side effect of printing to stdout
- `Stdin.line!` reads from standard in, meaning it can potentially return a different answer every time you call it (unlike a pure function, which must return the same answer when given the same arguments). 

> `Stdin.line!` also has the subtle side effect of consuming data from the sdtin stream, although we'd only notice that side effect if we were reading from the same stdin stream in parallel.

### Constants

This line defines a new _constant_ called `name`:

```ruby
name = Stdin.line!()
```

Constants can't be reassigned or shadowed, so if you try to do `name =` again in the same scope, 
`roc` will give a compile-time error.

### String interpolation

The string `"Hello, ${name}!"` will evaluate to `"Hello, "` followed by whatever `name` is, followed by `"!"`.

Note that `name` must be a string! Roc's string interpolation does not automatically convert other types
to strings. For example, if you wanted to print an integer you'd need to call `to_str` on it like so:

```ruby
Stdout.line!("Number of things: ${thing_count.to_str()}")
```

You can put any expression you like inside string interpolation, although it all has to be on a single line.

If you really wanted to, you could do something like this:

```ruby
Stdout.line!("Answer: ${((numerator / denominator) + 1).negate().to_str()}")
```

...but at that point it'd probably be easier to read if extracted that expression into a constant.

## `for` and `fold`

Sometimes code is easier to understand in a functional style, and other times it's easier to understand
in an imperative style. Roc has support for both styles, even though its APIs are designed around immutable
values and a functional style.

### Expect

To see both styles, let's write a function called `digits_to_num` which takes a list of digits and returns 
the number they represent. When we're done, we'll be able to run `roc test` and see these `expect`s pass:

```ruby
expect digits_to_num([1, 2, 3]) == 123
expect digits_to_num([4, 2]) == 42
expect digits_to_num([7]) == 7
```

`expect` is Roc's lightweight testing keyword. You can put a boolean expression after it, and if that expression
evaluates to `True`, the test will pass, and if it evaluates to `False`, the test will fail. When you run `roc test`,
it runs all of top-level `expect`s in your files, as well as all the files they `import`.

> Another useful command is `roc fmt`, which formats your source code according to standard Roc style. By design, `roc fmt` has no configuration options at all.

### `for` style

Here's an imperative-style implementation of `digits_to_num`, using a `for` loop and reassignable `var`s:

```ruby
digits_to_num = |digits| {
    var $num = 0
    
    for digit in digits {
        $num = ($num * 10) + digit
    }
  
    $num
}
```

Unlike a _constant_, a `var` like `$num` can be reassigned. Similarly to how effectful functions have names 
ending in `!` to distinguish them from pure functions, vars begin with `$` to distinguish them from constants. 
This means any time you see something beginning with `$`, you know it might be reassigned somewhere (such as 
in a `for` loop), whereas if you don't see the `$`, there's no need to think about that possibility.

Note that vars can only be reassigned inside the function where they were declared. This code would give an error:

```ruby
var $count = 0

things.for_each!(|thing| {
    $count = $count + 1
    
    # other logic goes here
})
```

The error would say that `$count` can't be reassigned inside a different function from where it was declared with `var`.

As such, if you see a `for_each!` being used instead of a `for` loop, that tells you whatever logic inside is guaranteed
not to modify any vars in the outer scope—whereas if you see a `for` loop, you know that it might be modifying vars.

### `fold` style

You can also implement `digits_to_num` in a functional style using `fold`:

```ruby
digits_to_num = |digits| digits.fold(0, |num, digit| (num * 10) + digit)
```

### Blocks

Note that there are no curly braces in this one. That's because actually every Roc function has one expression
after its arguments (e.g. you can write `|arg| arg.to_str()`), but that expression can be a _block_ if you like.
Here's an example of a block expression:

```ruby
answer = {
    inner_constant = foo - 1
    
    Stdout.line!("Inner constant: ${inner_constant.to_str()}")
    
    inner_constant.abs()
}
```

Block expressions go inside curly braces. They end with an expression (like `inner_constant.abs()` here), 
and the entire block evaluates to whatever that expression evaluates to. This is why, when we use curly
braces in functions, the function returns whatever is at the end of the curly braces—that's just how blocks work!

### Statements and expressions

Everything before the expression at the end of a block is a _statement_. The difference between statements
and expressions is:

- An _expression_ evaluates to a value.
- A _statement_ does not.

`for` loops and constant declarations like `inner_constant =` are statements. Since statements don't evaluate to
values, you can't do things like pass them as function arguments. You couldn't write `Stdout.line!(for ...)` or
`Stdout.line!(inner_constant = ...)` - you'd get a compile-time error if you did.

Blocks are expressions, so you *could* write something like:

```ruby
Stdout.line!({
    for ... {
      
    }
    
    some_expression 
})
```

An easy way to think of it is that blocks are a way to incorporate statements into your expressions.

### `return` statements

Roc's `return` keyword works the same way it works in most languages: it causes the function to immediately return.

```ruby
digits_to_num = |digits| {
    if digits.is_empty() {
        return 0
    }
    
    # ...the rest of the function would go here
}
```

If you use this in a block, you may get a warning if any statements or expressions come after it in the block,
as they will not be executed!

### `crash` statements

Roc's `crash` keyword does what it says: it crashes the currently-running Roc application.

```ruby
digits_to_num = |digits| {
    if digits.is_empty() {
        crash "TODO add proper error handling."
    }
    
    # ...the rest of the function would go here
}
```

The current tutorial—which is for an older version of Roc—has [a useful section on `crash`](https://www.roc-lang.org/tutorial#crashing). The section on [crashing for error handling](https://www.roc-lang.org/tutorial#crashing-for-error-handling)
is especially important, and has been copy/pasted here:

> `crash` is not for error handling.
> The reason Roc has a `crash` keyword is for scenarios where it's expected that no error will ever happen (like in [unreachable branches](https://www.roc-lang.org/tutorial#crashing-in-unreachable-branches)), or where graceful error handling is infeasible (like running out of memory).
> Errors that are recoverable should be represented using normal Roc types (like `Try`) and then handled without crashing. For example, by having the application report that something went wrong, and then continue running from there.

Just like `return`, if you use `crash` in a block, you may get a warning if any statements or expressions 
come after it in the block, as they will not be executed!

### `expect` statements

We mentioned `expect` earlier - if you put these at the top level of your file, they will be run whenever `roc test` runs.

```ruby
digits_to_num = |digits| {
    if digits.is_empty() {
        return 0
    }
        
    # From here on, we assume digits is nonempty!
    expect !digits.is_empty() 
    
    # ...the rest of the function would go here
}
```

You can also put them in blocks, in which case they will work essentially like a `crash` when you're doing `roc test` or
a debug build of `roc`, but when you do `roc --optimize`, they will be skipped.

Importantly, these are _not_ production [assertions](https://en.wikipedia.org/wiki/Assertion_(software_development))! 
The point is that these are checks of things you assume will be true, and if they turn out not to be true, 
you would like to be alerted about the assumption proving false during development or when running test. It is the 
responsibility of other code to handle (or not) the situation where these assumptions turn out to be false.

For example, here are three different ways you can handle an assumption turning out to be false in production:
- Detect it and gracefully recover from it, because that's the best experience for people using the software
- Do not attempt to detect it, because doing so is either impractical to implement or too costly in terms of runtime performance; accept that if it happens, it will be bad, but trying to detect it defensively would be worse overall
- Detect it and crash the program using the `crash` keyword

All three of these have different tradeoffs, and different situations can reasonably call for one over the others.

The point of `expect` working the way it does is that it does not run in `--optimize` builds at all, 
so it does not have production tradeoffs! You can use it as often as you like, and the consequences will 
only be felt during development.

### `dbg` statements

You can use the `dbg` keyword for the classic technique of _printline debugging_, like so:

```ruby
main! = |_args| {
    x = 5
    
    dbg x
    
    Ok({})
}
```

Although printing is an I/O operation, the `dbg` statement can be used even in pure functions. It works in two different ways:

- Roc evaluates all pure functions that are run on constants at compile time when possible. (This is known as [constant folding](https://en.wikipedia.org/wiki/Constant_folding), and Roc does it as much as possible.) When you put a `dbg` inside a pure function, and that pure function gets run at compile time, you'll see the `dbg` output at compile tie.
- At runtime, `dbg` may work differently depending on where you're running the program. For example, in this command-line application it will be printed to stderr. However, when running a Roc program that's compiled to WebAssembly and running in the browser, it would likely appear in the browser console instead.

Note that Roc's compile-time evaluation means there is no guarantee that a given `dbg` will run at runtime or at compile time. 

## Conditionals and Collections

### `if` expressions

In Roc, `if` can be used as an expression like so:

```ruby
name = if str.is_empty() "n/a" else str
```

You can optionally use blocks to make the different branches stand out more:

```ruby
name = if str.is_empty() {
    "n/a" 
} else {
    str
}
```

### Records

You can also create records, like so: 

```ruby
record = if str.is_empty() {
    { name: "n/a", has_name: Bool.False }
} else {
    { name: str, has_name: Bool.True }
}
```

You can "update" a record by creating a new one that has some of its fields changed:

```ruby
new_record = { ..record, name: "New Name" }
```

You can also destructure a record to bring some of its fields into scope as constants:

```ruby
{ name, has_name } = record
```

### Lists

In Roc, a `List` looks like this:

```ruby
animals = ["bird", "crab", "lizard"]
```

### Method Calling

You can get the length of the list by calling `List.len(animals)`, or as a shortcut, you can just call `animals.len()`.

Both of them do the same thing. When you call `.len()`, Roc's type inference knows that the type of `animials` is `List`,
so it translates that `animals.len()` call into `List.len(animals)` at compile time. `.len()` is known as a _method_ because
it's a function that is associated with a particular type (in this case, `List`).

### Pattern Matching

You can get the first element in the list using `animals.first()`, but you can also do it using pattern matching:

```ruby
points = match animals {
    ["bird", "crab", "lizard"] => 10 # exact match
    ["bird", "crab", ..] => 5 # partial match
    ["bird", ..] => 1 # partial match
    [first, second, "lizard", ..] => count_points(first, second)
    _ => 0 # default
}
```

Patterns can nest as deeply as you like; if you had a list of lists of strings, you could do a pattern like `[first_list, ["bird", ..], ..] =>` etc.

The default branch (`_ =>`) at the end is necessary so that it's always clear what value `points` should become,
since lists can be any length and have lots of different contents.

### Tags

All items in a list must have compatible types. 

The following will give a compile error because it's a list with both strings and numbers in it:

```ruby
animals = ["eagle", 1]
```

That said, you can _tag_ each of them like so:

```ruby
birds_or_numbers = [Bird("eagle"), Number(1)]
```

#### Pattern matching on tags

You can use pattern matching to access the contents of tags:

```ruby
label = match birds_or_numbers {
    [Bird(bird), Number(num)] => "${bird} number ${num.to_str()}"
    _ => "" # default
}
```

### `Try`

Roc does not have `null`, `nil`, `undefined`, or anything similar. Instead, it uses the tags `Ok` and `Err` to
represent whether an operation succeeded or failed:

```ruby
numbers = [1, 2 ,3 ]
number = match numbers.first() {
    Ok(first) => first + 1
    Err(ListWasEmpty) => 0
}
```

Here, `ListWasEmpty` is a tag that isn't wrapping anything. The `List.first` method is just returning it inside the `Err`
to describe what the failure was. This both makes the code more self-documenting and also lets you distinguish between
different error types.

For example:

```ruby
num_or_err = if numbers.is_empty() {
    I64.from_str("1")
} else {
    numbers.first()
}

answer = match num_or_err {
    Ok(num) => num + 1
    Err(ListWasEmpty) => 0
    Err(BadNumStr) => -1
}
```

Here we have an extra `Err` branch, because `List.first` can return `Err(ListWasEmpty)` if the list was empty,
whereas `I64.from_str` can return `Err(BadNumStr)`.

> Note: `I64` is a number type—specifically, a 64-bit integer. Roc also supports 8-bit, 16-bit, 32-bit, and 128-bit integers, and they can be either signed (like `I64`) or unsigned (like `U64`). For non-integer types, Roc has `F32` and `F64` for the classic 32-bit and 64-bit binary floating point numbers, and also `Dec` for a 128-bit fixed-point decimal. If you don't specify a number type, Roc uses `Dec` as the default number—which is why in Roc, `0.1 + 0.2 == 0.3` is `True`, whereas [in most languages it isn't](rtfeldman.com/0.1-plus-0.2).

#### Exhaustiveness

Neither of these `match` expressions had a default `_ =>` branch. They didn't need it because they are already _exhaustive_,
which means they have exhaustively covered all possible cases. In fact, if you tried to add an `_ =>` branch, the compiler
would give a warning that it was unreachable.

> Note: As of December 1, 2025, exhaustiveness checking has not been ported over from the old compiler to the new one,
> so you won't actually get these errors yet!

Roc code tends to avoid `_ =>` default branches because these exhaustiveness errors can be helpful for telling you when
you've forgotten to handle something. For example, in our second `match` above, if we'd written `_ => 0` instead of
`Err(ListWasEmpty) => 0`, we would have been silently handling the `Err(BadNumStr)` case using the same logic. That might
not have been what we wanted! By writing out `Err(ListWasEmpty)` instead of `_ =>`, the compiler would let us know if we
were forgetting to handle any cases that could come up at runtime.

#### `Try` methods

Both `List.first` and `I64.from_str` are returning an extremely common Roc type, named `Try`. We define and pattern-match
`Str` values using `"…"`, `List` values using `[…]`, and `Try` values using `Ok` and `Err`. So if I had a function that 
accepted a `Try` with strings for both its `Ok` and `Err` types, then I could pass it `Ok("foo")` or `Err("bar")`.

Just like `Str` and `List`, `Try` has methods. Here's an example of one:

```
number = numbers.first().ok_or(0)
```

#### Underscore patterns

The `Try.ok_or` method is defined like this:

```ruby
ok_or = |try, fallback| match try {
    Ok(val) => val
    Err(_) => fallback
}
```

It returns the value inside the `Ok` tag, or else the provided fallback value if the `Try` was an `Err` tag instead of `Ok`.

The underscore pattern inside `Err(_) =>` essentially means to ignore that part of the pattern. You can put underscores
anywhere in any pattern, including for the entire pattern. This method doesn't want to match a more specific pattern 
(like `Err(ListWasEmpty) =>` earlier) because it wants to be flexible. If it matched a more restrictive pattern, like
`Err(ListWasEmpty) =>`, then you couldn't use `.ok_or` with `I64.from_str` because it returns `Err(BadNumStr)`.

#### The `?` postfix operator

It's common to want to early-return an `Err` from a `Try`. It's so common, Roc has a dedicated operator for it:

```ruby
increment_first = |strings| {
    first_str = strings.first()?
    first_num = I64.from_str(first_str)?
    
    Ok(first_num + 1)
}
```

The `?` postfix operator is syntax sugar for "if this is an `Ok`, evaluate to its value; otherwise, `return` the `Err`."

The desugared version of the above function would be:

```ruby
increment_first = |strings| {
    first_str = match strings.first() {
        Ok(val) => val
        Err(err) => return Err(err)
    }
    
    first_num = match I64.from_str(first_str) {
        Ok(val) => val
        Err(err) => return Err(err)
    }
    
    Ok(first_num + 1)
}
```

The `?` version is quite a bit more concise!

## Types

So far we haven't seen any types. That's because although Roc is a statically type-checked language, it infers the types
of everything you write. All type annotations in Roc are optional, but the compiler still infers every type, so you'll
still get compile-time errors if you mix up types. Technically, Roc has [sound](https://en.wikipedia.org/wiki/Type_safety#Definitions), [decidable](https://en.wikipedia.org/wiki/Type_system), [principal](https://en.wikipedia.org/wiki/Principal_type) static type inference. All Roc values are semantically immutable, making them free of [Data races](https://en.wikipedia.org/wiki/Race_condition#Data_race) as well.

Roc has a "nonblocking compilation" design philosophy. This means that `roc` will still run your program and `roc test`
will still run your tests, even if you have compile-time errors—including static type mismatches. The assumption is 
that your editor will have tooling to tell you about any compile-time errors, so if you're choosing to run anyway, 
you want to be unblocked to try something out despite knowing there are problems that might cause it to `crash`. 

You can use `roc check && roc` for a one-line command that will check for errors first and then only actually run the 
program if there were none.

### Type Annotations

You can write type annotations above any constant or `var`, like so:

```ruby
name : Str
name = "Sam"

is_empty : Bool
is_empty = name.is_empty()
```

### Parameterized types

We noted earlier how all items in a `List` must have compatible types. That's represented in the `List` type 
using a _type parameter_ like so:

```ruby
strings : List(Str)
strings = ["a", "b", "c"]

integers : List(I64)
integers = [1, 2, 3]
```

### Pure and effectful function types

Pure functions use a thin arrow (`->`) and effectful functions use a thick arrow (`=>`) to separate parameter types from
return types:

```ruby
average : Dec, Dec -> Dec
average = |a, b| (a + b) / 2

# Note that you don't have to write out 2.0 to perform decimal division in Roc; you can just write 2 like normal!

read_str! : Path => Try(Str, ReadFileErr)
read_str! = |path| # ...
```

`Try` type is a parameterized type with two type parameters. The first one is the `Ok` type and the second one is the `Err` type.

### Type variables

### Structural types

#### Tag union types

It's very common to see a `Try` with a tag union for its error type, even if it only has one tag in it. This allows
multiple errors to neatly combine into a tag union of all the possible errors that could occur, so you can pattern
match on them later like we saw with `I64.from_str` and `List.first`.

### Nominal types

As we saw with `ListWasEmpty` and `BadNumStr`, Tags don't have to wrap anything. You can also use them as enumerations, like so:

```ruby
Bool := [True, False]
```

This is a _nominal_ type definition.

## Dependencies

Earlier we mentioned that we'd revisit the long URL in the first line of `main.roc`:

```ruby
app [main!] { pf: platform "https://github.com/lukewilliamboswell/roc-platform-template-zig/releases/download/0.6/2BfGn4M9uWJNhDVeMghGeXNVDFijMfPsmmVeo6M4QjKX.tar.zst" }
```

Now is the time to revisit it! Here is what this is doing:

- `app` means this .roc file specifies a Roc _application_ - an executable, as opposed to a bundle of reusable code like a package
- `[main!]` specifies the application's *entrypoint*. Some applications have multiple entrypoints, but it's most common to have just one—and also it's most common for that one to be named `main!`
- `{ pf: platform "https://..." }` specifies the application's *platform*. If we wanted to add other dependencies, this is where we'd specify them - e.g. we might write `pg: "https://..."` to add a dependency on [roc-pg](https://github.com/agu-z/roc-pg) for PostgreSQL access, at which point we'd be able to do things like `import pg.Cmd` and so on.

### Platforms

Roc has a first-class concept of _platforms_ and _applications_. You can [read about the design philosophy](https://www.roc-lang.org/platforms), but for our purposes what matters is:

- Every Roc application specifies [exactly one platform](https://www.roc-lang.org/faq#multiple-platforms) that it will be built on
- The platform provides all the I/O primitives (such as `Stdout` and `Stdin` - that's why they're imported as `pf.Stdout` and `pf.Stdin`)
- Roc's standard library does not include any effectful functions; they all come from the platform. We still have some platforms published in the old version of the compiler (e.g. [basic-cli](https://roc-lang.github.io/basic-cli/0.20.0/Stdout/) and [basic-webserver](https://roc-lang.github.io/basic-webserver/0.13.0/Http/#Request)) but at least as of December 1, 2025, the only platform that has been ported over to the new compiler is the one in this tutorial!

# Additional Resources

For more, check out:
- [All builtin functions](https://github.com/roc-lang/roc/blob/main/src/build/roc/Builtin.roc)
- [A single file demonstrating all Roc syntax](https://github.com/lukewilliamboswell/roc-platform-template-zig/blob/main/examples/all_roc_syntax.roc)
- [Roc getting started guide](https://github.com/rickhull/roc-init) (community contributed)
