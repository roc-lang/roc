# Mini Tutorial for Roc

Roc is still a work in progress and has not reached a 0.1 release, so expect missing features and bugs.

The [Roc website](https://www.roc-lang.org) and its [standard library and language documentation](https://www.roc-lang.org/docs/main/)
describe the current compiler and language design. Documentation for the previous alpha4 compiler is
[archived separately](https://alpha4-roc.cc02oj5kr.workers.dev/) and should only be used when working with alpha4 code.

If you want help, the best place to get it is [Roc Zulip](https://roc.zulipchat.com/). You're welcome to ask any questions in [#beginners](https://roc.zulipchat.com/#narrow/channel/231634-beginners/).

With those disclaimers in mind, let's get into the adventure!

## Hello, World

First, follow the [installation guide](https://www.roc-lang.org/install/) to install a nightly build.
It includes an executable named `roc` (or `roc.exe` on Windows). You'll know it is available on your `PATH`
if `roc version` prints a version beginning with `Roc compiler version nightly-`.

Next, copy/paste this into a new file named `main.roc`:

```ruby
main! = |_args| {
    echo!("Hello, World!")
    Ok({})
}
```

You can run this with:

```bash
roc main.roc
```

You should see this:

```
Hello, World!
```

Hooray!

> Tip: If you don't provide a path to a .roc file, `roc` will default to "main.roc" - so since your Roc program is named main.roc, you can run your program by just running `roc` in the future.

## The REPL

You can also try Roc expressions and definitions without creating a file by running `roc repl`.

```bash
roc repl
```

Inside the REPL, enter an expression to evaluate it:

```roc
1 + 1
```

You can define names and use them in later inputs:

```roc
x = 1 + 1
x * 2
```

The REPL also works well with piped input. In piped mode it prints only evaluation output to stdout; prompts, the welcome banner, and the goodbye message are not printed.

In PowerShell, send multiple input lines with an array:

```powershell
@("x = 1 + 1", "x * 2") | roc repl
```

You should see:

```text
assigned `x`
4.0
```

In Unix shells, use `printf` for the same thing:

```bash
printf 'x = 1 + 1\nx * 2\n' | roc repl
```

Diagnostics from piped input are written to stderr, so tools can read successful results from stdout separately from parse and type errors. If you want plain diagnostics without ANSI color codes, use `roc repl --no-color` or set `NO_COLOR` environment variable.

## The `main!` function

Let's take a look at `main!` next:

```ruby
main! = |_args| {
    echo!("Hello, World!")
    Ok({})
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

### Constants

This line defines a new _constant_ called `name`:

```ruby
name = "Rocco"
```

Constants should not be reassigned or shadowed, if you try to do `name =` again in the same scope, 
`roc` will give a compile-time warning with exit code 2. That way, you can quickly write something with
shadowing if you want but the non-zero exit code prevents it from ending up in production code because CI will fail.

### String interpolation

The string `"Hello, ${name}!"` will evaluate to `"Hello, Rocco!"`.

In an ordinary string interpolation, each interpolated expression must also produce a string; Roc does not automatically convert other types. For example, if you wanted to print an integer you'd need to call `to_str` on it like so:

```ruby
echo!("Number of things: ${thing_count.to_str()}")
```

You can put any expression you like inside a string interpolation. If you really wanted to, you could do something like this:

```ruby
echo!("Answer: ${((numerator / denominator) + 1).negate().to_str()}")
```

...but at that point it'd probably be easier to read if you extracted that expression into a constant.

## `for` and `fold`

Sometimes code is easier to understand in a functional style, and other times it's easier to understand
in an imperative style. Roc has support for both styles, even though its APIs are designed around immutable
values and a functional style.

### Expect

To see both styles, let's write a function called `digits_to_num` which takes a list of digits and returns the number they represent. When we're done, we'll be able to run `roc test` and see these `expect`s pass:

```ruby
expect digits_to_num([1, 2, 3]) == 123
expect digits_to_num([4, 2]) == 42
expect digits_to_num([7]) == 7
```

`expect` is Roc's lightweight testing keyword. You can put a boolean expression after it, and if that expression
evaluates to `True`, the test will pass, and if it evaluates to `False`, the test will fail. When you run `roc test`,
it runs all of top-level `expect`s in your files, as well as all the files they `import` (unless they belong to downloaded packages).

A file run with `roc test` does not need a `main!` function (or an `app` header, which will be introduced shortly).

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

Note that there are no curly braces in the `digits_to_num` definition. That's because every Roc function has one expression
after its arguments (e.g. you can write `|arg| arg.to_str()`), but that expression can be a _block_ if you like.
Here's an example of a block expression:

```ruby
answer = {
    foo = 43
    inner_constant = foo - 1
    
    echo!("Inner constant: ${inner_constant.to_str()}")
    
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
values, you can't do things like pass them as function arguments. You couldn't write `echo!(for ...)` or
`echo!(inner_constant = ...)` - you'd get a compile-time error if you did.

Blocks are expressions, so you *could* write something like this inside an effectful function:

```ruby
echo!({
    var $total = 0
    for number in 1..=10 {
        $total = $total + number
    }

    $total.to_str()
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
        crash "Expected a nonempty list."
    }

    # ...the rest of the function would go here
}
```

The archived tutorial for Roc alpha4 has [a useful section on `crash`](https://alpha4-roc.cc02oj5kr.workers.dev/tutorial#crashing). The section on [crashing for error handling](https://alpha4-roc.cc02oj5kr.workers.dev/tutorial#crashing-for-error-handling)
is especially important, and has been copy/pasted here:

> `crash` is not for error handling.
> The reason Roc has a `crash` keyword is for scenarios where it's expected that no error will ever happen (like in [unreachable branches](https://alpha4-roc.cc02oj5kr.workers.dev/tutorial#crashing-in-unreachable-branches)), or where graceful error handling is infeasible (like running out of memory).
> Errors that are recoverable should be represented using normal Roc types (like `Try`) and then handled without crashing. For example, by having the application report that something went wrong, and then continue running from there.

Just like `return`, if you use `crash` in a block, you may get a warning if any statements or expressions 
come after it in the block, as they will not be executed!

### `expect` statements

We mentioned `expect` earlier - if you put these at the top level of your file, they will be run whenever `roc test` runs.

You can also put them in blocks, in which case they will work essentially like a `crash` when you're doing `roc test` or
a debug build of `roc`, but when you do `roc --opt=speed`, they will be skipped.

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

Importantly, these are _not_ production [assertions](https://en.wikipedia.org/wiki/Assertion_(software_development))! 
The point is that these are checks of things you assume will be true, and if they turn out not to be true, 
you would like to be alerted about the assumption proving false during development or when running test. It is the 
responsibility of other code to handle (or not) the situation where these assumptions turn out to be false.

For example, here are three different ways you can handle an assumption turning out to be false in production:
- Detect it and gracefully recover from it, because that's the best experience for people using the software
- Do not attempt to detect it, because doing so is either impractical to implement or too costly in terms of runtime performance; accept that if it happens, it will be bad, but trying to detect it defensively would be worse overall
- Detect it and crash the program using the `crash` keyword

All three of these have different tradeoffs, and different situations can reasonably call for one over the others.

The point of `expect` working the way it does is that it does not run in `--opt=speed` builds at all,
so it does not have production tradeoffs! You can use it as often as you like, and the consequences will
only be felt during development.

### `dbg` statements

You can use the `dbg` keyword for the classic technique of _printline debugging_, like so:

```ruby
main! = |_args| {
    x = 5
    
    dbg x
    # dbg(x) works too
    
    Ok({})
}
```

Although printing is an I/O operation, the `dbg` statement can be used even in pure functions. It works in two different ways:

- Roc evaluates top-level constants at compile time. If evaluating one calls a pure function containing `dbg`, you'll see the `dbg` output at compile time. (Evaluating known expressions ahead of time is called [constant folding](https://en.wikipedia.org/wiki/Constant_folding).)
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

Both of them do the same thing. When you call `.len()`, Roc's type inference knows that the type of `animals` is `List`,
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

> Note: `I64` is a number type—specifically, a 64-bit integer. Roc also supports 8-bit, 16-bit, 32-bit, and 128-bit integers, and they can be either signed (like `I64`) or unsigned (like `U64`). For non-integer types, Roc has `F32` and `F64` for the classic 32-bit and 64-bit binary floating point numbers, and also `Dec` for a 128-bit fixed-point decimal. If you don't specify a number type, Roc uses `Dec` as the default number—which is why in Roc, `0.1 + 0.2 == 0.3` is `True`, whereas [in most languages it isn't](https://rtfeldman.com/0.1-plus-0.2/).

#### Exhaustiveness

Neither of these `match` expressions had a default `_ =>` branch. They didn't need it because they are already _exhaustive_,
which means they have covered all possible cases. If a `match` on a known type omits a possible case, the compiler reports
a non-exhaustive match error. If you add `_ =>` after earlier branches have already covered every case, the compiler warns
that the pattern is redundant.

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
still get compile-time errors if you mix up types. Technically, Roc has [sound](https://en.wikipedia.org/wiki/Type_safety#Definitions), [decidable](https://en.wikipedia.org/wiki/Type_system), [principal](https://en.wikipedia.org/wiki/Principal_type) static type inference. All Roc values are semantically immutable, making them free of [data races](https://en.wikipedia.org/wiki/Race_condition#Data_race) as well.

Roc has a "nonblocking compilation" design philosophy. This means that `roc` will still run your program and `roc test`
will still run your tests when possible, even if you have compile-time errors—including static type mismatches. The
diagnostics are still printed and the command exits with a nonzero status, so CI will treat the run as a failure.
This lets you try code despite known problems, although reaching invalid code at runtime may cause it to `crash`.

Use `roc check && roc` if you want to check for errors first and run the program only when there are none.

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

Type variables allow you to write functions that work with any type. They are written as lowercase identifiers (like `a`, `b`, `elem`, etc.) in type annotations:

```ruby
# This function works for a list of any type
type_var : List(a) -> List(a)
type_var = |lst| lst
```

The type variable `a` indicates that the function accepts a `List` containing elements of any type, and returns a `List` containing elements of that same type.

You can also constrain type variables to types that have specific methods using `where`:

```ruby
stringify : a -> Str where [a.to_str : a -> Str]
stringify = |value| value.to_str()
```

This function works for any type `a` that has a `to_str` method which takes an `a` and returns a `Str`.

### Structural types

#### Tag union types

It's very common to see a `Try` with a tag union for its error type, even if it only has one tag in it. This allows
multiple errors to neatly combine into a tag union of all the possible errors that could occur, so you can pattern
match on them later like we saw with `I64.from_str` and `List.first`.

### Nominal types

As we saw with `ListWasEmpty` and `BadNumStr`, tags don't have to wrap anything. You can also use them as enumerations, like so:

```ruby
Color := [Red, Green, Blue]
```

This is a _nominal_ type definition.

## Alpha4 vs Current Roc

| Alpha4 | Current |
|--------|---------|
| `List U8` | `List(U8)` |
| `if/then/else` | `if/else` |
| `Bool.true`/`Bool.false` | `Bool.True`/`Bool.False` |
| `Result` | `Try` |
| `Inspect.to_str` | `Str.inspect` |
| `Num.to_str(123)` | `123.to_str()` |

## Dependencies

The initial Hello World example is a headerless app. Headerless apps automatically use Roc's built-in
Echo Platform, which provides the unqualified `echo!` function and is intentionally minimal.
Other Roc apps select a platform explicitly. For example, this header uses
[basic-cli](https://github.com/roc-lang/basic-cli), a platform for command-line applications:

```ruby
app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.22.2/9zUBxb1LtXYVc4eR4hAtd1WQDwBYDhM6HQdZz1UFCm2m.tar.zst" }
```

Let's break the header down:

- `app` means this .roc file specifies a Roc _application_ - an executable, as opposed to a bundle of reusable code like a package
- `[main!]` specifies the application's *entrypoint*. Some applications have multiple entrypoints, but it's most common to have just one—and also it's most common for that one to be named `main!`
- `{ pf: platform "https://..." }` specifies the application's *platform*. If we wanted to add other dependencies, this is where we'd specify them - e.g. we might write `uni: "https://..."` to add a dependency on [unicode](https://github.com/roc-lang/unicode) to process Unicode strings, at which point we'd be able to do things like `import uni.Grapheme` and so on.

See the [modules documentation](https://www.roc-lang.org/docs/main/langref/modules/) for more examples.

### Platforms

Roc has a first-class concept of _platforms_ and _applications_. You can [read about the design philosophy](https://www.roc-lang.org/docs/main/langref/platforms/), but for our purposes what matters is:

- Every Roc application specifies [exactly one platform](https://www.roc-lang.org/faq#multiple-platforms) that it will be built on.
  A headerless app implicitly uses the Echo Platform.
- The selected platform provides the app's I/O primitives. For example, the `basic-cli` platform gives access to the standard in and out using `Stdout` and `Stdin`, which are imported using the platform's shorthand, such as `pf.Stdout` and `pf.Stdin`.
- Roc's standard library does not include any effectful functions; they all come from the platform. Several published platforms still use the old version of the compiler but ports are in progress!

## Additional Resources

For more, check out:

- [Roc's standard library and language documentation](https://www.roc-lang.org/docs/main/)
- [Examples built and tested with the current compiler](https://www.roc-lang.org/examples/)
- [The source for all builtin functions](https://github.com/roc-lang/roc/blob/main/src/build/roc/Builtin.roc)
- [A single file demonstrating Roc syntax](https://github.com/roc-lang/roc/blob/main/test/echo/all_syntax_test.roc)
- [The Exercism.org Roc track](https://exercism.org/tracks/roc/)
