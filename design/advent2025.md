# Roc's New Compiler for Advent of Code 2025

If you're feeling adventurous, you can try Roc's new bleeding-edge rewritten compiler for Advent of Code 2025. We're really excited about it!

That said, *just* got it working to the point where it's usable for these sorts of simple problems, so be warned - this is far from a polished experience! Expect missing features, bugs, and almost no documentation (everyting on [roc-lang.org](https://www.roc-lang.org) is referring to the old compiler and the old design; we haven't updated any of it yet).

The purpose of this document—which has been assembled in great haste at the last minute, since we weren't sure until very recently whether the new compiler would even be usable by December 1—is to give you a sense of how the revised language and new compiler work, since [roc-lang.org](https://www.roc-lang.org) is all about the old stuff.

With those disclaimers in mind, let's get into the adventure!

# Hello, World

First, [grab a nightly build of the new compiler](https://github.com/roc-lang/nightlies/releases). It'll have an executable named `roc` (or `roc.exe` on Windows). You'll want to [put that executable on your PATH] - you'll know it worked if you can run `roc version` in a terminal and see it print something like `Roc compiler version release-fast-123c5d78` (the hash at the very end there will be different from this one, as it changes with each nightly release).

We'll have this automated in the future, but for now you need to `git clone git@github.com:roc-lang/roc.git` and then cd into its root. Then try this:

```bash
$ roc test/fx/app.roc
```

You should see this:

```
Hello from stdout!
Line 1 to stdout
Line 2 to stderr
Line 3 to stdout
Error from stderr!
```

Hooray! Let's look at that `app.roc` file:

```ruby
app [main!] { pf: platform "/Users/kevin/src/roc/test/fx/platform/main.roc" }

import pf.Stdout

main! = || {
    Stdout.line!("Hello, World!")
}
```

For now we're going to ignore the first line (we'll come back to it later) and focus on the rest.

## Imports

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
main! = || {
    Stdout.line!("Hello, World!")
}
```

This defines our program's entrypoint. The code inside the curly braces will run when we do `roc app.roc`.

## Pure and effectful functions

That `|| { ... }` is Roc syntax for an anonymous function with no arguments. (If it had arguments, 
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

### `crash` statements

### `return` statements

## Error handling

### Tag unions

### The `?` postfix operator
