# Functions

Functions are first-class values in Roc. They can be passed to other functions, returned,
stored in data structures and so on.

Roc has only one syntax for defining functions, namely the "lambda" syntax:

```roc
my_fn = |arg1, arg2| arg1 + arg2
```

Some languages have alternative syntaxes for defining functions. Roc intentionally does not,
for the same reason that it doesn't have alternative syntax for defining any other type of
value (such as numbers or strings): functions are ordinary values, and all values in Roc are
defined using the same `name = value` syntax.

Functions take a fixed number of arguments. Calling a function always passes all of them at
once, as in `my_fn(1, 2)`, and there's no automatic [currying](https://en.wikipedia.org/wiki/Currying)
or partial application. If you want a function that has some of its arguments filled in already,
you can write one with a [closure](#closures): `|arg2| my_fn(1, arg2)`.

## Pure Functions

[Pure functions](https://en.wikipedia.org/wiki/Pure_function) are functions which:

* Always return the same answer when given the same values for arguments
* Have no _side effects_ (side effects involve either performing I/O or mutating state outside the function)

Roc makes a first-class distinction between _pure functions_ and _effectful functions_ (functions
that are not pure), and uses that distinction to enable automatic performance optimizations. A
function is effectful if it calls another effectful function, and otherwise it's pure. Effectful
functions can only be called by other effectful functions; pure functions and top-level
constants can only call pure functions.

A common example of a performance optimization where Roc's compiler makes use of this
distinction: all top-level values are evaluated at compile time. Pure functions always return
the same answers when given the same inputs, and top-level values all have inputs that are
fixed at compile time. That means running the pure functions at compile time can't possibly
give different answers than if they were run at runtime.

Pure functions also have no side effects. That said, pure functions in Roc can run things
that can reasonably be considered side effects:

1. Crashing. Roc pure functions can crash (or get stuck in an infinite loop), as they are not guaranteed to be _total_. If they crash at compile time, it will be reported as a compile error, which is considered an improvement over the alternative of the end user of the program encountering a crash at runtime. If they get stuck in an infinite loop, this currently hangs the compiler (although there are plans to improve this in the future), which is also considered better than the alternative of an end user encountering a program hang at runtime.
2. Memory allocation and deallocation. Although memory allocation and deallocation absolutely does depend on mutating state for bookkeeping, Roc does automatic memory management, and so program correctness should never depend on the state of this bookkeeping. Allocation can fail, which in Roc results in a crash—and as previously noted, a crash at compile time is considered preferable to the end user encountering a program crash at runtime.
3. `dbg` and `expect` output. Pure functions are allowed to use `dbg` and `expect`, as these outputs are intended to be for the programmer only. By design, program behavior should never depend on them, so it's considered fine to display these outputs at compile time only (or not at all, if their code paths end up getting optimized away entirely), as this only means the programmer will see them even earlier in the process.

## Effectful Functions

An _effectful function_ is a function that may perform [side effects](#side-effects) when it's called.
For example, `echo!` is an effectful function which prints a string:

```roc
greet! : Str => {}
greet! = |name| echo!("Hello, ${name}!")
```

A function is effectful if it calls another effectful function. Here, `greet!` is effectful because
it calls `echo!`. The type of an effectful function uses `=>` instead of `->` (see
[Function Type Annotations](#function-type-annotations)), and its name ends in `!` (see
[`!` suffix in function names](#-suffix-in-function-names)).

Effectful functions can only be called from within other effectful functions. That means
[pure functions](#pure-functions) can't call them, and neither can top-level constants:

```roc
message = greet!("Sam") # Error: top-level values can't perform effects

shout : Str -> {}
shout = |name| greet!(name) # Error: this function is annotated as pure
```

This rule is what makes it possible for Roc to evaluate top-level constants and pure function calls
at [compile time](compile-time), because it guarantees none of them will ever perform effects.

Ultimately, every effectful function traces back to one provided by the [platform](platforms),
because Roc code can only perform side effects by calling functions that the platform provides.
The program's entry point is also effectful (for example, `main!`), so that it can call those
functions. When the program runs, the platform calls the entry point, and the entry point calls
other effectful functions as needed.

### Side Effects

A _side effect_ is anything a function does other than computing its return value from its
arguments. Common examples include:

- Reading or writing files
- Printing to the terminal
- Sending or receiving network requests
- Reading the current time
- Generating random numbers
- Reading or changing any state that lives outside the function

A function without side effects always returns the same answer when given the same arguments.
That's not true of a function that reads the current time or a random number, and it's not true of
a function whose return value depends on the contents of a file (which could change between calls).
In Roc, those functions are all effectful.

Some things that might seem like side effects are allowed in pure functions, such as crashing,
memory allocation, and `dbg`; see [Pure Functions](#pure-functions) for details.

## Function Type Annotations

Here's a type annotation for a pure function, and then right below it,
an annotation for an effectful function:

```roc
pure_fn : Str, Str -> Str

run_fx! : Str, Str => Str
```

Each function takes two `Str` values as arguments, and returns a `Str`. Unlike `pure_fn`, `run_fx!` may perform side effects when called.

### `->` and `=>` in function type annotations

Note that `pure_fn` uses a `->` to indicate that it's a pure function, whereas `run_fx!`
uses a `=>` instead to indicate that it's an effectful function. That arrow style is the
way you can tell a pure function annotation apart from an effectful one.

> By design, Roc has no syntax for "either pure or effectful." That is, there's no concept
> of _effect polymorphism_ like you might find in some languages that support algebraic effects.

### `!` suffix in function names

By convention, all effectful functions—and _only_ effectful functions—have names that end
in  `!`. This design has two purposes:

* It makes it easy to see at a glance exactly which parts of your code are potentially performing effects.
* It makes it easy to distinguish between higher-order functions like `Try.map_ok` and `Try.map_ok!` which differ only in the effectfulness of the functions they accept.

Roc's compiler reports a warning if an effectful function's name does not end in `!`.

## Closures

A function can use names from the scope it was defined in, not just its own arguments:

```roc
make_adder : U64 -> (U64 -> U64)
make_adder = |amount| |n| n + amount

add_five = make_adder(5)

answer = add_five(1) # 6
```

Here, the inner function `|n| n + amount` uses `amount`, which isn't one of its arguments. It
_captures_ `amount` from `make_adder`'s scope. A function that captures values this way is
called a [closure](https://en.wikipedia.org/wiki/Closure_(computer_programming)).

Since values in Roc are immutable, a closure captures the captured value itself, not some
location in memory that might change later. So `add_five` will always add 5, no matter what
happens after it was created. (See [Performance](#performance) for how closures are stored.)

## Purity Inference

Roc infers which functions are pure and which are effectful. You can choose to annotate
functions as pure or effectful, and the compiler will warn you if the annotation is incorrect.

For example, suppose you wrote a function which called an effectful function, and annotated
it as if it were a pure function. You'd get a warning saying this is an effectful function
that's been incorrectly annotated as a pure function, but you could still run the program.

This is a useful feature, as it means you can do things like take a function that has been
historically pure and add some debugging that involves doing I/O in the middle of the function.
You'll get a warning (and potentially miss out on some optimizations) but you won't have to do the chore of going around changing a bunch of annotations just to be able to run the program.

(Note: this is not fully implemented yet. Currently, the compiler reports an incorrect purity annotation as a type mismatch error rather than a warning.)

Note that this still doesn't make it possible to call effectful functions at compile time.
The rule still applies that effectful functions can only be called from within other
effectful functions; if you annotate a function as pure, and the compiler warns you that
the annotation is mistaken, that's because the compiler knows the function _is_
effectful—and so can't be run at compile time.

> One reason for this rule is that all effectful functions originate in the platform,
> which provides their implementations using low-level code that has been compiled for a
> specific target system. Roc's compiler does not run platform-provided low-level code
> during compilation, which means that when you run `roc check` or `roc build`, none of
> your dependencies—including platforms—are permitted to perform arbitrary I/O operations
> on your system. You have to actually run the compiled Roc program for that.

## Recursive Functions

Functions can be recursive, meaning they call themselves.

### Self-recursive Functions

_Self-recursive_ functions are functions that call themselves directly.

Here's a `contains` function for lists of strings that calls itself directly:

```roc
contains : List(Str), Str -> Bool
contains = |list, item| match list {
    [] => False
    [first, .. as rest] => {
        if first == item {
            True
        } else {
            contains(rest, item) # recursion
        }
    }
}
```

By default, recursive functions have an increased risk of stack overflowing compared to
non-recursive functions, although [tail-call optimization](#tail-call-optimization)
can eliminate that risk for some recursive functions. (In doing so, the optimization does
mean a function that recurses forever will loop forever instead of overflowing the stack.)

## Tail Calls

A _tail call_ is a function call followed immediately by returning from the current function,
without doing any other work in between.

In the previous example, `contains(rest, item)` is a tail call. It's calling a function and then
immediately returning without doing any other work. In this case, it's calling itself (making
this a _self-tail-call,_ which is also known as _self-tail-recursion_), although tail calls
can be to other functions too.

### Self-Tail Calls

A _self-tail call_ is a tail call where a function calls itself. For example, here's a
function which computes a factorial:

```roc
factorial : U64 -> U64
factorial = |n| if n <= 1 1 else n * factorial(n - 1)
```

The recursive call `factorial(n - 1)` is _not_ a tail call, because after it returns, there's still
work to do: its result gets multiplied by `n`. That means each call needs to remember its `n` until
the recursive call returns, so calling `factorial(n)` requires `n` nested calls to be in progress
at the same time, each one using some stack space.

Here's a version that uses a self-tail call instead, by passing along an accumulated result as an
extra argument:

```roc
factorial : U64 -> U64
factorial = |n| factorial_help(n, 1)

factorial_help : U64, U64 -> U64
factorial_help = |n, acc| if n <= 1 acc else factorial_help(n - 1, n * acc)
```

Now the recursive call `factorial_help(n - 1, n * acc)` is the last thing the function does, so it's
a self-tail call. This version is eligible for [tail-call optimization](#tail-call-optimization),
which means it will run as a loop and can't overflow the stack.

Rewriting a function to pass along an accumulator like this is a common way to turn a recursive
call into a self-tail call.

### Tail-Call Optimization

Compilers can optimize tail calls in various ways. Here are some that Roc's compiler performs:

- If a self-recursive function only ever calls itself using tail calls, the entire function will be optimized into a `while` loop behind the scenes, and all the recursive calls will be eliminated. This optimization makes the function run faster, and makes it impossible for the function to stack overflow (although it can now loop forever), and otherwise will not affect observable program behavior.
- Tail calls _modulo cons_ get the same treatment, as described in the next section.
- When a tail call is to a _different_ function, the function being called takes over the
  current function's stack space instead of using more. This is guaranteed, and it includes
  calling a function value (like a function that was passed in as an argument). So a group of
  functions that only ever call each other using tail calls can't overflow the stack either. See
  [Mutually Recursive Functions](#mutually-recursive-functions).

One visible consequence of that last one is that a function which has made a tail call is no
longer on the stack, so it won't show up in a stack trace taken later on.

#### Modulo Cons

Some recursive calls aren't tail calls, but are _almost_ tail calls: the only thing that happens
to the result of the recursive call is that it gets wrapped in a tag, which is then returned.
This is sometimes described as being a tail call _modulo cons_, where "cons" refers to constructing
a value. Here's an example:

```roc
LinkedList(a) := [Nil, Cons(a, LinkedList(a))]

count_up : U64, U64 -> LinkedList(U64)
count_up = |current, end| {
    if current >= end {
        Nil
    } else {
        Cons(current, count_up(current + 1, end))
    }
}
```

The recursive call `count_up(current + 1, end)` is not a tail call, because after it returns, its
result gets put inside a `Cons` tag. However, Roc's compiler performs an optimization called
_tail recursion modulo cons_: when a self-recursive function's result is a tag of its own
(recursive) return type, and a recursive call's result goes directly into that tag, the compiler
allocates the tag first, and then has the recursive call write its result directly into the tag's
payload. This way, nothing is left to do after the recursive call, so it becomes a loop just like
a self-tail call would.

This means that `count_up(0, 1_000_000)` builds a linked list with a million elements without
overflowing the stack. It also means you don't need to rewrite functions like this one using an
accumulator in order to get this benefit.

## Mutually Recursive Functions

_Mutually recursive_ functions are functions that call each other. If one function calls
another, and that function calls the first one, then the first function did end up calling
itself (so, recursing)—just with the other function being involved in the middle.

```roc
is_even : U64 -> Bool
is_even = |n| if n == 0 True else is_odd(n - 1)

is_odd : U64 -> Bool
is_odd = |n| if n == 0 False else is_even(n - 1)
```

Both `is_odd(n - 1)` and `is_even(n - 1)` are [tail calls](#tail-calls), so calling
`is_even(10_000_000)` goes back and forth between the two functions ten million times without
overflowing the stack. Each call takes over the stack space of the one that called it, so the
stack never grows.

## Performance

### Calls

Calling a function whose name you wrote directly, like `contains(rest, item)`, compiles to an
ordinary direct call. The compiler can also [inline](https://en.wikipedia.org/wiki/Inline_expansion)
small functions, meaning it replaces the call with the function's body, so that there's no
call at all.

Calling a [generalized](types#generalization) function costs the same as calling any other
function, because each type it's used with gets its own
[monomorphized](types#performance) version.

### Arguments and Reference Counts

When a function only reads an argument (rather than, say, returning it or storing it in a
data structure), passing that argument doesn't change its
[reference count](expressions#reference-counting). The compiler figures this out
automatically, by looking at what each function does with each argument.

This is also what makes [opportunistic mutation](expressions#opportunistic-mutation)
work well across function calls. If you pass a list to a function that only reads it, the
list is still unique afterward, so you can keep updating it in place.

### Function Values

When you pass a function as an argument, return one, or store one in a record, the
compiler knows every function that could possibly end up there. So rather than storing a
pointer to a function's machine code, it stores the function value like a
[tag union](tag-unions), with one tag for each function that could be there. Each tag's
payload holds the values that function captured.

For example, in this code, the value returned by `pick` is either `double` or a closure that
captured an `amount`:

```roc
double = |n| n * 2

pick : Bool -> (U64 -> U64)
pick = |should_double| if should_double double else make_adder(10)
```

So `pick`'s return value is stored as if it were a tag union like
`[Double, Adder(U64)]`, and calling it checks the tag and then directly calls the right
function. This has some performance benefits:

- Function values don't need a heap allocation, even when they capture values. They take up
  as much space as the largest set of captured values (plus a tag, if there's more than one
  possible function).
- A function value that's only ever one specific function takes up no space at all if it
  captures nothing, and calling it is exactly as fast as calling that function directly.
- Since every possible call target is known, the compiler can inline them.

This technique is called [defunctionalization](https://en.wikipedia.org/wiki/Defunctionalization).

The exception is functions that cross the boundary between Roc and the [platform](platforms),
since the platform is compiled separately and the compiler can't see what it does with them.
Those are stored as a heap allocation that holds a pointer to the function along with its
captured values.

> Note that with `roc build --specialize=no`, all function values are stored this way
> instead. See the [types performance section](types#performance) for more on that option.
