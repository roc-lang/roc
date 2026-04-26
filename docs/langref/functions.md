# Functions

Functions are first-class values in Roc. They can be passed to other functions, returned,
stored in data structures and so on.

Roc has only one syntax for defining functions, namely the "lambda" syntax:

```roc
my_fn = |arg1, arg2| arg1 + arg2
```

Some languages have alternative syntaxes for defining functions. Roc intentionally does not,
for the same reason that it doesn't have alternative syntax for defining any other type of
value (such as numbers or strings): functions are ordinary values, just like any other value,
and all values in Roc are declared using the same sytnax. This syntax design decision is
intended to emphasize that functions are ordinary values like any other.

## Pure Functions

[Pure functions](https://en.wikipedia.org/wiki/Pure_function) are functions which:

* Always return the same answer when given the same values for arguments
* Have no _side effects_ (side effects involve either performing I/O or mutating state outside the function)

Roc makes a first-class distinction between _pure functions_ and _effectful functions_ (functions
that are not pure), and uses that distinction to enable automatic performance optimizations. A
function is effectful if it calls another effectful function, and otherwise it's pure. Effectful
functions can only be called by other effectful functions; pure functions and top-level
contants can only call pure functions.

A common example of a performance optimization where Roc's compiler makes use of this
distinction: all top-level values are evaluated at compile time. Pure functions always return 
the same answers when given the same inputs, and top-level values all have inputs that are
fixed at compile time. That means running the pure functions at compile time can't possibly
give different answers than if they were run at runtime. 

Pure functions also have no side effects. That said, pure functions in Roc can run things
that can reasonably be considered side effects:

1. Crashing. Roc pure functions can crash (or get stuck in an infinite loop), as they are not guaranteed to be _total_. If they crash at compile time, it will be reported as a compile error, which is considered an improvement over the alternative of the end user of the program encountering a crash at runtime. If they get stuck in an infinite loop, this currently hangs the compiler (although there are plans to improve this in the future), which is also considered better than the alternative of an end user encountering a program hang at runtime.
2. Memory allocation and deallocation. Although memory allocation and deallocation absolutely does depend on mutating state for bookkeeping, Roc does automatic memory management, and so program correctness should never depend on the state of this bookkeeping. Allocation can fail, which in Roc results in a crash—and as previously noted, a crash at compile time is considered preferable to the end user encountering a program crash at runtime.
3. `dbg` and `expect` output. Pure functions are allowed to use `dbg` and `expect`, as these are outputs are intended to be for the programmer only. By design, program behavior should never depend on them, so it's considered fine to display these outputs at compile time only (or not at all, if their code paths end up getting optimized away entirely), as this only means the programmer will see them even earlier in the process.

## Function Type Annotations

Here's a type annotation for a pure fucntion, and then right below it, 
an annotation for an effectful function:

```roc
pure_fn : Str, Str -> Str

run_fx! : Str, Str => Str
```

Each function takes two `Str` values as arguments, and returns as `Str`. `run_fx!` may

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
* It makes it easy to distinguish between higher-order functions like `List.keep_if` and `List.keep_if!` which differ only in the effectfulness of the functions they accept.

Roc's compiler reports a warning if this naming convention is not followed.

## Purity inference

Roc infers which functions are pure and which are effectful. You can choose to annotate 
functions as pure or effectful, and the compiler will warn you if the annotation is incorrect.

For example, suppose you wrote a function which called an effectful function, and annotated
it as if it were a pure function. You'd get a warning saying this is an effectful function
that's been incorrectly annotated as a pure function, but you could still run the program. 

This is a useful feature, as it means you can do things like take a function that has been
historically pure and add some debugging that involves doing I/O in the middle of the function. 
You'll get a warning (and potentially miss out on some optimizations) but you won't have to do the chore of going around changing a bunch of annotations just to be able to run the program.

Note that this still doesn't make it possible to call effectful functions at compile time.
The rule still applies that effectful functions can only be called from within other
effectful functions; if you annotate a function as pure, and the compiler warns you that
the annotation is mistaken, that's because the compiler knows the function _is_ 
effectful—and so can't be run at compile time.

> One reason for this rule is that all effectful functions originate in the platform,
> which provides their implementations using low-level code that has been compiled for a 
> specific target system. Roc's compiler does not run platform-provided low-level code
> during compilation, which means that when you run `roc check` or `roc build`, none of
> your dependencies—inclduing platforms—are permitted to perform arbitrary I/O operations 
> on your system. You have to actually run the compiled Roc program for that.

## Recursive Functions

Functions can be recursive, meaning they call themselves.

### Self-recursive Functions

_Self-recursive_ functions are functions that call themselves directly.

Here's an implementation of `List.contains` that calls itself directly:

```
contains : List(item), item -> Bool
contains = |list, item| match list.split_first() {
    Ok((first, rest)) => {
        if first == item {
            True
        } else {
            find(rest, query) # recursion
        }
    }
    Err(WasEmpty) => False
}
```

By default, recursive functions have an increased risk of stack overflowing compared to
non-recursive functions, although [tail-call optimization](#tail-call-optimization) 
can eliminate that risk for some recursive functions. (In doing so, the optimization does
mean a function that recurses forever will loop forever instead of overflowing the stack.)

## Tail Calls

A _tail call_ is a function call followed immediately by returning from the current function,
without doing any other work in between.

In the previous example, `find(rest, query)` is a tail call. It's calling a function and then
immediately returning without doing any other work. In this case, it's calling itself (making
this a _self-tail-call,_ which is also known as _self-tail-recursion),_ although tail calls
can be to other functions too.

### Tail-Call Optimization

Compilers can optimize tail calls in various ways. Here are some that Roc's compiler performs:

- If a self-recursive function only ever calls itself using tail calls, the entire function will be optimized into a `while` loop behind the scenes, and all the recursive calls will be eliminated. This optimization makes the function run faster, and makes it impossible for the function to stack overflow (although it can now loop forever), and otherwise will not affect observable program behavior.

#### Modulo Cons

## Mutually Recursive Functions

_Mutually recursive_ functions are functions that call each another. If one function calls
another, and that function calls the first one, then the first function did end up calling
itself (so, recursing)—just with the other function being involved in the middle.
