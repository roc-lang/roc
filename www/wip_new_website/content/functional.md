# Functional

Roc is designed to have a small number of simple language primitives. This goal leads Roc to be a single-paradigm [functional](https://en.wikipedia.org/wiki/Functional_programming) language, while its [performance goals](/fast) lead to some design choices that are uncommon in functional languages.

## Opportunistic mutation

All Roc values are semantically immutable, but may be opportunistically mutated behind the scenes when it would improve performance (without affecting the program's behavior). For example:

```roc
colors
|> Set.insert "Purple"
|> Set.insert "Orange"
|> Set.insert "Blue"
```

The [`Set.insert`](https://www.roc-lang.org/builtins/Set#insert) function takes a `Set` and returns a `Set`. The returned one has the given value inserted into it. Knowing this, it might seem like these three `Set.insert` calls would result in the creation of three brand-new sets, but Roc's *opportunistic mutation* optimizations mean this will be much more efficient than that.

Opportunistic mutation works by detecting when a semantically immutable value can be safely mutated in-place without changing the behavior of the program. If `colors` is *unique* here—that is, nothing else is currently referencing it, and nothing else will reference it before it goes out of scope—then `Set.insert` will mutate it and then return it. Cloning it first would have no benefit, because nothing in the program could possibly tell the difference!

If `colors` is not unique, then the first call to `Set.insert` will not mutate it. Instead, it will clone `colors`, insert `"Purple"` into the clone, and then return that. At that point, since the clone will be unique (nothing else is referencing it, since it was just created, and the only thing that will reference it in the future is the `Set.insert` function it's handed off to), the subsequent `Set.insert` calls will all mutate in-place. Roc has ways of detecting uniqueness at compile time, so this optimization will often have no runtime cost, but in some cases it instead uses automatic reference counting to tell when something that was previously shared has become unique over the course of the running program.

## Everything is immutable (semantically)

This design means that all Roc values are semantically immutable, even though they can still benefit from the performance of in-place mutation. In many languages, this is reversed; everything is mutable, and it's up to the programmer to "defensively" clone wherever modification is undesirable. Roc's approach means that cloning happens automatically, which can be less error-prone than defensive cloning (which might be accidentally forgotten), but which—to be fair—can also increase unintentional cloning. It's a different default with different tradeoffs.

A performance benefit of this design compared to having direct mutation primitives is that it lets Roc rule out [reference cycles](https://en.wikipedia.org/wiki/Reference_counting#Dealing_with_reference_cycles). Any language which supports direct mutation can have reference cycles. Detecting these cycles automatically at runtime has a cost (which has similar characteristics to the cost of a tracing garbage collector), and failing to detect them can result in memory leaks. Roc's automatic reference counting neither pays for runtime cycle collection nor memory leaks from cycles, because the language's lack of direct mutation primitives lets it rule out reference cycles at language design time.

An ergonomics benefit of having no direct mutation primitives is that functions in Roc tend to be chainable by default. For example, consider the `Set.insert` function. In many languages, this function would be written to accept a `Set` to mutate, and then return nothing. In contrast, in Roc it will necessarily be written to return a (potentially) new `Set`, even if in-place mutation will end up happening anyway if it's unique.

This makes Roc functions naturally amenable to pipelining, as we saw in the earlier example:

```roc
colors
|> Set.insert "Purple"
|> Set.insert "Orange"
|> Set.insert "Blue"
```

To be fair, direct mutation primitives have benefits too. Some algorithms are more concise or otherwise easier to read when written with direct mutation, and direct mutation can make the performance characteristics of some operations clearer.

As such, Roc's opportunistic mutation design means that reference cycles can be ruled out, and functions will tend to be more ameanable for chaining, but also that some algorithms will be harder to express, and that performance optimization will likely tend to involve more profiling. These tradeoffs fit well with the language's overall design goals.

## No reassignment or shadowing

In some languages, the following is allowed.

```
x = 1
x = 2
```

In Roc, this will give a compile-time error. Once a name has been assigned to a value, nothing in the same scope can assign it again. (This includes [shadowing](https://en.wikipedia.org/wiki/Variable_shadowing), which is disallowed.) This can make Roc code easier to read, because the answer to the question "might this have a different value at some later point in the scope?" is always "no." That said, this can also make Roc code take longer to write, because of needing to come up with unique names to avoid shadowing, although pipelining (as shown in the previous section) reduces how often intermediate values need to be named.

A benefit of this design is that it makes Roc code easier to rearrange without causing regressions. Consider this code:

```roc
func = \arg ->
    greeting = "Hello"
    welcome = \name -> "\(greeting), \(name)!"
    # …
    message = welcome "friend"
    # …
```

Suppose I decide to extract the `welcome` function to the top level, so I can reuse it elsewhere:

```roc
func = \arg ->
    # …
    message = welcome "Hello" "friend"
    # …

welcome = \prefix, name -> "\(prefix), \(name)!"
```

Without knowing the rest of `func`, we can be confident this change will not alter the code's behavior. In contrast, suppose Roc allowed reassignment. Then it's possible something in the `# …` parts of the code could have modified `greeting` before it was used in the `message =` declaration. For example:

```roc
func = \arg ->
    greeting = "Hello"
    welcome = \name -> "\(greeting), \(name)!"
    # …
    if someCondition then
        greeting = "Hi"
        # …
    else
        # …
    # …
    message = welcome "friend"
    # …
```

In this example, if we didn't read the whole function to see that `greeting` was later sometimes (but not always) changed from `"Hello"` to `"Hi"`, we might not have realized that changing it to `message = welcome "Hello" "friend"` would cause a regression due to having the greeting always be `"Hello"`. Because Roc disallows reassignment, this particular regression can't happen, and so the code can be confidently rearranged without checking the rest of the function.

Even if Roc disallowed reassignment but allowed shadowing, a similar regression could happen if the `welcome` function were shadowed between when it was defined here and when `message` later called it in the same scope. Because Roc allows neither shadowing nor reassignment, these regressions can't happen, and rearranging code can be done with more confidence.

In fairness, reassignment has benefits too. For example, using it with [early-exit control flow operations](https://en.wikipedia.org/wiki/Control_flow#Early_exit_from_loops) such as a `break` keyword can be a nice way to represent certain types of logic without incurring extra runtime overhead. Roc does not have these operations; looping is done either with convenience functions like [`List.walkUntil`](https://www.roc-lang.org/builtins/List#walkUntil) or with recursion (Roc implements [tail-call optimization](https://en.wikipedia.org/wiki/Tail_call), including [modulo cons](https://en.wikipedia.org/wiki/Tail_call#Tail_recursion_modulo_cons)), but early-exit operators can potentially make some code easier to follow (and potentially even slightly more efficient) when used in scenarios where breaking out of nested loops with a single instruction is desirable.

## Managed effects over side effects

Many languages support first-class [asynchronous](https://en.wikipedia.org/wiki/Asynchronous_I/O) effects, which can improve a system's throughput (usually at the cost of some latency) especially in the presence of long-running I/O operations like network requests. Asynchronous effects are commonly represented by a value such as a [Promise or Future](https://en.wikipedia.org/wiki/Futures_and_promises) (Roc calls these Tasks), which represent an effect to be performed. These values can be composed together, potentially while customizing their concurrency properties and supporting I/O interruptions like cancellation and timeouts.

Most languages also have a separate system for synchronous effects, namely [side effects](https://en.wikipedia.org/wiki/Side_effect_(computer_science)). Having demand for two ways to perform the same effect can lead to a great deal of duplication across a language's ecosystem.

Instead of having [side effects](https://en.wikipedia.org/wiki/Side_effect_(computer_science)), Roc functions exclusively use *managed effects* in which they return descriptions of effects to run, in the form of Tasks. Tasks can be composed and chained together, until they are ultimately handed off (usually via a `main` function or something similar) to an effect runner outside the program, which actually performs the effects the tasks describe.

Having only (potentially asynchronous) *managed effects* and no (synchronous) *side effects* both simplifies the language's ecosystem and makes certain guarantees possible. For example, the combination of managed effects and semantically immutable values means all Roc functions are [pure](https://en.wikipedia.org/wiki/Pure_function)—that is, they have no side effects and always return the same answer when called with the same arguments.

## Pure functions

Pure functions have a number of implementation benefits, such as [referential transparency](https://en.wikipedia.org/wiki/Referential_transparency) and being trivial to [memoize](https://en.wikipedia.org/wiki/Memoization). They also have testing benefits; for example, Roc tests which either use simulated effects (or which do not involve tasks at all) never flake. They either consistently pass or consistently fail. Because of this, their results can be cached, so `roc test` can skip re-running them unless their source code (including dependencies) changed. (This caching has not yet been implemented, but is planned.)

Roc does support [tracing](https://en.wikipedia.org/wiki/Tracing_(software)) via the `dbg` keyword, an essential [debugging](https://en.wikipedia.org/wiki/Debugging) tool which is unusual among side effects in that—similarly to opportunistic mutation—using it should not affect the behavior of the program. As such, it usually does not impact the guarantees of pure functions aside from potentially impeding optimizations. (An example of an exception to this would be if a program sent its `dbg` traces to log files which the program itself then read back in. This is not recommended.)

Pure functions are notably amenable to compiler optimizations. Roc already takes advantage of them to implement [function-level dead code elimination](https://elm-lang.org/news/small-assets-without-the-headache). Here are some other examples of optimizations that will benefit from this in the future; these are planned, but not yet implemented:

- [Loop fusion](https://en.wikipedia.org/wiki/Loop_fission_and_fusion), which can do things like combining consecutive `List.map` calls (potentially intermingled with other operations that traverse the list) into one pass over the list.
- [Compile-time evaluation](https://en.wikipedia.org/wiki/Compile-time_function_execution), which basically takes [constant folding](https://en.wikipedia.org/wiki/Constant_folding) to its natural limit: anything that can be evaluated at compile time is evaluated then. This saves work at runtime, and is easy to opt out of: if you want evaluation to happen at runtime, you can instead wrap the logic in a function and call it as needed.
- [Hoisting](https://en.wikipedia.org/wiki/Loop-invariant_code_motion), which moves certain operations outside loops to prevent them from being re-evaluated unnecessarily on each step of the loop. It's always safe to hoist calls to pure functions, and in some cases they can be hoisted all the way to the top level, at which point they become eligible for compile-time evaluation.

## Get started

If this design sounds interesting to you, you can give Roc a try by heading over to the [tutorial](/tutorial)!
