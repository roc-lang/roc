# Functional

Roc is a functional programming language with a set of tradeoffs that are different from those of most functional programming languages.

## Pure functions for performance

Whenever you call a Roc function with the same arguments, it returns the same answer. It also doesn't perform side effects (aside from memory allocation and possibly debug tracing); instead,
Roc uses *managed effects* (discussed [later](#managed-effects)) for things like I/O operations. Also, all Roc data structures are semantically immutable.

Roc takes advantage of these invariants to improve performance. For example, mutable data structures can have data cycles, which is where a value has a reference to itself. Data cycles cause memory leaks for automatic reference counting systems, a performance problem laguages can either ignore (meaning the programmer has to manually deal with the cycles somehow) or else pay for extra runtime logic to detect and collect cyclic data.

In Roc, data cycles are ruled out at language design time. Not supporting direct mutation means there is no way to create a cyclic data structure, so there is no chance of a memory leak and no need to pay for runtime cycle detection. This lets Roc perform automatic reference counting faster (or less prone to memory leaks, or both) than in languages which support direct mutation.

Pure functions are also more amenable to compiler optimizations than functions which have side effects or aren't idempotent. Here are some examples of optimizations that will benefit from this in the future; these are planned, but not yet implemented:

- Automatic deforestation
- Compile-time evaluation - this is basically taking constant folding to its logical conclusion; anything that can be evaluated at compile time is evaluated then. This saves work at runtime, and is easy to opt out of: if you want evaluation to happen at runtime, you can instead wrap the logic in a function and call that as needed. In the design, infinite loops are permitted. Don't do that!

## Opportunistic mutation

This

## Managed effects over side effects

(note that Roc is strictly evaluated? Nah.)

This approach both simplifies the language and makes certain guarantees possible. For example:

* Making tests so determinstic they can be cached and automatically skipped until relevant source code changes.
* No split between synchronous and asynchronous effects. All effects are treated as semantically asynchronous, so the ecosystem never needs to split and you never need to rewrite code after choosing one and later deciding to switch to the other.
* Automatic reference counting not need to spend time on [cycle detection](https://en.wikipedia.org/wiki/Reference_counting#Dealing_with_reference_cycles) because reference cycles only come up in languages that support mutation.

Roc's approach is to minimize the types of major choice where any one could work (such as which paradigm to use) to help focus on building delightful software. simplify have a small set of simple language primitives that work well together. The

At a basic level, these primitives are:

- Functions
- Modules
- Immutable constants
- Types
- Conditionals
- Abilities

Abilities are a way to associate functions with types, for example defining what "equals" should do in a custom data structure. There are also a few language keywords that look like functions—namely, `crash`, `expect`, and `dbg`—and learning how they work is about the same as learning how any function works.
