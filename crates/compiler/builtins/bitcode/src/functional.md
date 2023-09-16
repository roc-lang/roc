# Functional

Roc is a functional programming language. It doesn't try to support other styles of programming like object-oriented or imperative; instead, it tries to focus on making the most delightful experience possible in one paradigm.

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
