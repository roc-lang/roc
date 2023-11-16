# Friendly

Roc prioritizes being a user-friendly language. This impacts the syntax, semantics, and tools Roc ships with.

## Syntax and Formatter

Roc's syntax isn't trivial, but there also isn't much of it to learn. It's designed to be uncluttered and unambiguous. A goal is that you can normally look at a piece of code and quickly get an accurate mental model of what it means, without having to think through several layers of indirection. Here are some examples:

- `x = combine y z` always declares a new constant `x` (Roc has [no mutable variables, reassignment, or shadowing](/functional)) to be whatever the `combine` function returns when passed the arguments `y` and `z`. (Function calls in Roc don't need parentheses or commas.)
- `user.email` always accesses the `email` field of a record named `user`. (Roc has no inheritance, subclassing, or proxying.)
- `Email.isValid` always refers to something named `isValid` exported by a module named `Email`. (Module names are always capitalized, and variables/constants never are.) Modules are always defined statically and can't be modified at runtime; there's no [monkey patching](https://en.wikipedia.org/wiki/Monkey_patch) to consider.
- `"My name is \(Str.trim name)"` uses *string interpolation* syntax: a backslash inside a string literal, followed by an expression in parentheses. This code is the same as combining the string `"My name is "` with the string returned by the function call `Str.trim name`. Because Roc's string interpolation syntax begins with a backslash (just like other backlash-escapes such as `\n` and `\"`), you can always tell which parts of a string involve special handling: the parts that begin with backslashes. Everything else works as normal.

Roc also ships with a source code formatter that helps you maintain a consistent style with little effort. The `roc format` command neatly formats your source code according to a common style, and it's designed with the time-saving feature of having no configuration options. This feature saves you all the time that would otherwise be spent debating which stylistic tweaks to settle on!

## Helpful Compiler

Roc's compiler is designed to help you out. It does complete type inference across all your code, and the type system is *sound*. This means you'll never get a runtime type mismatch if everything type-checked (including null exceptions; Roc doesn't have the [billion-dollar mistake](https://en.wikipedia.org/wiki/Null_pointer#History)), and you also don't have to write any type annotations for the compiler to be able to infer all the types in your program.

If there's a problem at compile time, the compiler is designed to report it in a helpful way. Here's an example:

```
── TYPE MISMATCH ────────────────── /home/my-roc-project/main.roc ─

Something is off with the `then` branch of this `if` expression:

4│      someInteger : I64
5│      someInteger =
6│          if someDecimal > 0 then
7│              someDecimal + 1
                ^^^^^^^^^^^^^^^

This branch is a fraction of type:

    Dec

But the type annotation on `someInteger` says it should be:

    I64

Tip: You can convert between integers and fractions using functions like
`Num.toFrac` and `Num.round`.
```

If you like, you can run a program that has compile-time errors like this. (If the program reaches the error at runtime, it will crash.) This lets you do things like trying out code that's only partially finished, or running tests for one part of your code base while other parts have compile errors. (Note that this feature is only partially completed, and often errors out; it has a ways to go before it works for all compile errors!)

## Serialization Inference

- Type inference is used for schema inference, but you can also spell it out if you like
- Reports errors immediately

## Testing

You can run `roc test` to run all your tests. Each test is declared with the `expect` keyword, and can be as short as one line. For example, this is a complete test:

```roc
## One plus one should equal two.
expect 1 + 1 == 2
```

If the test fails, `roc test` will show you the source code of the `expect`, along with the values of any named variables inside it, so you don't have to separately check what they were. If you write a documentation comment right before it (like `## One plus one should equal two` here), that will also be included in the test output, so you can use that to optionally describe the test if you want to.

In the future, there are plans to add built-in support for [benchmarking](https://en.wikipedia.org/wiki/Benchmark_(computing)), [generative tests](https://en.wikipedia.org/wiki/Software_testing#Property_testing), [snapshot tests](https://en.wikipedia.org/wiki/Software_testing#Output_comparison_testing), simulated I/O (so you don't have to actually run the real I/O operations, but also don't have to change your code to accommodate the tests), and "reproduction replays"—tests generated from a recording of what actually happened during a particular run of your program, which deterministically simulate all the I/O that happened.

- also note: future plan to cache tests so we only re-run tests whose answers could possibly have changed. also maybe note: tests that don't perform I/O are guaranteed not to flake b/c pure functions.

## Future Plans

- Package manager (Currently just URLs, content-hashed to make them immutable) with searchable index and no installation step, global cache of immutable downloads instead of per-project folders (no need to .gitignore anything)
- Step debugger with replay
- Customizable "linter" (e.g. code mods, project-specific rules to enforce)
- Editor plugin ecosystem that works across editors, where plugins ship with packages
- `roc edit`

## Functional

Besides being designed to be [fast](/wip/fast) and friendly, Roc is also a functional programming language.

[What does _functional_ mean here?](/wip/functional)
