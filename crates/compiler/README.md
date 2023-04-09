# The Roc Compiler

Here's how the compiler is laid out.

## Parsing

The main goal of parsing is to take a plain old String (such as the contents a .roc source file read from the filesystem) and translate that String into an `Expr` value.

`Expr` is an `enum` defined in the `expr` module. An `Expr` represents a Roc expression.

For example, parsing would translate this string...

    "1 + 2"

...into this `Expr` value:

    BinOp(Int(1), Plus, Int(2))

> Technically it would be `Box::new(Int(1))` and `Box::new(Int(2))`, but that's beside the point for now.

This `Expr` representation of the expression is useful for things like:

- Checking that all variables are declared before they're used
- Type checking

> As of this writing, the compiler doesn't do any of those things yet. They'll be added later!

Since the parser is only concerned with translating String values into Expr values, it will happily translate syntactically valid strings into expressions that won't work at runtime.

For example, parsing will translate this string:

not "foo", "bar"

...into this `Expr`:

    CallByName("not", vec!["foo", "bar"])

Now we may know that `not` takes a `Bool` and returns another `Bool`, but the parser doesn't know that.

The parser only knows how to translate a `String` into an `Expr`; it's the job of other parts of the compiler to figure out if `Expr` values have problems like type mismatches and non-exhaustive patterns.

That said, the parser can still run into syntax errors. This won't parse:

    if then 5 then else then

This is gibberish to the parser, so it will produce an error rather than an `Expr`.

Roc's parser is implemented using the [`marwes/combine`](http://github.com/marwes/combine-language/) crate.

## Evaluating

One of the useful things we can do with an `Expr` is to evaluate it.

The process of evaluation is basically to transform an `Expr` into the simplest `Expr` we can that's still equivalent to the original.

For example, let's say we had this code:

    "1 + 8 - 3"

The parser will translate this into the following `Expr`:

    BinOp(
        Int(1),
        Plus,
        BinOp(Int(8), Minus, Int(3))
    )

The `eval` function will take this `Expr` and translate it into this much simpler `Expr`:

    Int(6)

At this point it's become so simple that we can display it to the end user as the number `6`. So running `parse` and then `eval` on the original Roc string of `1 + 8 - 3` will result in displaying `6` as the final output.

> The `expr` module includes an `impl fmt::Display for Expr` that takes care of translating `Int(6)` into `6`, `Char('x')` as `'x'`, and so on.

`eval` accomplishes this by doing a `match` on an `Expr` and resolving every operation it encounters. For example, when it first sees this:

    BinOp(
        Int(1),
        Plus,
        BinOp(Int(8), Minus, Int(3))
    )

The first thing it does is to call `eval` on the right `Expr` values on either side of the `Plus`. That results in:

1. Calling `eval` on `Int(1)`, which returns `Int(1)` since it can't be reduced any further.
2. Calling `eval` on `BinOp(Int(8), Minus, Int(3))`, which in fact can be reduced further.

Since the second call to `eval` will match on another `BinOp`, it's once again going to recursively call `eval` on both of its `Expr` values. Since those are both `Int` values, though, their `eval` calls will return them right away without doing anything else.

Now that it's evaluated the expressions on either side of the `Minus`, `eval` will look at the particular operator being applied to those expressions (in this case, a minus operator) and check to see if the expressions it was given work with that operation.

> Remember, this `Expr` value potentially came directly from the parser. `eval` can't be sure any type checking has been done on it!

If `eval` detects a non-numeric `Expr` value (that is, the `Expr` is not `Int` or `Frac`) on either side of the `Minus`, then it will immediately give an error and halt the evaluation. This sort of runtime type error is common to dynamic languages, and you can think of `eval` as being a dynamic evaluation of Roc code that hasn't necessarily been type-checked.

Assuming there's no type problem, `eval` can go ahead and run the Rust code of `8 - 3` and store the result in an `Int` expr.

That concludes our original recursive call to `eval`, after which point we'll be evaluating this expression:

    BinOp(
        Int(1),
        Plus,
        Int(5)
    )

This will work the same way as `Minus` did, and will reduce down to `Int(6)`.

## Optimization philosophy

Focus on optimizations which are only safe in the absence of side effects, and leave the rest to LLVM.

This focus may lead to some optimizations becoming transitively in scope. For example, some deforestation
examples in the MSR paper benefit from multiple rounds of interleaved deforestation, beta-reduction, and inlining.
To get those benefits, we'd have to do some inlining and beta-reduction that we could otherwise leave to LLVM's
inlining and constant propagation/folding.

Even if we're doing those things, it may still make sense to have LLVM do a pass for them as well, since
early LLVM optimization passes may unlock later opportunities for inlining and constant propagation/folding.

## Inlining

If a function is called exactly once (it's a helper function), presumably we always want to inline those.
If a function is "small enough" it's probably worth inlining too.

## Fusion

<https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/deforestation-short-cut.pdf>

Basic approach:

Do list stuff using `build` passing Cons Nil (like a cons list) and then do foldr/build substitution/reduction.
Afterwards, we can do a separate pass to flatten nested Cons structures into properly initialized RRBTs.
This way we get both deforestation and efficient RRBT construction. Should work for the other collection types too.

It looks like we need to do some amount of inlining and beta reductions on the Roc side, rather than
leaving all of those to LLVM.

Advanced approach:

Express operations like map and filter in terms of toStream and fromStream, to unlock more deforestation.
More info on here:

<https://wiki.haskell.org/GHC_optimisations#Fusion>

## Getting started with the code

The compiler contains a lot of code! If you're new to the project it can be hard to know where to start. It's useful to have some sort of "main entry point", or at least a "good place to start" for each of the main phases.

After you get into the details, you'll discover that some parts of the compiler have more than one entry point. And things can be interwoven together in subtle and complex ways, for reasons to do with performance, edge case handling, etc. But if this is "day one" for you, and you're just trying to get familiar with things, this should be "good enough".

The compiler is invoked from the CLI via `build_file` in cli/src/build.rs

| Phase                                 | Entry point / main functions                     |
| ------------------------------------- | ------------------------------------------------ |
| Compiler entry point                  | load/src/file.rs: load, load_and_monomorphize    |
| Parse header                          | parse/src/module.rs: parse_header                |
| Parse definitions                     | parse/src/module.rs: module_defs                 |
| Canonicalize                          | can/src/def.rs: canonicalize_defs                |
| Type check                            | solve/src/module.rs: run_solve                   |
| Gather types to specialize            | mono/src/ir.rs: PartialProc::from_named_function |
| Solve specialized types               | mono/src/ir.rs: from_can, with_hole              |
| Insert reference counting             | mono/src/ir.rs: Proc::insert_refcount_operations |
| Code gen (optimized but slow)         | gen_llvm/src/llvm/build.rs: build_procedures     |
| Code gen (unoptimized but fast, CPU)  | gen_dev/src/object_builder.rs: build_module      |
| Code gen (unoptimized but fast, Wasm) | gen_wasm/src/lib.rs: build_module                |

For a more detailed understanding of the compilation phases, see the `Phase`, `BuildTask`, and `Msg` enums in `load/src/file.rs`.

## Debugging the compiler

Please see the [debug flags](./debug_flags/src/lib.rs) for information on how to
ask the compiler to emit debug information during various stages of compilation.

There are some goals for more sophisticated debugging tools:

- A nicer unification debugger, see <https://github.com/roc-lang/roc/issues/2486>.
  Any interest in helping out here is greatly appreciated.

### General Tips

#### Miscompilations

If you observe a miscomplication, you may first want to check the generated mono
IR for your code - maybe there was a problem during specialization or layout
generation. One way to do this is to add a test to `test_mono/src/tests.rs`
and run the tests with `cargo test -p test_mono`; this will write the mono
IR to a file.

#### Typechecking errors

First, try to minimize your reproduction into a test that fits in
[`solve_expr`](./solve/tests/solve_expr.rs).

Once you've done this, check out the `ROC_PRINT_UNIFICATIONS` debug flag. It
will show you where type unification went right and wrong. This is usually
enough to figure out a fix for the bug.

If that doesn't work and you know your error has something to do with ranks,
you may want to instrument `deep_copy_var_help` in [solve](./solve/src/solve.rs).

If that doesn't work, chatting on Zulip is always a good strategy.
