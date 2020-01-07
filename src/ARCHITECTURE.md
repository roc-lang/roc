Here's how the compiler is laid out.

# Parsing

The main goal of parsing is to take a plain old String (such as the contents a .roc source file read from the filesystem) and translate that String into an `Expr` value.

`Expr` is an `enum` defined in the `expr` module. An `Expr` represents a Roc expression.

For example, parsing would translate this string...

    "1 + 2"

...into this `Expr` value:

    BinOp(Int(1), Plus, Int(2))

> Technically it would be `Box::new(Int(1))` and `Box::new(Int(2))`, but that's beside the point for now.

This `Expr` representation of the expression is useful for things like:

* Checking that all variables are declared before they're used
* Type checking
* Running Roc code in Interpreted Mode (that is, without having to compile it to Rust first - useful for development, since it's a faster feedback loop, but there's a runtime performance penalty compared to doing a full compile to Rust).

> As of this writing, the compiler doesn't do any of those things yet. They'll be added later!

Since the parser is only concerned with translating String values into Expr values, it will happily translate syntactically valid strings into expressions that won't work at runtime.

For example, parsing will transalte this string:

  not "foo", "bar"

...into this `Expr`:

    CallByName("not", vec!["foo", "bar"])

Now we may know that `not` takes a `Bool` and returns another `Bool`, but the parser doesn't know that.

The parser only knows how to translate a `String` into an `Expr`; it's the job of other parts of the compiler to figure out if `Expr` values have problems like type mismatches and non-exhaustive patterns.

That said, the parser can still run into syntax errors. This won't parse:

    if then 5 then else then

This is gibberish to the parser, so it will produce an error rather than an `Expr`.

Roc's has it's own [parser][./parse/parser.rs].

# Evaluating

One of the useful things we can do with an `Expr` is to evaluate it.

The process of evaluation is basically to transform an `Expr` into the simplest `Expr` we can that's still equivalent to the original.

For example, let's say we had this code:

    "1 + 2 - 3"

The parser will translate this into the following `Expr`:

    BinOp(
        Int(1),
        Plus,
        BinOp(Int(2), Minus, Int(3))
    )

The `eval` function will take this `Expr` and translate it into this much simpler `Expr`:

    Int(6)

At this point it's become so simple that we can display it to the end user as the number `6`.  So running `parse` and then `eval` on the original Roc string of `1 + 8 - 3` will result in displaying `6` as the final output.

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

Now that it's evaluated the expressions on either side of the `Minus`, `eval` will look at the particular operator being applied to those expressoins (in this case, a minus operator) and check to see if the expressions it was given work with that operation.

> Remember, this `Expr` value potentially came directly from the parser. `eval` can't be sure any type checking has been done on it!

If `eval` detects a non-numeric `Expr` value (that is, the `Expr` is not `Int` or `Frac`) on either side of the `Mnus`, then it will immediately give an error and halt the evaluation. This sort of runtime type error is common to dynamic languages, and you can think of `eval` as being a dynamic evaluation of Roc code that hasn't necessarily been type-checked.

Assuming there's no type problem, `eval` can go ahead and run the Rust code of `8 - 3` and store the result in an `Int` expr.

That concludes our original recursive call to `eval`, after which point we'll be evaluating this expression:

    BinOp(
        Int(1),
        Plus,
        Int(5)
    )

This will work the same way as `Minus` did, and will reduce down to `Int(6)`.

