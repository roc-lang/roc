# Statements

Statements are run as soon as they are encountered at runtime.
They do not [evaluate](expressions#evaluation) to a [value](expressions#values).

## [`=` (assignment)](#assignment) {#assignment}

An _assignment statement_ gives a name to a [value](expressions#values) inside the current scope.

```roc
answer = 42
```

The name an assignment gives must be a valid Roc [lowercase name](naming#lowercase-names).

### [Pattern matching in assignments](#assignment-patterns) {#assignment-patterns}

You can use [pattern matching](pattern-matching) in assignments to do things like destructuring:

```roc
(x, y) = (1.1, 2.2)
```

The pattern you use here must be [exhaustive](pattern-matching#exhaustiveness). For example, the following would give an exhaustiveness error because it doesn't specify what to do if `list.first()` returned an `Err` instead of `Ok`:

```roc
Ok(item) = list.first()
```

If you can't write an exhaustive pattern-match, you can use [`match`](pattern-matching#match) instead of an assignment.

### [Assignment Order](#assignment-order) {#assignment-order}

Assignments inside expressions can only reference names that were assigned earlier in scope.
For example, this would be an error:

```roc
foo({
    y = z + 1
    z = 5

    z + 1
})
```

However, at the top level of a module, assignments can reference each other
regardless of declaration order:

```roc
x = y + 1
y = 5
```

### [Assignment Cycles](#assignment-cycles) {#assignment-cycles}

Top-level assignments can only mutually reference each other if they are all assigning to functions.
This gives an error at compile time:

```roc
x = y + 1
y = x + 1
```

(If it did not give an error at compile time, it would either crash or loop infinitely at runtime.)

In contrast, this gives no error because all the assignments in the cycle are assigning to functions:

```roc
x = |arg| if arg >= 1 { y(arg + 1) } else { 0 }
y = |arg| if arg <= 9 { x(arg + 1) } else { 0 }
```

### [Reassignment with `var`](#reassignment) {#reassignment}

Reassigning to an existing name is only allowed when the name was declared with
[`var`](naming#var-keyword). This is allowed:

```roc
var $foo = 0
$foo = 1
```

However, this gives a [shadowing](naming#shadowing) warning:

```roc
foo = 0
foo = 1
```

## [`import`](#import) {#import}

The `import` statement imports a [type](types) into scope from a [type module](modules#type-modules).

```roc
import Color
import json.Parser
```

See [`import` statements](modules#import-statements) for details on how imports work.

### [`import` with `exposing`](#import-exposing) {#import-exposing}

Adding `exposing` and a list of names to an `import` brings those associated items of the
imported type into scope directly, so they can be used without the type's name in front:

```roc
import Color exposing [to_str, all]

red_str = to_str(Red) # instead of Color.to_str(Red)
```

The type itself is still imported as usual, so `Color.to_str` also continues to work.

### [Renaming Imports with `as`](#renaming-imports) {#renaming-imports}

Adding `as` and an [uppercase name](naming#uppercase-names) to an `import` makes the imported type
available under that name instead of its original name:

```roc
import json.Parser as JsonParser

parser : JsonParser
```

This is useful when two imported types would otherwise have the same name, or when a name is
long and used often.

### [Importing non-Roc files](#importing-non-roc-files) {#importing-non-roc-files}

An `import` can also bring the contents of an arbitrary file into scope, by giving the file's path
as a string, then `as`, then a name and a type:

```roc
import "config.json" as config_text : Str
import "logo.png" as logo_bytes : List(U8)
```

The path is relative to the `.roc` file containing the `import`. The file is read at compile time,
and its contents are embedded in the compiled program as a constant, so the program doesn't need
to read the file at runtime (and the file doesn't need to exist when the program runs).

The type must be either `Str` or `List(U8)`:

- `Str` gives the file's contents as a string. It's a compile-time error if the file isn't valid UTF-8.
- `List(U8)` gives the file's raw bytes, which works for any file, including binary files such as images.

It's a compile-time error if the file can't be found.

## [`dbg`](#dbg) {#dbg}

The `dbg` statement prints the value of an expression, for debugging purposes:

```roc
total = price * quantity
dbg total
```

When this runs, it prints something like `[dbg] 42` to the program's standard error. The value is
printed using [`Str.inspect`](../Str#inspect), so `dbg` works on values of any type which has an
inspect representation (including the `to_inspect` method described in
[static dispatch](static-dispatch#well-known-methods)).

`dbg` can be used anywhere statements can appear, including inside [pure functions](functions#pure-functions).
This is allowed because `dbg` output is only for the programmer, so program behavior never depends on it.

`dbg` statements that run during [compile-time evaluation](compile-time) print their output when
the program is compiled (for example, during `roc check` or `roc build`), rather than when the compiled
program runs. `dbg` statements that run at runtime print their output each time they run.

`dbg` is intended for temporary debugging, so building an optimized program (which is what `roc build`
does by default) gives a warning for each `dbg` statement it contains.

## [`expect`](#expect) {#expect}

The `expect` statement states that a [`Bool`](../Bool) expression should evaluate to `True`.
It can be used for tests, and for checking assumptions at runtime.

### Top-level `expect`

An `expect` at the top level of a module is a test:

```roc
double = |n| n * 2

expect double(21) == 42
```

Top-level `expect`s are run by the `roc test` command, which reports how many passed and failed.
When one fails, the report shows the failing expression, along with the values of any top-level
names it referenced.

`roc test` runs the top-level `expect`s in the given module and in every module it imports (including
modules in packages that are imported through a filesystem path, but not packages downloaded
from a URL, since those `expect`s are the responsibility of the package's author).

Inside a top-level `expect`, the [`?` operator](operators#-unwrap-if-ok-early-return-if-err) causes
the `expect` to fail if its expression evaluates to an `Err`, rather than returning early from a function.

### Inline `expect`

An `expect` inside a function body checks an assumption each time that code runs:

```roc
withdraw = |balance, amount| {
    expect amount <= balance

    balance - amount
}
```

If an inline `expect`'s condition evaluates to `False`, the failure is reported, and then
the program continues running as if the `expect` had not been there. (Unlike
[`crash`](#crash), a failed `expect` does not stop the program.) Exactly how the failure is
reported is up to the platform.

Inline `expect`s are for catching bugs during development, so they are omitted from optimized
builds (such as the default for `roc build`). As such, programs should never depend on
an `expect` running.

## [`return`](#return) {#return}

The `return` statement immediately exits a function, returning the given value.

```roc
my_func = |arg| {
    if arg == 0 {
        return 0

        # This line will never be reached.
    }

    arg - 1
}
```

## [`break`](#break) {#break}

The `break` statement immediately exits the innermost `for` or `while` loop. See [loops](loops#break-statement) for details.

## [`continue`](#continue) {#continue}

(This has not been implemented yet. It will continue to the next iteration of a `for` or `while` loop.)

## [`crash`](#crash) {#crash}

A `crash` statement crashes the running application. All code following the `crash`
becomes unreachable and will not be executed.

```roc
if some_condition {
    crash "There is no way this program could possibly continue."
}

# This line will never be reached if `some_condition` was `True`
```

What happens after a `crash` is determined by the platform. Some may gracefully recover
and have some way of continuing the process, but others may terminate the process immediately.

## [Block Statements](#block-statements) {#block-statements}

A _block statement_ is a group of statements which has its own scope, so
anything [assigned](#assignment) in it can't be accessed outside the block.

It's different from a [block expression](expressions#block-expressions) in that
a block statement does not have an expression at the end. A common block
statement is one that does an early `return` in a conditional branch:

```roc
if foo {
    …
} else {
    bar = …

    return bar
}
```

Having a single statement in a block statement is allowed:

```roc
if foo {
    …
} else {
    return bar
}
```
