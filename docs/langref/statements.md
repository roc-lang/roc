# Statements

Statements are run as soon as they are encountered at runtime.
They do not [evaluate](expressions.md#evaluation) to a [value](expressions.md#value).

## [`=` (assignment)](#assignment) {#assignment}

An _assignment statement_ gives a name to a [value](expressions.md#value) inside the current scope.

### [Pattern matching in assignments](#import-exposing) {#import-exposing}

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

### [Reassignment](#reassignment) {#reassignment}

Reassigning to an existing name is only allowed when the name was declared with
[`var`](pattern-matching#var). This is allowed:

```roc
var $foo = 0
$foo = 1
```

However, this gives a [shadowing](naming.md#shadowing) error:

```roc
foo = 0
foo = 1
```

## [`import`](#import) {#import}

The `import` statement imports a [type](types.md) into scope from a [type module](modules.md#type-modules).

### [`import` with `exposing`](#import-exposing) {#import-exposing}

TODO

### [Renaming Imports with `as`](#renaming-imports) {#renaming-imports}

TODO

### [Importing non-Roc files](#importing-non-roc-files) {#importing-non-roc-files}

TODO

## [`dbg`](#dbg) {#dbg}

TODO

## [`expect`](#expect) {#expect}

TODO

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

(This has not been implemented yet. It will exit a `for` or `while` loop.)

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
anything [assigned](#assignment) in it can't be accessed outsdie the block.

It's different from a [block expression](expressions.md#block-expressions) in that
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

Having a single statement in a block expression is allowed:

```roc
if foo {
    …
} else {
    return bar
}
```
