# `if` / `else`

Like most languages, Roc has `if` and `else` keywords that determine which code to run based on a boolean's value at runtime.

## `if` and `else`

Roc's `if` keyword works exactly like a [`match`](pattern-matching#match) on a boolean. This code:

```roc
if foo {
    bar()
} else {
    baz()
}
```

Does the exact same thing as this code:

```roc
match foo {
    True => bar()
    False => baz()
}
```

Roc does not have a concept of "truthiness" (where values can be "truthy" or "falsy"); `if` accepts only `Bool` values.

## `else if`

Roc does not have a separate `else if` concept like some languages do, but you can write `else` followed immediately by `if` to achieve that functionality. This code...

```roc
if foo {
    bar()
} else {
    if something {
        baz()
    } else {
        blah()
    }
}
```

...does exactly the same as this code, without the extra curly braces:

```roc
if foo {
    bar()
} else if something {
    baz()
} else {
    blah()
}
```

## `if` without `else`

You can write an `if` without an `else`, but only when the body of the `if` evaluates to [`{}`](records#empty-record). For example, this is allowed:

```roc
if foo {
    do_something!()
}
```

It is essentially equivalent to:

```roc
{} = if foo {
    do_something!()
} else {
    {}
}
```

...except that you don't have to write the `{} =` or the `else`. The rules are the same, though; the body of the `if` must evaluate to `{}`, which in practice only makes sense for [effectful functions](functions#effectful-functions), or statements like [`return`](statements#return), [`crash`](statements#crash), or [`expect`](statements#expect).

## `and` / `or`

The keywords `and` and `or` perform [short-circuiting evaluation](https://en.wikipedia.org/wiki/Short-circuit_evaluation). This code:

```roc
a() or b()
```

...does exactly the same thing as:

```roc
if a() True else b()
```

Similarly:

```roc
a() and b()
```

...does exactly the same thing as:

```roc
if a() b() else False
```

Note that Roc does not have `&&` or `||` operators for boolean logic like some languages do. This is mostly because Roc uses `|` to define [functions](functions), and `||` means a function with no arguments. Using the keyword _or_ avoids overloading `||` to mean a function sometimes and a conditional other times.
