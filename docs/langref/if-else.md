# `if` / `else`

Like most language, Roc has ways to run either one code pathway or another depending on a runtime value.

## `if`

Roc's `if` keyword is syntax sugar for [`match`](pattern-matching#match). This code:

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

## `else if`

## `if` without `else`

## `and` / `or`

The keywords `and` and `or` perform [short-circuiting evaluation](https://en.wikipedia.org/wiki/Short-circuit_evaluation) by desugaring to `if`:

```roc
a() or b()
```

...desugars to:

```roc
if a() True else b()
```

Similarly:

```roc
a() and b()
```

...desugars to:

```roc
if a() b() else False
```

The desugared versions compile to the same machine instructions as the operator versions, even in debug builds.
