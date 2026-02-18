# Tuples

A tuple is an ordered, fixed-size collection of values that can have different types. Tuples are useful when you need to group a small number of values together without defining a named record type.

Tuples are stack-allocated and not reference-counted.

## [Tuple Literals](#tuple-literals) {#tuple-literals}

Tuple literals are written with parentheses and comma-separated values:

```roc
point = (10, 20)
mixed = ("hello", 42, True)
nested = ((1, 2), (3, 4))
```

Tuples must have at least two elements. A single value in parentheses is just that value (parentheses for grouping), not a tuple:

```roc
x = (42)  # This is just 42, not a tuple
```

## [Accessing Tuple Elements](#accessing-tuple-elements) {#accessing-tuple-elements}

Tuple elements are accessed using dot notation with a zero-based numeric index:

```roc
point = (10, 20)

x = point.0  # 10
y = point.1  # 20
```

This works with any expression that evaluates to a tuple:

```roc
getPoint = || (100, 200)

x = getPoint().0  # 100
```

Chained access works for nested tuples:

```roc
nested = ((1, 2), (3, 4))

value = nested.0.1  # 2 (second element of the first tuple)
```

The index must be a literal integer, not a variable or computed value. This allows the compiler to verify at compile time that the index is valid for the tuple's size.

## [Tuple Types](#tuple-types) {#tuple-types}

Tuple types are written with parentheses containing comma-separated types:

```roc
point : (I64, I64)
point = (10, 20)

mixed : (Str, I64, Bool)
mixed = ("hello", 42, True)
```

Each position in a tuple can have a different type, and the type of each position is part of the tuple's type.

## [Destructuring Tuples](#destructuring-tuples) {#destructuring-tuples}

Tuples can be destructured in assignments and pattern matching:

```roc
point = (10, 20)
(x, y) = point  # x = 10, y = 20
```

In `match` expressions:

```roc
describePoint = |point|
    match point
        (0, 0) -> "origin"
        (0, _) -> "on y-axis"
        (_, 0) -> "on x-axis"
        (x, y) -> "at (${x.toStr()}, ${y.toStr()})"
```