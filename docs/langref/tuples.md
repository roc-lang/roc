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
get_point = || (100, 200)

x = get_point().0  # 100
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
describe_point = |point|
    match point {
        (0, 0) => "origin"
        (0, _) => "on y-axis"
        (_, 0) => "on x-axis"
        (x, y) => "at (${x.to_str()}, ${y.to_str()})"
    }
```

## [Tuples vs Records](#tuples-vs-records) {#tuples-vs-records}

Tuples and [records](records) are very similar. Both group a fixed number of values together,
the values can have different types, and neither one involves a heap allocation. The difference is
that a record gives each of its values a name, whereas a tuple identifies its values only by position.

This makes records more self-documenting. Compare:

```roc
tuple_user = ("Sam", "sam@example.com", 30)

record_user = { name: "Sam", email: "sam@example.com", age: 30 }
```

With the tuple, you have to remember that `tuple_user.1` is the email address, whereas with the
record, you can write `record_user.email`. Records also have features that tuples don't, like
[optional fields](records#optional-fields) and [record update syntax](records#record-update-syntax).

Tuples are most useful when the meaning of each position is obvious from context, and the
group of values is small. Common examples include:

- Coordinates, like `(x, y)`
- Returning two values from a function, like a result and some updated state
- Key-value pairs, like the `(k, v)` pairs used by [`Dict`](dictionaries-and-sets)

When a tuple grows beyond two or three elements, or when it's not obvious what each position
means, a record is usually the better choice.
