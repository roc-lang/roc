# Tuples

A _tuple_ is a fixed number of values in a particular order, like `(10, "foo")`. The values
can have different types. Tuples are useful when you want to group a few values together, and
giving each one a name (like you would in a [record](records)) would be more trouble than it's
worth.

## [Tuple Literals](#tuple-literals) {#tuple-literals}

Tuple literals are written with parentheses and comma-separated values:

```roc
point = (10, 20)
mixed = ("hello", 42, True)
nested = ((1, 2), (3, 4))
```

Tuples have at least two elements. A single value in parentheses is just that value, not a tuple:

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

The index has to be a number written right there in the code, not a variable or anything
computed. That's because each element of a tuple can have a different type, so the compiler
needs to know which element you're accessing in order to know what type you get back. (It also
means there's no such thing as an out-of-bounds tuple access at runtime; using an index that's
too big is a compile-time error.)

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
[optional fields](records#optional-fields) and [record update syntax](records#updating-records).

Tuples are most useful when the meaning of each position is obvious from context, and the
group of values is small. Common examples include:

- Coordinates, like `(x, y)`
- Returning two values from a function, like a result and some updated state
- Key-value pairs, like the `(k, v)` pairs used by [`Dict`](dictionaries-and-sets)

When a tuple grows beyond two or three elements, or when it's not obvious what each position
means, a record is usually the better choice.

## Performance

A tuple is stored exactly like a [record](records#memory-layout) whose fields are its
elements. It's stored inline, with no heap allocation and no reference count of its own, and
accessing an element is a read from a known offset.

As with records, the order of a tuple's elements in memory doesn't have to match the order
you wrote them in. The compiler sorts them by alignment (keeping elements with the same
alignment in their original order) to minimize padding. For example, on a 64-bit target,
`(U8, U64, U8)` takes 16 bytes, not the 24 it would take if its elements were stored in
order.
