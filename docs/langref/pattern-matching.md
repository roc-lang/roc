# Pattern Matching

A pattern describes the shape a value must have and can give names to parts of that value. Patterns are used by [`match`](#match), destructuring assignments, function arguments, and [`for`](loops#for-loops) loops.

For example, `Ok(value)` matches an `Ok` tag with one payload and gives that payload the name `value`. It does not match an `Err` tag.

## `match`

A `match` expression evaluates one value, compares it with each branch from top to bottom, and evaluates the body of the first branch that matches:

```roc
describe = |result|
    match result {
        Ok(value) => "success: ${value}"
        Err(_) => "error"
    }
```

The expression after `match` is evaluated once. Every branch pattern must accept the same type, and every branch body must produce the same type. Names introduced by a pattern are in scope in that branch's guard and body, but not in other branches.

Branch order matters whenever patterns overlap. In this example, `0` reaches the first branch, not the second:

```roc
classify = |number|
    match number {
        0 => "zero"
        _ => "another number"
    }
```

### Branch Alternatives

Use `|` to give several patterns the same body:

```roc
is_primary = |color|
    match color {
        Red | Green | Blue => True
        Yellow | Orange | Purple => False
    }
```

This is equivalent to writing a separate branch with the same body for each alternative. Every alternative must introduce the same names, with compatible types, because the guard and body must be valid regardless of which alternative matched.

### `if` Guards on Branches

A branch can have an `if` guard between its pattern and `=>`:

```roc
describe = |numbers|
    match numbers {
        [first, .. as rest] if first > 0 => PositiveStart(first, rest)
        _ => Other
    }
```

The guard must evaluate to `Bool`. It runs only after its pattern matches, so it can use names introduced by that pattern. If the guard evaluates to `False`, matching continues with the next branch.

A guarded branch does not by itself cover any case for exhaustiveness. Even a pattern such as `_ if condition` needs an unguarded branch after it, because the condition can be `False`.

## Pattern Forms

### Bindings and Wildcards

A lowercase name matches any value and binds that value to the name:

```roc
identity = |value|
    match value {
        anything => anything
    }
```

`_` also matches any value, but does not bind a name. A name beginning with an underscore, such as `_ignored`, still binds a name; the prefix indicates that leaving it unused is intentional.

### Literal Patterns

Number and string literals match equal values:

```roc
describe = |value|
    match value {
        0 => "zero"
        1 => "one"
        _ => "many"
    }
```

Literal patterns for types with other possible values normally need a catch-all branch. A numeric pattern can have an explicit type suffix, just like a numeric expression:

```roc
classify : F32 -> Str
classify = |number|
    match number {
        1.5.F32 => "one and a half"
        _ => "another number"
    }
```

A single-quoted pattern matches one Unicode codepoint. It can also have a numeric type suffix:

```roc
is_ascii_a = |byte|
    match byte {
        'A'.U8 => True
        _ => False
    }
```

`True` and `False` are tags in the `Bool` type, so they use the same pattern syntax as other tags.

String patterns can contain captures. A capture gives a name to the part of the string in that position:

```roc
user_id = |path|
    match path {
        "users/${id}" => Some(id)
        _ => None
    }
```

Here the literal prefix must match `"users/"`, and `id` receives the remainder of the string. A capture followed by more literal text receives everything up to the first occurrence of that text. Write `${_}` to match that portion without binding it. Two captures cannot be adjacent, because there would be no delimiter showing where the first one ends.

### Tag Patterns

A tag pattern matches a particular tag and can contain a pattern for each payload:

```roc
unwrap_or = |result, fallback|
    match result {
        Ok(value) => value
        Err(_) => fallback
    }
```

Payload patterns can be nested. For example, `Ok(Pair(left, right))` matches an `Ok` whose payload is a `Pair` with two payloads.

### Record Patterns

A record pattern destructures fields by name:

```roc
full_name = |person| {
    { first, last } = person
    "${first} ${last}"
}
```

Use `:` to bind a field to a different name or to match it with another pattern:

```roc
name = |person|
    match person {
        { name: displayed_name, active: True } => Some(displayed_name)
        { active: False, .. } => None
    }
```

A record pattern without `..` describes exactly the listed fields. A bare `..` leaves the remaining fields unmatched. `..rest` additionally binds a record containing those remaining fields:

```roc
remove_name = |person|
    match person {
        { name, ..rest } => Pair(name, rest)
    }
```

### Tuple Patterns

A tuple pattern has one pattern for each position:

```roc
swap = |pair| {
    (first, second) = pair
    (second, first)
}
```

Like tuple values, tuple patterns have at least two elements. See [Tuples](tuples#destructuring-tuples).

### List Patterns

A list pattern can match an exact number of elements:

```roc
describe = |items|
    match items {
        [] => Empty
        [only] => One(only)
        [first, second] => Two(first, second)
        [_, _, ..] => Many
    }
```

`..` matches any number of elements, including none. It can appear once and can have fixed patterns on either side:

```roc
ends = |items|
    match items {
        [] => None
        [first, .., last] => Some(Pair(first, last))
        _ => None
    }
```

Use `.. as name` to bind the matched middle portion as a list:

```roc
split_first = |items|
    match items {
        [] => None
        [first, .. as rest] => Some(Pair(first, rest))
    }
```

### Nested Patterns and `as`

Patterns can be nested wherever a value can be nested:

```roc
get_name = |response|
    match response {
        Ok({ user: { name, } }) => Some(name)
        Err(_) => None
    }
```

An `as` pattern gives a name to the whole matched value as well as destructuring its parts:

```roc
keep_point = |value|
    match value {
        (x, y) as point => { x, y, point }
    }
```

### Nominal Patterns

A nominal value must be destructured through its nominal type. If `Distance` is declared with `Distance := U64`, its backing value is matched with `Distance.(pattern)`:

```roc
Distance := U64

unwrap : Distance -> U64
unwrap = |distance|
    match distance {
        Distance.(meters) => meters
    }
```

The same syntax wraps and unwraps tuple, tag-union, and other backing types. Nominal records have the shorthand `Type.{ fields }`, without parentheses:

```roc
Point := { x : U64, y : U64 }

sum : Point -> U64
sum = |Point.{ x, y }| x + y
```

The general form `Point.({ x, y })` is also accepted, but `Point.{ x, y }` is the conventional record-specific form.

## Exhaustiveness

Every possible input to a `match` must be handled. The compiler reports a non-exhaustive match and shows patterns for cases that are missing.

Closed tag unions can be covered by listing every tag:

```roc
is_ok = |result|
    match result {
        Ok(_) => True
        Err(_) => False
    }
```

Types such as numbers and strings have too many possible values to list individually, so matches on their literals generally end with a catch-all pattern.

The compiler also reports redundant patterns. A pattern is redundant when every value it could match was already handled by earlier branches. For example, `Ok(_)` is unreachable after `_`:

```roc
match result {
    _ => "handled"
    Ok(_) => "unreachable"
}
```

### Catch-all Patterns (`_`) {#underscore}

`_` is the usual catch-all pattern. A binding such as `other` is also a catch-all, with the difference that it makes the matched value available to the branch body.

Catch-all branches are often last because any later branch would be redundant.

## Destructuring

Patterns also appear outside `match`. In those positions, one pattern must cover every value of the inferred type. If it can fail, the compiler reports a non-exhaustive destructure rather than inserting a runtime failure.

### Destructuring Assignments (with `=`)

The left side of `=` can be a pattern:

```roc
coordinates = |point| {
    { x, y } = point
    (x, y)
}
```

Destructuring assignments are useful for records, tuples, nominal values, and tags that are known to be the only possible case. A refutable pattern is rejected when the value's type permits another case. For example, `Ok(value) = result` is rejected if `result` can also be `Err(_)`; use `match` to handle both cases.

Function arguments are patterns too:

```roc
sum_point = |{ x, y }| x + y
```

Their patterns must likewise cover every value accepted by the function's argument type.

The binding before `in` in a `for` loop is also a pattern:

```roc
var $total = 0
for (key, value) in [(1, 2), (3, 4)] {
    $total = $total + key + value
}
```

That pattern must cover every item produced by the iterator. Use a `match` inside the loop when different item shapes need different behavior.
