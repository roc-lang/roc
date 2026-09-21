# Pattern Matching

A _pattern_ describes the shape of a value, and optionally gives names to parts of it.
Patterns are used in [`match`](#match) branches, on the left side of
[assignments](statements#assignment), in function arguments, and between `for` and `in` in
[`for` loops](loops#for-loops).

Here are the kinds of patterns Roc supports:

| Pattern | Example | Matches |
| --- | --- | --- |
| Name | `x` | Anything, and names it `x` |
| Underscore | `_` | Anything, without naming it |
| Number literal | `0` | That exact number |
| String literal | `"hello"` | That exact string |
| Tag | `Ok(value)` | That tag, with a payload matching the inner pattern |
| Record | `{ x, y: 0 }` | A record whose fields match the given patterns |
| Tuple | `(a, _)` | A tuple whose elements match the given patterns |
| List | `[first, .. as rest]` | A list whose elements match the given patterns |
| Alternatives | See [Branch Alternatives](#branch-alternatives) | Anything matching any of the alternatives |
| `as` | `Ok(n) as result` | Anything matching the pattern on the left, and names the whole value |

## `match`

A `match` expression compares a value against a series of patterns, one _branch_ at a time,
starting from the top. The first branch whose pattern matches is the one that runs, and the
whole `match` expression evaluates to what that branch evaluates to.

```roc
describe : [Red, Green, Blue, Custom(U8)] -> Str
describe = |color| match color {
    Red => "red"
    Green => "green"
    Blue => "blue"
    Custom(brightness) => "custom with brightness ${brightness.to_str()}"
}
```

Each branch is a pattern, then `=>`, then an expression. Any names the pattern introduces
(like `brightness` above) are in scope only in that branch.

Patterns can be nested inside one another as deeply as you like:

```roc
greet : Try({ name : Str, age : U64 }, Str) -> Str
greet = |result| match result {
    Ok({ name: "", .. }) => "Hello, stranger!"
    Ok({ name, age: 0 }) => "Hello, newborn ${name}!"
    Ok({ name, .. }) => "Hello, ${name}!"
    Err(problem) => "Something went wrong: ${problem}"
}
```

### List Patterns

List patterns match lists based on their length and contents. A `..` inside a list pattern
matches any number of elements (including zero), and `.. as name` gives those elements a name:

```roc
summarize : List(U64) -> Str
summarize = |list| match list {
    [] => "empty"
    [only] => "just ${only.to_str()}"
    [first, .. as rest] => "${first.to_str()} and ${rest.len().to_str()} more"
}
```

The `..` can appear at the beginning, middle, or end of a list pattern. For example,
`[.., last]` matches any nonempty list and names its last element.

### Branch Alternatives

When several patterns should run the same code, you can put them in one branch, separated by `|`:

```roc
is_warm : [Red, Orange, Yellow, Green, Blue] -> Bool
is_warm = |color| match color {
    Red | Orange | Yellow => True
    Green | Blue => False
}
```

This works for any kind of pattern, including literals:

```roc
greet : Str -> Str
greet = |name| match name {
    "Sam" | "Sammy" => "Hey, Sam!"
    other => "Hello, ${other}!"
}
```

If the alternatives introduce names, every alternative must introduce the same names,
with the same types. That way, the branch's expression can use those names no matter
which alternative matched:

```roc
amount : [Deposit(U64), Withdrawal(U64), Fee] -> U64
amount = |transaction| match transaction {
    Deposit(n) | Withdrawal(n) => n
    Fee => 1
}
```

### `if` Guards on Branches

A branch's pattern can be followed by `if` and a condition. The branch is only taken
if the pattern matches _and_ the condition evaluates to `True`. If the condition
evaluates to `False`, matching continues with the next branch.

```roc
describe_temp : [Celsius(I64), Kelvin(I64)] -> Str
describe_temp = |temp| match temp {
    Celsius(degrees) if degrees > 30 => "hot"
    Celsius(degrees) if degrees < 5 => "cold"
    Celsius(_) => "pleasant"
    Kelvin(_) => "scientific"
}
```

The guard can use any names introduced by the pattern.

Since the compiler can't know in advance which way a guard's condition will go,
a branch with a guard never counts toward [exhaustiveness](#exhaustiveness). In the example above,
removing the unguarded `Celsius(_)` branch would give an exhaustiveness error, even though
the guards cover a lot of possible temperatures.

### Naming the Whole Value with `as`

Writing `as` and then a name after a pattern gives a name to the entire value that matched it,
in addition to any names the pattern itself introduces:

```roc
describe : Try(U64, Str) -> Str
describe = |result| match result {
    Ok(n) as ok if n > 10 => "large: ${Str.inspect(ok)}"
    Ok(_) => "small"
    Err(msg) => msg
}
```

## Exhaustiveness

A `match` is _exhaustive_ if every possible value of the type being matched on is
matched by at least one of its branches. Roc's compiler checks every `match` for
exhaustiveness, and reports an error listing the missing patterns if it isn't exhaustive:

```roc
to_num : [A, B, C] -> U8
to_num = |letter| match letter {
    A => 1
    B => 2
    # Error: this match doesn't cover C
}
```

This check means that if you add a new tag to a tag union, the compiler will tell you
about every `match` that needs a new branch to handle it.

Types with an effectively unlimited number of possible values, such as strings and numbers,
can't be covered by listing literal patterns alone, so a `match` on them needs a
[catch-all pattern](#underscore) somewhere.

As with other compile-time errors in Roc, you can still run the program. If execution
actually reaches a `match` that has no branch for the given value, the program will
[crash](statements#crash).

The compiler also warns about _redundant_ branches, meaning branches that can never be
reached because earlier branches already cover every value they would match:

```roc
to_num : [A, B] -> U8
to_num = |letter| match letter {
    A => 1
    B => 2
    A => 3 # Warning: this branch is redundant
}
```

### Catch-all Patterns (`_`) {#underscore}

The `_` pattern matches any value, and does not give it a name. It's commonly used as
the last branch of a `match` to handle "everything else":

```roc
http_status_name : U16 -> Str
http_status_name = |code| match code {
    200 => "OK"
    404 => "Not Found"
    500 => "Internal Server Error"
    _ => "Unknown"
}
```

A name pattern (like `other` in `other => ...`) also matches any value, but gives it
a name you can use in the branch.

`_` can also appear anywhere inside other patterns, to ignore parts of a value you
don't care about. For example, `Ok(_)` matches any `Ok` regardless of its payload,
and `(x, _)` matches any two-element tuple while only naming its first element.

Since `_` matches everything, any branches after a top-level `_` branch are redundant.

## Destructuring

_Destructuring_ is using a pattern to take a value apart and give names to its parts.
Record, tuple, and tag patterns can all be used to destructure:

```roc
{ name, email } = user
(x, y) = point
```

In a record pattern, `{ name }` is shorthand for `{ name: name }`. To give the field's
value a different name, write `{ name: user_name }`. To destructure a record while
only naming some of its fields, end the pattern with `..` to indicate that the record
may have other fields as well:

```roc
{ name, .. } = { name: "Sam", email: "sam@example.com" }
```

Function arguments are patterns too, so they can be destructured directly:

```roc
area : { width : U64, height : U64 } -> U64
area = |{ width, height }| width * height

add_pair : (U64, U64) -> U64
add_pair = |(a, b)| a + b
```

### Destructuring Assignments (with `=`)

The left side of an [assignment](statements#assignment) is a pattern, so assignments can destructure:

```roc
{ name, pos: (x, y) } = { name: "origin", pos: (0, 0) }
```

This assigns `name`, `x`, and `y` all at once.

The pattern in an assignment must be exhaustive, because there's no other branch to fall back
on if it doesn't match. A tag union with only one tag works, but a pattern that only
covers some of a type's possible values does not:

```roc
Wrapped(n) = Wrapped(5) # OK, because Wrapped is the only possible tag

Ok(item) = list.first() # Error: this doesn't handle the case where it's Err
```

When a pattern only covers some possibilities, use [`match`](#match) instead.
