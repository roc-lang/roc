# Pattern Matching

A _pattern_ describes what a value looks like, and can give names to parts of it. For example,
the pattern `Ok(value)` matches any `Ok` tag, and gives the name `value` to its payload. It
doesn't match an `Err` tag.

Patterns show up in four places:

- The branches of a [`match`](#match) expression
- The left side of an `=` [assignment](#destructuring-assignments-with-)
- Function arguments, like the `{ x, y }` in `|{ x, y }| x + y`
- The part of a [`for` loop](loops#for-loops) between `for` and `in`

## `match`

A `match` expression evaluates a value, then goes through its _branches_ from top to bottom,
looking for the first one whose pattern matches the value. Then it evaluates that branch's body,
and that's what the whole `match` expression evaluates to:

```roc
describe = |result|
    match result {
        Ok(value) => "success: ${value}"
        Err(_) => "error"
    }
```

The value after `match` only gets evaluated once. All the branches' patterns have to work on the
same type (the type of the value being matched), and all the branches' bodies have to evaluate to
the same type (the type of the whole `match`).

Names that a pattern introduces, like `value` above, can only be used in that branch.

Since the first matching branch wins, the order of branches matters when more than one could
match. Here, `0` matches the first branch, not the second:

```roc
classify = |number|
    match number {
        0 => "zero"
        _ => "another number"
    }
```

### Branch Alternatives

You can use `|` to give several patterns the same body:

```roc
is_primary = |color|
    match color {
        Red | Green | Blue => True
        Yellow | Orange | Purple => False
    }
```

This works the same way as writing out a separate branch for each pattern, each with the same body.

If the alternatives give names to things, they all have to give the same names, with the same
types. Otherwise the body might refer to a name that the matching alternative didn't provide.

### `if` Guards on Branches

A branch can have an `if` after its pattern, which is called a _guard_:

```roc
describe = |numbers|
    match numbers {
        [first, .. as rest] if first > 0 => PositiveStart(first, rest)
        _ => Other
    }
```

The guard only runs if the pattern matched, so it can use names from the pattern (like `first`
here). If the guard is `False`, the branch doesn't count as matching after all, and `match` moves
on to the next branch.

For [exhaustiveness](#exhaustiveness) purposes, the compiler doesn't count a guarded branch as
covering anything, since its guard might be `False`. So even `_ if condition => …` needs another
branch after it.

## Kinds of Patterns {#pattern-forms}

### Names and `_`

A lowercase name matches anything, and gives that name to the value:

```roc
identity = |value|
    match value {
        anything => anything
    }
```

`_` also matches anything, but doesn't give it a name. A name that starts with an underscore, like
`_ignored`, does give the value a name, but tells the compiler (and anyone reading the code) that
you don't plan to use it, so you don't get an [unused name](naming#unused-names) warning.

### Literals

Number and string literals match values that are equal to them:

```roc
describe = |value|
    match value {
        0 => "zero"
        1 => "one"
        _ => "many"
    }
```

Since numbers and strings have way too many possible values to list them all, matching on them
almost always needs a `_` branch at the end.

Number literal patterns can have [type suffixes](numbers#number-literals), just like number literals
in expressions:

```roc
classify : F32 -> Str
classify = |number|
    match number {
        1.5.F32 => "one and a half"
        _ => "another number"
    }
```

So can single-quoted characters, which are [number literals](strings#single-quote-syntax) too:

```roc
is_ascii_a = |byte|
    match byte {
        'A'.U8 => True
        _ => False
    }
```

`True` and `False` are tags, so they're matched the same way as any other tag.

### String Patterns with Captures

A string pattern can contain `${…}` with a name inside, which matches any text in that position and
gives it that name:

```roc
user_id = |path|
    match path {
        "users/${id}" => Some(id)
        _ => None
    }
```

Here, `"users/42"` matches, with `id` being `"42"`. `"posts/42"` doesn't match, because it doesn't
start with `"users/"`.

When there's more text after a capture, the capture stops at the first place that text appears.
For example, matching `"a/b/c.txt"` against `"${dir}/${file}.txt"` gives `dir` the value `"a"` and
`file` the value `"b/c"`.

You can write `${_}` to match some text without naming it. Two captures can't be right next to
each other, as in `"${a}${b}"`, because there would be no way to tell where `a` should stop and `b`
should start.

### Tags

A tag pattern matches a particular tag, and has a pattern for each of its payloads:

```roc
unwrap_or = |result, fallback|
    match result {
        Ok(value) => value
        Err(_) => fallback
    }
```

The payload patterns can be any kind of pattern, including other tags. For example,
`Ok(Pair(left, right))` matches an `Ok` whose payload is a `Pair` tag, and names the `Pair`'s two
payloads.

### Records

A record pattern names a record's fields:

```roc
full_name = |person| {
    { first, last } = person
    "${first} ${last}"
}
```

You can use `:` to give a field's value a different name, or to match it against another pattern:

```roc
name = |person|
    match person {
        { name: displayed_name, active: True } => Some(displayed_name)
        { active: False, .. } => None
    }
```

A record pattern only matches records with exactly the fields it lists, unless it ends in `..`,
which means "and any other fields." If you write a name after the `..`, that name gets a record
containing all the other fields:

```roc
remove_name = |person|
    match person {
        { name, ..rest } => Pair(name, rest)
    }

answer = remove_name({ name: "Sam", age: 32 }) # Pair("Sam", { age: 32 })
```

### Tuples

A tuple pattern has a pattern for each element:

```roc
swap = |pair| {
    (first, second) = pair
    (second, first)
}
```

Like tuples themselves, tuple patterns have at least two elements. See
[Destructuring Tuples](tuples#destructuring-tuples).

### Lists {#list-patterns}

A list pattern matches lists of a particular length:

```roc
describe = |items|
    match items {
        [] => Empty
        [only] => One(only)
        [first, second] => Two(first, second)
        [_, _, ..] => Many
    }
```

`..` matches any number of elements (including zero), so `[_, _, ..]` matches any list with at
least two elements. A list pattern can have one `..`, and it can be at the beginning, the middle,
or the end:

```roc
ends = |items|
    match items {
        [first, .., last] => Some(Pair(first, last))
        _ => None
    }
```

(Note that `[first, .., last]` doesn't match a list with only one element, since `first` and `last`
are two separate elements.)

You can write `.. as name` to give a name to the elements that `..` matched, as a list:

```roc
split_first = |items|
    match items {
        [] => None
        [first, .. as rest] => Some(Pair(first, rest))
    }
```

### Nested Patterns and `as`

Patterns can go inside other patterns, the same way values can go inside other values:

```roc
get_name = |response|
    match response {
        Ok({ user: { name } }) => Some(name)
        Err(_) => None
    }
```

You can write `as` and a name after a pattern, to give a name to the whole value in addition to
its parts:

```roc
keep_point = |value|
    match value {
        (x, y) as point => { x, y, point }
    }
```

### Nominal Types

To match a [nominal type](types#nominal-types)'s backing value, you write the type's name, then `.`,
then a pattern in parentheses:

```roc
Distance := U64

unwrap : Distance -> U64
unwrap = |distance|
    match distance {
        Distance.(meters) => meters
    }
```

For nominal records, you can leave off the parentheses:

```roc
Point := { x : U64, y : U64 }

sum : Point -> U64
sum = |Point.{ x, y }| x + y
```

(`Point.({ x, y })` also works, but `Point.{ x, y }` is the usual way to write it.) For nominal tag
unions, you can match the tags directly, as in `Red =>` or `Color.Red =>`. See
[Qualified Tags](tag-unions#qualified-tags).

## Exhaustiveness

A `match` has to handle every possible value. If it doesn't, you get a compile-time error that
shows some example values that aren't handled.

For example, this `match` is _exhaustive_ (it handles every possible value), because a `Try` is
always either `Ok` or `Err`:

```roc
is_ok = |result|
    match result {
        Ok(_) => True
        Err(_) => False
    }
```

The compiler also tells you about branches that can never match, because every value they'd
match was already handled by earlier branches. Here, the `Ok(_)` branch can never be reached:

```roc
match result {
    _ => "handled"
    Ok(_) => "unreachable" # This gives a warning.
}
```

### Catch-all Patterns (`_`) {#underscore}

A _catch-all_ pattern matches every value. `_` is the most common one. A plain name, like `other`,
is also a catch-all; the difference is that `other` gives the value a name you can use in the
branch's body.

Since a catch-all matches everything, any branch after it can never match, which is why catch-all
branches go last.

## Destructuring

Patterns outside `match` (in assignments, function arguments, and `for` loops) don't have other
branches to fall back on. So they have to match every possible value of their type. If one
doesn't, you get a compile-time error rather than a runtime crash.

### Destructuring Assignments (with `=`)

The left side of an `=` can be a pattern:

```roc
coordinates = |point| {
    { x, y } = point
    (x, y)
}
```

This is useful for records, tuples, nominal types, and tag unions that only have one tag. It
doesn't work with patterns that might not match. For example, `Ok(value) = result` gives an error
if `result` could be an `Err`; use a `match` instead, so you can say what to do in that case.

(If the error type is the [empty tag union](tag-unions#void), as in `Try(U64, [])`, then `result`
can't be an `Err`, so `Ok(value) = result` is allowed.)

Function arguments are patterns too:

```roc
sum_point = |{ x, y }| x + y
```

So is the part of a `for` loop between `for` and `in`:

```roc
var $total = 0
for (key, value) in [(1, 2), (3, 4)] {
    $total = $total + key + value
}
```

If different elements need different handling, use a `match` inside the loop.

## Performance

Matching on a value never allocates memory or copies the value. The compiler turns a whole `match`
into a single [decision tree](https://en.wikipedia.org/wiki/Decision_tree) that checks each part of
the value at most once, so having lots of branches doesn't mean checking the same thing over and
over. Some details for specific kinds of patterns:

- **Tags** compile to a check of the tag union's [discriminant](tag-unions#memory-layout), which is
  a small number. Matching on many tags can compile to a single jump to the right branch, like a
  `switch` statement in C.
- **Records and tuples** cost nothing to destructure. Their fields are at known offsets, so naming
  a field is just reading it.
- **Lists** check the list's length first, then the elements the pattern needs. Naming the rest of
  a list with `.. as rest` doesn't copy any elements; `rest` refers to part of the original list's
  memory (and keeps that memory alive until `rest` is no longer used).
- **Strings** compare bytes. A string pattern with captures has to search for the text after each
  capture, which takes time proportional to the length of the string being matched.
