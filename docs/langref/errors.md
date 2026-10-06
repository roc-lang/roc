# Error Handling

Roc has two ways for something to go wrong:

- A [`Try`](#try) is a value that's either a success (`Ok`) or a failure (`Err`). Most errors are
  represented this way, and the type system makes sure they get handled.
- A [crash](#crashing) stops the program. This is for situations where there's no reasonable way
  to continue, like a bug that has put the program in a state it was never supposed to be in.

Roc doesn't have exceptions. A function's type always tells you how it can fail (other than by
crashing), because the ways it can fail are part of its return type.

## `Try`

[`Try`](../Try) is a [nominal tag union](tag-unions#nominal-tag-unions) with two tags:

```roc
Try(ok, err) := [Ok(ok), Err(err)]
```

A function that might fail returns a `Try`. For example, [`List.first`](../List#first) returns a
`Try(item, [ListWasEmpty])`, since an empty list has no first element:

```roc
[1, 2, 3].first() # Ok(1)
[].first()        # Err(ListWasEmpty)
```

Since a `Try` is an ordinary value, you handle it the same way you'd handle any other tag union,
for example with a [`match`](pattern-matching#match):

```roc
describe = |list|
    match list.first() {
        Ok(first) => "It starts with ${first}"
        Err(ListWasEmpty) => "It's empty"
    }
```

### Error Types Are Tag Unions

Notice that the error type above is `[ListWasEmpty]`: a [structural tag union](tag-unions#structural-tag-unions)
with one tag. That's the usual way to write error types in Roc, and it works especially well with
the [`?` operator](#returning-errors-early-with), because structural tag unions can be combined:

```roc
parse_and_double : Str -> Try(U64, [BadNumStr, Overflow])
parse_and_double = |str| {
    n = U64.from_str(str)?      # can fail with BadNumStr
    doubled = n.times_try(2)?   # can fail with Overflow

    Ok(doubled)
}
```

Here, the two `?`s can return different errors, and the function's error type is the union of
both: `[BadNumStr, Overflow]`. If you leave off the type annotation, the compiler infers that for
you. You don't need to declare an error type that wraps both of them, or convert one into the
other.

And since the error type lists every error that can happen, a `match` on the result knows exactly
which errors to handle, and gives an [exhaustiveness](pattern-matching#exhaustiveness) error if
you forget one.

## Handling Errors

There are several ways to handle a `Try`, depending on what you want to do with the error.

### Returning Errors Early (with `?`)

Writing [`?`](operators#-unwrap-if-ok-early-return-if-err) after an expression that evaluates to a
`Try` gives you the `Ok` payload if it's `Ok`. If it's `Err`, the function immediately returns that
`Err`:

```roc
x = U64.from_str(a)?
```

This is the right choice when the caller is in a better position to decide what to do about the
error.

If you'd like to change the error before it gets returned (for example, to say which input was
bad), put a function or a tag after the `?`, with spaces around it. See
[`?` with a Handler](operators#-with-a-handler):

```roc
x = U64.from_str(a) ? |_| InvalidFirst # returns Err(InvalidFirst)
y = U64.from_str(b) ? InvalidSecond    # returns Err(InvalidSecond(BadNumStr))
```

### Using a Default Value (with `??`)

[`??`](operators#-default-value-on-err) gives you the `Ok` payload, or a default value if it's `Err`:

```roc
first = list.first() ?? 0
```

### Transforming Errors

The `Try` module has functions for working with `Try` values without unwrapping them:

| Function | What it does |
| --- | --- |
| [`map_ok`](../Try#map_ok) | Transforms the `Ok` payload, leaving `Err` alone |
| [`map_err`](../Try#map_err) | Transforms the `Err` payload, leaving `Ok` alone |
| [`on_err`](../Try#on_err) | Runs a function on the `Err` payload which returns another `Try`, for example to try something else |
| [`ok_or`](../Try#ok_or) | Gives the `Ok` payload, or a default value (like `??`) |
| [`is_ok`](../Try#is_ok), [`is_err`](../Try#is_err) | Returns whether it's `Ok` or `Err` |

Each of these that takes a function also has a version ending in `!` (like `map_ok!`) which takes an
[effectful function](functions#effectful-functions).

## Crashing

The [`crash`](statements#crash) statement stops the program, with a message:

```roc
if index >= len {
    crash "This should never happen: index ${index.to_str()} was past the end."
}
```

What happens after a crash is up to the [platform](platforms). Some platforms exit the program,
whereas others (such as a web server) might recover by returning an error response and moving on
to the next request.

A few builtin operations crash too, when there's no sensible value they could return:

- Integer [overflow](numbers#overflow-and-division-by-zero), like `255.U8 + 1`
- Integer division by zero
- Running out of memory

Each of these has a non-crashing alternative when you want to handle the problem yourself. For
example, `a.plus_try(b)` returns `Err(Overflow)` instead of crashing.

### When to Crash

Use `Try` for things that can go wrong even when the program is working correctly: a file that
doesn't exist, user input that isn't a valid number, a network request that times out, and so on.
The person calling your function needs to know these can happen, and `Try` makes sure they do.

Use `crash` for things that should be impossible if the program is correct. Returning a `Try` for
these would force every caller to write code for a situation that can't happen, and there would
be no good way for that code to handle it anyway.

A crash during [compile-time evaluation](compile-time) is reported as a compile-time error, so a
crash in code that only depends on compile-time constants never reaches your users.

## Performance

A `Try` is an ordinary [tag union](tag-unions#memory-layout). It's stored inline, it never
allocates, and creating one or checking which tag it has is about as cheap as it gets. `?` and `??`
compile to a single check of the tag, followed by a jump.

So there's no performance reason to avoid `Try` in favor of crashing. In particular, unlike
throwing an exception in some languages, returning an `Err` costs the same as returning an `Ok`.

One thing to watch out for: since a tag union takes up as much space as its largest payload, a
large error payload makes every `Try` of that type large, including the `Ok`s. If an error payload
is much bigger than the `Ok` payload, and the function is called a lot, consider putting the
error's details in a [`Box`](boxes).
