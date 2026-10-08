# Operators

## Desugaring

Most of Roc's operators are syntax sugar for calling a [method](static-dispatch#methods). For
example, `a + b` is sugar for `a.plus(b)`, and `-x` is sugar for `x.negate()`. This means that
you can use these operators on any type that has the right method, including your own types.

| Operator | Method |
| --- | --- |
| `a + b` | `a.plus(b)` |
| `a - b` | `a.minus(b)` |
| `a * b` | `a.times(b)` |
| `a / b` | `a.div_by(b)` |
| `a // b` | `a.div_trunc_by(b)` |
| `a % b` | `a.rem_by(b)` |
| `a == b` | `a.is_eq(b)` |
| `a != b` | `a.is_eq(b)`, then `Bool.not` on the result |
| `a < b` | `a.is_lt(b)` |
| `a <= b` | `a.is_lte(b)` |
| `a > b` | `a.is_gt(b)` |
| `a >= b` | `a.is_gte(b)` |
| `a ..< b` | `a.range_exclusive_to(b)` |
| `a ..= b` | `a.range_inclusive_to(b)` |
| `-a` | `a.negate()` |
| `!a` | `a.not()` |

Which method gets called is decided at compile time, based on the type of `a`. (That's what
[static dispatch](static-dispatch) means.) So there's no runtime cost to the desugaring; `a + b`
on two `U64`s compiles to the same machine instruction that adding two integers would in C.

The operators that aren't in this table (`and`, `or`, `??`, `?`, and `|>`) don't desugar to method
calls. They're described in their own sections below.

## Precedence

When an expression has more than one operator in it, the ones higher in this table are grouped
together first. For example, `1 + 2 * 3` is `1 + (2 * 3)`, because `*` is higher than `+`.

| Operators | Grouping |
| --- | --- |
| Function calls, method calls, `.field` access, the [pipe operator](#-pipe), postfix `?`, prefix `-` and `!` | left to right |
| `*` `/` `//` `%` | left to right: `a / b * c` is `(a / b) * c` |
| `+` `-` | left to right: `a - b - c` is `(a - b) - c` |
| `??` | |
| `?` (with spaces around it) | |
| `==` `!=` `<` `<=` `>` `>=` | can't be chained (see below) |
| `and` | |
| `or` | |
| `..<` `..=` | can't be chained |

Comparison operators return a `Bool`, so chaining them, as in `a < b < c`, would compare a `Bool`
to `c`. That gives a compile-time error (unless `c` happens to be a `Bool`, which is never what
you want), so write `a < b and b < c` instead.

## Binary Infix Operations

### And

`a and b` evaluates to `True` if both `a` and `b` are `True`, and `False` otherwise. Both operands must be [`Bool`](../Bool) values.

The `and` operator _short-circuits_: if `a` is `False`, then `b` is not evaluated at all, because the answer is already known to be `False`. This matters when `b` is expensive to compute, or when it calls an [effectful function](functions#effectful-functions):

```roc
is_valid = input.len() > 0 and expensive_check(input)
```

Here, `expensive_check` is only called if `input.len() > 0`.

Since it has to skip evaluating `b` sometimes, `and` can't be an ordinary method call (method
calls evaluate all their arguments first). [`and` / `or`](if-else#and--or) shows the
equivalent `if` expression.

### Or

`a or b` evaluates to `True` if either `a` or `b` (or both) is `True`, and `False` otherwise. Both operands must be [`Bool`](../Bool) values.

Like `and`, `or` short-circuits: if `a` is `True`, then `b` is not evaluated, because the answer is already known to be `True`.

```roc
use_default = config_missing or user_requested_default()
```

Here, `user_requested_default` is only called if `config_missing` is `False`.

### Arithmetic Operators

`+`, `-`, `*`, `/`, `//`, and `%` call methods on their left operand. The result has the
left operand's type. The right operand usually has the same type as the left one, but it doesn't
have to; that's up to the method. [Static dispatch](static-dispatch#operators) covers how to
define these methods for your own types, and [numbers](numbers) covers what they do on the
builtin number types (including what happens on overflow and division by zero).

### Comparison Operators

`==`, `!=`, `<`, `<=`, `>`, and `>=` call methods that return a `Bool`. Both operands must have
the same type. [Static dispatch](static-dispatch#operators) covers how to define these for your
own types.

### Range Operators

`start..<end` and `start..=end` create a [`Range`](../Num#Range), which describes the numbers
from `start` up to `end`. With `..<`, the range stops just before `end`; with `..=`, it includes
`end`:

```roc
for n in 1..<4 {
    # n is 1, then 2, then 3
}

for n in 1..=4 {
    # n is 1, then 2, then 3, then 4
}
```

Both operands must have the same type, and the result is a `Range` of that type. Creating a range
doesn't allocate anything or produce any numbers yet; it's just a description of the numbers. See
[Ranges](numbers#ranges) for more.

Range operators group after every other operator, so `1..<n + 1` is `1..<(n + 1)`. They can't be
chained, so `1..<5..<10` gives a compile-time error.

### `??` (default value on `Err`)

`a ?? b` evaluates to the `Ok` payload if `a` is `Ok`, and to `b` if `a` is `Err`:

```roc
first = List.first(items) ?? 0
name = Dict.get(users, id) ?? "Unknown"
```

It's equivalent to this:

```roc
first = match List.first(items) {
    Ok(val) => val
    Err(_) => 0
}
```

Like `and` and `or`, `??` short-circuits: `b` only gets evaluated if `a` is an `Err`.

### `?` with a Handler

Writing `?` with spaces around it, followed by a function, works like [postfix `?`](#-unwrap-if-ok-early-return-if-err),
except that the error gets passed to that function first. Whatever the function returns is what
gets returned (wrapped in `Err`):

```roc
parse_pair : Str, Str -> Try((U64, U64), [InvalidFirst, InvalidSecond])
parse_pair = |a, b| {
    x = U64.from_str(a) ? |_| InvalidFirst
    y = U64.from_str(b) ? |_| InvalidSecond

    Ok((x, y))
}
```

If `U64.from_str(a)` returns an `Err`, then `parse_pair` returns `Err(InvalidFirst)`.

You can also write a tag instead of a function. In that case, the original error becomes the
tag's payload. So `U64.from_str(b) ? InvalidSecond` would return `Err(InvalidSecond(BadNumStr))`
if `b` wasn't a valid number. This is a convenient way to keep track of where an error came
from, while keeping the details of the original error.

### `|>` (pipe)

`a |> f(b, c)` is another way to write `f(a, b, c)`. In other words, it calls the function on the
right, passing the value on the left as the first argument. If there are no other arguments, you
can leave off the parentheses, so `a |> f` is the same as `f(a)`.

This is useful for writing a series of function calls in the order they happen, rather than
nested inside each other:

```roc
result = input |> parse |> List.map(normalize) |> summarize

# This is the same as:
result = summarize(List.map(parse(input), normalize))
```

For calling methods, you can usually write `value.method(arg)` instead (see
[Calling Methods](static-dispatch#calling-methods)). `|>` is for calling functions that aren't
methods on the value's type, or for when you'd rather name the function explicitly.

The thing after `|>` has to be a function's name (like `parse` or `List.map`), not some other
expression. Also note that `|>` groups before binary operators like `+`, so `1 + 2 |> double` is
`1 + double(2)`, not `double(1 + 2)`.

## Unary Prefix Operators

### `-` (`.negate()`)

`-x` is sugar for `x.negate()`. The operand and result have the same type.

### `!` (`.not()`)

`!x` is sugar for `x.not()`. The operand and result have the same type.

## Unary Postfix Operators

### `?` (unwrap if `Ok`; early `return` if `Err`)

Writing `?` right after an expression which evaluates to a [`Try`](../Try) does one of two things:

- If the expression evaluated to `Ok`, the `?` expression evaluates to the `Ok` tag's payload.
- If the expression evaluated to `Err`, the enclosing function immediately [returns](statements#return) that `Err`.

For example:

```roc
parse_pair : Str, Str -> Try((U64, U64), [BadNumStr])
parse_pair = |a, b| {
    x = U64.from_str(a)?
    y = U64.from_str(b)?

    Ok((x, y))
}
```

That's equivalent to this:

```roc
parse_pair = |a, b| {
    x = match U64.from_str(a) {
        Ok(val) => val
        Err(err) => return Err(err)
    }
    y = match U64.from_str(b) {
        Ok(val) => val
        Err(err) => return Err(err)
    }

    Ok((x, y))
}
```

If `U64.from_str(a)` returns `Err(BadNumStr)`, then `parse_pair` returns `Err(BadNumStr)` right away, and `U64.from_str(b)` never runs.

Since `?` can return from the function, the function's return type has to be a `Try` whose error
type includes every error that `?` might return. When different `?`s in the same function have
different error types, the function's error type is the union of all of them. For example, if
one call can fail with `Err(BadNumStr)` and another with `Err(FileNotFound)`, then the function's
return type could be `Try(…, [BadNumStr, FileNotFound])`.

`?` is often more convenient than [`??`](#-default-value-on-err) when the right way to handle an
error is to let the caller deal with it, whereas `??` is more convenient when there's a sensible
default value to use instead.

When `?` is used directly inside a top-level [`expect`](statements#expect), there's no function to return from. Instead, if the expression evaluates to `Err`, the `expect` fails, and the test report shows which `Err` it was.

Using `?` directly inside an inline [`expect`](statements#expect) gives a compile-time error,
because returning from the enclosing function would make the program behave differently in
optimized builds, which leave out inline `expect`s. (See
[control flow and variables inside `expect`](statements#expect-control-flow).)

### `[…]` (subscript operator)

(This has not been implemented yet.)

Writing `[` and `]` directly after an expression, with another expression in between them (for example, `list[index]`), will call the `subscript` method on the first expression, passing the second expression as its argument. In other words, `collection[key]` will desugar to `collection.subscript(key)`.

Several builtin types already have `subscript` methods, which you can call directly in the meantime:

| Expression | Equivalent to | Evaluates to |
| --- | --- | --- |
| `list.subscript(index)` | [`List.get`](../List#get) | `Try(item, [OutOfBounds])` |
| `dict.subscript(key)` | [`Dict.get`](../Dict#get) | `Try(value, [KeyNotFound])` |
| `set.subscript(item)` | [`Set.contains`](../Set#contains) | `Bool` |

Since this uses [static dispatch](static-dispatch), any type can opt into subscript syntax by defining a `subscript` method.
