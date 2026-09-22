# Operators

## Desugaring

Several operators are syntax for [well-known static-dispatch methods](static-dispatch#well-known-methods).
The method is selected at compile time from the operand types.

| Operator | Method |
| --- | --- |
| `+` | `plus` |
| `-` | `minus` |
| `*` | `times` |
| `/` | `div_by` |
| `//` | `div_trunc_by` |
| `%` | `rem_by` |
| `==` | `is_eq` |
| `!=` | `is_eq`, then `Bool.not` |
| `<` | `is_lt` |
| `<=` | `is_lte` |
| `>` | `is_gt` |
| `>=` | `is_gte` |
| `..<` | `range_exclusive_to` |
| `..=` | `range_inclusive_to` |
| `-x` | `negate` |
| `!x` | `not` |

## Binary Infix Operations

### And

`a and b` evaluates to `True` if both `a` and `b` are `True`, and `False` otherwise. Both operands must be [`Bool`](../Bool) values.

The `and` operator _short-circuits_: if `a` is `False`, then `b` is not evaluated at all, because the answer is already known to be `False`. This matters when `b` is expensive to compute, or when it calls an [effectful function](functions#effectful-functions):

```roc
is_valid = input.len() > 0 and expensive_check(input)
```

Here, `expensive_check` is only called if `input.len() > 0`.

Unlike most other binary operators, `and` does not desugar to a method call. See [`and` / `or`](if-else#and--or) for the equivalent `if` expression.

### Or

`a or b` evaluates to `True` if either `a` or `b` (or both) is `True`, and `False` otherwise. Both operands must be [`Bool`](../Bool) values.

Like `and`, `or` short-circuits: if `a` is `True`, then `b` is not evaluated, because the answer is already known to be `True`.

```roc
use_default = config_missing or user_requested_default()
```

Here, `user_requested_default` is only called if `config_missing` is `False`.

Like `and`, `or` does not desugar to a method call.

### Arithmetic Operators

Arithmetic operators dispatch to methods on the left operand. Their result type
is the left operand's type, but the right operand can have a different type if
the method signature allows it. See [Operators](static-dispatch#operators)
in the static dispatch page.

### Comparison Operators

Comparison operators dispatch to methods that return `Bool`. Both operands must
have the same type. See [Operators](static-dispatch#operators) in the static
dispatch page.

### Range Operators

`start..<end` and `start..=end` build a reusable [`Range`](../Num#Range)
describing the numbers from `start` up to `end`—excluding `end` with `..<`,
including it with `..=`. They dispatch to methods on the bound type: `..<`
calls `range_exclusive_to` and `..=` calls `range_inclusive_to`. Both operands
must have the same type, and the result is a `Range` of that type. See
[Ranges](numbers#ranges) in the numbers page.

Range operators bind more loosely than the other binary operators, so
`1..<n + 1` parses as `1..<(n + 1)`. They cannot be chained: `1..<5..<10` is
an error.

### `??` (default value on `Err`)

The `??` operator provides a default value when an expression evaluates to `Err`.

```roc
value = fallible_expr ?? default_value
```

This desugars to:

```roc
value = match fallible_expr {
    Ok(val) => val
    Err(_) => default_value
}
```

This is useful for providing fallback values:

```roc
first = List.first(items) ?? 0
name = Dict.get(users, id) ?? "Unknown"
```

Unlike the `?` operator which propagates errors via early return, `??` handles the error case inline by substituting a default value.

## Unary Prefix Operators

### `-` (`.negate()`)

Unary `-x` dispatches to `x.negate()`. The operand and result have the same
type.

### `!` (`.not()`)

Unary `!x` dispatches to `x.not()`. The operand and result have the same type.

## Unary Postfix Operators

### `?` (unwrap if `Ok`; early `return` if `Err`)

Writing `?` after an expression which evaluates to a [`Try`](../Try) does one of two things:

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

This desugars to:

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

Since `?` can return early from the function, the function's return type must be a `Try` whose error type can hold the errors that `?` might return. When different `?` expressions in the same function have different error types, the function's error type is the union of all of them. For example, if one call can fail with `Err(BadNumStr)` and another with `Err(FileNotFound)`, then the function's return type could be `Try(…, [BadNumStr, FileNotFound])`.

`?` is often more convenient than the [`??` operator](#-default-value-on-err) when the right way to handle an error is to let the caller deal with it, whereas `??` is more convenient when there's a sensible default value to use instead.

When `?` is used directly inside a top-level [`expect`](statements#expect), there's no function to return from. Instead, if the expression evaluates to `Err`, the `expect` fails, and the test report shows which `Err` it was.

### `[…]` (subscript operator)

(This has not been implemented yet.)

Writing `[` and `]` directly after an expression, with another expression in between them (for example, `list[index]`), will call the `subscript` method on the first expression, passing the second expression as its argument. In other words, `collection[key]` will desugar to `collection.subscript(key)`.

Several builtin types already have `subscript` methods, which can be called directly in the meantime:

| Expression | Equivalent to | Evaluates to |
| --- | --- | --- |
| `list.subscript(index)` | [`List.get`](../List#get) | `Try(item, [OutOfBounds])` |
| `dict.subscript(key)` | [`Dict.get`](../Dict#get) | `Try(value, [KeyNotFound])` |
| `set.subscript(item)` | [`Set.contains`](../Set#contains) | `Bool` |

Since this uses [static dispatch](static-dispatch), any type can opt into subscript syntax by defining a `subscript` method.
