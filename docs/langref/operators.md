# Operators

## Desugaring

Several operators are syntax for [well-known static-dispatch methods](static-dispatch.md#well-known-methods).
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
| `-x` | `negate` |
| `!x` | `not` |

## Binary Infix Operations

### And

TODO

### Or

TODO

### Arithmetic Operators

Arithmetic operators dispatch to methods on the left operand. Their result type
is the left operand's type, but the right operand can have a different type if
the method signature allows it. See [Operators](static-dispatch.md#operators)
in the static dispatch page.

### Comparison Operators

Comparison operators dispatch to methods that return `Bool`. Both operands must
have the same type. See [Operators](static-dispatch.md#operators) in the static
dispatch page.

### `??` (default value on `Err`)

The `??` operator provides a default value when an expression evaluates to `Err`.

```roc
value = fallible_expr ?? default_value
```

This desugars to:

```roc
value = match fallible_expr {
    Ok(val) -> val
    Err(_) -> default_value
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

TODO

### `[…]` (subscript operator)

TODO
