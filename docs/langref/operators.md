# Operators

## Desugaring

TODO

## Binary Infix Operations

### And

TODO

### Or

TODO

### Arithmetic Operators

TODO

### Comparison Operators

TODO

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

TODO

### `!` (`.not()`)

TODO

## Unary Postfix Operators

### `?` (unwrap if `Ok`; early `return` if `Err`)

TODO

### `[…]` (subscript operator)

TODO