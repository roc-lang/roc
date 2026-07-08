# Static Dispatch

_Dispatch_ is where the same call expression can result in a different function being run,
depending on the types of its arguments and/or return value. It's a form of [ad hoc polymorphism](https://en.wikipedia.org/wiki/Ad_hoc_polymorphism).

[_Static_ dispatch](https://en.wikipedia.org/wiki/Static_dispatch) is where only types known
at compile time affect which function gets run. This is in contrast to [_dynamic_ dispatch](https://en.wikipedia.org/wiki/Dynamic_dispatch),
which uses runtime information to decide which function gets run.

Roc's only ad hoc polymorphism system is static dispatch, and dynamic dispatch is unsupported
by design. A major reason for this is that Roc's static dispatch has no runtime overhead;
after compilation, it's exactly as if you had called the function directly. (In contrast,
it's impossible to avoid runtime overhead in dynamic dispatch, because it has to process
information at runtime to do the dispatch.)

## Methods

A _method_ is a function associated with a type. On a nominal type, methods are
defined in the `.{ }` block after the type declaration:

```roc
Counter := { value: I64 }.{
    new : () -> Counter
    new = || { value: 0 }

    increment : Counter -> Counter
    increment = |{ value }| { value: value + 1 }
}
```

```roc
counter : Counter
counter = Counter.new().increment()
```

Most method names are only used when they are called explicitly. Some names are
also recognized by language syntax or builtin APIs. Defining one of those
well-known methods opts the type into that syntax or API while still using
ordinary static dispatch.

## Well-Known Methods

These methods are not dynamic interfaces. The checker resolves each use to a
specific method implementation, and later compilation emits a direct call or a
derived structural operation. This table is not a limit on method names;
packages can define and require their own methods with `where` clauses.

| Method | Used by | Implement when |
| --- | --- | --- |
| `to_inspect : T -> Str` | `Str.inspect(value)` | The type needs a custom debug representation. |
| `is_eq : T, T -> Bool` | `==`, `!=` | Equality should be available or customized. |
| `to_hash : T, Hasher -> Hasher` | `Dict`, `Set`, and hash-based APIs | Values of the type should participate in hashing. |
| `plus`, `minus`, `times`, `div_by`, `div_trunc_by`, `rem_by` | `+`, `-`, `*`, `/`, `//`, `%` | The type has arithmetic-like operations. |
| `is_lt`, `is_lte`, `is_gt`, `is_gte` | `<`, `<=`, `>`, `>=` | The type has an ordering. |
| `negate`, `not` | Unary `-`, unary `!` | The type has a unary negation or complement operation. |
| `from_numeral : Num.Numeral -> Try(T, [InvalidNumeral(Str)])` | Number literals with target type `T` | Plain numeric literal syntax should construct the type. |
| `from_quote : Str -> Try(T, [BadQuotedBytes(Str)])` | Quoted string literals with target type `T` | Plain quoted literal syntax should construct the type. |
| `from_interpolation : Str, Iter((item, Str)) -> T` | Interpolated string literals with target type `T` | Interpolation should construct the type. |
| `iter : T -> Iter(item)` | `for item in value` | The type should be iterable in `for` loops. |
| `next` | `for` loop iteration steps | Usually provided by `Iter`; collection authors usually implement `iter`. |
| `parser_for : encoding -> (state -> Try({ value : T, rest : state }, err))` | Generic parser APIs such as JSON parsing | A format should be able to parse the type. |
| `encoder_for : encoding -> (T, state -> Try(state, err))` | Generic encoder APIs such as JSON encoding | A format should be able to encode the type. |

### `to_inspect`

The `to_inspect` method customizes how a value is rendered by `Str.inspect`.
Use it for debug, logging, and test-failure output.

```roc
Color := [Red, Green, Blue].{
    to_inspect : Color -> Str
    to_inspect = |color| match color {
        Red => "Color.Red"
        Green => "Color.Green"
        Blue => "Color.Blue"
    }
}
```

When `Str.inspect` is called on a `Color` value, it uses `Color.to_inspect`:

```roc
red : Color
red = Red

Str.inspect(red)  # "Color.Red"
```

Without `to_inspect`, `Str.inspect` uses Roc's built-in structural
representation for the value.

### Equality and Hashing

The `is_eq` method customizes how equality is checked using the `==` and `!=` operators.

```roc
Point := { x: I64, y: I64 }.{
    is_eq : Point, Point -> Bool
    is_eq = |a, b| a.x == b.x and a.y == b.y
}
```

When `==` is used on `Point` values, it calls the `is_eq` method:

```roc
p1 : Point
p1 = { x: 1, y: 2 }

p2 : Point
p2 = { x: 1, y: 2 }

expect p1 == p2  # calls Point.is_eq(p1, p2)
expect (p1 != p2) == False
```

For `!=`, Roc calls `is_eq` and negates the `Bool` result.

The `to_hash` method feeds a value into a `Hasher`:

```roc
to_hash : T, Hasher -> Hasher
```

Hash-based APIs use `to_hash` together with `is_eq`. For example, dictionary
keys must be hashable and comparable. If you define custom equality, make sure
the hash is consistent with it: equal values must feed the same hash data.

Roc can derive structural equality and hashing for supported structural shapes.
Define explicit methods when the derived behavior is not the behavior you want,
or when a nominal type should expose a stable custom definition.

### Operators

Binary arithmetic operators dispatch to methods on the left operand. The return
type is the left operand's type, but the right operand can have a different
type if the method signature allows it.

```roc
Vec := { x: I64, y: I64 }.{
    plus : Vec, Vec -> Vec
    plus = |a, b| { x: a.x + b.x, y: a.y + b.y }
}
```

When `+` is used on `Vec` values, it calls the `plus` method:

```roc
v1 : Vec
v1 = { x: 1, y: 2 }

v2 : Vec
v2 = { x: 3, y: 4 }

# v1 + v2 calls Vec.plus(v1, v2)
```

The arithmetic operator mapping is:

| Operator | Method |
| --- | --- |
| `+` | `plus` |
| `-` | `minus` |
| `*` | `times` |
| `/` | `div_by` |
| `//` | `div_trunc_by` |
| `%` | `rem_by` |

Comparison operators dispatch to methods whose result is `Bool`. Both operands
must have the same type.

| Operator | Method |
| --- | --- |
| `<` | `is_lt` |
| `<=` | `is_lte` |
| `>` | `is_gt` |
| `>=` | `is_gte` |

Unary operators dispatch to methods whose argument and return type are the same:

| Operator | Method |
| --- | --- |
| `-x` | `negate` |
| `!x` | `not` |

```roc
Duration := { millis : I64 }.{
    times : Duration, I64 -> Duration
    times = |duration, scale| { millis: duration.millis * scale }
}

longer : Duration
longer = Duration.{ millis: 10 } * 3
```

### Literal Conversion

Number literals dispatch the `from_numeral` method when the target type is a
nominal type that defines it:

```roc
Celsius := { degrees: I64 }.{
    from_numeral : Num.Numeral -> Try(Celsius, [InvalidNumeral(Str)])
    from_numeral = |n| match I64.from_numeral(n) {
        Ok(degrees) => Ok({ degrees })
        Err(err) => Err(err)
    }
}

temp : Celsius
temp = 21  # calls Celsius.from_numeral
```

`Num.Numeral` carries the literal's exact digits, so a custom type can accept
the literal range its representation supports and reject the rest with
`InvalidNumeral`.

Quoted string literals dispatch `from_quote` when the target type defines it:

```roc
HttpMethod := [Get, Post, Put, Delete].{
    from_quote : Str -> Try(HttpMethod, [BadQuotedBytes(Str)])
    from_quote = |raw| match raw {
        "GET" => Ok(Get)
        "POST" => Ok(Post)
        "PUT" => Ok(Put)
        "DELETE" => Ok(Delete)
        _ => Err(BadQuotedBytes("expected GET, POST, PUT, or DELETE"))
    }
}

method : HttpMethod
method = "POST"  # calls HttpMethod.from_quote
```

If the method returns `Err(BadQuotedBytes(message))`, the compiler reports the
literal conversion error before the program runs.

Interpolated string literals dispatch `from_interpolation` based on the result
type. The first argument is the literal segment before the first interpolation.
The iterator yields each interpolated value paired with the literal segment that
follows it.

```roc
# For a target type named Html:
from_interpolation : Str, Iter((Html, Str)) -> Html
```

Plain quoted string segments inside an interpolation are always `Str` values;
the interpolated values are the `item` type in `Iter((item, Str))`.

### Iteration

A `for` loop calls `iter` on the value after `in`. The `iter` method must return
an `Iter(item)` whose item type matches the loop pattern.

```roc
Rows := { items : List(Row) }.{
    iter : Rows -> Iter(Row)
    iter = |rows| rows.items.iter()
}

for row in rows {
    process(row)
}
```

The loop then repeatedly calls `next` on the `Iter(item)` value:

```roc
next : Iter(item) -> [One({ item : item, rest : Iter(item) }), Skip({ rest : Iter(item) }), Done]
```

Package authors usually implement `iter` for their collection type and build
the returned iterator with the `Iter` APIs. The `next` method is the hook on the
iterator value itself.

### Parsing and Encoding

Generic parser and encoder APIs use `parser_for` and `encoder_for` to ask a type
how it should be read or written for a particular format.

```roc
Token := { raw : Str }.{
    parser_for : encoding -> (state -> Try({ value : Token, rest : state }, err))
        where [
            encoding.parse_str : encoding, state -> Try({ value : Str, rest : state }, err),
        ]
    parser_for = |encoding| {
        Encoding : encoding

        |state| {
            parsed = Encoding.parse_str(encoding, state)?
            Ok({ value: Token.{ raw: parsed.value }, rest: parsed.rest })
        }
    }

    encoder_for : encoding -> (Token, state -> Try(state, err))
        where [
            encoding.encode_str : encoding, Str, state -> Try(state, err),
        ]
    encoder_for = |encoding| {
        Encoding : encoding

        |token, state| Encoding.encode_str(encoding, token.raw, state)
    }
}
```

Structural records, tag unions, lists, sets, dictionaries, and supported
builtins can use derived parser and encoder implementations when the selected
format supports their shape. A nominal type can provide explicit `parser_for`
or `encoder_for` methods when it wants a custom representation or when its
backing should remain hidden.

### Number Literal Defaulting

When nothing in the program pins a literal's type, the compiler commits the
first type in `Dec, I64, U64, I128, U128, I32, U32, I16, U16, I8, U8, F64,
F32` that satisfies all of the literal's constraints. A plain `5` defaults to
`Dec`; a `5` whose surrounding code demands an integer gets the first integer
type that fits.

If committing a default narrows a function's inferred type, the compiler
emits a `LITERAL DEFAULTED` warning. To pick a different type, add a type
annotation or a suffix (`5.U64`).
