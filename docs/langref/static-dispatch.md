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

A _method_ is a function that's associated with a type. You define methods for a
[nominal type](types#nominal-types) in the `.{ … }` block after its declaration:

```roc
Counter := { value : I64 }.{
    new : () -> Counter
    new = || { value: 0 }

    increment : Counter -> Counter
    increment = |{ value }| { value: value + 1 }
}
```

### Calling Methods

There are two ways to call a method. One is to write the type's name, then `.`, then the method's
name, like any other qualified function call:

```roc
counter = Counter.increment(Counter.new())
```

The other is to write a value, then `.`, then the method's name, and then the rest of the
arguments:

```roc
counter = Counter.new().increment()
```

Here, `.increment()` looks at the type of the value before the `.`, sees that it's a `Counter`,
and calls `Counter.increment` with that value as the first argument. So `value.method(a, b)` is
the same as `Type.method(value, a, b)`, where `Type` is the type of `value`.

This is static dispatch: the compiler decides which `increment` to call based on the type of
`counter`, which it knows at compile time. If you call `.increment()` on a value whose type has
no `increment` method, you get a compile-time error.

The builtin types work the same way. For example, `list.len()` calls [`List.len`](../List#len),
and `name.is_empty()` calls [`Str.is_empty`](../Str#is_empty).

### Well-Known Methods

Most method names don't mean anything special to the compiler. However, some of them are used by
syntax or by builtin functions. For example, `a + b` calls `a.plus(b)`, so defining a `plus`
method on your type means you can use `+` with it.

Here are all the method names that the language or the builtins use:

| Method | Used by | Define it when |
| --- | --- | --- |
| `to_inspect : T -> Str` | `Str.inspect(value)`, `dbg` | You want a custom debug representation. |
| `is_eq : T, T -> Bool` | `==`, `!=` | You want values of the type to be comparable for equality. |
| `to_hash : T, Hasher -> Hasher` | `Dict`, `Set`, and other hash-based APIs | You want values of the type to be usable as dictionary keys or set elements. |
| `plus`, `minus`, `times`, `div_by`, `div_trunc_by`, `rem_by` | `+`, `-`, `*`, `/`, `//`, `%` | The type has arithmetic-like operations. |
| `is_lt`, `is_lte`, `is_gt`, `is_gte` | `<`, `<=`, `>`, `>=` | The type has an ordering. |
| `range_exclusive_to : T, T -> Range(T)`, `range_inclusive_to : T, T -> Range(T)` | `..<`, `..=` | You want range syntax to work with the type. |
| `range_exclusive_from : T, T -> Range(T)`, `range_inclusive_from : T, T -> Range(T)` | `Range.iter_rev` | The type's ranges can be reversed exactly. |
| `range_iter` | `Range.iter`, `Range.iter_rev` | The type's ranges can be iterated. |
| `range_len_if_known` | Numeric range constructors, `Range.step_by` | The type can say how many numbers are in a range (when that fits in a `U64`). |
| `negate`, `not` | Unary `-`, unary `!` | The type has a negation or complement operation. |
| `from_numeral : Numeral -> Try(T, [InvalidNumeral(Str)])` | Number literals | You want number literals to work as values of the type. |
| `from_quote : Str -> Try(T, [BadQuotedBytes(Str)])` | String literals | You want string literals to work as values of the type. |
| `from_interpolation : List(Str) -> Try((List(item) -> T), [InvalidInterpolation(Str)])` | String literals with interpolation | You want interpolated string literals to work as values of the type. |
| `iter : T -> Iter(item)` | `for item in value` | You want `for` loops to work on the type. |
| `next` | Each step of a `for` loop | Usually only `Iter` needs this; collections define `iter` instead. |
| `parser_for : encoding -> (state -> Try({ value : T, rest : state }, err))` | Parsers, such as JSON parsing | You want the type to be parseable from formats like JSON. |
| `encoder_for : encoding -> (T, state -> Try(state, err))` | Encoders, such as JSON encoding | You want the type to be encodable into formats like JSON. |
| `map` | Transforming a tag union's payload | You want to transform a payload with a pure function. |
| `map!` | Transforming a tag union's payload | You want to transform a payload with an effectful function. |

These aren't interfaces or traits that a type "implements." A type just has a method with the
right name and type, and then code that calls that method works with it. Packages can use any
method names they like in the same way, using [`where` clauses](#where-clauses).

### Compiler-Derived Methods

For some of these methods, the compiler can write the implementation for you, based on the shape
of the type: `is_eq`, `to_hash`, `parser_for`, `encoder_for`, `map`, and `map!`.

[Structural types](types#structural-types) (records, tuples, and structural tag unions) get these
automatically, whenever all the types inside them support the method too. That's why you can
compare two records with `==`, or use a tuple as a dictionary key, without doing anything.

Nominal types don't get them automatically, because for a nominal type, the right answer isn't
always the structural one. (For example, two values of a `Fraction` type might be equal even if
their numerators are different, like 1/2 and 2/4.) Instead, a nominal type opts in by listing the
method with `_` as its type and no implementation:

```roc
Model := { value : Str }.{
    is_eq : _
    to_hash : _
    parser_for : _
    encoder_for : _
}
```

This only works for those six method names. Writing a method with `_` and no implementation is a
compile-time error for any other name (except in platform modules, where it declares a
host-provided function).

If you write an implementation instead, that's what gets used. So you can, for example, write your
own `is_eq` while letting the compiler derive `to_hash` and `encoder_for`.

The derived `map` and `map!` methods work on tag unions that have one type parameter appearing
directly in a payload. `map` takes a pure function, while `map!` takes an effectful one:

```roc
Maybe(a) := [Just(a), Nothing].{
    map : _
    map! : _
}
```

### `to_inspect`

The `to_inspect` method lets you choose how values of a type look when they're passed to
`Str.inspect`, which is what `dbg` and test failure reports use:

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

Now `Str.inspect` uses `Color.to_inspect`:

```roc
red : Color
red = Red

Str.inspect(red)  # "Color.Red"
```

Without a `to_inspect` method, `Str.inspect` shows the value's structure.

`Str.inspect` only uses a `to_inspect` method whose type is exactly `T -> Str`, where any type
parameters of `T` are plain type variables. For example, `Wrap(a) := [W(a)]` can use
`to_inspect : Wrap(a) -> Str`, but not `to_inspect : Wrap(I64) -> Str`, and not one with a `where`
clause. A `to_inspect` method with any other type is still a method you can call yourself; it's just
that `Str.inspect` won't use it.

To show a value's contents from inside `to_inspect`, call `Str.inspect` on them, which works on any
type:

```roc
Wrap(a) := [W(a)].{
    to_inspect : Wrap(a) -> Str
    to_inspect = |Wrap.W(value)| "Wrap(${Str.inspect(value)})"
}
```

### Equality and Hashing

The `is_eq` method decides what `==` and `!=` do:

```roc
Point := { x : I64, y : I64 }.{
    is_eq : Point, Point -> Bool
    is_eq = |a, b| a.x == b.x and a.y == b.y
}

expect Point.{ x: 1, y: 2 } == Point.{ x: 1, y: 2 } # calls Point.is_eq
```

`a != b` calls `a.is_eq(b)` and then flips the answer.

The `to_hash` method feeds a value's data into a `Hasher`:

```roc
to_hash : T, Hasher -> Hasher
```

Dictionaries and sets use `to_hash` and `is_eq` together, so if you write your own `is_eq`, make
sure your `to_hash` agrees with it: whenever two values are equal, they must feed exactly the same
data into the hasher. Otherwise, a dictionary might not find a key that's equal to one it has.

### Operators

[Operators](operators#desugaring) call methods on their left operand, so any type can support
them by defining the right methods:

```roc
Vec := { x : I64, y : I64 }.{
    plus : Vec, Vec -> Vec
    plus = |a, b| { x: a.x + b.x, y: a.y + b.y }
}

sum = Vec.{ x: 1, y: 2 } + Vec.{ x: 3, y: 4 } # calls Vec.plus
```

The result of an arithmetic operator has the left operand's type. The right operand usually has
the same type, but it doesn't have to; that's up to the method. For example, it can make sense to
multiply a `Duration` by a plain number:

```roc
Duration := { millis : I64 }.{
    times : Duration, I64 -> Duration
    times = |duration, scale| { millis: duration.millis * scale }
}

longer = Duration.{ millis: 10 } * 3
```

The comparison operators (`<`, `<=`, `>`, `>=`) call methods that return a `Bool`, and both
operands must have the same type.

The range operators (`..<` and `..=`) call methods that return a `Range` of the operands' type.
All the builtin number types have them, and you can define them for your own types:

```roc
PageNum := { num : U32 }.{
    range_exclusive_to : PageNum, PageNum -> Range(PageNum)
    range_exclusive_to = |start, end|
        Range.custom({
            lower: start,
            upper: end,
            step: PageNum.{ num: 1 },
            upper_bound: Exclusive,
            direction: To,
            len_if_known: Unknown,
        })
}

pages : Range(PageNum)
pages = first_page..<last_page
```

To make that range iterable, `PageNum` would also define `range_iter`. Defining the two `_from`
methods makes `Range.iter_rev` work too; types whose steps can't be reversed exactly (like
floating-point numbers) leave those out.

### Literal Conversion

A number literal can become a value of any type that has a `from_numeral` method, whenever that
type is what's expected:

```roc
Celsius := { degrees : I64 }.{
    from_numeral : Numeral -> Try(Celsius, [InvalidNumeral(Str)])
    from_numeral = |n| match I64.from_numeral(n) {
        Ok(degrees) => Ok(Celsius.{ degrees })
        Err(err) => Err(err)
    }
}

temp : Celsius
temp = 21 # calls Celsius.from_numeral
```

The `Numeral` contains the literal's exact digits, so the type can decide which literals it
accepts. Since this happens at compile time, returning `Err(InvalidNumeral(message))` gives a
compile-time error with that message. [Custom Number Types](numbers#custom-number-types) goes into
more detail.

String literals work the same way, with `from_quote`:

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
method = "POST" # calls HttpMethod.from_quote
```

Here, writing `method = "PATCH"` would give a compile-time error saying
`expected GET, POST, PUT, or DELETE`.

A string literal with interpolations in it, like `"<p>Hello, ${name}!</p>"`, uses
`from_interpolation`. This happens in two stages:

1. At compile time, `from_interpolation` gets called with the literal's _segments_, which are the
   pieces of text around the interpolations. Here, those are `["<p>Hello, ", "!</p>"]`.
2. It returns a function, which gets called each time the program evaluates the string literal.
   That function receives the interpolated values (here, just `name`) as a list, and returns the
   finished value.

```roc
Html := [Html(Str)].{
    from_interpolation : List(Str) -> Try((List(Str) -> Html), [InvalidInterpolation(Str)])
    from_interpolation = |segments|
        if segments.any(|segment| segment.contains("<script")) {
            Err(InvalidInterpolation("Html literals can't contain script tags"))
        } else {
            Str.from_interpolation(segments).map_ok(|assemble|
                |values| Html(assemble(values.map(|value| value.replace_each("<", "&lt;")))))
        }
}

page : Str -> Html
page = |name| "<p>Hello, ${name}!</p>"
```

The reason for having two stages is that the segments are written in the source code, so they
can be checked at compile time, just like a `from_quote` literal can. If `from_interpolation`
returns `Err(InvalidInterpolation(message))`, you get a compile-time error with that message. The
interpolated values, on the other hand, aren't known until the program runs, so the function that
handles them can't fail. Instead, it decides how those values get into the result. In this
example, `Html` rejects literals with `<script` written in them, and escapes any `<` in the
interpolated values.

A literal with `n` interpolations always has `n + 1` segments (some of which may be empty
strings). The segments are always `Str` values, whereas the interpolated values can be whatever
type the returned function accepts.

### Iteration

A `for` loop calls the `iter` method on the value after `in`. So to make your own type work in
`for` loops, give it an `iter` method that returns an `Iter`:

```roc
Rows := { items : List(Row) }.{
    iter : Rows -> Iter(Row)
    iter = |rows| rows.items.iter()
}

for row in rows {
    process(row)
}
```

The loop then repeatedly calls `next` on the iterator, as described in
[How Iteration Works](iterators#how-iteration-works). Collection types usually build their iterator
using the [`Iter`](../Iter) functions rather than defining `next` themselves.

### Parsing and Encoding

[Parsers](parsers) and encoders call `parser_for` and `encoder_for` to find out how to read or
write a value of a particular type in a particular format (like JSON).

Structural records, tag unions, lists, sets, dictionaries, and the builtin types all have these
already (as long as the format supports them), and nominal types can opt into the
[derived](#compiler-derived-methods) versions with `parser_for : _` and `encoder_for : _`. You'd
write them yourself when you want a type to be represented differently from its structure, or
when you want its backing type to stay hidden:

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

This `Token` is parsed from (and encoded as) a plain string, rather than a record with a `raw`
field. (`Encoding : encoding` is explained in
[Calling Methods on Type Variables](#calling-methods-on-type-variables).)

### Number Literal Defaulting

When nothing says what type a number literal should be, the compiler picks the first type in this
list that works with everything the literal is used for:

`Dec`, `I64`, `U64`, `I128`, `U128`, `I32`, `U32`, `I16`, `U16`, `I8`, `U8`, `F64`, `F32`

So a plain `5` becomes a `Dec` (see [Defaulting to `Dec`](numbers#defaulting-to-dec)), but a `5`
that's used in a way `Dec` doesn't support moves on down the list to the first type that does
support it.

If picking a default this way makes a function's inferred type more specific than it would
otherwise be, the compiler gives a `LITERAL DEFAULTED` warning. To choose a type yourself, add a
type annotation or a suffix (like `5.U64`).

## Where Clauses

A function that calls a method on a value whose type is a type variable needs to say which methods
that type must have. This is what `where` clauses are for:

```roc
show_all : List(a) -> Str where [a.to_str : a -> Str]
show_all = |items| {
    var $out = ""

    for item in items {
        $out = $out.concat(item.to_str()).concat(" ")
    }

    $out
}
```

The `where [a.to_str : a -> Str]` clause says that `show_all` accepts a list of any type `a`,
as long as `a` has a `to_str` method with the type `a -> Str`. Inside the function, that's what
makes it possible to call `item.to_str()`, and at each call site, the compiler checks that the
list's element type actually has that method. For example, `show_all([1.U8, 2, 3])` works because
`U8` has a `to_str` method, whereas `show_all([{ x: 1 }])` gives an error because records don't.

Since this is static dispatch, each call site knows exactly which `to_str` implementation it's using,
and the compiled program calls it directly.

If a function calls a method on a type variable, but its annotation doesn't have a `where` clause
listing that method, the compiler reports an error. If the function doesn't have an annotation,
the compiler infers the `where` clause automatically.

A `where` clause can list multiple constraints, separated by commas, and they can involve different
type variables:

```roc
convert_all : List(a) -> List(b) where [a.to_b : a -> b, b.is_valid : b -> Bool]
```

See [Where Clauses](types#where-clauses) in the types page for more on the syntax.

### Calling Methods on Type Variables

Some methods don't take a value of the type as an argument; for example, a method which creates a
new value of the type from scratch. To call one of those on a type variable, first give the type
variable an uppercase name by writing `UppercaseName : lowercase_type_variable` inside the function
body. After that, the uppercase name can be used to call the type variable's methods:

```roc
make_default : {} -> thing where [thing.default : () -> thing]
make_default = |_| {
    Thing : thing

    Thing.default()
}
```

Here, `Thing.default()` calls whichever `default` method belongs to the type that `thing` turns out
to be at the call site. The [`parser_for` example](#parsing-and-encoding) above uses this technique
to call `Encoding.parse_str(…)`.

## Aliases

When the same group of `where` constraints is needed in many places, you can give the group a name
with a _where alias_:

```roc
a.Showable : where [a.to_str : a -> Str]
```

This declares a where alias named `Showable`. Now, instead of repeating the constraint, you can
write `a.Showable` in a `where` clause:

```roc
show_one : a -> Str where [a.Showable]
show_one = |value| value.to_str()
```

This means exactly the same thing as `where [a.to_str : a -> Str]`. Any type that has the required
methods satisfies the alias; there's no need to declare that a type "implements" `Showable`.

A where alias can combine other where aliases, as well as individual method constraints:

```roc
a.Comparable : where [a.is_lt : a, a -> Bool]

a.Sortable : where [a.Showable, a.Comparable]
```

Where aliases can also take parameters, which their constraints can mention:

```roc
a.Encodable(fmt) : where [a.encode : a, fmt -> fmt]

encode_twice : a, fmt -> fmt where [a.Encodable(fmt)]
encode_twice = |value, fmt| value.encode(value.encode(fmt))
```

A where alias describes constraints on a type, not a type itself, so it can only be used inside a
`where` clause. Writing something like `describe : Showable -> Str` is an error.

## Performance

Static dispatch has no runtime cost. By the time your program is compiled, `value.method()` has
been replaced by a direct call to the specific method for `value`'s type, exactly as if you had
written `Type.method(value)` yourself. (And like any other direct call, the compiler can
[inline](functions#calls) it.)

That's also true of functions with `where` clauses. A function like `show_all` above gets
[compiled separately](types#performance) for each type it's used with, and in each version,
`item.to_str()` is a direct call to that type's `to_str`. There's no table of methods passed
around at runtime, which is how some other languages implement this kind of feature.

Derived methods are compiled the same way. A derived `is_eq` on a record compiles to code that
compares each field in turn, much like what you'd write by hand.
