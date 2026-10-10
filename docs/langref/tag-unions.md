# Tag Unions

A tag union represents a [tagged union](https://en.wikipedia.org/wiki/Tagged_union), which is an example of a sum type.

## Tags

A _tag_ is a name for one of the alternatives in a tag union. Tags can optionally have payloads.

- In `x = Foo`, `Foo` is a tag.
- In `y = Foo(4)`, `Foo` is a tag with a payload of `4`.
- In `y = Foo(4, 2)`, `Foo` is a tag with payloads of `4` and `2`.

> Note that at runtime, payloads are stored the same way as tuples. `Foo(4, 2)` and
> `Foo((4, 2))` compile to exactly the same thing.

Tag unions can't have multiple tags with the same name but different payload types. So for example,
`Foo("a string")` and `Foo(1, 2)` couldn't go in the same tag union, because their tags have
the same name but their payloads are incompatible.

## Structural Tag Unions

_Structural_ tag unions are both structural and extensible.

- **Structural** means that you don't have to choose a name for the type (or declare it in any way), and that two types are considered equivalent if they have the same structure.
- **Extensible** means that the type can accumulate new tags based on how it's used, and also that you can use type variables to represent additional tags the union might contain.

In contrast, [_nominal_ tag unions](#nominal-tag-unions) are neither structural nor extensible.

### Extending Structural Tag Unions

Structural tag unions can be extended by having a conditional branch introduce a new tag:

```roc
color : [Purple, Green]
color = if some_condition {
    Purple
} else {
    Green
}
```

The type of `color` is the union of all the tags in each branch of the `if`.

### Structural Tag Union Type Parameters

Each structural tag union type can optionally include a type parameter representing
other tags that might be included in it. For example:

```roc
green_to_blue : [Red, Green, Blue, ..others] -> [Red, Green, Blue, ..others]
green_to_blue = |color| match color {
    Green => Blue
    other => other
}
```

Here, `..others` stands for whatever other tags the argument might have. Since the return type
has the same `..others`, those tags come back out. So if you call `green_to_blue` on a value
that might also be `Purple`, you get back a value that might also be `Purple`:

```roc
is_purple = |color| match green_to_blue(color) {
    Purple => True
    _ => False
}
```

(Note that the `other => other` branch returns the original `color`, so the return type has to
include every tag the argument type has, which is why `Green` appears in both.)

You can use these type parameters in type aliases:

```roc
Letters(others) : [A, B, ..others]
```

If you match on a [catch-all underscore pattern](pattern-matching#underscore),
you can accept a tag union containing _at least_ some tags, but also arbitrary others:

```roc
to_str : [Red, Green, .._others] -> Str
to_str = |color| match color {
    Red => "red"
    Green => "green"
    _ => "other"
}
```

When you don't need to reference the extension type variable at all, you can use
anonymous open unions by writing just `..` without a name:

```roc
process : [Count(U32), Custom(Str), ..] -> Str
process = |result| match result {
    Count(n) => n.to_str()
    Custom(n_str) => n_str
    _ => "unknown"
}
```

This is equivalent to writing `.._` but is more concise.

### Closed Tag Unions

It's very rare, but occasionally useful to restrict a tag union's ability to be extended. (Note: the `..[]` syntax shown here has not been implemented yet.)

```roc
to_color : Str -> [Red, Green, Blue, Other, ..[]]
to_color = |string| match string {
    "red" => Red
    "green" => Green
    "blue" => Blue
    _ => Other
}
```

This returned tag union may not be extended further, making the following an error:

```roc
extended_color : [Red, Green, Blue, Other, Purple]
extended_color = if some_condition {
    to_color(some_string)
} else {
    Purple # ERROR! to_color returns ..[] and so does not accept new tags.
}
```

This is rarely useful to application authors, but it is useful to platform authors.
Platform authors can't send extensible types across the host boundary (as otherwise
the host couldn't reliably know which tags map to which integer discriminants),
so this is a way to (for example) make a structural tag union for errors, and then
mark it as closed so that it can be sent across the host boundary.

### Limitations

Structural tag unions are not allowed to be recursive. To make a recursive tag union,
use a [nominal tag union](#nominal-tag-unions) instead. (Recursive structural types have some
[nonobvious downsides](https://github.com/roc-lang/rfcs/pull/1), which is why they aren't
supported.)

Platform authors should note the previous section on [closed tag unions](#closed-tag-unions),
which explains why only closed tag unions can be sent across the host boundary.

## Nominal Tag Unions

Nominal tag unions are like structural tag unions, except that they are neither structural nor growable. That means:

* You have to name a nominal tag union, and no other tag union will be considered equivalent to it—even if they have exactly the same shape.
* The number of alternatives it has is fixed, and can't be extended regardless of how it's used.

As a consequence of this, nominal tag unions don't have the optional `..others` type parameter that
structural tag unions do.

Nominal tag unions are declared with `:=`, like other [nominal types](types#nominal-types):

```roc
Color := [Red, Green, Blue, Custom(U8, U8, U8)]
```

### Qualified Tags

A tag can be _qualified_ by writing the nominal type's name, then a `.`, then the tag. For example,
`Color.Red` is the `Red` tag of the `Color` type, and `Color.Custom(0, 128, 255)` is its `Custom` tag
with a payload.

A qualified tag always has its nominal type. In contrast, an unqualified tag like `Red` is a
[structural](#structural-tag-unions) tag, which only becomes a `Color` when it's used where a `Color`
is expected (see [Structural-Nominal Compatibility](#structural-nominal-compatibility)):

```roc
a = Color.Red # a is a Color

b : Color
b = Red # b is a Color, because the annotation says so

c = Red # c is a structural tag, not a Color
```

Qualified tags are also allowed in patterns:

```roc
describe : Color -> Str
describe = |color| match color {
    Color.Red => "red"
    Green => "green"
    Blue => "blue"
    Custom(r, g, b) => "rgb(${r.to_str()}, ${g.to_str()}, ${b.to_str()})"
}
```

Since `describe` already says its argument is a `Color`, the unqualified patterns (like `Green`)
work too, so qualifying them is optional.

A qualified tag is a value like any other, so methods can be called on it directly. If `Color`
has a `to_hex` method, `Color.Red.to_hex()` calls it on `Color.Red`. (If `Color` also had a
nested type named `Red`, then `Color.Red.to_hex` would refer to that type's `to_hex` instead.)

### Opaque Tag Unions

Like other [opaque nominal types](types#opaque-nominal-types), a tag union declared with `::` instead
of `:=` hides its tags from other modules:

```roc
Level :: [Low, Medium, High].{
    low : Level
    low = Low

    is_high : Level -> Bool
    is_high = |level| match level {
        High => True
        _ => False
    }
}
```

Inside the module that defines `Level`, its tags can be used like any other nominal tag union's. In other
modules, `Level.Low` and `High` can't be used to create a `Level`, and `Level` values can't be matched
against its tags. Instead, other modules must go through the methods `Level` exposes, like
`Level.low` and `Level.is_high`.

This lets the module that defines an opaque tag union change its tags later (for example, by adding
a `Critical` level, or renaming `Medium` to `Moderate`) without breaking any code in other modules.

### Structural-Nominal Compatibility

As a convenience, you can use structural tags to represent nominal tags with the same shape.

For example, [`Try`](../Try) is a nominal tag union defined as
`Try(ok, err) := [Ok(ok), Err(err)]`, yet you can write `Ok` and `Err` without qualifying them:

```roc
parse_age : Str -> Try(U8, [InvalidAge])
parse_age = |str| match U8.from_str(str) {
    Ok(age) if age < 150 => Ok(age)
    _ => Err(InvalidAge)
}
```

Here, `Ok(age)` and `Err(InvalidAge)` are structural tags, but since they're used where a
`Try(U8, [InvalidAge])` is expected, they become that nominal type. The same thing happens with
`Ok(age)` and `_` in the patterns, since they're matching on a value whose type is already known to be a `Try`.

This works as long as the structural tag is one that the nominal tag union actually has, with a
compatible payload. `Ok(1, 2)` or `Maybe(5)` could not become a `Try`.

> Note that this doesn't work with [opaque](#opaque-tag-unions) tag unions outside the module
> that defines them, since their tags are hidden there.

### Limitations

Unlike structural tag unions, nominal tag unions can be recursive, and can always be sent
across the host boundary.

However, nominal tag unions are not extensible.

## "Void" (Empty Tag Union) {#void}

The empty tag union, written `[]`, is a tag union with no tags. Since there are no tags, it's
impossible to create a value of this type.

That might not sound useful, but it can express that something is impossible. For example, a
function that returns `Try(U64, [])` can never return an `Err`, because there would be no way to
create the `Err` tag's payload:

```roc
always_ok : U64 -> Try(U64, [])
always_ok = |n| Ok(n)
```

The compiler knows this too, so when matching on a `Try(U64, [])`, a branch for `Ok` alone is
[exhaustive](pattern-matching#exhaustiveness):

```roc
unwrap : Try(U64, []) -> U64
unwrap = |result| match result {
    Ok(n) => n
}
```

Similarly, `Ok(n) = always_ok(5)` is allowed as a [destructuring assignment](pattern-matching#destructuring-assignments-with-),
because the `Err` case can't happen.

## Performance

### Memory Layout

A tag union is stored inline (not in a separate heap allocation), as a payload area followed by
a _discriminant_, which is a number saying which tag it is. The payload area is as big as the
biggest payload of any tag in the union, so every value of the union takes up the same amount of
space, no matter which tag it has.

The discriminant is as small as it can be:

| Number of tags | Discriminant size |
| --- | --- |
| 1 | 0 bytes (there's nothing to distinguish) |
| 2 to 256 | 1 byte |
| 257 to 65,536 | 2 bytes |

Like a [record](records#memory-layout), the whole tag union is then padded so its size is a
multiple of its alignment. Some examples on a 64-bit target:

- `[Red, Green, Blue]` has no payloads, so it's just a 1-byte discriminant.
- `[Circle(F64), Rectangle(F64, F64)]` is 24 bytes: 16 for the biggest payload, 1 for the
  discriminant, and 7 bytes of padding to make the total a multiple of 8.
- `[Name(Str)]` has only one tag, so it's 24 bytes, exactly the same as a plain `Str`.

This means one tag with a large payload makes every value of the union large, even if most
values use a different tag. If you have a tag union where one tag's payload is much bigger than
the others, and you store lots of these values (say, in a big list), it can be worth putting the
large payload in a [`Box`](../Box), so that the payload area only needs to be big enough for a
pointer.

### Recursive Tag Unions

When a nominal tag union refers to itself, each place where it does gets stored in its own heap
allocation, which is [reference counted](expressions#reference-counting). For example:

```roc
Tree := [Leaf, Node(Tree, U64, Tree)]
```

Here, each `Node`'s payload holds a `U64` and two pointers (one to each child `Tree`'s
allocation). Without the pointers, a `Tree` would have to contain two more `Tree`s inline, which
would each contain two more, and so on forever.

So building a tree with a million `Node`s involves a million heap allocations, and like other
reference-counted values, nodes are freed as soon as nothing refers to them anymore. Since
values can't refer to themselves (Roc has no [reference cycles](expressions#reference-cycles)),
freeing a tree never leaves any nodes behind.

### Matching

Since the discriminant is a small number, a `match` on a tag union compiles to the same kind of
code as a `switch` statement in C: the compiler can jump straight to the right branch rather than
checking each tag one at a time.

### At the Host Boundary

For the purposes of the discriminant, tags are numbered in alphabetical order, starting at 0.
So in `[Red, Green, Blue]`, `Blue` is 0, `Green` is 1, and `Red` is 2, regardless of the order
they're written in. Platform authors don't need to compute these by hand, though; `roc glue`
generates host code that uses the right numbers.
