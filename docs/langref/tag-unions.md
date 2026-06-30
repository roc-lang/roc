# Tag Unions

A tag union represents a [tagged union](https://en.wikipedia.org/wiki/Tagged_union), which is an example of a sum type.

## Tags

A _tag_ is a name for one of the alternatives in a tag union. Tags can optionally have payloads.

- In `x = Foo`, `Foo` is a tag.
- In `y = Foo(4)`, `Foo` is a tag with a payload of `4`.
- In `y = Foo(4, 2)`, `Foo` is a tag with payloads of `4` and `2`.

> **Note:** At runtime, Payloads optimize to the same thing as tuples. After optimizations, `Foo(4, 2)` and `Foo((4, 2))` compile to exactly the same thing.

Tag unions can't have multiple tags with the same name but different payload types. So for example,
`Foo("a string")` and `Foo(1, 2)` couldn't go in the same tag union, because their tags have
the same name but their payloads are incompatible.

## Structural Tag Unions

_Structural_ tag unions are both structural and extensible.

- **Structural** means that you don't have to choose a name for the type (or declare it in any way), and that two types are considered equivalent if they have the same structure.
- **Extensible** means that the type can accumulate new tags based on how it's used, and also that you can use type variables to .

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
add_blue : [Red, Green, ..others], Bool -> [Red, Green, Blue, ..others]
add_blue = |color, green_to_blue| match color {
    Red => Red
    Green => if (green_to_blue) Blue else Green
    other => other
}
```

If the `Green =>` branch always returned `Blue`, then the return type here would become `[Red, Blue, ..others]`.

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

It's very rare, but occasionally useful to restrict a tag union's ability to be extended:

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
the host couldn't reliably know which tags map to which integer descriminants), 
so this is a way to (for example) make a structural tag union for errors, and then 
mark it as closed so that it can be sent across the host boundary.

### Limitations

Structural tag unions are not allowed to be recursive. To make a recursive tag union, 
use a [nominal tag union](#nominal-tag-unions) instead. (This used to be supported,
but [was removed because of its nonobvious downsides](https://github.com/roc-lang/rfcs/pull/1).)

Platform authors should note the previous section on [closed tag unions](#closed-tag-unions), 
which explains why only closed tag unions can be sent across the host boundary.

## Nominal Tag Unions

Nominal tag unions are like structural tag unions, except that they are neither structural nor growable. That means:

* You have to name a nominal tag union, and no other tag union will be considered equivalent to it—even if they have exactly the same shape.
* The number of alternatives it has is fixed, and can't be extended regardless of how it's used.

As a consequence of this, nominal tag unions don't have the optional `..others` type parameter that
structural tag unions do.

### Qualified Tags

### Opaque Tag Unions

### Structural-Nominal Compatibility

As a convenience, you can use structural tags to represent nominal tags with the same shape.

For example, you can do this:

```roc

```

> **Note:** This does not get around opaque access boundaries.

### Limitations

Unlike structural tag unions, nominal tag unions can be recursive, and can always be sent
across the host boundary.

However, nominal tag unions are not extensible.
