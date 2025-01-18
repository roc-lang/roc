# Abilities

An Ability defines a set of functions that can be implemented by different types.

Abilities are used to constrain the types of function arguments to only those which implement the required functions.

The function `toJson` below is an example which uses the `Encoding` Ability.

```roc
toJson : a -> List U8 where a implements Encoding
toJson = \val ->
    val |> Encode.toBytes JSON.encoder
```

By specifying the type variable `a` implements the `Encoding` Ability, this function can make use of `Encode.toBytes` and `JSON.encoder` to serialise `val`, without knowing its specific type.

All types which implement the `Encoding` Ability can therefore use the `Encode.toBytes` (and also `Encode.append`) functions to conveniently serialise values to bytes.

- [Builtins](#builtins)
  - [`Eq` Ability](#eq-ability)
  - [`Hash` Ability](#hash-ability)
  - [`Sort` Ability](#sort-ability)
  - [`Encoding` Ability](#encoding-ability)
  - [`Decoding` Ability](#decoding-ability)
  - [`Inspect` Ability](#inspect-ability)
- [Opaque Types](#opaque-types)
  - [Derived Implementations](#derived-implementations)
  - [Custom Implementations ](#custom-implementations)
- [Advanced Topic: Defining a new Ability](#defining-a-new-ability)

## [Builtins](#builtins) {#builtins}

Roc's Builtin types such as numbers, records, and tags, are automatically derived for the builtin Abilities. This means that you can use these Abilities without needing to provide a custom implementation.

### [`Eq` Ability](#eq-ability) {#eq-ability}

The `Eq` Ability defines the `isEq` function, which can be used to compare two values for structural equality. The infix operator `==` can be used as shorthand for `isEq`.

`Eq` is not derived for `F32` or `F64` as these types do not support structural equality. If you need to compare floating point numbers, you must provide your own function for comparison.

**Example** showing the use of `isEq` and `==` to compare two values.

```roc
Colors : [Red, Green, Blue]

red = Red
blue = Blue

expect isEq red Red # true
expect red == blue # false
```

**Definition** of the `Eq` Ability.

```roc
# Bool.roc
Eq implements
    isEq : a, a -> Bool where a implements Eq
```

**Structural equality** is defined as follows:

1. Tags are equal if their name and also contents are equal.
2. Records are equal if their fields are equal.
3. The collections `Str`, `List`, `Dict`, and `Set` are equal iff they are the same length and their elements are equal.
4. `Num` values are equal if their numbers are equal. However, if both inputs are _NaN_ then `isEq` returns `Bool.false`. Refer to `Num.isNaN` for more detail.
5. Functions cannot be compared for structural equality, therefore Roc cannot derive `isEq` for types that contain functions.

### [`Hash` Ability](#hash-ability) {#hash-ability}

The `Hash` Ability defines the `hash` function, which can be used to hash a value. The `hash` function takes a `Hasher` as an argument, which is used to compute the hash.

```roc
# Hash.roc
Hash implements
    hash : hasher, a -> hasher where a implements Hash, hasher implements Hasher
```

### [`Sort` Ability](#sort-ability) {#sort-ability}

**Implementation Status** - Design Proposal, implementation has not yet started. See [zulip discussion thread](https://roc.zulipchat.com/#narrow/stream/304641-ideas/topic/ordering.2Fsorting.20ability/near/395539545) for more information. If you would like to help implement this, please let us know.

The `Sort` Ability defines the `compare` function, which can be used to compare two values for ordering.

`Sort` is not derived for `Str` as working with utf-8 strings which is a variable length encoding scheme is complex and is achieved through a dedicated library such as [roc-lang/unicode](https://github.com/roc-lang/unicode).

**Proposed Definition** of the `Sort` Ability.

```roc
# Sort.roc
Sort implements
    compare : a, a -> [LessThan, Equals, GreaterThan] where a implements Sort
```

### [`Encoding` Ability](#encoding-ability) {#encoding-ability}

The `Encoding` Ability defines `toEncoder` which can be used with an Encoder to serialise value from Roc to bytes using the `Encoding.toBytes` and `Encoding.append` functions.

Functions are not serialisable, therefore Roc does not derive `Encoding` for types that contain functions.

Encoding for `Dict` values **has not been implemented**, see [#5294](https://github.com/roc-lang/roc/issues/5294) for more details. If you would like to help implement this, please let us know.

**Example** showing the use of `Encoding.toBytes` to serialise a Roc `List (Str, U32)` to a [JSON](https://www.json.org/json-en.html) encoded string.

```roc
bytes : List U8
bytes = "[[\"Apples\",10],[\"Bananas\",12],[\"Oranges\",5]]" |> Str.toUtf8

fruitBasket : List (Str, U32)
fruitBasket = [
    ("Apples", 10),
    ("Bananas", 12),
    ("Oranges", 5)
]

expect Encode.toBytes fruitBasket json == bytes # true
```

**Definition** of the `Encoding` Ability.

```roc
# Encode.roc
Encoding implements
    toEncoder : val -> Encoder fmt where val implements Encoding, fmt implements EncoderFormatting
```

### [`Decoding` Ability](#decoding-ability) {#decoding-ability}

The `Decoding` Ability defines `decoder` which can be used with a Decoder to de-serialise from bytes to Roc values using the `Decoding.fromBytesPartial` and `Decoding.fromBytes` functions.

Decoding for `Dict` values **has not been implemented**, see [#5294](https://github.com/roc-lang/roc/issues/5294) for more details. If you would like to help implement this, please let us know.

**Example** showing the use of `Decoding.fromBytes` to decode a Roc `List (U32, Str)` from a [JSON](https://www.json.org/json-en.html) encoded string.

```roc
bytes : List U8
bytes =
    """
    [
        [ 10, \"Green Bottles\" ],
        [ 12, \"Buckle My Shoe\" ],
        [ 5, \"Little Ducks\" ]
    ]
    """
    |> Str.toUtf8

result : Result (List (U32, Str)) _
result = Decode.fromBytes bytes json

expect result == Ok [(10, "Green Bottles"), (12, "Buckle My Shoe"), (5, "Little Ducks")] # true
```

**Definition** of the `Decoding` Ability.

```roc
# Decode.roc
Decoding implements
    decoder : Decoder val fmt where val implements Decoding, fmt implements DecoderFormatting
```

### [`Inspect` Ability](#inspect-ability) {#inspect-ability}

The `Inspect` Ability lets you turn values into strings (or other things, but most commonly strings) that inform Roc programmers about the contents of the value.

Every Roc value has the `Inspect` ability automatically, although some of them (such as opaque types which do not list `Inspect` as one of their abilities) use a default `Inspect` implementation which doesn't expose any information about the value. Any opaque type can replace this default `Inspect` implementation with a custom one which exposes as much information as desired about that type's values.

**Definition** of the `Inspect` Ability.

```roc
Inspect implements
    toInspector : val -> Inspector f
        where
            val implements Inspect,
            f implements InspectFormatter
```

The `toInspector` function takes a value and returns an `Inspector` which describes how to abstractly represent that value's contents in a way that doesn't tie it to a particular representation (such as a string). Then a separate "formatter" can translate a given `Inspector` to a specific format (such as a string in the case of [`Inspect.toStr`](https://www.roc-lang.org/builtins/Inspect#toStr), but also possibly a structured log format, or an interactive GUI element).

Example formatters:

- A [DbgFormatter](https://github.com/roc-lang/roc/blob/16db390d5f95516c95c9676797f85b5b2a75cda9/crates/compiler/builtins/roc/Inspect.roc#L106) which creates a string representation of Roc values, for e.g. debug printing to the console.
- A [GuiFormatter](https://github.com/roc-lang/roc/blob/main/examples/GuiFormatter.roc) which creates a GUI representation of Roc values for e.g. debug visualization in a graphical application.

## [Opaque Types](#opaque-types) {#opaque-types}

Opaque Types are used to hide implementation details of a type. Modules export functions to define a _public_ API for working with a type.

By default abilities are not derived for Opaque Types. However, [Derived](#derived-implementations) and [Custom](#custom-implementations) implementations are two ways to work with abilities for your Opaque Types.

### [Derived Implementations](#derived-implementations) {#derived-implementations}

Abilities can be automatically derived for Opaque Types where the type is an alias for a builtin, or it is composed of other types which also implement that ability.

For example you can automatically derive the `Eq` and `Hash` abilities using `implements [ Eq, Hash ]`.

**Example** showing how to automatically derive the `Eq` and `Hash` abilities for an Opaque Type.

```roc
StatsDB := Dict Str { score : Dec, average : Dec } implements [ Eq, Hash ]

add : StatsDB, Str, { score : Dec, average : Dec } -> StatsDB
add = \@StatsDB db, name, stats -> db |> Dict.insert name stats |> @StatsDB

expect
    db1 = Dict.empty {} |> @StatsDB |> add "John" { score: 10, average: 10 }
    db2 = Dict.empty {} |> @StatsDB |> add "John" { score: 10, average: 10 }

    db1 == db2 # true
```

### [Custom Implementations](#custom-implementations) {#custom-implementations}

You can provide a custom implementation for an ability. This may be useful if a type is composed of other types which do not implement an ability, or if you would like to override the default behaviour.

**Example** showing how to provide a custom implementation for the `Eq` ability.

```roc
Color := [
        RgbaU8 U8 U8 U8 U8,
        RgbaF32 F32 F32 F32 F32,
    ]
    implements [
        Eq { isEq: colorEquality },
    ]

# Note that Eq is not available for an F32, hence we provide a custom implementation here.
colorEquality : Color, Color -> Bool
colorEquality = \a, b -> colorToU8 a == colorToU8 b

colorToU8 : Color -> (U8, U8, U8, U8)
colorToU8 = \@Color c->
    when c is
        RgbaU8 r g b a -> (r, g, b, a)
        RgbaF32 r g b a -> (f32toU8 r, f32toU8 g, f32toU8 b, f32toU8 a)

f32toU8 : F32 -> U8
f32toU8 = \f ->
    Num.floor (f * 255.0)

fromU8 : U8, U8, U8, U8 -> Color
fromU8 = \r, g, b, a -> @Color (RgbaU8 r g b a)

fromI16 : I16, I16, I16, I16 -> Result Color [OutOfRange]
fromI16 = \r, g, b, a ->
    if r < 0 or r > 255 or g < 0 or g > 255 or b < 0 or b > 255 or a < 0 or a > 255 then
        Err OutOfRange
    else
        Ok (@Color (RgbaU8 (Num.toU8 r) (Num.toU8 g) (Num.toU8 b) (Num.toU8 a)))

fromF32 : F32, F32, F32, F32 -> Result Color [OutOfRange]
fromF32 = \r, g, b, a ->
    if r < 0.0 or r > 1.0 or g < 0.0 or g > 1.0 or b < 0.0 or b > 1.0 or a < 0.0 or a > 1.0 then
        Err OutOfRange
    else
        Ok (@Color (RgbaF32 r g b a))
```

## [Advanced Topic: Defining a new Ability](#defining-a-new-ability) {#defining-a-new-ability}

It is possible to define a new Ability in addition to those provided in builtins. This should be avoided if possible and only used in rare circumstances by package authors.

**Example** showing how to define a new Ability.

```roc
CustomInspect implements
    inspectMe : val -> Str where val implements CustomInspect

inspect : val -> Str where val implements CustomInspect
inspect = \val -> inspectMe val

Color := [Red, Green, Blue]
    implements [
        Eq,
        CustomInspect {
            inspectMe: inspectColor,
        },
    ]

inspectColor : Color -> Str
inspectColor = \@Color color ->
    when color is
        Red -> "Red"
        Green -> "Green"
        Blue -> "Blue"

expect
    [@Color Red, @Color Green, @Color Blue]
    |> List.map inspect
    |> Str.joinWith ","
    |> Bool.isEq "Red,Green,Blue"
```
