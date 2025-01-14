# Writing a glue plugin

In order to write a platform, the language the platform is written in must know how to interoperate with Roc. As Roc is statically typed, the required FFI wrapper code must be tailored to the datatypes that form the FFI boundry between the host and the Roc application. Handily, the `roc glue` subcommand will take these types as defined in a `platform.roc` file and turn them into layout information. A glue plugin then in turn uses this to generate the appropriate source code in a target language. This layout information comes in the form of a data structure that requires some additional explanation.

- [Writing a glue plugin](#writing-a-glue-plugin)
- [The Types data structure](#the-types-data-structure)
- [The Shape data structure](#the-shape-data-structure)
  - [Unit](#unit)
  - [Bool](#bool)
  - [Num](#num)
  - [RocStr, RocList, RocDict, RocSet, RocBox](#rocstr-roclist-rocdict-rocset-rocbox)
  - [Struct](#struct)
    - [HasNoClosure](#hasnoclosure)
    - [HasClosure](#hasclosure)
  - [TagUnion](#tagunion)
    - [NonRecursive](#nonrecursive)
    - [Enumeration](#enumeration)
    - [SingleTagStruct](#singletagstruct)
    - [Recursive](#recursive)
      - [With pointer tagging](#with-pointer-tagging)
      - [Without pointer tagging](#without-pointer-tagging)
    - [NullableWrapped](#nullablewrapped)
    - [NullableUnwrapped](#nullableunwrapped)
    - [NonNullableUnwrapped](#nonnullableunwrapped)
  - [EmptyTagUnion](#emptytagunion)
  - [TagUnionPayload](#tagunionpayload)
  - [Function](#function)
  - [Unsized](#unsized)
  - [RecursivePointer](#recursivepointer)

# The Types data structure

`Types` is a data structure that stores the layout information of all types on the host-application FFI boundry for a specific architecture. This includes these types' size, alignment, order of declaration, and their `Shape`.

# The Shape data structure

`Shape` is where `roc glue` stores all layout information of a type specific to its kind. It consists of a tag union with the following tags:

## Unit

An empty record `{}`, Roc's anonymous zero-sized type.

## Bool

A one-byte type representing `true` as 1 and `false` as 0.

## Num

A number type. Either a signed integer (I8 through I128), an unsigned integer (U8 through U128), an IEEE 754 floating point number (F32 or F64), or a Roc-specific fixed-point number type ([Dec](https://www.roc-lang.org/builtins/Num#Dec)).

## RocStr, RocList, RocDict, RocSet, RocBox

Roc builtin datatypes. The Roc compiler is planned to expose parts of the standard library to glue code via C FFI to avoid having the reimplement them by hand in every host language. In the meantime, Rust implementations of the `Str`, `List`, and `Box` datatypes can be found under `crates/roc_std` for reference.

## Struct

A Roc record type. Also known as a struct or a product type. Its `fields` field can be one of two variants:

### HasNoClosure

Contains a list of fields with a name and type. Fields are arranged in memory in listed order as they would be in a C struct. Fields are not guaranteed to be in order of declaration in Roc.

```roc
# in `platform.roc`
Color : { red : U8, green : U8, blue : U8 }
```
```roc
# shape reported by `roc glue`
Struct {
    name: "Color",
    fields: (HasNoClosure [
        # `@TypeId 1` == `Num U8`
        {id: (@TypeId 1), name: "blue"},
        {id: (@TypeId 1), name: "green"},
        {id: (@TypeId 1), name: "red"},
    ]),
}
```
```c
// example C layout
struct Color {
    uint8_t blue;
    uint8_t green;
    uint8_t red;
};
```

### HasClosure

(TODO: no glue spec currently supports this. Unsure if this either does not work or if nobody who knows how has gotten around to it.)

## TagUnion

Roc's tagged union/sum types. Roc may choose to optimize a tag union's in-memory representation behind the scenes. This results in tag unions coming in seven different flavors.

Some info common across all of these are:
- All of these are guaranteed have at least one tag (see also: [EmptyTagUnion](#emptytagunion)).
- Discriminants (when stored in a discriminant field or as tags in a tagged pointer) are equal to a tag's index in the list the variants are given in (e.g: the `tags` field in an `Enumeration`) (if applicable).
- Discriminants (when stored in a discriminant field) are currently either an 8-bit or 16-bit unsigned integer.

### NonRecursive

The base case: a tag union with more than one tag which does not contain itself, even through indirection.

```roc
# in `platform.roc`
Cell : [
    Empty,
    Number I32,
    Text Str,
]
```
```roc
# shape reported by `roc glue`
TagUnion (NonRecursive {
    name: "Cell",
    discriminantSize: 1,
    discriminantOffset: 24,
    tags: [
        {name: "Empty", payload: None},
        {name: "Number", payload: (Some (@TypeId 1))},
        {name: "Text", payload: (Some (@TypeId 2))}
    ]
})
```
```c
// example C layout
// note: it is currently guaranteed that the discriminant comes after the
// space reserved in memory for the union, including padding. this means
// NonRecursive tag unions may be represented as a struct, as below. this may
// change in the future, as this currently makes for a missed optimization:
// https://github.com/roc-lang/roc/issues/7171
struct Cell {
    union {
        int32_t Number;
        RocStr Text;
    } payload;
    enum {
        Empty, Number, Text
    } discriminant;
};
```

### Enumeration

A special case where none of the tags in a tag union contains a payload. This means it can be represented as just its discriminant.

```roc
# in `platform.roc`
Color : [Red, Green, Blue]
```
```roc
# shape reported by `roc glue`
TagUnion (Enumeration {
    name: "Expr", 
    size: 1,
    tags: ["Blue", "Green", "Red"]
})
```
```c
// example C layout
enum Color {
    Blue = 0,
    Green = 1,
    Red = 2,
};
```

### SingleTagStruct

A special case where a tag union contains exactly one tag. This means it can be represented as just that tag's payload.

(TODO: the only difference between a `SingleTagStruct` and a `NonNullableUnwrapped` is that the former is non-recursive. It seems this does not result in any differences in layout. Is this one of these shapes vestigial?)

### Recursive

A recursive tag union ineligable for other optimizations.

(TOOO: the `Recursive` variant should only occur when all tags have payloads. Couldn't it store a `TypeId` per payload instead of a `[None, Some TypeId]`?)

Here, the Roc compiler has one last trick up its sleeve. If there are few enough tags to where the discriminant may fit inside a pointer tag (2 bits on 32-bit architectures, 3 bits on 64; 4 and 8 tags respectively), it will use pointer tagging. If this is not possible, it will instead represent the value as a pointer to the payload and discriminant on the heap.

Notably, the `Shape` data structure does not currently reflect whether pointer tagging is being used. This means that (currently) the above determination will need to be replicated in the glue plugin.

(TODO: can the glue platform provide this information directly?)

#### With pointer tagging

```roc
# in `platform.roc`
Expr : [
    String Str,
    Concat Expr Expr,
]
```
```roc
# shape reported by `roc glue`
Recursive {
    name: "Expr",
    discriminantSize: 1,
    discriminantOffset: 24,
    tags: [
        {name: "Concat", payload: (Some (@TypeId 2))},
        {name: "String", payload: (Some (@TypeId 4))},
    ]
}
```
```c
// example C layout
struct Expr {
    size_t tagged_ptr;
};

struct Expr_Concat {
    Expr f0;
    Expr f1;
};

union Expr_payload {
    Expr_Concat Concat;
    RocStr String;
};

enum Expr_discriminant {
    Concat = 0,
    String = 1,
};

#if sizeof(void*) == 8
    #define POINTER_TAG_MASK 0b111ul
#else
    #define POINTER_TAG_MASK 0b11ul
#endif

Expr_payload get_Expr_payload(Expr value) {
    return *(Expr_payload*)value.tagged_ptr & ~POINTER_TAG_MASK;
}

Expr_discriminant get_Expr_discriminant(Expr value) {
    return (Expr_discriminant)value.tagged_ptr & POINTER_TAG_MASK;
}
```

#### Without pointer tagging

```roc
# in `platform.roc`
Expr : [
    String Str,
    Concat Expr Expr,
    Tag3 U8,
    Tag4 U8,
    Tag5 U8,
    Tag6 U8,
    Tag7 U8,
    Tag8 U8,
    Tag9 U8,
]
```
```roc
# shape reported by `roc glue`
TagUnion (Recursive {
    discriminantOffset: 24,
    discriminantSize: 1,
    name: "Expr",
    tags: [
        {name: "Concat", payload: (Some (@TypeId 2))},
        {name: "String", payload: (Some (@TypeId 4))},
        {name: "Tag3", payload: (Some (@TypeId 6))},
        {name: "Tag4", payload: (Some (@TypeId 6))},
        {name: "Tag5", payload: (Some (@TypeId 6))},
        {name: "Tag6", payload: (Some (@TypeId 6))},
        {name: "Tag7", payload: (Some (@TypeId 6))},
        {name: "Tag8", payload: (Some (@TypeId 6))},
        {name: "Tag9", payload: (Some (@TypeId 6))}
    ]
})
```
```c
// example C implementation
struct Expr_allocation;

struct Expr {
    Expr_allocation* ptr;
};

struct Expr_Concat {
    Expr f0;
    Expr f1;
};

struct Expr_allocation {
    union {
        Expr_Concat Concat;
        RocStr String;
        uint8_t Tag3;
        uint8_t Tag4;
        uint8_t Tag5;
        uint8_t Tag6;
        uint8_t Tag7;
        uint8_t Tag8;
        uint8_t Tag9;
    } payload;
    enum {
        Concat = 0,
        String = 1,
        Tag3 = 2,
        Tag4 = 3,
        Tag5 = 4,
        Tag6 = 5,
        Tag7 = 6,
        Tag8 = 7,
        Tag9 = 8,
    } discriminant;
};
```

### NullableWrapped

A special case of `Recursive` where at least one of the tags has no payload. One such tag is then represented as NULL, removing the need to allocate that tag's payload/discriminant. Otherwise, this tag union has the same layout as a `Recursive` tag union.

Notable is that this tag still takes up a discriminant number. This means that:

- all discriminant indices are still equal to their place in the `.tags` list
- a tag being nullable does not stop it from counting as taking up space in the discriminant. This means a tag union with nine tags, one of which being nullable, will not use pointer tagging on 64-bit systems, despite this being possible. (TODO: is this an oversight?)

### NullableUnwrapped

A recursive tag union with:

- one tag without a payload (represented by a NULL pointer);
- one recursive tag (represented by a pointer to the tag's payload on the heap).

```roc
# in `platform.roc`
ConsList : [
    Cons Str ConsList,
    Nil,
]
```
```roc
# shape reported by `roc glue`
TagUnion (NullableUnwrapped {
    name: "ConsList", 
    nonNullPayload: (@TypeId 3), # == TagUnionPayload { name: ConsList_Cons, .. }
    nonNullTag: "Cons",
    nullTag: "Nil",
    whichTagIsNull: FirstTagIsNull
})
```
```c
// example C implementation

struct ConsList_Cons;

struct ConsList {
    ConsList_Cons* nullable_ptr;
};

struct ConsList_Cons {
    RocStr f0;
    ConsList f1;
};

enum ConsList_discriminant {
    Cons,
    Nil,
};

ConsList_discriminant get_ConsList_discriminant(ConsList value) {
    return value.nullable_ptr ? Cons : Nil;
}
```

### NonNullableUnwrapped

A tag union with one recursive variant. This is the only recursive tag union type whose payload isn't behind a pointer. This is possible because all points of recursion happen behind a pointer indirection anyway, such as `List` or `Box`.

```roc
# in `platform.roc`
Tree : [Node (List Tree)]
```
```roc
# shape reported by `roc glue`
TagUnion (NonNullableUnwrapped {
    name: "Tree",
    payload: (@TypeId 3), # == List Tree
    tagName: "Node"
})
```
```c
// example C implementation
struct Tree {
    RocList f0;
};
```

## EmptyTagUnion

An uninhabited, empty sum type. Values of this type cannot be constructed.

## TagUnionPayload

Same layout as a [Struct](#struct). This may be generated when a tag union variant has more than one payload, e.g:

```roc
# in `platform.roc`
Color [
    Transparent,
    Rgb U8 U8 U8,
]
```
```roc
# shape reported by `roc glue`
(TagUnionPayload {
    name: "Color_Rgb",
    fields: (HasNoClosure [
        # `@TypeId 1` == `Num U8`
        {id: (@TypeId 1), name: "0"}, 
        {id: (@TypeId 1), name: "1"},
        {id: (@TypeId 1), name: "2"}
    ]),
})
```
```c
// example C layout
// note: numbers are not valid identifiers in most languages.
// you may have to change the field names somewhat to make it work, e.g:
struct Color_Rgb {
    uint8_t f0;
    uint8_t f1;
    uint8_t f2;
};
```

## Function

A Roc closure.

TODO

## Unsized

(TODO: this seems to only be used in the `Function` shape's `.lambda_set` field)

## RecursivePointer

(TODO: a `RecursivePointer` causes no code to be generated in `RustGlue.roc`, and is otherwise always treated as an instance of the type it's "pointing to", with no pointer indirection like the name implies. Is this shape vestigial?)