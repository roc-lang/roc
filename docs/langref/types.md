# Types

Roc is statically typed, and types are inferred — you rarely have to write them,
but you can, and any annotation you write is checked.

## Roc's Type System

Roc uses Hindley–Milner type inference with a few deliberate restrictions:

- **Rank-1.** Quantification happens once per definition, at the outermost
  level. A definition can be polymorphic (`identity : a -> a`), but a function
  argument is used at a single type. I.E. you can't take an `(a -> a)` argument
  and apply it at several element types within one call (that needs rank-2).
- **No higher-kinded polymorphism.** Type variables range over types
  (`Str`, `List(U64)`), never over type constructors — you can't abstract over
  `List` itself, as in `f : m(a) -> m(b)`.
- **No subtyping.** Types relate by unification, not a sub/supertype lattice.
  The width flexibility of records and tag unions is expressed with extension
  variables (see [Structural Types](#structural-types)), not subtyping.

### Generalization

A definition is *generalized* — made reusable at many types — only in these
cases:

- **Functions** are always generalized; each call site is checked at its own
  types.
- **Number literals** default rather than generalize: an unsuffixed literal
  resolves to a concrete type, ultimately falling back to `Dec`. See
  [numbers](numbers.md).
- **An explicitly annotated value** is generalized to its annotated scheme.
  Annotating a value with a free type variable opts into a type scheme, so
  the binding is generalized to it (`empty : List(a)` is then reusable at any
  `a`). Note that we report an error for top-level values with free vars, so in
  practice this only applies to let-defs.
- **A value alias** — a binding whose right-hand side is a bare reference to an
  already-generalized binding (`shorthand = Foo.my_func`) — stays generalized,
  since copying a reference does no work and so is safe to reuse at many types.

Every other value is monomorphic: one type, fixed by its definition and uses.
This is what stops a value (or its `dbg`/`expect`) from being silently
recomputed at each type it might otherwise take.

A mutable variable (`var`) is never generalized, even with an annotation: it has
a single type, fixed by its first use. This is the value restriction in its
original, soundness role — a polymorphic mutable cell could be written at one
type and read back at another, so a `var` is always monomorphic.

## Type Annotations

Annotate a definition by writing `name : Type` above it. Lowercase names in a
type are type variables; repeating a name means the same type.

```roc
greeting : Str
greeting = "hello"

identity : a -> a
identity = |x| x
```

Capitalized declarations introduce *types* rather than values — see
[Nominal Types](#nominal-types) (`:=`) and [Type Aliases](#type-aliases) (`:`).

### Where Clauses

A `where` clause lists the [methods](static-dispatch.md) a type variable must
provide. Each constraint has the form `var.method : signature`:

```roc
join : List(a) -> Str where [a.to_str : a -> Str]
```

A `where` clause can appear on any annotation, including a value's:

```roc
items : List(a) where [a.to_str : a -> Str]
items = []
```

## Structural Types

Structural types are defined by their shape: two of them are the same type when
their shapes match, with no declaration required.

- **Records** — `{ name : Str, age : U64 }`. See [records](records.md).
- **Tag unions** — `[Ok(a), Err(e)]`. See [tag unions](tag-unions.md).
- **Tuples** — `(Str, U64)`. See [tuples](tuples.md).

Records and tag unions are either **closed** (exactly the listed fields or tags)
or **open**, ending in an extension variable that stands for "and possibly
more":

```roc
{ name : Str, .. }     # any record with at least a `name : Str` field
{ name : Str, ..r }    # the same, naming the rest `r`
[Red, Green, ..]       # this union, or any wider one
[Red, Green, ..u]      # the same, naming the rest `u`
```

An anonymous extension (`..`) is a fresh variable each time; a named one (`..r`)
lets you refer to the same "rest" in more than one place.

## Nominal Types

A nominal type is a distinct type with its own identity, declared with `:=`:

```roc
UserId := U64
```

`UserId` and `U64` share a representation but are different types — unification
will not silently mix them. Nominal types may take parameters (`Tree(a) := …`)
and define associated methods in a trailing `.{ }` block.

### Constructing Nominal Types

You construct a nominal value by writing its backing value where the nominal
type is expected; the annotation (or surrounding context) supplies the identity.

```roc
Color := [Red, Green, Blue]
Point := { x : F64, y : F64 }
UserId := U64

c : Color
c = Red               # or `Color.Red`; tags with payloads use `Color.Tag(payload)`

p : Point
p = { x: 1, y: 2 }    # a bare record literal becomes a Point here

uid : UserId
uid = UserId.(0)      # a number literal needs explicit construction (see below)
```

You can also construct a nominal value **explicitly** by naming the type. This is
required when no expected type drives the conversion — for example, returning a
nominal from a function whose argument is the plain backing value:

```roc
Distance := U64
Pair := (U64, Str)

d = Distance.(26)         # value backing
pair = Pair.(1, "two")    # tuple backing
p = Point.{ x: 1, y: 2 }  # record backing
c = Color.Red             # tag backing (with a payload: `Color.Tag(payload)`)

to_distance : U64 -> Distance
to_distance = |n| Distance.(n)   # `|n| n` is a type error: a plain U64 is not a Distance
```

**Structural** literals — records and tags — coerce into a nominal type
implicitly when the expected type supplies the identity (`p` and `c` above): the
literal *is* the backing shape, so it lifts by unification.

**Number and string** literals do not implicitly become a nominal. They coerce
only into a builtin number/string type, or into a nominal that declares its own
`from_numeral` / `from_quote`. For any other nominal — including a transparent
newtype like `UserId := U64` — use explicit construction (`UserId.(0)`).

A value that already has a concrete type — like the `U64` parameter `n` above —
must also be constructed explicitly; it does not silently become a different
nominal.

### Destructuring Nominal Types

Pattern can be destructured to access inner values.

```roc
Color := [Red, Green, Blue].{
    my_color : Color
    my_color = Red
    
    is_red : Color -> Bool
    is_red = |color| match color {
        Red -> True
        _ -> False
    }
}

expect my_color.is_red() # PASS
```

For records:

```roc
get_x : Point -> F64
get_x = |{ x }| x
```

### Opaque Nominal Types

Declaring with `::` instead of `:=` makes a nominal type *opaque*: outside its
defining module the backing representation is hidden, so the type can only be
created and inspected through the methods that module exposes.

```roc
Token :: Str         # other modules see `Token`, never the `Str` inside
```

Inside the defining module an opaque type is constructed and destructured just
like any nominal type; the restriction applies only to other modules.

### Nested Nominal Types

Nominal types can be defined inside other nominal types using the associated items block:

```roc
Geometry := [].{
    Point := { x: F64, y: F64 }.{
        origin : Point
        origin = { x: 0, y: 0 }
    }

    Rectangle := { top_left: Point, bottom_right: Point }.{
        area : Rectangle -> F64
        area = |{ top_left, bottom_right }|
            width = bottom_right.x - top_left.x
            height = bottom_right.y - top_left.y
            width * height
    }
}
```

Nested types are accessed using dot notation:

```roc
rect = Geometry.Rectangle.{ top_left: Geometry.Point.origin, bottom_right: { x: 10, y: 10 } }
```

This is useful for grouping related types under a common namespace.

## Type Aliases

A type alias names an existing type with `:` (not `:=`). Aliases are transparent
— substituted away during compilation — so an alias and its definition are the
*same* type and interchange freely:

```roc
Bytes : List(U8)
Pair : (U64, U64)
```

Reach for an alias when you only want a shorter or clearer name; reach for a
[nominal type](#nominal-types) when you want a genuinely distinct type the
compiler keeps separate.
