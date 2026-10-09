# Types

Roc is [statically typed](https://en.wikipedia.org/wiki/Type_system#Static_type_checking),
which means every value's type is known at compile time, and type mismatches are compile-time
errors rather than runtime errors.

You rarely have to write types yourself, though, because Roc has
[type inference](https://en.wikipedia.org/wiki/Type_inference). The compiler can figure out
the type of every value in your program without any type annotations at all. You can still
write annotations whenever you like (and it's common to write them on top-level functions,
as documentation), and the compiler will check that they're correct.

## Roc's Type System

Roc's type inference is based on the [Hindley–Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
type system, which is also the basis for type inference in languages like OCaml, Elm, and
Haskell. A practical consequence of this is that type inference never needs hints. If a
program has no type annotations, the compiler can still infer the most general type for
everything in it.

That guarantee rules out some type system features which other languages have, because
inference can't work through them without annotations. By design, Roc does not have:

- **Higher-rank types.** A function's arguments each have one type for the whole call. For
  example, if a function takes an argument `f : a -> a`, the caller picks what `a` is. The function
  body can't then call `f` once with a `Str` and once with a `U64`.
- **Higher-kinded types.** A type variable stands for a type, like `Str` or `List(U64)`, never
  for a type constructor like `List` on its own. So there's no way to write a type like
  `f : m(a) -> m(b)` that works for `List`, `Try`, and so on.
- **Subtyping.** No type is a "subtype" of another. Records and tag unions get similar
  flexibility in a different way, using [open types](#structural-types).

### Generalization

A function like this works on any type of list:

```roc
first_or : List(a), a -> a
first_or = |list, default| list.first() ?? default
```

You can call `first_or` on a `List(Str)` in one place, and a `List(U64)` in another. That's
because its type is _generalized_, meaning that `a` gets chosen separately at each place
`first_or` is used.

Only functions are generalized. A value that isn't a function has exactly one type,
determined by how it's defined and used. For example:

```roc
n = 5

as_u8 : U8
as_u8 = n

as_u64 : U64
as_u64 = n # ERROR! n is a U8 now, because of how as_u8 used it.
```

If `n` were generalized, each of those uses would get its own copy of `n`, and in general,
that means each one would have to evaluate `n`'s definition separately. That's harmless for
`5`, but if `n` were defined using an expensive computation (or a [`dbg`](statements#dbg)),
it would be surprising to have it silently run more than once.

A number literal with nothing to say which type it should be becomes a [`Dec`](numbers).

Giving a name to an existing function also keeps it generalized, since copying a function
doesn't evaluate anything:

```roc
shorthand = first_or
```

A type annotation can make a definition's type more specific, but never more general. So
annotating a value that isn't a function with a type variable is an error:

```roc
empty : List(a) # ERROR! empty isn't a function, so it can only have one type.
empty = []
```

If you want it to have one type, write that type (or write `List(_)` to let the compiler figure
out which type it is). If you want to be able to use it with many types, make it a function:

```roc
empty : {} -> List(a)
empty = |{}| []
```

(Inside a function, it's fine to annotate a value with a type variable that the enclosing
function's annotation introduced, since it's the enclosing function that's generalized.)

A mutable [variable](naming#variables-with-var) is never generalized, even if it's a function.
If it were, you could assign it a value of one type and then read it back as a different type.

## Type Annotations

A _type annotation_ goes on the line before a definition. It's the name, then `:`, then a type:

```roc
greeting : Str
greeting = "hello"

identity : a -> a
identity = |x| x
```

Lowercase names in a type, like `a` here, are _type variables_. A type variable can be any type,
but using the same type variable in more than one place means those places must all be the
same type. So `identity : a -> a` says that `identity` returns the same type it was given.

Uppercase names, like `Str`, refer to specific types. Declarations that introduce new
uppercase names are covered in [Nominal Types](#nominal-types) and [Type Aliases](#type-aliases).

### Where Clauses

A `where` clause says that a type variable must have certain [methods](static-dispatch#methods).
Each requirement is the type variable, a `.`, the method's name, `:`, and the method's type:

```roc
join : List(a) -> Str where [a.to_str : a -> Str]
join = |items| Str.join_with(items.map(|item| item.to_str()), ", ")
```

This `join` function works on a list of any type, as long as that type has a `to_str` method
that returns a `Str`. Calling `join` on a list whose elements don't have that method gives a
compile-time error. [Static dispatch](static-dispatch#where-clauses) covers `where` clauses in
more detail.

## Structural Types

_Structural types_ are types that you don't need to declare before using. Two of them are the
same type if they have the same shape. These are the structural types in Roc:

- **Records**, like `{ name : Str, age : U64 }`. See [records](records).
- **Tag unions**, like `[Ok(a), Err(e)]`. See [tag unions](tag-unions).
- **Tuples**, like `(Str, U64)`. See [tuples](tuples).
- **Functions**, like `Str, U64 -> Str`. See [functions](functions).

Record types and tag union types can be either _closed_, meaning they have exactly the
fields or tags listed, or _open_, meaning they have the listed ones and possibly others. An
open type has `..` at the end:

```roc
{ name : Str }            # a record with a name field and no other fields
{ name : Str, .. }        # a record with a name field, and maybe others
{ name : Str, ..others }  # the same, but you can refer to the other fields as `others`

[Red, Green]              # Red or Green, and nothing else
[Red, Green, ..]          # Red, Green, or maybe other tags
[Red, Green, ..others]    # the same, but you can refer to the other tags as `others`
```

Naming the rest of the type (`..others`) is useful when it needs to appear in more than one
place, for example when a function returns a record with the same extra fields it was given.

## Nominal Types

A _nominal type_ is a type with a name, which is different from every other type, even
types with exactly the same shape. You declare one with `:=`:

```roc
UserId := U64
```

`UserId` is stored in memory exactly the same way as a `U64` is, but the compiler won't let you
use one where the other is expected. This means you can't accidentally pass an order ID to
a function that wants a `UserId`, even though both are numbers underneath.

The type after the `:=` is called the nominal type's _backing type_. Nominal types can have
type parameters, like `Tree(a) := …`, and they can have [methods](static-dispatch#methods),
which go in a `.{ … }` block after the backing type.

### Constructing Nominal Types

You can always create a value of a nominal type by writing the type's name, then `.`, then the
backing value:

```roc
Distance := U64
Pair := (U64, Str)
Point := { x : F64, y : F64 }
Color := [Red, Green, Blue]

d = Distance.(26)         # backed by a number
pair = Pair.(1, "two")    # backed by a tuple
p = Point.{ x: 1, y: 2 }  # backed by a record
c = Color.Red             # backed by a tag union
```

When the backing type is a record or a tag union, you can leave off the name if the compiler
already knows which nominal type is expected (for example, because of a type annotation):

```roc
p : Point
p = { x: 1, y: 2 }

c : Color
c = Red
```

Number and string literals are different. You have to write the type's name for those, unless
the nominal type has its own [`from_numeral` or `from_quote`](static-dispatch#literal-conversion)
method:

```roc
UserId := U64

a : UserId
a = 5 # ERROR! Write UserId.(5) instead.

b : UserId
b = UserId.(5)
```

The same goes for a value that already has some other type. For example, a `U64` argument doesn't
turn into a `Distance` on its own:

```roc
to_distance : U64 -> Distance
to_distance = |n| Distance.(n) # Writing just `n` here would be an error.
```

The reason for these rules is that a record literal or tag can only mean one thing when a
`Point` or `Color` is expected, but a number like `5` could reasonably be meant as either a plain
number or a `UserId`, and part of the point of a type like `UserId` is to make you say which.

### Destructuring Nominal Types

You can [pattern match](pattern-matching) on a nominal type's backing value. If the backing type
is a tag union, you can match on its tags directly:

```roc
Color := [Red, Green, Blue].{
    is_red : Color -> Bool
    is_red = |color| match color {
        Red => True
        _ => False
    }
}
```

If the backing type is a record, write the type's name before the record pattern:

```roc
get_x : Point -> F64
get_x = |Point.{ x, .. }| x
```

(The `..` means "and other fields," since `Point` also has a `y` field. Record patterns
without `..` have to list every field.)

### Opaque Nominal Types

If you declare a nominal type with `::` instead of `:=`, it's _opaque_:

```roc
Token :: Str
```

Outside the module where `Token` is declared, nobody can see that it's backed by a `Str`.
Other modules can't create a `Token` themselves, or destructure one to get the `Str` out.
All they can do is use the methods `Token`'s module provides.

Inside its own module, an opaque type works exactly like any other nominal type.

This is how you can guarantee that every value of a type follows some rule. For example, if
every `Token` has to be a nonempty string, and the only method that creates a `Token` checks
for that, then every `Token` in the whole program is nonempty.

### Nested Nominal Types

Nominal types can be declared inside other nominal types' `.{ … }` blocks:

```roc
Geometry := [].{
    Point := { x : F64, y : F64 }.{
        origin : Point
        origin = { x: 0, y: 0 }
    }

    Rectangle := { top_left : Point, bottom_right : Point }.{
        area : Rectangle -> F64
        area = |{ top_left, bottom_right }| {
            width = bottom_right.x - top_left.x
            height = bottom_right.y - top_left.y

            width * height
        }
    }
}
```

You refer to nested types with a `.` between the names:

```roc
rect = Geometry.Rectangle.{
    top_left: Geometry.Point.origin,
    bottom_right: { x: 10, y: 10 },
}
```

This is useful for grouping related types together, especially since each
[type module](modules#type-modules) exposes only one type.

## Type Aliases

A _type alias_ is a different name for an existing type. You declare one with `:` (instead of
the `:=` that nominal types use):

```roc
Bytes : List(U8)
Pair : (U64, U64)
```

Unlike a nominal type, an alias is not a new type. `Bytes` and `List(U8)` are the same type, so
you can use them interchangeably anywhere.

So if you just want a shorter or more descriptive name for a type, use an alias. If you want the
compiler to keep it separate from other types, use a [nominal type](#nominal-types).

## Recursive Types {#recursive}

A nominal type can refer to itself in its own definition. This is how you define data structures
like trees:

```roc
Tree := [Leaf, Node(Tree, U64, Tree)]
```

Type aliases can't be recursive. An alias is just a different name for the type it's defined as,
so replacing a recursive alias with its definition would never end. Use a nominal type (`:=`)
instead.

[Structural tag unions](tag-unions#limitations) can't be recursive either, so all recursive types
are nominal types.

## Mutually Recursive Types {#mutually-recursive}

Nominal types can refer to each other in their definitions. For example, a syntax tree for a small
programming language might have expressions which contain statements, and statements which contain
expressions:

```roc
Expr := [Num(I64), Add(Expr, Expr), Block(List(Stmt))]

Stmt := [Let(Str, Expr), Print(Expr)]
```

As with other recursive types, every type in the cycle must be a nominal type.

Since each [type module](modules#type-modules) exposes only one type, mutually recursive types that
need to be used from other modules usually go inside a single type's `.{ … }` block.
[Importing mutually recursive types](modules#importing-mutually-recursive-types) shows how.

## Performance

Types exist only at compile time. None of the following has any cost at runtime:

- **Type annotations.** Adding or removing one never changes what the compiled program does.
- **Type aliases.** An alias is the same type as what it's an alias for.
- **Nominal types.** A nominal type is stored exactly the same way as its backing type, so
  wrapping a `U64` in `UserId.(…)` and unwrapping it again compiles to nothing.
- **Opaque types.** Being opaque only affects which code is allowed to see the backing type.

Generalized functions don't cost anything at runtime either. When the compiler builds your
program, it makes a separate copy of each generalized function for every combination of types
it's actually used with. So if `first_or` is called on a `List(Str)` in one place and a
`List(U64)` in another, the compiled program has two versions of `first_or`, each of which works
directly on its own type's memory layout. This is called
[monomorphization](https://en.wikipedia.org/wiki/Monomorphization), and it's what Rust and C++
templates do too. The tradeoff is that using one function with many different types makes the
compiled program larger, but it means you never pay for generic code being generic at runtime.

> Note that `roc build --specialize=no` uses an experimental alternative to monomorphization,
> which compiles each generalized function only once, and passes it values in heap-allocated
> boxes. Programs compiled this way do extra work at runtime that monomorphized programs don't.
