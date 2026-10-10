# Boxes

A [`Box`](../Box) holds one value in its own heap allocation. `Box.box(value)` puts a value in a
box, and `Box.unbox(box)` gets it back out:

```roc
boxed : Box(U64)
boxed = Box.box(42)

answer = Box.unbox(boxed) # 42
```

A `Box` doesn't do anything a plain value can't do, other than change how the value is stored in
memory. So it's only for situations where that matters, which mostly means performance and
[platform](platforms) interop.

## Memory Layout

A `Box` is a pointer to a heap allocation, which holds the value along with a
[reference count](expressions#reference-counting). So a `Box` takes up 8 bytes on a 64-bit target
(4 bytes on a 32-bit target), no matter how big the value inside it is.

Boxing a value copies it into a new heap allocation, and unboxing it copies it back out. So
boxing and unboxing aren't free, but copying a `Box` around afterward only copies the pointer
(and updates the reference count).

## When to Use a Box

### Shrinking a Tag Union

A [tag union](tag-unions#memory-layout) takes up as much space as its largest payload. If one tag
has a much larger payload than the others, and you store lots of these values, then boxing the
large payload means the tag union only needs room for a pointer:

```roc
Event : [
    KeyPress(U8),
    Click(U16, U16),
    Snapshot(Box({ width : U64, height : U64, title : Str, pixels : List(U8) })),
]
```

Without the `Box`, every `Event` in a `List(Event)` would take up 72 bytes on a 64-bit target (64
for a `Snapshot`'s payload, plus 1 for the tag, plus padding), even the `KeyPress` events that only
need 1 byte for their payload. With the `Box`, each `Event` takes up 16 bytes.

### Recursive Types

You don't need a `Box` to make a recursive type. When a [nominal type](types#recursive) refers to
itself, the compiler already stores that part in its own heap allocation, just as if you had
boxed it yourself.

### Passing Values to the Platform

A platform's [hosted functions](modules#hosted-type-modules) can only have type variables inside a
`Box`. That's because the [host](platforms#host) is compiled ahead of time, so it needs to know
how big each argument is. A `Box(a)` is always the size of a pointer, no matter what `a` is, so a
single host function can work with every type `a` could be.

## Equality

Boxes don't have an `is_eq` method, so you can't compare them with `==`. To compare the values
inside two boxes, unbox them first: `Box.unbox(a) == Box.unbox(b)`.
