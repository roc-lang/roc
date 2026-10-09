# Lists

A [`List`](../List) is a sequence of values, all of the same type, stored next to each other in
memory. For example, `[1, 2, 3]` is a list of three numbers. `List(Str)` is the type of a list of
strings, `List(List(U8))` is the type of a list of lists of bytes, and so on.

Lists are Roc's most common collection. Unlike in some other functional languages, a Roc list is
not a [linked list](https://en.wikipedia.org/wiki/Linked_list). It's an
[array](https://en.wikipedia.org/wiki/Array_(data_structure)), more like a `Vec` in Rust or an
`ArrayList` in Java, which means you can get any element by its index without going through the
elements before it.

## List Literals

A list literal is a list of comma-separated expressions between `[` and `]`:

```roc
numbers = [1, 2, 3]
names = ["Sam", "Alex"]
empty = []
```

Every element has to have the same type. If you want a list that holds a few different kinds of
values, you can make it a list of [tags](tag-unions):

```roc
shapes = [Circle(1.5), Rectangle(2, 3)]
```

## Using Lists

Lists have lots of functions in the [`List`](../List) module. Since they're
[methods](static-dispatch#calling-methods), you can call them with `.`:

```roc
[1, 2, 3].len()                # 3
[1, 2, 3].append(4)            # [1, 2, 3, 4]
[1, 2, 3].map(|n| n * 2)       # [2, 4, 6]
[1, 2, 3].drop_first(1)        # [2, 3]
[1, 2].concat([3, 4])          # [1, 2, 3, 4]
```

Like all values in Roc, lists are immutable, so functions like `append` return a new list rather
than changing the one they were given. (Behind the scenes, they often do change the original
list; see [Performance](#performance).)

### Indexes

List indexes are `U64` numbers, starting at 0. Since an index might be past the end of the list,
functions that take an index return a [`Try`](../Try):

```roc
[1, 2, 3].get(1)    # Ok(2)
[1, 2, 3].get(5)    # Err(OutOfBounds)
[1, 2, 3].set(1, 9) # Ok([1, 9, 3])
```

So there's no such thing as an "index out of bounds" crash in Roc; you always get an `Err` you can
handle (or pass along with [`?`](operators#-unwrap-if-ok-early-return-if-err)).

### Iterating

You can go through a list's elements with a [`for` loop](loops#for-loops):

```roc
var $total = 0

for n in [1, 2, 3] {
    $total = $total + n
}
```

Lists also have an [`iter`](../List#iter) method, which returns an [iterator](iterators) over the
list's elements. That's useful for chaining several transformations together without building a
new list for each step.

### Pattern Matching

You can [pattern match](pattern-matching#list-patterns) on lists, to check how many elements they
have and what those elements are:

```roc
describe = |list|
    match list {
        [] => "empty"
        [only] => "just ${only}"
        [first, .. as rest] => "${first} and ${rest.len().to_str()} more"
    }
```

## Performance

### Memory Layout

A `List` value is 24 bytes on a 64-bit target (12 bytes on a 32-bit target), made up of three
pointer-sized pieces: a pointer to the elements, the number of elements (the list's _length_), and
how many elements there's room for before the list needs more memory (its _capacity_).

The elements themselves are stored next to each other in a heap allocation, along with a
[reference count](expressions#reference-counting). Each element takes up exactly as much space as
its type does; for example, a `List(U8)` with a million elements takes up a megabyte (plus a few
bytes for the reference count), and a `List({ x : F64, y : F64 })` with a million elements takes up
16 megabytes. Storing elements next to each other like this makes going through a list in order
very fast, because CPUs are good at reading memory in order.

An element that has heap-allocated parts, like a `Str`, stores its fixed-size part in the list,
and its heap-allocated part separately. So a `List(Str)` stores one 24-byte `Str` per element,
each of which points to its own allocation (unless the string is
[small enough](strings#memory-layout) to fit in the 24 bytes).

An empty list doesn't allocate anything. Neither does a list whose elements take up no space at
all, like a `List({})`.

### Updating in Place

Lists use [opportunistic mutation](expressions#opportunistic-mutation): when nothing else refers
to a list, functions like `set`, `append`, `concat`, `sort`, and `map` change it in place, rather
than making a new list. For example:

```roc
squares = |count| {
    var $list = List.with_capacity(count)

    for n in 0..<count {
        $list = $list.append(n * n)
    }

    $list
}
```

Here, `$list` is the only reference to the list, so each `append` writes the new element into the
existing memory. That's exactly what you'd do with a growable array in a language with mutation,
and it performs the same way.

When something else _does_ refer to the list, the function copies the list first, and then changes
the copy. That copy takes time proportional to the length of the list, so the most important thing
to know about list performance in Roc is: if you're updating a list many times (for example, in a
loop), make sure nothing else is still holding onto the old version.

`map` can reuse the original list's memory too, as long as nothing else refers to the list, and the
new elements take up the same amount of space as the old ones. So `list.map(|n| n * 2)` on a unique
`List(U64)` doesn't allocate anything.

### Growing

When `append` (or `concat`, and so on) needs more room than the list's capacity, it allocates a
bigger block of memory and moves the elements there. To avoid doing that on every append, it
allocates extra room each time: an empty list grows to at least 64 bytes' worth of elements, and
after that, each time the list grows, its capacity goes up by 1.5 to 2 times. That means appending
`n` elements one at a time only grows the list about `log(n)` times.

If you know how many elements a list will end up with, you can skip all of that by starting with
[`List.with_capacity`](../List#with_capacity), or by calling [`List.reserve`](../List#reserve) on an
existing list. If a list ends up with a lot more capacity than it needs, and it's going to stick
around for a while, [`List.release_excess_capacity`](../List#release_excess_capacity) gives the
extra memory back.

Adding to the end of a list is fast, but adding to the front with
[`prepend`](../List#prepend) has to move every element over by one, so it takes time proportional
to the list's length. If you need to build a list in reverse order, it's usually faster to `append`
everything and then reverse the list once at the end (or iterate over it backwards with
[`iter_rev`](loops#looping-backwards)).

### Slices

Functions that return part of a list, like [`drop_first`](../List#drop_first),
[`take_first`](../List#take_first), and [`sublist`](../List#sublist), don't copy any elements.
Instead, they return a _slice_: a list that points into the middle of the original list's memory.
So they take the same amount of time no matter how long the list is. The same is true of naming
the rest of a list in a [pattern](pattern-matching#list-patterns), like `[first, .. as rest]`.

The tradeoff is that a slice keeps the whole original list's memory alive. So if you take a small
slice of a huge list, and then keep the slice around for a long time after you're done with the
rest of the list, all of the huge list's memory stays allocated too.

When a function like `drop_last` takes elements off the end of a unique list, it doesn't need a
slice at all; it just makes the list's length shorter, in place.

### Lists of Lists

In a `List(List(U8))`, each inner list is a separate heap allocation, with its own reference count.
The outer list's memory only holds the 24-byte `List` values that point to them. So a list of a
million small lists means a million and one allocations.

When all the inner lists are the same length (for example, the rows of a grid), it's often faster
to use one flat list, and compute each element's index yourself:

```roc
get_cell = |grid, width, x, y| grid.get(y * width + x)
```

### Lists Built at Compile Time

A list literal (or any other list) that's [evaluated at compile time](compile-time) gets embedded
in the compiled program, and using it at runtime doesn't allocate anything. These lists are never
unique, though, so the first time the program changes one of them, it gets copied. After that,
the copy is unique, and later changes to it happen in place.
