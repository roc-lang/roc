# Iterators

An _iterator_ produces a sequence of values, one at a time. The builtin [`Iter`](../Iter) type is Roc's
iterator type: an `Iter(item)` produces values of type `item`.

Iterators are what [`for` loops](loops#for-loops) use to go through collections. Many types have an `iter`
method which returns an iterator over their contents. For example, [`List.iter`](../List#iter) returns an
iterator over a list's elements, and `Dict.iter` returns an iterator over a dictionary's `(key, value)` pairs.
[Ranges](numbers#ranges) have one too:

```roc
total = (1..=100).iter().sum() # 5050
```

## Transforming Iterators

Iterators can be transformed using methods like [`map`](../Iter#map), [`keep_if`](../Iter#keep_if),
[`drop_if`](../Iter#drop_if), [`with_index`](../Iter#with_index), [`take_first`](../Iter#take_first),
and [`drop_first`](../Iter#drop_first). Each of these returns a new iterator:

```roc
evens_times_ten : List(U64)
evens_times_ten = [1, 2, 3, 4, 5, 6]
    .iter()
    .keep_if(|n| n % 2 == 0)
    .map(|n| n * 10)
    .collect() # [20, 40, 60]
```

Iterators can also be combined into a single value, using methods like [`fold`](../Iter#fold),
[`sum`](../Iter#sum), [`min`](../Iter#min), and [`max`](../Iter#max).

### Collecting

[`collect`](../Iter#collect) gathers all of an iterator's items into a collection. The type of
collection is determined by type inference; it works for any type with a `from_iter` method, such as
`List`, `Set`, and `Dict`:

```roc
as_list : List(U64)
as_list = [3, 1, 3].iter().collect() # [3, 1, 3]

as_set : Set(U64)
as_set = [3, 1, 3].iter().collect() # Set.from_list([3, 1])
```

Alternatively, you can name the collection type explicitly by calling its `from_iter` function,
as in `List.from_iter(iterator)`.

## Laziness

Iterators are _lazy_, meaning they don't do any work until something asks them for their next item.
For example, in this code, the function passed to `map` only runs twice, because `take_first(2)`
only asks for two items:

```roc
first_two : List(U64)
first_two = (0..<1_000_000)
    .iter()
    .map(|n| n * 2)
    .take_first(2)
    .collect() # [0, 2]
```

This also means that chaining several transformations together doesn't create a new collection for
each step. Instead, each item goes through the whole chain before the next item is produced, and only
the final `collect` creates a collection.

Since they're lazy, iterators can even represent sequences that never end, as long as whatever uses
them eventually stops asking for more items (for example, using `take_first`).

## Custom Iterators

[`Iter.custom`](../Iter#custom) creates an iterator from a starting state and a function that
advances it:

```roc
powers_of_two : Iter(U64)
powers_of_two = Iter.custom(1, Unknown, |n| Ok((n, n * 2)))

first_five : List(U64)
first_five = powers_of_two.take_first(5).collect() # [1, 2, 4, 8, 16]
```

`Iter.custom` takes three arguments:

1. The initial state. Here, it's `1`.
2. The number of items the iterator will produce, if it's known in advance: `Known(count)` or `Unknown`. Collections use this to allocate the right amount of memory up front.
3. A function that takes the current state and returns either `Ok((item, next_state))` to produce an item and continue with a new state, or `Err(NoMore)` to end the iteration.

Here's an iterator that counts down to 1, and then ends:

```roc
countdown : U64 -> Iter(U64)
countdown = |start| Iter.custom(start, Known(start), |n| {
    if n == 0 {
        Err(NoMore)
    } else {
        Ok((n, n - 1))
    }
})
```

To make your own type work with `for` loops, give it an `iter` method which returns an `Iter`;
see [Iteration](static-dispatch#iteration) for an example.

## How Iteration Works

Under the hood, going through an iterator means repeatedly calling its [`next`](../Iter#next) method, which returns one of:

- `One({ item, rest })`, meaning the iterator produced `item`, and `rest` is an iterator for the remaining items
- `Skip({ rest })`, meaning the iterator didn't produce an item this time (for example, because `keep_if` filtered one out), but there may be more items in `rest`
- `Done`, meaning there are no more items

A `for` loop does this automatically, but you can also call `next` directly:

```roc
match iterator.next() {
    One({ item, rest }) => …
    Skip({ rest }) => …
    Done => …
}
```

Iterators themselves are values, so calling `next` doesn't change the iterator you called it on. Instead,
it gives you `rest`, which you use to continue the iteration.

## Effectful Iteration

`Iter` is for pure computations, so its functions (like the one passed to `map`) can't be
[effectful](functions#effectful-functions). For sequences that involve effects, there's a separate
[`Stream`](../Stream) type, whose functions can be effectful. [`Iter.stream`](../Iter#stream) turns an
iterator into a stream, and [`Stream.custom`](../Stream#custom) builds one from a source whose items
come from effects.

Like iterators, streams are lazy. [`map`](../Stream#map), [`keep_if`](../Stream#keep_if),
[`drop_if`](../Stream#drop_if), [`with_index`](../Stream#with_index),
[`take_first`](../Stream#take_first), and [`drop_first`](../Stream#drop_first) each return a new stream
without pulling any items, and the functions passed to them can be effectful.

A stream's items are pulled when something consumes it: a [`for!` loop](loops#looping-over-streams-with-for),
[`fold!`](../Stream#fold!), [`for_each!`](../Stream#for_each!), or [`collect!`](../Stream#collect!).
Pulling runs effects, so all of these can only be used in effectful functions:

```roc
print_long_lines! : Stream(Str) => {}
print_long_lines! = |lines| {
    for! line in lines.keep_if(|line| line.count_utf8_bytes() > 80) {
        Stdout.line!(line)
    }
}
```

Loops that perform effects on each item don't need a `Stream`, though; a `for` loop can call effectful
functions on each item of an ordinary iterator.

## Performance

Even though iterators are values, and a chain like `.keep_if(…).map(…)` looks like it builds one
iterator on top of another, none of those iterators exist at runtime. The compiler knows at
compile time exactly which operations are in the chain, so it combines the whole chain into a
single loop, the same way a [`for` loop](loops#performance) over a list or range compiles to a
plain loop. For example, this:

```roc
total = [1, 2, 3, 4, 5, 6]
    .iter()
    .keep_if(|n| n % 2 == 0)
    .map(|n| n * 10)
    .sum()
```

compiles to a loop that goes through the list once, checking each element, multiplying the ones
that pass, and adding them to a running total. No iterator gets allocated, and the functions
passed to `keep_if` and `map` can be inlined into the loop body.

This works for chains of up to 16 operations, including when you pass an iterator to a function
or return one from a function. It doesn't work when the number of operations in the chain
depends on something that's only known at runtime. For example, a recursive function that calls
`map` once per recursive call might build a chain with any number of `map`s in it. Iterators like
that (and chains longer than 16 operations) are stored on the heap instead, and getting each item
involves an indirect function call per operation in the chain. They still work; they're just
slower.

### Collecting

When you [`collect`](#collecting) an iterator into a list, the number of items the iterator says
it will produce (the `Known(count)` or `Unknown` that [`Iter.custom`](#custom-iterators) takes)
determines how the list gets allocated. If it's `Known`, the list's memory gets allocated once,
at exactly the right size. If it's `Unknown` (for example, after a `keep_if`, since nobody knows
in advance how many items will be kept), the list starts small and grows as items are added.

So when you write a custom iterator, it's worth passing `Known(count)` whenever you know the count.
