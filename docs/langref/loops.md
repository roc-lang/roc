# Loops

Loops let you run the same code multiple times, in...well, in a loop.

## `for` Loops

A `for` loop lets you run code on each item in an [iterator](iterators). For example:

```roc
var $sum = 0

for n in 1..<5 {
    $sum = $sum + n
}
```

Here, `1..<5` is a [range](numbers#ranges), which describes the numbers from 1 up to (but not including) 5. Writing `1..=5` instead would include the 5.

A loop body only includes statements; it does not have a final expression. The loop itself evaluates to `{}`.

### Iterating over types that have `iter`

`for` can also be used on types that have an
[`iter`](static-dispatch#iteration) method, as long as that method returns an
[`Iter`](../Iter). The loop then calls `next` on the returned iterator.
For example, [`List`](../List) has `List.iter`, so you can do a `for`
loop over a list:

```roc
var $sum = 0

for n in [1, 2, 3, 4] {
    $sum = $sum + n
}
```

This `[1, 2, 3, 4]` code snippet works the same way as the earlier `1..<5` one. The range stores its bounds and produces an iterator when the loop calls its `iter` method; the list's `iter` method also produces an iterator over the same values. The loop then repeatedly calls `next` on that `Iter`.

### Looping backwards

To visit a list's items from last to first, use `List.iter_rev` instead of
`iter`:

```roc
var $visited = []

for n in [1, 2, 3, 4].iter_rev() {
    $visited = $visited.append(n)
}

# $visited is now [4, 3, 2, 1]
```

This reads the list backwards in place. Unlike `List.rev`, it does not build a
reversed copy of the list first. Dictionaries and sets also provide `iter_rev`
to traverse their current iteration order backwards. To reverse values from
another iterator source, first collect them with `List.from_iter`, then call
`iter_rev` on that list.

### Pattern matching in `for`

Whatever you put between `for` and `in` is treated as a [pattern](pattern-matching), meaning (for example) that the item can be destructured inline:

```roc
var $total = 0
for (x, y) in [(1, 2), (3, 4), (5, 6)] {
    $total = $total + x + y
}
```

As usual, you can nest patterns as much as you like, and can use `_` if you don't want to name a pattern:

```roc
var $count = 0
for _ in items {
    $count = $count + 1
}
```

Just like with [assignments](statements#assignment), the pattern you use here must be [exhaustive](pattern-matching#exhaustiveness). For example, the following would give an exhaustiveness error because the loop body couldn't know what value to use for `amount_to_add` if the item was ever `Err` at runtime:

```roc
var $count = 0
for Ok(amount_to_add) in items {
    $count = $count + amount_to_add
}
```

If you can't write an exhaustive pattern-match, you can name the entire iterator item and then use [`match`](pattern-matching#match) on it inside the loop body.

(Note: the dedicated exhaustiveness error is not implemented yet for `for` patterns, even though it is for [assignments](statements#assignment). Currently, a non-exhaustive tag pattern like this one is reported as a type mismatch instead, and non-exhaustive patterns that the type checker can't rule out—such as a number literal pattern—are not caught at compile time and crash at runtime.)

## Looping over Streams with `for!`

A `for!` loop goes through a [`Stream`](iterators#effectful-iteration): a sequence whose items
come from running effects, like reading lines from a file. It works like a `for` loop,
except that it gets each item by calling the stream's effectful `next!` method:

```roc
print_lines! : Stream(Str) => {}
print_lines! = |lines| {
    for! line in lines {
        Stdout.line!(line)
    }
}
```

Getting each item runs an effect, so a `for!` loop can only be used in an
[effectful function](functions#effectful-functions), just like calling a function whose name ends in `!`.

`for!` calls the `stream` method on the value it loops over, so it works on any `Stream`, and also
on any `Iter` (through `Iter.stream`). Everything else about it is the same as a `for` loop:
the pattern between `for!` and `in` must be exhaustive, and `break` and `return` work the same way.

A plain `for` loop can't go through a `Stream`, because a `for` loop only gets its items
from pure iterators. (A plain `for` loop can still call effectful functions in its body, though.)

## `while` Loops

A `while` loop repeatedly executes its body while a condition is true:

```roc
var $i = 0
var $sum = 0
while $i < 5 {
    $sum = $sum + $i
    $i = $i + 1
}
```

The condition must evaluate to a boolean value.

## `break` Statement

`break` exits the innermost loop immediately:

```roc
var $sum = 0
for i in [1, 2, 3, 4, 5] {
    if i == 4 {
        break
    }
    $sum = $sum + i
}
# $sum is 6 (1 + 2 + 3, loop exits before 4)
```

In nested loops, `break` only exits the innermost loop:

```roc
var $result = 0
for i in [1, 2, 3] {
    for j in [10, 20, 30] {
        if j == 20 {
            break  # only exits inner loop
        }
        $result = $result + j
    }
}
```

Loops are typically used for [variable reassignment](statements#reassignment) or for calling [effectful functions](functions#effectful-functions).

## Infinite Loops

A `while` loop whose condition is `True` will keep running until something inside it
exits the loop, such as a [`break`](#break-statement) or a [`return`](statements#return):

```roc
var $n = 1

while True {
    $n = $n * 2

    if $n > 100 {
        break
    }
}

# $n is now 128
```

This is useful when the condition for exiting the loop is easiest to check in the middle of
the loop body, rather than at the beginning.

If nothing ever exits the loop, it will run forever. For example, a server might have a loop
that waits for a request, handles it, and then goes back to waiting for the next request.

A loop that runs forever during [compile-time evaluation](compile-time) will currently hang the compiler, as
noted in [Pure Functions](functions#pure-functions).

## Performance

A `for` loop over a list or a range compiles to the same kind of loop you'd write by hand in C:
a counter that goes up by one each time. No iterator gets allocated on the heap, and there's no
function call per item. This is also true when the loop goes over a chain of
[iterator](iterators) operations, like `for n in list.iter().map(double).keep_if(is_big)`. The
compiler combines the whole chain into one loop, which does the mapping and filtering for each
item as it goes, without building any intermediate lists.

`while` loops compile to ordinary loops too, and since [variables](naming#variables-with-var) are
just names for values, reassigning them in a loop doesn't allocate anything by itself.

Building up a list in a loop is fast when the list is unique, because each `append` can add to the
list in place:

```roc
squares = |count| {
    var $list = List.with_capacity(count)

    for n in 0..<count {
        $list = $list.append(n * n)
    }

    $list
}
```

Since `$list` holds the only reference to the list, each `append` just writes the new element
into the list's existing memory. When the list runs out of room, `append` gets more memory (with
room to spare, so this doesn't happen on every append). Starting with
[`List.with_capacity`](../List#with_capacity) means it never runs out of room in this loop, since
the loop knows how many elements it'll add.
