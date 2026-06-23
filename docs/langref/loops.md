# Loops

Loops let you run the same code multiple times, in...well, in a loop.

## `for` Loops

A `for` loop lets you run code on each item in an [iterator](iterators). For example:

```roc
var $sum = 0

for n in 1.to(5) {
    $sum = $sum + n
}
```

> The `to` method returns a [range](numbers#ranges) which is an `Iter` of the number in question. For example, `I64.to` returns `Iter(I64)`, and so `n` in this example would be an `I64`.

A loop body only includes statements; it does not have a final expression. The loop itself evaluates to `{}`.

### Iterating over types that have `iter`

`for` can also be used on types that have an `iter` method, as long as that method returns an [`Iter`](builtins/Iter). For example, [`List`](builtins/list) has (`List.iter`)[builtins/List#iter], so you can do a `for` loop over a list:

```roc
var $sum = 0

for n in [1, 2, 3, 4] {
    $sum = $sum + n
}
```

At runtime, this `[1, 2, 3, 4]` code snippet is exactly as efficient as the earlier `1.to(5)` one. In one case, `1.to(5)` will be evaluated to an `Iter` at compile time, and in the other, `[1, 2, 3, 4].iter()` will be evaluated at compile time to an identical `Iter`. By the time either program actually runs, they will have the same memory contents and will be executing the same instructions.

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

Just like with [assignments](statements#assignments), the pattern you use here must be [exhaustive](pattern-matching#exhaustiveness). For example, the following would give an exhaustiveness error because the loop body couldn't know what value to use for `amount_to_add` if the item was ever `Err` at runtime:

```roc
var $count = 0
for Ok(amount_to_add) in items {
    $count = $count + amount_to_add
}
```

If you can't write an exhaustive pattern-match, you can name the entire iterator item and then use [`match`](pattern-matching#match) on it inside the loop body.

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

Use `break` to exit the innermost loop immediately:

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

Loops typically use [variable reassignment](./statements.md#reassignment) or for calling [effectful functions](./functions.md#effectful-functions).
