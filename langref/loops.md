# Loops

## `for` Loops

A `for` loop iterates over each item in a list:

```roc
var $sum = 0
for n in [1, 2, 3, 4] {
    $sum = $sum + n
}
```

The item can be destructured inline:

```roc
var $total = 0
for (x, y) in [(1, 2), (3, 4), (5, 6)] {
    $total = $total + x + y
}
```

Use `_` to ignore the element value:

```roc
var $count = 0
for _ in items {
    $count = $count + 1
}
```

A loop body only includes statements; it does not have a final expression. The loop itself evaluates to `{}`.

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

Loops typically utilise [variable reassignment](./statements.md#reassignment) or for calling [effectful functions](./functions.md#effectful-functions).
