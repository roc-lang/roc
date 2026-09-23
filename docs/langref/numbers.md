# Numbers

Roc ships with several numeric types, on top of which you can make new [custom number types](#custom-number-types).

## Number Literals

Roc number literals can consist of any combination of the following:
* **Number Digits** (0-9; zeros at the very beginning of the number never change the number)
* **Letter Digits** (a-f, optionally capitalized; these represent the digits 10 through 15 in hexadecimal number literals)
* **Base Prefix** (`0x` in front of the number means the digits will be treated as hexadecimal, which is base-16 instead of base-10. The other options are `0o` for octal, which is base-8, and `0b`, for binary, which is base-2. If there's no prefix, the digits default to being interpreted as decimal, which is base-10. The letter must be lowercase.)
* **Scientific Notation Suffix** (if the number is base-10 and ends in `e___`, everything before the `e` will be multiplied by 10 to the power of the number in the `___`. This suffix can't be used if any base prefix is specified.)
* **A decimal point** (can optionally be combined with scientific notation, but cannot be used if a base prefix is specified because uppercase hexadecimal letters after the decimal point would be ambiguous with a [type suffix](#type-suffixes) such as `.F64`.)
* **Underscores** (the compiler skips over these; they're just for making long numbers easier to read. They can appear in between any digits, including letter digits, and digits after the decimal point, but each underscore must always have a digit on either side of it.)
* **Minus sign in front** (for negative numbers, not to be confused with [the unary negate operator](operators#--negate) which is an operator that applies to expressions. For example, `-x` applies the unary negate operator to `x`, but `-1` is just an ordinary number literal and no negate operation will be executed. This distinction can matter for [custom number types](#custom-number-types).)

Here are some examples of valid number literals:

```roc
1
-1
1.23
-123.456e789
0x1abcde42
-1_000_000.123_456_789
```

## Type Suffixes

Roc's compiler will infer the type of your number literal based on how it's used. For example:

```roc
List.get(my_list, 3)
```

Here, the type of `3` will be `U64` based on how it's used here, because [`List.get`](../List#get) takes a [`List`](../List) as its first argument and a [`U64`](../Num#U64) as its second argument. 

If you want to specify an explicit type for the number (perhaps for documentation, or maybe because you want an error report if it gets used as any other type), you can add the type you want after a dot at the end. For example, here's how you would specify that the number `-12.34` should be interpreted as a [`Dec`](../Num#Dec):

```roc
-12.34.Dec
```

This not only works with builtin number types, but also with any [custom number type](#custom-number-types) you might make—the only requirement is that the type name must be in scope (which you can accomplish using [`import`](modules#import-statements) as long as it's accessible to your module).

## Defaulting to `Dec`

In some situations, a number literal never gets inferred to a specific type. For example:

```roc
if 2 > 1 {
    # ...
}
```

Here, `2 > 1` must be evaluated in order to tell whether the `if` should be taken, yet it's never used in a way that would associate it with any particular number type. In these cases, Roc will use the builtin [`Dec`](../Num#Dec) number type for the literal. So this code will do exactly the same thing as:

```roc
if 2.Dec > 1.Dec {
    # ...
}
```

This almost never comes up in practice, unless you're playing around putting numbers into the REPL. In those situations, `Dec` can be a nice default because it both supports fractions and can give precise answers when doing quick calculations in the REPL.

## Builtin Number Types

All of Roc's builtin number types have a fixed size (and that size never varies by what target you're building for), and they only ever perform heap allocations when converting to heap-allocated types like [Str](../Str).

### Integers

Here are Roc's builtin integer types, along with their ranges and sizes in memory:

| Range                                                  | Type  | Size     |
|--------------------------------------------------------|-------|----------|
| `                                                -128` | [I8](../Num#I8) | 1 Byte   |
| `                                                 127` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                                   0` | [U8](../Num#U8) | 1 Byte   |
| `                                                 255` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                             -32_768` | [I16](../Num#I16) | 2 Bytes  |
| `                                              32_767` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                                   0` | [U16](../Num#U16) | 2 Bytes  |
| `                                              65_535` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                      -2_147_483_648` | [I32](../Num#I32) | 4 Bytes  |
| `                                       2_147_483_647` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                                   0` | [U32](../Num#U32) | 4 Bytes  |
| ` (over 4 billion)                      4_294_967_295` |       |          |
|--------------------------------------------------------|-------|----------|
| `                          -9_223_372_036_854_775_808` | [I64](../Num#I64) | 8 Bytes  |
| `                           9_223_372_036_854_775_807` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                                   0` | [U64](../Num#U64) | 8 Bytes  |
| ` (over 18 quintillion)    18_446_744_073_709_551_615` |       |          |
|--------------------------------------------------------|-------|----------|
| `-170_141_183_460_469_231_731_687_303_715_884_105_728` | [I128](../Num#I128) | 16 Bytes |
| ` 170_141_183_460_469_231_731_687_303_715_884_105_727` |       |          |
|--------------------------------------------------------|-------|----------|
| ` (number below is over 340 undecillion)            0` | [U128](../Num#U128) | 16 Bytes |
| ` 340_282_366_920_938_463_463_374_607_431_768_211_455` |       |          |

Integers come in two flavors: *signed* and *unsigned*.

* *Unsigned* integers can never be negative. The lowest value they can hold is zero.
* *Signed* integers can be negative.

Integers also come in different sizes. Choosing a size depends on your performance
needs and the range of numbers you need to represent. At a high level, the
general trade-offs are:

* Larger integer sizes can represent a wider range of numbers. If you absolutely need to represent numbers in a certain range, make sure to pick an integer size that can hold them!
* Smaller integer sizes take up less memory. These savings rarely matter in variables and function arguments, but the sizes of integers that you use in data structures can add up. This can also affect whether those data structures fit in [cache lines](https://en.wikipedia.org/wiki/CPU_cache#Cache_performance), which can be a performance bottleneck.
* Certain CPUs work faster on some numeric sizes than others. If the CPU is taking too long to run numeric calculations, you may find a performance improvement by experimenting with numeric sizes that are larger than otherwise necessary. However, in practice, doing this typically degrades overall performance, so be careful to measure properly!

### Fractions

Roc has three builtin types for numbers that can have digits after the decimal point:

| Type | Size | Representation |
|------|------|----------------|
| [`Dec`](../Num#Dec) | 16 Bytes | Fixed-point decimal with 18 decimal places |
| [`F64`](../Num#F64) | 8 Bytes | 64-bit binary floating-point |
| [`F32`](../Num#F32) | 4 Bytes | 32-bit binary floating-point |

`Dec` stores numbers in base-10, so it can represent decimal fractions like `0.1` exactly. That means
arithmetic on it gives the answers you'd get by doing the math by hand:

```roc
0.1.Dec + 0.2 == 0.3 # True
```

`Dec` has exactly 18 digits after the decimal point, and can represent numbers between
`-170141183460469231731.687303715884105728` and `170141183460469231731.687303715884105727`. Dividing
a `Dec` by zero crashes.

`F32` and `F64` are [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754) floating-point numbers, which
store numbers in base-2. Many decimal fractions (including `0.1`) can't be represented exactly in base-2,
so floating-point arithmetic often gives answers that are slightly off:

```roc
0.1.F64 + 0.2 == 0.3 # False, because the sum is 0.30000000000000004
```

In exchange, floating-point numbers can represent a much wider range of values (including extremely large and
extremely small numbers), and CPUs have dedicated hardware for floating-point arithmetic, which makes it
much faster than `Dec` arithmetic. Floats also have special values that `Dec` doesn't: dividing a float
by zero doesn't crash, but rather evaluates to infinity (or negative infinity), and some operations
evaluate to [NaN](https://en.wikipedia.org/wiki/NaN).

As a rule of thumb, `Dec` is a good choice for things like money, where exact decimal answers matter,
and floats are a good choice for things like graphics and scientific simulations, where speed matters more
than exact decimal answers.

## Ranges

The range operators build a reusable [`Range`](../Num#Range) value describing
a span of numbers. `start..<end` excludes `end`, while `start..=end` includes
it:

```roc
var $sum = 0

for n in 0..<3 {
    $sum = $sum + n  # runs with n = 0, then 1, then 2
}
```

Both bounds must have the same type. A `U8` range is a `Range(U8)`, a `Dec`
range is a `Range(Dec)`, and so on. When nothing pins the bounds' type, range
literals [default](#defaulting-to-dec) the same way other number literals do.

A range starts with a step of 1. Calling `range.step_by(new_step)` replaces that
absolute step; calling it a second time replaces the first step rather than
multiplying them. `range.size_hint()` returns `Known(count)` when the exact
count fits in a `U64`, and `Unknown` otherwise.

Call `range.iter()` to obtain a forward iterator. Integer and `Dec` ranges also
support `range.iter_rev()`, which produces the same lower-anchored members in
reverse order. For example, `(5.I64..=12).step_by(2).iter_rev()` yields 11, 9,
7, and 5. `F32` and `F64` support forward ranges but deliberately do not
support `iter_rev`, because repeated floating-point addition is not exactly
reversible. Once adding the step can no longer produce a larger float, forward
iteration yields the current value once and then ends.

A range is empty when its lower bound is not below (`..<`) or at (`..=`) its
upper bound, or when its step is not positive. `for` loops call the range's
`iter` method automatically, as in the example above.

Like the other operators, ranges use [static dispatch](static-dispatch#operators):
`start..<end` calls `range_exclusive_to` on the bounds' type, and
`start..=end` calls `range_inclusive_to`. The corresponding
`range_exclusive_from` and `range_inclusive_from` methods opt a type into exact
reverse iteration. Third-party numeric types can construct the stored value
with `Range.custom` and define `range_iter` using their own arithmetic—no
interaction with `U64` is required for the stored step.

## Custom Number Types

We already saw how you can use optional [number type suffixes](#type-suffixes) to specify the type of a number literal instead of letting it be inferred. For example:

```
-12.34.F64
```

[`F64`](../Num#F64) is a builtin type, but you can use your own custom number type in the same way. Let's say you made a custom number type called `Ratio` which stores both a numerator and denominator, so it can represent fractions like two-thirds which can't be precisely represented using either decimals or floating-point numbers. You could create a `Ratio` value like this:

```
-12.34.Ratio
```

Here's what will happen if you write this:

* Just based on the syntax here, at compile time, Roc will call `Ratio.from_numeral(...)` 
* It will pass an argument to specify that this is a negative number with the digits `12` before the decimal point and `34` after it.
* `Ratio.from_numeral` will return a `Try` representing whether the specified digits are a valid `Ratio`. (Some custom number types may have limits on the size of the numbers they store, may or may not support negative numbers, may or may not support digits after the decimal point, etc.)
  * If `Ratio.from_numeral` returned a [`Try.Ok`](../Try) tag, then that tag's [payload](tag-unions#tags) will contain the actual number value that these digits resolved to.
  * If it returned an `Err`, then (since this is all being evaluated at compile time), the compiler will report an error for this number literal before the program even runs.

`from_numeral` is one of Roc's
[well-known static-dispatch methods](static-dispatch#literal-conversion).
  
### Inferred Custom Number Types

Just like with builtin number types, you don't have to annotate your number literals to specify that they use your custom number type. Instead, you can let the compiler infer the type based on usage. For example, let's say you have a function named `from_ratio` which takes a `Ratio`. Then you could write:

```
from_ratio(12.57)
```

First, the compiler would determine that the argument to `from_ratio` is a `Ratio`, and therefore call `Ratio.from_numeral` specifying that the digits before the decimal point are `12` and the digits after the decimal point are `57`. Assuming that returns `Ok`, the value inside that `Ok` would be what ended up getting passed to `from_ratio`.

From there, everything works the same way as in the previous example with the explicit `.Ratio` suffix. The only difference is that you didn't have to write the word `Ratio` because the compiler inferred that was the type, and called its `from_numeral` method.

### Custom Number Types and Operators

Operators like `+` and `/` work on custom number types the same way they work on builtin ones: they
[desugar](operators#desugaring) to method calls. So if `Ratio` has a `div_by` method, then `a / b`
calls `Ratio.div_by(a, b)` when `a` is a `Ratio`.

Here's a `Ratio` type which supports number literals, `+`, and `/`:

```roc
Ratio := { numerator : I64, denominator : I64 }.{
    from_numeral : Numeral -> Try(Ratio, [InvalidNumeral(Str)])
    from_numeral = |numeral| {
        if numeral.digits_after_pt_count() > 0 {
            Err(InvalidNumeral("Ratio literals must be whole numbers, like 3 or 5"))
        } else {
            match I64.from_numeral(numeral) {
                Ok(n) => Ok({ numerator: n, denominator: 1 })
                Err(err) => Err(err)
            }
        }
    }

    plus : Ratio, Ratio -> Ratio
    plus = |a, b| {
        numerator: (a.numerator * b.denominator) + (b.numerator * a.denominator),
        denominator: a.denominator * b.denominator,
    }

    div_by : Ratio, Ratio -> Ratio
    div_by = |a, b| {
        if b.numerator == 0 {
            crash "Ratio division by zero"
        } else {
            { numerator: a.numerator * b.denominator, denominator: a.denominator * b.numerator }
        }
    }
}
```

Now we can write two-thirds as a fraction:

```roc
two_thirds : Ratio
two_thirds = 2 / 3
```

Here, the literals `2` and `3` are converted to `Ratio` values using `Ratio.from_numeral`, and then `/`
calls `Ratio.div_by` on them. Since `two_thirds` is a top-level constant, all of this happens at
[compile time](compile-time), so the program just has the finished `Ratio` value embedded in it.

That also means problems get reported at compile time. There are two ways a custom number type can reject
something:

- `from_numeral` can return an `Err`, which rejects the literal itself. Here, `Ratio` rejects literals with
  digits after the decimal point, so writing `2.5 / 3` would give a compile-time error with the message
  `"Ratio literals must be whole numbers, like 3 or 5"`.
- An operator's method can [`crash`](statements#crash), which rejects the operation. Here, `2 / 0` is made of two valid
  literals, but dividing by zero crashes in `div_by`. Since this happens during compile-time evaluation, it's
  reported as a compile-time error too.

Of course, if the same operation happens at runtime (for example, if the denominator came from user input),
then a crash in the operator's method would happen at runtime.

### Creating a `from_numeral` Implementation

A `from_numeral` method has this type (where `T` is the custom number type):

```roc
from_numeral : Numeral -> Try(T, [InvalidNumeral(Str)])
```

The `Numeral` argument describes the literal's exact value, using these methods:

| Method | Returns |
|--------|---------|
| `numeral.is_negative()` | `True` if the literal had a minus sign in front of it |
| `numeral.digits_before_pt()` | A `List(U8)` of the digits before the decimal point, in base-256 |
| `numeral.digits_after_pt()` | A `List(U8)` of the digits after the decimal point, in base-256 |
| `numeral.digits_after_pt_count()` | How many base-10 digits the literal had after its decimal point |

The digits are given in base-256, with the most significant digit first, because that uses every bit of
each `U8`. For example, for the literal `356.5170`:

- `digits_before_pt` is `[1, 100]`, because 356 = (1 × 256) + 100
- `digits_after_pt` is `[20, 50]`, because 5170 = (20 × 256) + 50
- `digits_after_pt_count` is `4`, because `5170` has four digits

The count is needed because the digits after the point are stored as a whole number, so the count
is what distinguishes `.5170` from `.517` or `.005170`. (For example, `1.0` has a `digits_after_pt` of `[]` and a
`digits_after_pt_count` of `1`, whereas `1` has a `digits_after_pt_count` of `0`, which is how you can tell
whether the literal had a decimal point.)

Zero digits are represented by an empty list, so the literal `0` has a `digits_before_pt` of `[]`,
and `0.25` has a `digits_before_pt` of `[]` and a `digits_after_pt` of `[25]`.

The compiler normalizes the literal before calling `from_numeral`, so these don't need to be handled separately:

- Underscores are removed, so `1_000` is the same as `1000`.
- Base prefixes are applied, so `0xff` has a `digits_before_pt` of `[255]`, just like `255` does.
- Scientific notation is applied, so `1e3` is the same as `1000`, and `1.5e-2` is the same as `0.015`.

Since the digits can be arbitrarily long, a custom number type can support numbers of any size. For
example, an arbitrary-precision integer type could accept literals with hundreds of digits.

The simplest way to implement `from_numeral` is often to delegate to a builtin number type's
`from_numeral`, as in `I64.from_numeral(numeral)` in the [`Ratio` example](#custom-number-types-and-operators) above,
and then convert the result.

If the literal isn't valid for the type, return `Err(InvalidNumeral(message))`. The compiler will
report the message as a compile-time error, pointing at the literal.

