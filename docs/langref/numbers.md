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
| `                                                -128` | [I8]  | 1 Byte   |
| `                                                 127` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                                   0` | [U8]  | 1 Byte   |
| `                                                 255` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                             -32_768` | [I16] | 2 Bytes  |
| `                                              32_767` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                                   0` | [U16] | 2 Bytes  |
| `                                              65_535` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                      -2_147_483_648` | [I32] | 4 Bytes  |
| `                                       2_147_483_647` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                                   0` | [U32] | 4 Bytes  |
| ` (over 4 billion)                      4_294_967_295` |       |          |
|--------------------------------------------------------|-------|----------|
| `                          -9_223_372_036_854_775_808` | [I64] | 8 Bytes  |
| `                           9_223_372_036_854_775_807` |       |          |
|--------------------------------------------------------|-------|----------|
| `                                                   0` | [U64] | 8 Bytes  |
| ` (over 18 quintillion)    18_446_744_073_709_551_615` |       |          |
|--------------------------------------------------------|-------|----------|
| `-170_141_183_460_469_231_731_687_303_715_884_105_728` | [I128]| 16 Bytes |
| ` 170_141_183_460_469_231_731_687_303_715_884_105_727` |       |          |
|--------------------------------------------------------|-------|----------|
| ` (number below is over 340 undecillion)            0` | [U128]| 16 Bytes |
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

## Ranges

The range operators build an [iterator](iterators) over a span of numbers.
`start..<end` counts up from `start` to `end` without including `end`, and
`start..=end` includes `end`:

```roc
var $sum = 0

for n in 0..<3 {
    $sum = $sum + n  # runs with n = 0, then 1, then 2
}
```

Both bounds must have the same type, and the range is an `Iter` of that type:
a `U8` range is an `Iter(U8)`, a `Dec` range is an `Iter(Dec)`, and so on.
When nothing pins the bounds' type, range literals [default](#defaulting-to-dec)
the same way other number literals do.

A range counts up in steps of 1, and is empty when `start` is not below (`..<`)
or at (`..=`) `end` — there are no reversed ranges. All the builtin number
types support ranges, including the fractional ones: `0.5..<3.5` yields `0.5`,
`1.5`, and `2.5`. For `F32` and `F64`, once the values are large enough that
adding 1 can no longer produce a bigger float, the range yields that value once
and then ends.

Like the other operators, ranges use [static dispatch](static-dispatch#operators):
`start..<end` calls the `range_exclusive` method on the bounds' type, and
`start..=end` calls `range_inclusive`, so custom number types can support range
syntax by defining those methods.

### Fractions

TODO

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

TODO

<!-- notes:
- note Ratio and like `3 / 4` and compile time, including if you write `3 / 0` what happens - can either do it in the literal's `Try`, or can just let it execute and then return a `crash`; either way, that will all get executed at compile time.
-->

### Creating a `from_numeral` Implementation

TODO

<!-- notes:
- it converts all other forms to this base-10 representation, so hex/octal/etc. doesn't matter (and neither do underscores ofc).
- mention the implication that you can make arbitrary-sized nums this way
- if the List of digits_after_decimal_pt is empty, then you know it didn't have a decimal point
- if the List of digits_before_decimal_pt is empty, then ___________? should we allow that?
-->
