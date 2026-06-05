# Numbers

Roc ships with several numeric types, and you can make new [custom number types](custom) as well.

## Number Literals

You always put underscores in your number literals. They have no effect on the number's value, but can make large numbers easier to read. Here's an example of a large negative number literal:

```roc
-1_000_000.123_456_789
```



## Number Suffixes


```roc
-1_000_000.123_456_789.Dec
```

```roc
-1_000_000.I64
```

### Defaulting to `Dec`

It's possible to use number literals in a way where the number never gets inferred to a specific type. For example:

```roc
if 2 > 1 {
    # ...
}
```

Here, `2 > 1` must be evaluated in order to tell whether the `if` should be taken, yet it's never used in a way that would associate it with a particular number type. In these cases, Roc will use the builtin [`Dec`](builtins/Dec) number type. So this code will do exactly the same thing as:

```roc
if 2.Dec > 1.Dec {
    # ...
}
```

This comes up very rarely in practice, unless you're playing around putting numbers into the REPL. In those situations, `Dec` can be a nice default because it both supports fractions and can give precise answers when doing quick calculations in the repl.

## Builtin Number Types

All of Roc's builtin number types have a fixed size (which is the same no matter what target you're building for), and they only ever perform heap allocations when converting to heap-allocated types like [Str](builtins/Str).

## Integers

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
| ` (over 340 undecillion)                            0` | [U128]| 16 Bytes |
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


Represents a number that could be either an [Int] or a [Frac].

This is useful for functions that can work on either, for example [Num.add], whose type is:
```roc
add : Num a, Num a -> Num a
```
The number 1.5 technically has the type `Num (Fraction *)`, so when you pass
two of them to [Num.add], the answer you get is `3.0 : Num (Fraction *)`.

Similarly, the number 0x1 (that is, the integer 1 in hexadecimal notation)
technically has the type `Num (Integer *)`, so when you pass two of them to
[Num.add], the answer you get is `2 : Num (Integer *)`.

The type [`Frac a`](#Frac) is defined to be an alias for `Num (Fraction a)`,
so `3.0 : Num (Fraction *)` is the same value as `3.0 : Frac *`.
Similarly, the type [`Int a`](#Int) is defined to be an alias for
`Num (Integer a)`, so `2 : Num (Integer *)` is the same value as
`2 : Int *`.

In this way, the [Num] type makes it possible to have `1 + 0x1` return
`2 : Int *` and `1.5 + 1.5` return `3.0 : Frac`.

## Number Literals


If this default of [I64] is not big enough for your purposes,
you can add an `i128` to the end of the number literal, like so:
```roc
Num.to_str(5_000_000_000i128)
```
This `i128` suffix specifies that you want this number literal to be
an [I128] instead of a `Num *`. All the other numeric types have
suffixes just like `i128`; here are some other examples:

* `215u8` is a `215` value of type [U8]
* `76.4f32` is a `76.4` value of type [F32]
* `123.45dec` is a `123.45` value of type [Dec]

In practice, these are rarely needed. It's most common to write
number literals without any suffix.
m range := range

A fixed-size integer - that is, a number with no fractional component.

Integers come in two flavors: signed and unsigned. Signed integers can be
negative ("signed" refers to how they can incorporate a minus sign),
whereas unsigned integers cannot be negative.

Since integers have a fixed size, the size you choose determines both the
range of numbers it can represent, and also how much memory it takes up.

[U8] is an an example of an integer. It is an unsigned [Int] that takes up 8 bits
(aka 1 byte) in memory. The `U` is for Unsigned and the 8 is for 8 bits.
Because it has 8 bits to work with, it can store 256 numbers (2^8),
and because it is unsigned, its min value is 0. This means the 256 numbers
it can store range from 0 to 255.

[I8] is a signed integer that takes up 8 bits. The `I` is for Integer, since
integers in mathematics are signed by default. Because it has 8 bits just
like [U8], it can store 256 numbers (still 2^8), but because it is signed,
the range is different. Its 256 numbers range from -128 to 127.

Here are some other examples:

* [U16] is like [U8], except it takes up 16 bits in memory. It can store 65,536 numbers (2^16), ranging from 0 to 65,536.
* [I16] is like [U16], except it is signed. It can still store the same 65,536 numbers (2^16), ranging from -32,768 to 32,767.

This pattern continues up to [U128] and [I128].

## Performance Details

In general, using smaller numeric sizes means your program will use less memory.
However, if a mathematical operation results in an answer that is too big
or too small to fit in the size available for that answer (which is typically
the same size as the inputs), then you'll get an overflow error.

As such, minimizing memory usage without causing overflows involves choosing
number sizes based on your knowledge of what numbers you expect your program
to encounter at runtime.

Minimizing memory usage does not imply maximum runtime speed!
CPUs are typically fastest at performing integer operations on integers that
are the same size as that CPU's native machine word size. That means a 64-bit
CPU is typically fastest at executing instructions on [U64] and [I64] values,
whereas a 32-bit CPU is typically fastest on [U32] and [I32] values.

Putting these factors together, here are some reasonable guidelines for optimizing performance through integer size choice:

* Start by deciding if this integer should allow negative numbers, and choose signed or unsigned accordingly.
* Next, think about the range of numbers you expect this number to hold. Choose the smallest size you will never expect to overflow, no matter the inputs your program receives. (Validating inputs for size, and presenting the user with an error if they are too big, can help guard against overflow.)
* Finally, if a particular numeric calculation is running too slowly, you can try experimenting with other number sizes. This rarely makes a meaningful difference, but some processors can operate on different number sizes at different speeds.

All number literals without decimal points are compatible with [Int] values.

You can optionally put underscores in your [Int] literals.
They have no effect on the number's value, but can make large numbers easier to read.
```roc
1_000_000
```
Integers come in two flavors: *signed* and *unsigned*.

* *Unsigned* integers can never be negative. The lowest value they can hold is zero.
* *Signed* integers can be negative.

Integers also come in different sizes. Choosing a size depends on your performance
needs and the range of numbers you need to represent. At a high level, the
general trade-offs are:

* Larger integer sizes can represent a wider range of numbers. If you absolutely need to represent numbers in a certain range, make sure to pick an integer size that can hold them!
* Smaller integer sizes take up less memory. These savings rarely matter in variables and function arguments, but the sizes of integers that you use in data structures can add up. This can also affect whether those data structures fit in [cache lines](https://en.wikipedia.org/wiki/CPU_cache#Cache_performance), which can be a performance bottleneck.
* Certain CPUs work faster on some numeric sizes than others. If the CPU is taking too long to run numeric calculations, you may find a performance improvement by experimenting with numeric sizes that are larger than otherwise necessary. However, in practice, doing this typically degrades overall performance, so be careful to measure properly!

Here are the different fixed size integer types:

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
| ` (over 340 undecillion)                            0` | [U128]| 16 Bytes |
| ` 340_282_366_920_938_463_463_374_607_431_768_211_455` |       |          |

If any operation would result in an [Int] that is either too big
or too small to fit in that range (e.g. calling `Num.max_i32 + 1`),
then the operation will *overflow*. When an overflow occurs, the program will crash.

As such, it's very important to design your code not to exceed these bounds!
If you need to do math outside these bounds, consider using a larger numeric size.
t range : Num (Integer range)

A fixed-size number with a fractional component.

Roc fractions come in two flavors: fixed-point base-10 and floating-point base-2.

* [Dec] is a 128-bit [fixed-point](https://en.wikipedia.org/wiki/Fixed-point_arithmetic) base-10 number. It's a great default choice, especially when precision is important - for example when representing currency. With [Dec], `0.1 + 0.2` returns `0.3`. [Dec] has 18 decimal places of precision and a range from `-170_141_183_460_469_231_731.687303715884105728` to `170_141_183_460_469_231_731.687303715884105727`.
* [F64] and [F32] are [floating-point](https://en.wikipedia.org/wiki/Floating-point_arithmetic) base-2 numbers. They sacrifice precision for lower memory usage and improved performance on some operations. This makes them a good fit for representing graphical coordinates. With [F64], `0.1 + 0.2` returns `0.30000000000000004`.

If you don't specify a type, Roc will default to using [Dec] because it's
the least error-prone overall. For example, suppose you write this:
```roc
was_it_precise = 0.1 + 0.2 == 0.3
```
The value of `was_it_precise` here will be `Bool.true`, because Roc uses [Dec]
by default when there are no types specified.

In contrast, suppose we use `f32` or `f64` for one of these numbers:
```roc
was_it_precise = 0.1f64 + 0.2 == 0.3
```
Here, `was_it_precise` will be `Bool.false` because the entire calculation will have
been done in a base-2 floating point calculation, which causes noticeable
precision loss in this case.

The floating-point numbers ([F32] and [F64]) also have three values which
are not ordinary [finite numbers](https://en.wikipedia.org/wiki/Finite_number).
They are:
* ∞ ([infinity](https://en.wikipedia.org/wiki/Infinity))
* -∞ (negative infinity)
* *NaN* ([not a number](https://en.wikipedia.org/wiki/NaN))

These values are different from ordinary numbers in that they only occur
when a floating-point calculation encounters an error. For example:
* Dividing a positive [F64] by `0.0` returns ∞.
* Dividing a negative [F64] by `0.0` returns -∞.
* Dividing a [F64] of `0.0` by `0.0` returns [*NaN*](Num#is_nan).

These rules come from the [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754)
floating point standard. Because almost all modern processors are built to
this standard, deviating from these rules has a significant performance
cost! Since the most common reason to choose [F64] or [F32] over [Dec] is
access to hardware-accelerated performance, Roc follows these rules exactly.

There's no literal syntax for these error values, but you can check to see if
you ended up with one of them by using #is_nan, #is_finite, and #is_infinite.
Whenever a function in this module could return one of these values, that
possibility is noted in the function's documentation.

## Performance Details

On typical modern CPUs, performance is similar between [Dec], [F64], and [F32]
for addition and subtraction. For example, [F32] and [F64] do addition using
a single CPU floating-point addition instruction, which typically takes a
few clock cycles to complete. In contrast, [Dec] does addition using a few
CPU integer arithmetic instructions, each of which typically takes only one
clock cycle to complete. Exact numbers will vary by CPU, but they should be
similar overall.

[Dec] is significantly slower for multiplication and division. It not only
needs to do more arithmetic instructions than [F32] and [F64] do, but also
those instructions typically take more clock cycles to complete.

With [Num.sqrt] and trigonometry functions like [Num.cos], there is
an even bigger performance difference. [F32] and [F64] can do these in a
single instruction, whereas [Dec] needs entire custom procedures - which use
loops and conditionals. If you need to do performance-critical trigonometry
or square roots, either [F64] or [F32] is probably a better choice than the
usual default choice of [Dec], despite the precision problems they bring.
