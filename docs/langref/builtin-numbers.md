


## Floating-Point and Fixed-Point

Roc's standard library has two floating-point number types, `F32` and `F64`, as well as one fixed-point number type, `Dec`.

The point of `F32` and `F64` is to be fast. They are designed to have each of their operations translate directly to as few processor instructions as possible, ideally one instruction when possible. This extreme focus on performance means their design is heavily constrained by hardware design, even when the hardware's design might be error-prone.

The point of `Dec` is to be much less error-prone than `F32` and `F64`, especially when it comes to base-10 calculations (such as working with currency), at the cost of performance.

`F32` and `F64` do not support `is_eq` because of how NaN, Infinity, and -Infinity work.

## Floating-Point Error Handling

`F32` and `F64` tend to have great performance because they have dedicated hardware support at the processor level. However, their hardware-level error handling (which comes from the IEEE-754 specification for binary floating-point numbers) is optimized for solving a hardware coordination problem (namely, how to get CPU vendors in the 1980s to agree on a common way to handle floating-point arithmetic error cases, using only the common-denominator hardware primitives available to all of those disparate CPU architectures) rather than for programmer ergonomics decades later.

The result is an error-handling strategy that successfully solved a serious hardware coordination problem in 1985, but which causes problems today. The way it works is that there are four error cases:
- If the answer of a calculation is too large or too small to represent accurately, then the answer is Infinity or -Infinity
- 1 divided by 0 is NaN

The rules for NaN are that NaN is never equal to anything, including itself. So if you did `(1.F64 / 0.F64) == (1.F64 / 0.F64)`, it would return False...except that instead it will return a type error, because `F32` and `F64` do not support `is_eq`. This maintains the semantics of `==` for everything else, while making floats require more work to compare.

## Fixed-Point Decimal

- performance
- differences vs floating point
- overflow
- div by zero
- tradeoffs compared to floating-point
