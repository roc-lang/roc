# Flat Iterator Representation for Float Ranges

## Status

Resolved as part of the reusable `Num.Range` design.

## Original problem

Range syntax used to construct an `Iter` directly. Integer and `Dec` range
methods were specially recognized as flat iterator producers, while `F32` and
`F64` used `Iter.custom` and retained the boxed public `Iter` representation
under adapters. This created a silent performance cliff based on the range's
element type.

## Resolution

Range syntax now constructs a reusable `Num.Range(num)` value. The single
`Builtin.Num.Range.iter` method is the iterator source recognized by
monomorphization, independent of the element type. It delegates iteration to
the numeric type's `range_iter` method while propagating the source's selected
runtime iterator representation through that call.

Integer, `Dec`, and floating-point implementations all build the iterator with
`Iter.custom` using the same tagged seed shape. Consequently, adapters over
both integer and float ranges retain flat, by-value iterator state rather than
boxing the public recursive `Iter` nominal.

Reverse iteration is intentionally a separate capability. Integer and `Dec`
types provide the `_from` range constructors required by `Range.iter_rev`;
`F32` and `F64` omit them because repeated floating-point addition is not
exactly reversible.

## Verification

The range allocation tests in `src/eval/test/eval_iter_alloc_tests.zig` cover
flat runtime behavior, including reverse iteration. The static iterator tests
in `src/eval/test/lir_inline_test.zig` cover float range fusion and the absence
of boxed iterator operations. Range semantics and third-party `Range.custom`
implementations are covered in `src/eval/test/eval_tests.zig`.
