# Flat Iterator Representation for Float Ranges

## Problem

Integer and `Dec` ranges get the bounded by-value iterator
representation: `isFlatRangeMethodText`
(`src/postcheck/monotype/lower.zig`) recognizes
`Builtin.Num.{U8..I128,Dec}.range_exclusive/.range_inclusive`, mints
`generatedIteratorType(.range_exclusive/.range_inclusive, …)` at the
method call, and the flat self-recursive construction in
`Builtin.roc` (`Iter.exclusive_range`/`inclusive_range` over
`iter_from_step` + `range_done()`) closes at the minted type. Wrapped
in adapters, these chains lower with zero heap allocations — the
property the `iter alloc` gates in
`src/eval/test/eval_iter_alloc_tests.zig` and the `iter alloc static`
gates in `src/eval/test/lir_inline_test.zig` enforce.

`F32` and `F64` ranges are deliberately excluded from that list. Their
`range_exclusive`/`range_inclusive` methods build seed-state stepping
chains on `Iter.custom` (fractional steps cannot reuse the integer
add-and-compare recursion), which is a different minted kind: including
the float methods in `isFlatRangeMethodText` tripped "generated
iterator did not contain the expected component count" on the F32 REPL
snapshots. So a float range that an adapter wraps keeps the recursive
public `Iter` nominal, whose layout cycle is boxed: `Iter.map(f, 0.0.F64..<10.0)`
heap-boxes its inner iterator state per step while the same chain over
`U64` is flat.

This is correct but quietly slower, and it is a representation cliff
keyed on the element type — exactly the kind of silent perf divergence
the zero-alloc gates exist to catch, except no gate covers floats.

## Background

The iterator machinery mints per-chain nominal representations for
finite chains and reserves the dynamic (boxed) representation for
recursive or over-cap chains. Two minting shapes exist today:

- Component-carrying mints (`.single`, with `components=[item_ty]`)
  keep their state by value under adapters.
- The range mints fire at the numeric method call because the method
  body's flat recursion closes at the minted type
  (`src/postcheck/monotype/lower.zig`, the
  `isBuiltinExclusiveRangeText`/`isBuiltinInclusiveRangeText`
  recognition and the mint at the `expected == null` producer).

`Iter.custom` chains never mint: `Iter.custom` is reached with the
public `Iter` as its expected return type (imposed by the caller's
declared signature), and the `expected == null` condition is the mint
gate. Float ranges are just the in-tree case of the general gap: any
`Iter.custom`-built source (including `List.iter` shapes routed through
custom stepping) stays on the public nominal under adapters.

## Evidence

- `src/postcheck/monotype/lower.zig:9674`: `isFlatRangeMethodText`
  lists the eleven integer/`Dec` types; the doc comment above it states
  the F32/F64 exclusion and why (their range methods build an
  `Iter.custom` chain carrying seed state).
- `src/build/roc/Builtin.roc`: the float `range_exclusive`/
  `range_inclusive` methods construct via `Iter.custom`; the integer
  methods route through `range_exclusive_with_len`/
  `range_inclusive_with_len` onto the flat shape.
- Measured while fixing the integer regression: `Iter.map` over a
  custom-built source lowers with 3-4 `box_box` of iterator state
  (`assignNominalBoundary`, `src/postcheck/solved_lir_lower.zig`),
  and runtime allocations scale with elements × chain depth.

## Solution design

Make `Iter.custom` itself mintable when its call site is a
producer the machinery can close: accept a producer-imposed expected
type through the float method chain (method body → `with_len` helper →
`Iter.custom`), so the custom seed/step state becomes a component of
the minted nominal instead of a boxed public `Iter`. The seed type is
the natural component (mirroring how `.single` carries `item_ty`); the
step closure joins the minted callable set the same way the integer
ranges' step closures do after the de-freeze of non-evidence slots in
evidence-bearing call instantiation.

The alternative — porting float ranges onto the integer-style flat
recursion — does not generalize: fractional stepping needs seed state,
and the `Iter.custom` gap would remain for every other custom source.
Minting custom sources fixes the class; float ranges are its
acceptance case.

## What success looks like

- `Iter.map(0.0.F64..<10.0, f)` folds with zero heap allocations on
  all backends, matching the `U64` chain.
- `isFlatRangeMethodText` disappears or stops being type-keyed: the
  float methods no longer need exclusion because the mint decision is
  made where the producer shape is known, not by symbol name list.
- The F32 REPL snapshots that previously tripped the component-count
  invariant pass unchanged or with faithfully regenerated output.

## How to evaluate the result

Extend the `iter alloc` eval gates and the `iter alloc static`
lir-inline gates with `F64` variants of the existing range chains
(map, keep_if, concat, deep chain). They must assert zero allocations
and `box_box_count == 0`, alongside the existing integer gates staying
green (`zig build run-test-eval`, `zig build run-test-zig-lir-inline`).
Range correctness must hold across the float boundary cases the
existing `range` eval tests cover.

## Tests to add

- `eval_iter_alloc_tests.zig`: `F64` range fold, map fold, and one
  deep chain, each asserting zero allocations.
- `lir_inline_test.zig`: one `iter alloc static` gate over an escaping
  `F64` range chain.
- A correctness test for a fractional-step custom iterator wrapped in
  an adapter, guarding the minted-custom path beyond ranges.

## Related projects

- `projects/small/spec-constr-specialization-limits.md` — bounds on
  the specialization machinery the minted chains feed.
