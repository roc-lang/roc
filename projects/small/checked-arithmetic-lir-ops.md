# Checked Arithmetic Ops in LIR

## Problem

Roc integer operations trap on overflow. That one sentence of semantics is
currently implemented in THREE different layers, with three different
mechanisms, and the layers do not agree on coverage or crash messages:

1. **LIR lowering** (PR 9818) macro-expands overflow checks for multiply,
   negate, abs, and division: `num_times` becomes a wrapping multiply
   followed by "if rhs != 0 then quotient = result / rhs; crash if
   quotient != lhs" (signed types additionally pre-check lowest × −1). So
   EVERY integer multiplication in generated code carries a compare, a REAL
   DIVISION, and another compare — at every opt level, on every backend.
   The expansion was duplicated near-verbatim (~312 lines each) into BOTH
   LIR lowerers, and the copies already show cosmetic drift. The "is this
   an integer layout" predicate is literally
   `integerDivisionByZeroMessage(layout) == null` — a diagnostic-message
   table reused as a type test.
2. **The dev backend and interpreter** (PR 9761) implement add/sub overflow
   themselves: `emitCheckedIntAddSubOverflow` in
   src/backend/dev/LirCodeGen.zig, ADCS support in the aarch64 emitter, and
   `@addWithOverflow`-based checks in src/eval/interpreter.zig.
3. **The LLVM backend** was untouched by 9761: `num_plus` maps to a plain
   `add` in `emitNumericBinary` (src/backend/llvm/MonoLlvmCodeGen.zig) —
   add/sub overflow there is only "checked" because LIR lowering never
   expands add/sub, i.e. it is not checked at all by that layer.

This split produced a stream of engine-divergence bugs: issues 9360/9361
(dev backend silently wrapped u8 add/sub while the interpreter errored —
differently), and 9783/9812/9813/9814 (mul/negate/abs/div wrapped or hit
UB). Engine divergence is the current bug detector.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck:
Monotype IR → Monotype Lifted → Lambda Solved → Lambda Mono → LIR lowering →
ARC insertion → backends. design.md at the repo root is authoritative.

There are TWO LIR lowerers — src/postcheck/solved_lir_lower.zig (~7.7k
lines, production, lowers Lambda Solved IR directly) and
src/postcheck/lir_lower.zig (~4.2k lines, a parallel Lambda-Mono-based
lowerer used by debug verification/test harnesses). Fixes routinely have to
land twice, and the copies drift.

FIVE executors run Roc code: the interpreter (src/eval/interpreter.zig),
dev backends for x86-64 and aarch64 (src/backend/dev/), the LLVM backend
(src/backend/llvm/MonoLlvmCodeGen.zig), and the wasm backend
(src/backend/wasm/). LIR is the last IR they all share, which makes it the
right place to define semantics once.

LIR op encoding: numeric primitives are members of the shared `LowLevel`
enum (src/base/LowLevel.zig — `num_plus`, `num_minus`, `num_times`,
`num_negate`, `num_abs`, `num_div_by`, `num_div_trunc_by`, ...) carried on
the `assign_low_level` control-flow statement in src/lir/LIR.zig
(`{ target, op, rc_effect, args, next }`).

## Evidence

- PR roc-lang/roc#9818 (fix-integer-overflow-lir):
  `lowerIntegerOverflowCheckedMultiplicationLowLevelInto`,
  `lowLevelNeedsIntegerMultiplicationOverflowCheck` (matches `num_times`),
  `lowLevelNeedsSignedIntegerLowestCheck` (`num_negate`, `num_abs`),
  `signedIntegerLowestValue`, and the div-by-zero / lowest÷−1 expansion —
  all in src/postcheck/solved_lir_lower.zig AND src/postcheck/lir_lower.zig.
  The type test `integerDivisionByZeroMessage(lhs_layout) == null` gates
  the expansion in both.
- PR roc-lang/roc#9761 (fix-dev-checked-int-arithmetic):
  `emitCheckedIntAddSubOverflow` and `emitCheckedI128AddSubOverflow` in
  src/backend/dev/LirCodeGen.zig — u64/i64/i128/u128 via CPU flags
  (`condOverflow`, `condUnsignedAddOverflow`), narrower-than-64-bit ints do
  the op at 64-bit then range-check (`emitUnsignedIntRangeCheck`,
  `emitSignedIntRangeCheck`); `adcsRegRegReg` added to
  src/backend/dev/aarch64/Emit.zig; interpreter `checkedIntAdd` /
  `checkedIntSub` → `triggerCrash` using `@addWithOverflow` in
  src/eval/interpreter.zig.
- LLVM gap: `emitNumericBinary` in src/backend/llvm/MonoLlvmCodeGen.zig
  maps `.num_plus => .add` (plain, no overflow intrinsic).
- Message drift is already visible: the LIR expansion crashes with
  "integer multiplication overflowed" while dev/interpreter add/sub crash
  with "Integer addition overflowed!".
- Issues: roc-lang/roc#9360, #9361, #9783, #9812, #9813, #9814. The same
  engine-divergence bug class extends beyond overflow, e.g. float→int
  conversions (PR roc-lang/roc#9648).

## Solution design

Add checked arithmetic to LIR itself, with crash-on-overflow semantics
DEFINED at the LIR level so all five executors inherit one contract.

1. **Encoding**: add checked members to the `LowLevel` enum, following the
   existing naming scheme: `num_plus_checked`, `num_minus_checked`,
   `num_times_checked`, `num_negate_checked`, `num_abs_checked`, and
   checked division variants covering div-by-zero and lowest÷−1. They ride
   the existing `assign_low_level` statement unchanged. (A
   `traps_on_overflow` flag on `assign_low_level` would also work, but
   every backend already switches on `op`; distinct ops keep those switches
   flat and make unchecked ops impossible to flag incorrectly.) Define the
   canonical crash message for each checked op in ONE table next to the op
   definitions; executors must use it verbatim.
2. **Lowering**: both LIR lowerers emit the single checked op for integer
   layouts (one shared predicate on `layout.Idx`, replacing the
   `integerDivisionByZeroMessage(...) == null` trick) and the plain op
   otherwise. DELETE the macro-expansions from both lowerers:
   `lowerIntegerOverflowCheckedMultiplicationLowLevelInto`, the
   signed-lowest expansion for negate/abs, the div-by-zero/lowest÷−1
   expansion, and the diagnostic-table type test (~312 lines × 2).
3. **Backends** map checked ops to native facilities:
   - LLVM: `@llvm.sadd/uadd/ssub/usub/smul/umul.with.overflow` intrinsics +
     conditional branch to a crash block.
   - Dev x86-64: flags — `jo`/`seto` after add/sub/imul; unsigned mul via
     the carry/overflow flag of `mul`/`mulx` (high-half nonzero check).
   - Dev aarch64: `ADDS`/`SUBS`/`ADCS` + overflow flag; mul via widening
     multiply (`smulh`/`umulh`) high-half check. PR 9761's existing
     machinery (`emitCheckedIntAddSubOverflow` and friends) moves behind
     the checked ops rather than behind `num_plus`.
   - Wasm: explicit compare-based expansion written ONCE, in the wasm
     backend (src/backend/wasm/WasmCodeGen.zig), which already has
     carry-tracking 128-bit routines to build on.
   - Interpreter: `@addWithOverflow` / `@subWithOverflow` /
     `@mulWithOverflow` → `triggerCrash` with the canonical message.
4. **Migration order**: add ops + interpreter support; port LLVM (closing
   the add/sub gap); port dev backends; port wasm; flip both lowerers to
   emit checked ops; delete the expansions; land the conformance suite
   before the flip so old and new paths are compared.

## What success looks like

- No division instruction is emitted anywhere for a multiply overflow
  check, on any backend.
- One place (the LIR op table) defines overflow semantics and crash
  messages; all five executors inherit them.
- Both lowerers emit the same single checked op; the ~624 duplicated
  expansion lines and the diagnostic-table type test are gone.
- LLVM-compiled code traps on add/sub overflow (it currently does not).

## How to evaluate the result

### Correctness ideal

A cross-engine conformance suite: identical numeric programs run on
interpreter / dev-x86-64 / dev-aarch64 / LLVM / wasm, asserting identical
results INCLUDING crash messages, over boundary values per width and op:
min/max, min±1/max±1, 0, lowest × −1, lowest ÷ −1, div by zero — for every
integer width u8..u128/i8..i128. Build the suite to be extended: the
engine-divergence bug class is bigger than overflow (float→int conversions,
PR 9648, belong in the same harness).

### Performance ideal

- Hot numeric loop benchmarks (dev and LLVM) before/after:
  multiplication-heavy code should speed up materially since the division
  disappears from every multiply.
- ReleaseFast assembly spot-check: LLVM emits a mul + `jo`-style pattern
  (or `umulh` check on aarch64), not a `div`.
- No regression for unchecked ops (float math, wrapping intrinsics) — they
  keep their existing single-instruction paths.
- Compile speed: LIR statement counts per function drop (no expansion
  trees), which should slightly improve downstream pass times; verify
  LIR-lowering time does not regress.

## Tests to add

- The cross-engine conformance suite described above (all five executors,
  identical outputs and crash messages).
- Per-op boundary tests for each integer width: add/sub/mul/negate/abs/div
  at min, max, ±1 around them, 0, lowest × −1, lowest ÷ −1, div by zero.
- A codegen test asserting the lowered LIR for an integer multiply contains
  no `num_div_trunc_by` (statement-level assertion on the lowered output).
- A test that both lowerers produce identical LIR for the same numeric
  function (guards against future drift until the pipelines unify).

## Related projects

- [Unify the two build pipelines](../big/unify-build-pipelines.md) — ends
  the land-every-fix-twice tax that doubled this feature's cost.
- [A decision-tree match compiler](../big/decision-tree-match-compiler.md)
  — same philosophy: define behavior once at the LIR boundary instead of
  patching each consumer.
- [A single exact-numeral pipeline](../big/exact-numeral-pipeline.md) — its
  literal-heavy conformance tests should run in this suite's harness.
