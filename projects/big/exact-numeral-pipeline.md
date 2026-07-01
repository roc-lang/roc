# A Single Exact-Numeral Pipeline

## Problem

A numeric literal's type and bit pattern are decided at least three times,
by machinery that must agree by convention rather than by construction:

(a) **Defaulting** ("what type does `1.5` get when nothing pins it?") is
distributed across at least six coordinating sites:
`finalizeLiteralDefaults`, `defaultLiteralsAtGeneralizationBoundary`,
`runLiteralDefaultingRounds`, `commitLiteralGroupDefault`,
`checkFlexVarConstraintCompatibility`, and `varLiteralKind` in
src/check/Check.zig; `flexLiteralDefaultKind` in
src/check/canonical_type_keys.zig; and `numericDefaultPhaseForConstraints`
in src/check/checked_artifact.zig. The shared tie-break helper
`dominantLiteralKind` in src/types/types.zig carries an in-code warning
that the checker, the canonical-key builder, and the mono default-phase
scan "MUST agree, or mirror-image programs get different keys/diagnostics."

(b) **Checker finalization bakes concrete bits** (Dec/f64/i128) before the
instantiation type is known. The doc comment on `lowerFracLiteral`
(src/postcheck/monotype/lower.zig) states it plainly: "The checked stage
finalizes a fractional literal's value to one numeric representation, but a
generalized literal can be instantiated at a different fractional primitive
than its finalized default."

(c) **Monotype lowering converts bits across representations** to fix (b):
`lowerFracLiteral` selects the constant kind from the instantiated type and
converts via lossy Dec↔F64 (`FracLiteralValue.asDec` calls
`RocDec.fromF64(...) orelse` an invariant panic; `asF64`/`asF32` go through
`Dec.toF64`). `lowerIntLiteral` adapts int values into f32/f64/dec slots
similarly.

Each decision point has produced real bugs (see Evidence), and range/fit
validation now exists in at least three independent implementations — one
of which validates by reconstructing the literal's decimal TEXT from digit
limbs.

## Background

The compiler pipeline: parse → canonicalize → type-check → postcheck:
Monotype IR → Monotype Lifted → Lambda Solved → Lambda Mono → LIR lowering →
ARC insertion → backends. design.md at the repo root is authoritative.
FIVE executors run Roc code (interpreter, dev x86-64/aarch64, LLVM, wasm);
engine divergence has been the de facto bug detector, and wrong literal
bits are exactly the kind of bug it catches late.

How a literal flows today: the parser (src/parse/NumericLiteral.zig)
records exact digit facts — the module env exposes big-endian base-256
digit bytes via `numeralDigitsBefore` / `numeralDigitsAfter`, and the
parser's arithmetic works in base-1e9 limbs. Canonicalization and checking
carry a compact `NumeralInfo` (src/types/types.zig): 16 value bytes
(i128/u128, Dec-scaled if fractional) plus flags (`is_negative`,
`is_fractional`, `fits_dec`, `frac_requirements`). The checker defaults
unresolved literal vars (Dec for fractional, i128-family for ints),
finalizes a concrete representation, and the checked artifact hands
Monotype lowering a value that lowering then re-shapes for the instantiated
primitive (`lowerIntLiteral` / `lowerFracLiteral`).

Custom number types dispatch literals through their own `from_numeral`
method — see design.md, section "Compile-Time Literal Conversions".
`numeralLiteralDecimalText` (src/check/checked_artifact.zig) renders a
recorded numeral back to canonical decimal text so monotype lowering can
fold a monomorphized `from_numeral` call into a constant.

## Evidence

- The "MUST agree" contract: `dominantLiteralKind` in src/types/types.zig,
  naming `varLiteralKind` (checker), `flexLiteralDefaultKind`
  (canonical-key builder), and `numericDefaultPhaseForConstraints` (mono
  default-phase scan). Note: earlier notes placed `flexLiteralDefaultKind`
  in monotype; in the code it lives in src/check/canonical_type_keys.zig,
  and the mono-side scan is `numericDefaultPhaseForConstraints` in
  src/check/checked_artifact.zig.
- Bit conversion at lowering: `lowerIntLiteral`, `lowerFracLiteral`, and
  `FracLiteralValue` (`asDec` → `RocDec.fromF64 orelse` invariant panic) in
  src/postcheck/monotype/lower.zig.
- Issue roc-lang/roc#9691 / PR #9701: a Dec bit pattern stored into an f64
  constant slot; the dev backend multiplied garbage; the interpreter
  coerced on read and masked it.
- Issue roc-lang/roc#9693 / PR #9702: the validator checked a generalized
  flex var against the Dec default → spurious MISSING METHOD; fixed by a
  one-line rank guard.
- Issue roc-lang/roc#9740: a literal in a U64-annotated recursive call arg
  defaulted to Dec because defaulting ran before deferred recursive-def
  constraints; fixed by annotation-peeking seeding — another guard.
- Issues roc-lang/roc#9567 + #9561 / PR #9760: huge exponents (`1E80000`)
  caused a parser panic and a 42-second check from reconstructing an
  80,000-digit decimal string; fixed with exact base-256/base-1e9 digit
  facts and u128 checked arithmetic — proving the parser already records
  exact numeral facts sufficient for every downstream decision.
- Issue roc-lang/roc#9565: literal range facts were discarded once the
  union-find root became a nominal. Range/fit validation now exists in at
  least three independent implementations: `numeralInfoFitsDec`
  (src/check/unify.zig), `numeralLiteralFitsDec` and
  `rangeNumeralDigitsFit` (src/check/Check.zig), and
  `numeralLiteralFitsBuiltin` / `numeralTextFitsBuiltin`
  (src/check/checked_artifact.zig) — the last validating against decimal
  text reconstructed from base-256 limbs (`numeralLiteralDecimalText`,
  `base256DecimalText`).

## Solution design

Carry the exact numeral as THE literal's value payload from parser through
checking; decide the default in one module; produce bits exactly once.

1. **Exact numeral payload**: `{ sign, digits (base-256 or base-1e9
   limbs), decimal_scale }` — the facts the parser already computes (PR
   9760). Replace `NumeralInfo`'s pre-baked 16-byte value as the source of
   truth; small literals may keep an inline fast path (limbs that fit u128)
   but the exact facts are always recoverable without re-reading source
   text. Canonicalization stores this payload; checking never converts it
   to a concrete representation.
2. **One defaulting oracle**: a module owning "given this literal group's
   constraints, what type does it default to?" — the logic now smeared
   across `dominantLiteralKind` + the six sites above. Migration: every
   current site calls the oracle first (behavior-preserving); then the
   sites collapse — `flexLiteralDefaultKind` and
   `numericDefaultPhaseForConstraints` become thin calls or disappear, and
   the "MUST agree" comment is deleted because there is nothing left to
   agree.
3. **Range/fit validation from exact facts**: one function `fits(exact,
   target_type) -> bool` computed on limbs (digit-count prefilter + u128
   checked arithmetic, per 9760). It runs whenever the concrete type
   becomes known — entirely within the checker once
   ../small/check-app-against-platform-requires.md lands, which removes the
   second drop site where platform-implied types arrive after checking.
   DELETE `numeralInfoFitsDec`, `numeralLiteralFitsDec`, and the
   text-reconstruction validation path; `rangeNumeralDigitsFit` becomes a
   call into the shared function.
4. **Bits produced exactly once**, at monotype lowering, for the
   instantiated primitive, from the exact facts: exact → i8..i128/u128 by
   limb assembly + fit check, exact → Dec by scale shift with exact
   rounding rules, exact → f32/f64 by correctly-rounded decimal→binary
   conversion. DELETE the finalized-bits storage in the checked stage and
   the `FracLiteralValue` Dec↔F64 conversion lattice including its
   `RocDec.fromF64 orelse` panic. `from_numeral` folding consumes the exact
   payload directly instead of round-tripping through
   `numeralLiteralDecimalText`.
5. **Ordering interaction**: ../big/total-dispatch-plans.md requires a
   single type-finalization point; defaulting must be complete before
   dispatch plans freeze, so the oracle runs as part of that finalization
   point, not scattered across generalization boundaries.

Unrepresentable literals become check-time errors at step 3; lowering-time
panics for representability are deleted, not relocated.

## What success looks like

- One function decides literal defaults; one function produces literal
  bits; one function answers fit questions.
- The "MUST agree" comment in src/types/types.zig and its three coordinated
  sites are gone.
- `FracLiteralValue`'s conversion lattice and its invariant panic no longer
  exist in src/postcheck/monotype/lower.zig.
- No code path reconstructs decimal text to validate or convert a literal.
- Repros for 9691, 9693, 9740, 9567, 9561, and 9565 all pass.

## How to evaluate the result

### Correctness ideal

- Property tests: for every primitive numeric type and every representable
  exact numeral, literal → bits → value round-trips exactly; for f32/f64
  the produced bits equal correctly-rounded decimal conversion.
- Unrepresentable literals are check-time errors, never lowering panics —
  fuzz literals at type boundaries (max_i128 ± 1, Dec scale edges, f32
  overflow thresholds) at multiple instantiation types.
- Cross-engine agreement on literal-heavy programs (all five executors),
  including `from_numeral` custom types.

### Performance ideal

- Check time bounded on adversarial literals: `1E80000` must stay fast —
  keep PR 9760's regression tests with a time bound.
- No decimal-text reconstruction anywhere in the pipeline (grep-able:
  `numeralLiteralDecimalText` and `base256DecimalText` callers limited to
  error reporting, or deleted).
- Defaulting oracle O(1) per literal group — no per-literal re-scans at
  every generalization boundary; measure check time on modules with tens of
  thousands of literals.
- Generated code unchanged or better: constants are already folded; verify
  no new lowering-time cost from limb conversion (it replaces conversion
  work, not adds to it).

## Tests to add

- Repro suite: 9691 (Dec bits in f64 slot), 9693 (generalized flex var vs
  Dec default), 9740 (U64-annotated recursive call arg), 9567/9561 (huge
  exponents), 9565 (range facts through nominal roots).
- Property round-trip tests as described above, per primitive type.
- The 9760 performance tests with explicit time bounds.
- Cross-engine literal conformance programs, run in the harness from
  ../small/checked-arithmetic-lir-ops.md.
- A test that mirror-image programs (same literals, swapped branch order)
  produce identical types, keys, and diagnostics — the failure mode the
  "MUST agree" comment warned about.

## Related projects

- [Check app against platform requires](../small/check-app-against-platform-requires.md)
  — removes the post-checking type-arrival path this design depends on.
- [Total dispatch plans](../big/total-dispatch-plans.md) — defines the
  single type-finalization point the defaulting oracle plugs into.
- [Checked arithmetic ops in LIR](../small/checked-arithmetic-lir-ops.md)
  — its cross-engine conformance suite hosts the literal tests.
