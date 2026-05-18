# Problems: Remaining Late Guessing And Recovery

This file tracks places that still look like a later compiler stage is guessing,
searching, or reconstructing information that an earlier stage either knew
directly or should have published explicitly.

The question for each entry is:

> What is the most long-term ideal thing we should do here?

The standard is strict:

- The stage that knows a fact directly should publish that fact.
- Later stages should consume explicit facts, not recreate them.
- Canonical keys are for identity checks and dedupe, not for finding the missing
  payload that should have been carried.
- Layouts describe runtime representation, not source semantics or call identity.
- Backend and interpreter code should reject impossible LIR instead of picking a
  default behavior.

## 1. Static Data Callable-Set Capture Uses Raw Member Index

Location:

- `src/compile/static_data_exports.zig`
- `src/check/canonical_names.zig`

Suspicion:

Static data materialization writes finite callable-set captures by converting
`finite.selected_member` directly with `@intFromEnum`. The compiler now has
central helpers for callable-set runtime encoding, but this path bypasses them.

Risk:

If callable-set member ids and runtime tag-union variant order ever diverge, this
path will materialize the wrong payload layout and write the wrong discriminant.

## 2. Nominal Backing Is Rediscovered By Name And Argument Keys

Location:

- `src/mir/mono/specialize.zig`
- `src/check/checked_artifact.zig`

Suspicion:

Mono materialization finds instantiated nominal backing types by searching the
root artifact, imports, and relation artifacts for a matching nominal name plus
argument keys. The nominal source payload or published nominal capability should
give the exact backing payload instead.

Risk:

The consumer is reconstructing a payload relationship from names and keys. That
creates another path where two independently equivalent keys can hide the fact
that the exact payload identity was lost.

## 3. Record And Tag Source Children Are Recovered By Label Text

Location:

- `src/mir/lambda_solved/solve.zig`
- `src/mir/lambda_solved/representation.zig`
- related const-backed paths in `src/mir/lambda_solved/solve.zig`

Suspicion:

Several source-child lookup paths compare record field labels and tag labels by
text across canonical-name stores to find the checked child payload for a field
or tag payload.

Risk:

The semantic fact wanted by the consumer is "this logical field/tag child maps
to this checked source child." Repeated text matching is weaker than carrying
that correspondence from the projection or import boundary.

## 4. Imported Checked-Type Projection Still Selects Source Roots By Key

Location:

- `src/mir/mono/specialize.zig`
- `src/check/checked_artifact.zig`
- `src/mir/concrete_source_type.zig`

Suspicion:

Some compile-time dependency publication paths have exact artifact payload ids,
but still project imported checked types by searching the imported root table for
a canonical key.

Risk:

Key-based dedupe in the target store is fine. Key-based source selection is the
problem: it asks the compiler to find the payload again even though the exact
source payload id is already available.

## 5. Wasm Numeric Low-Level Lowering Uses Return Layout For Some Signedness

Location:

- `src/backend/wasm/WasmCodeGen.zig`
- comparison point: `src/backend/dev/LirCodeGen.zig`

Suspicion:

The Wasm backend now uses operand layout for comparisons, modulo, and
`abs_diff`, but division and remainder still choose signed vs unsigned behavior
from `ret_layout`.

Risk:

For ordinary division the return layout usually matches the operand layout, so
this may not currently fail. It is still the wrong owner for the information:
signedness belongs to the operands.

## 6. LIR Interpreter Numeric Helpers Silently Default On Unsupported Layouts

Location:

- `src/eval/interpreter.zig`

Suspicion:

Some interpreter numeric helpers return default behavior for unsupported operand
sizes. Examples include `evalCompare` returning equality for unknown sizes and
shift/binop/widen helpers doing nothing for unknown sizes.

Risk:

The interpreter can accept invalid LIR and produce a plausible value instead of
failing loudly. That makes later-stage bugs harder to find and can make the
interpreter disagree with real backends.
