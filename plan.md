# Canonical Publication Plan

## Goal

Complete the remaining cleanup in this exact order:

1. make monotype provisional ids builder-private and publish only canonical ids
2. delete monotype specialization-boundary canonicalization
3. make lambdamono provisional ids builder-private and publish only canonical ids
4. delete lambdamono layout-facts-boundary canonicalization

`~/code/cor` is the guide for the overall shape:

- recursive builder state is private to the builder
- published stage facts are already final
- later stages consume one explicit earlier fact
- stage boundaries never "upgrade" incoming ids

Every phase must follow the same architectural rules:

- no workarounds
- no fallbacks
- no heuristics
- no dual systems
- no late repair of earlier-stage ids

## Phase 1. Monotype Canonical Publication

### Current offenders

- `src/monotype/lower.zig`
  - provisional-to-canonical upgrade inside `TypeCloneScope`
- all monotype AST/type publication sites that still allow provisional ids to
  escape

### Target end state

- active/provisional recursive clone ids remain private to monotype lowering
- anything published out of monotype is already canonical
- no monotype AST node, typed symbol, def bind, or cross-subsystem request
  carries a provisional `TypeId`

### Work

1. identify the publication boundary for monotype facts:
   - AST expr types
   - AST pat types
   - AST typed-symbol types
   - specialized top-level request types
2. add one explicit publication helper that finalizes a monotype type for
   output from the current clone scope
3. make all monotype publication sites consume that helper
4. keep builder-private recursive/provisional state internal to the clone
   cache only
5. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
6. update `reintern.md`
7. commit

## Phase 2. Delete Monotype Boundary Canonicalization

### Current offender

- `src/monotype/specializations.zig`
  - `keyId(...)` at specialization queue lookup

### Target end state

- specialization requests arrive already canonical
- specialization lookup only consumes canonical ids
- any non-canonical incoming id is a compiler bug

### Work

1. replace specialization-boundary `keyId(...)` with a debug-only invariant
2. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
3. update `reintern.md`
4. commit

## Phase 3. Lambdamono Canonical Publication

### Current offenders

- `src/lambdamono/lower_type.zig`
  - provisional-to-canonical upgrade inside `MonoCache`
- all lambdamono AST/signature publication sites that still allow provisional
  executable type ids to escape

### Target end state

- active/provisional executable ids remain private to executable-type lowering
- published lambdamono AST facts and specialization signatures are already
  canonical
- no later lambdamono consumer upgrades incoming executable type ids

### Work

1. identify the publication boundary for lambdamono facts:
   - expr types
   - pat types
   - typed-symbol types
   - def result types
   - specialization signature facts
2. add one explicit publication helper that finalizes executable types for
   output from the current mono cache
3. make all lambdamono publication sites consume that helper
4. keep builder-private recursive/provisional executable ids internal to
   `lower_type`
5. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
6. update `reintern.md`
7. commit

## Phase 4. Delete Lambdamono Boundary Canonicalization

### Current offender

- `src/lambdamono/layout_facts.zig`
  - `keyId(...)` at logical-layout-facts lowering boundary

### Target end state

- logical-layout-facts lowering receives already-canonical executable type ids
- boundary `keyId(...)` is gone
- any non-canonical incoming executable type id is a compiler bug

### Work

1. replace layout-facts-boundary `keyId(...)` with a debug-only invariant
2. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
3. update `reintern.md` and `reinfer.md`
4. commit

## Finalization

When all phases are complete:

1. rerun:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
2. keep `reinfer.md` and `reintern.md` untracked
3. push the branch
4. report what remains on:
   - `reinfer.md`
   - `reintern.md`
