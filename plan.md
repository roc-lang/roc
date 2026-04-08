# Explicit Facts Completion Plan

## Goal

Complete the remaining explicit-facts cleanup in this exact order:

1. finish monotype explicit-fact cleanup
2. finish IR/layout explicit-fact cleanup
3. eliminate unresolved-var-driven layout resolution
4. eliminate late RC/ownership rebuilding residue

Every phase follows the same architectural rule:

- no workarounds
- no fallback paths
- no heuristics
- no transitional dual systems
- every later stage consumes explicit earlier facts instead of reconstructing
  them

## Phase 1. Monotype Explicit Facts

Status: complete.

Completed work:
- lambda bodies consume only the explicit function-return fact
- plain calls and recorded-method calls consume explicit result facts
- arithmetic/binops consume the checker’s explicit binop static-dispatch site
  fact instead of borrowing caller-seeded result vars
- placeholder recovery paths that survived only because of missing expected
  facts have been deleted

## Phase 2. IR/Layout Explicit Facts

Status: complete.

Completed work:
- IR now carries the shared logical layout graph directly
- IR field indices and tag discriminants come from explicit earlier semantic
  facts instead of being recomputed from type shape
- IR uses explicit shared layout facts through `src/ir/layout_facts.zig`
- `FromIr` commits the shared layout graph directly instead of lowering through
  a separate IR layout store

## Phase 3. Explicit Layout Resolution Inputs

Status: complete.

Completed work:
- unresolved-var-driven layout resolution is deleted
- `src/layout/type_layout_resolver.zig` is gone
- the layout boundary now consumes explicit executable layout facts instead of
  walking checker vars and constraints

## Phase 4. Explicit RC Ownership Facts

Status: complete.

Completed work:
- confirmed the fixed-point RC insertion pass was dead code in the active
  pipeline
- confirmed the associated debug ownership re-derivation helpers were also dead
  residue
- deleted:
  - `src/lir/rc_insert.zig`
  - `src/lir/DebugOwnershipSummary.zig`
  - `src/lir/DebugVerifyLir.zig`
- removed the dead exports and test references from `src/lir/mod.zig`

The active compiler now relies only on the explicit provenance/ownership facts
already attached in `FromIr`, and backends continue to follow only explicit
`incref` / `decref` statements when such statements exist.

## Finalization

Before closing this project:

1. rerun:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
2. update the local audits:
   - `reinfer.md`
   - `reintern.md`
3. commit each completed cleanup slice
4. push the branch
5. report the remaining contents of `reinfer.md` and `reintern.md`
