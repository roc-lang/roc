# Explicit-Facts Cleanup Plan

## Goal

Finish the remaining compiler cleanup in this order:

1. eliminate the remaining monotype re-inference / result-var recovery paths
2. replace the current double layout lowering with one shared layout-commit boundary
3. replace late RC/layout-resolution rebuilding with explicit earlier facts

Every phase must optimize for the long-term architectural target only:

- no workarounds
- no fallback paths
- no heuristics
- no transitional dual systems left behind
- every later stage consumes explicit earlier facts instead of reconstructing them

## Phase 1. Monotype Explicit Facts

### Scope

This phase clears the remaining monotype items from `reinfer.md`:

- `src/monotype/lower.zig:1030`
  `resolveLambdaBodyExpectation(...)`
- `src/monotype/lower.zig:4149`
  `lowerExprWithExpectedType(...)`
- `src/monotype/lower.zig:4800`
  recorded-method result type recovery
- `src/monotype/lower.zig:4803`
  field-access result type recovery
- `src/monotype/lower.zig:4817`
  arithmetic result type recovery
- `src/monotype/lower.zig:6443`
  plain-call result-var recovery
- `src/monotype/lower.zig:6467`
  recorded-method result-var recovery

### Target end state

Monotype must behave the way `cor` does:

- every executable expression entering monotype has an explicit authoritative result fact
- call-like expressions have explicit authoritative result-type and result-var facts
- method syntax is resolved into explicit call facts before monotype needs those facts
- field access uses the access expression's own explicit result fact
- arithmetic consumes explicit builtin/kernel result facts
- named result locals are created from the expression's own explicit result fact, not rediscovered from the callee
- if an expected result fact is missing, that is a compiler bug and must hit a debug invariant

### Work

1. Trace every caller of `lowerExprWithExpectedType(...)` and replace placeholder recovery with explicit expected facts produced earlier.
2. Make lambda body lowering consume one explicit return fact model and delete the fallback to body-expression facts.
3. Make plain calls and recorded-method calls carry one explicit result-fact representation through monotype.
4. Make field access and arithmetic consume explicit result facts from the expression itself rather than reconstructing them locally.
5. Delete the monotype recovery helpers once the explicit-fact path is the only path.
6. Verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
7. Update `reinfer.md`.
8. Commit the monotype milestone.

## Phase 2. Single Layout-Commit Boundary

### Scope

This phase clears the remaining layout re-work from `reintern.md`:

- `src/ir/lower_type.zig`
  `lambdamono.TypeId -> ir.LayoutId`
- `src/lir/FromIr.zig`
  `ir.LayoutId -> layout.Idx`
- `src/ir/lower.zig`
  field index recovery from type + name
- `src/ir/lower.zig`
  tag discriminant recovery from type + tag name
- `src/ir/lower.zig`
  tag payload layout recovery from union type
- `src/ir/lower.zig`
  nominal layout unwrapping to recover physical shape
- `src/ir/lower.zig`
  fresh temp / proc result type-to-layout relowering
- `src/lir/FromIr.zig`
  structural IR-layout equality scan before graph commit

### Target end state

- logical executable facts flow forward once
- final physical layout is committed once at the shared LIR/layout boundary
- IR does not recover field/tag executable metadata from type shape
- IR locals/proc signatures already carry explicit executable layout facts
- `FromIr` does not scan old IR layouts for structural equality

### Work

1. Decide the one explicit executable layout fact representation carried before `FromIr`.
2. Move field index / tag discriminant / payload-layout facts earlier so IR consumes them directly.
3. Remove `ir/lower_type.zig` as a second semantic layout-lowering layer, or reduce it to a dumb projection of already-explicit layout facts.
4. Make `FromIr` commit those explicit logical layout facts once into the shared layout store.
5. Delete the structural equality scan in `FromIr`.
6. Verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
7. Update `reintern.md`.
8. Commit the layout milestone.

## Phase 3. RC / Layout-Resolution Explicit Facts

### Scope

This phase clears the remaining late rebuilding from `reinfer.md`:

- `src/lir/rc_insert.zig`
  fixed-point rebuilding of proc param use kinds / fresh returns / join ownership / alias sources
- `src/layout/type_layout_resolver.zig`
  unresolved-var-driven layout resolution

### Target end state

- RC insertion consumes explicit ownership facts already attached to LIR
- backends follow only explicit `incref` / `decref`
- layout resolution consumes explicit earlier executable layout facts, not unresolved checker vars

### Work

1. Identify the earlier stage that must own ownership/use-kind facts.
2. Move those facts earlier and make `rc_insert` a dumb consumer or delete it if it becomes unnecessary.
3. Move layout-resolution inputs off unresolved checker vars and onto explicit executable layout facts.
4. Verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
5. Update `reinfer.md` and `reintern.md`.
6. Commit the final cleanup milestone.

## Finalization

When all three phases are complete:

1. run the full verification suite again
2. make sure only the intended code changes are committed
3. push the branch
4. report the remaining contents of `reinfer.md` and `reintern.md`

The plan is not complete until that final report reflects the real post-cleanup state.
