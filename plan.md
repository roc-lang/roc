# Explicit Facts Completion Plan

## Goal

Finish the remaining compiler cleanup in this exact order:

1. eliminate the remaining monotype fact fallbacks
2. eliminate the remaining IR/layout fact re-work
3. eliminate unresolved-var-driven layout resolution
4. eliminate late RC/ownership rebuilding

Every phase must optimize only for the long-term architectural target:

- no workarounds
- no fallback paths
- no heuristics
- no transitional dual systems left behind
- every later stage consumes explicit earlier facts instead of reconstructing them

## Phase 1. Monotype Explicit Facts

### Remaining offenders

- `src/monotype/lower.zig`
  - `lowerExprWithExpectedType(...)` placeholder fallback
  - arithmetic expected-result seeding

### Target end state

- every executable expression entering monotype has one authoritative result fact
- lambda bodies consume one authoritative function-return fact
- arithmetic/binops lower from one explicit earlier-produced solved arithmetic fact,
  the `cor` way, not from caller-seeded recovery
- missing expected facts are compiler bugs and hit debug invariants

### Work

1. thread one authoritative solved arithmetic/binop fact into monotype before any
   binop lowering happens, and delete the old caller-seeding path
2. trace every remaining `lowerExprWithExpectedType(...)` caller that can still
   hit a placeholder target and make the earlier caller own that fact explicitly
3. delete the placeholder fallback only after those explicit expected facts exist
   everywhere it was previously reachable
4. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
5. update `reinfer.md`
6. commit

## Phase 2. IR/Layout Explicit Facts

### Remaining offenders

- `src/ir/lower_type.zig`
  - on-demand `lambdamono.TypeId -> layout.GraphRef`
- `src/ir/lower.zig`
  - tag payload layout recovery
  - nominal layout unwrapping to recover physical shape
  - fresh temp layout lowering from type
  - proc/result layout lowering from type

### Target end state

- IR consumes explicit logical executable layout facts
- tag payload layout and nominal physical-shape facts are attached before IR needs them
- temps and proc signatures already carry explicit logical layout refs
- IR no longer lowers executable types into layout refs at individual use sites

### Work

1. decide the explicit logical-layout facts that `lambdamono` must attach for IR
2. attach payload-layout / struct-field-layout / union-layout facts before IR lowering
3. move temp/proc layout ownership out of ad hoc `lower_type.lowerType(...)` calls and onto explicit carried facts
4. reduce `src/ir/lower_type.zig` to a dumb adapter or delete it if the earlier facts make it unnecessary
5. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
6. update `reintern.md`
7. commit

## Phase 3. Explicit Layout Resolution Inputs

### Remaining offenders

- `src/layout/type_layout_resolver.zig`
  - `Resolver.resolve`
  - `buildRefForVar`

### Target end state

- layout resolution consumes explicit executable layout facts, not unresolved checker vars
- no late walk over flex/rigid constraints to decide runtime layout

### Work

1. identify the earlier stage that must own the executable-layout facts currently reconstructed from checker vars
2. thread those facts into the layout boundary explicitly
3. delete unresolved-var-driven resolution
4. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
5. update `reinfer.md` and `reintern.md`
6. commit

## Phase 4. Explicit RC Ownership Facts

### Remaining offenders

- `src/lir/rc_insert.zig`
  - `rebuildProcParamUseKinds`
  - `normalizeFreshProcReturns`
  - `inferOwningJoinParams`
  - `inferJoinParamAliasSources`

### Target end state

- RC insertion consumes explicit ownership and alias facts already attached to LIR
- backends follow only explicit `incref` / `decref`
- no fixed-point ownership rebuilding remains in LIR

### Work

1. decide which earlier stage owns proc param use kinds, fresh-return ownership, join ownership, and join alias-source facts
2. attach those facts explicitly before RC insertion
3. reduce `rc_insert` to a dumb consumer or delete it if it becomes unnecessary
4. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
5. update `reinfer.md`
6. commit

## Finalization

When all four phases are complete:

1. run the verification suite again
2. make sure only the intended code changes are committed
3. push the branch
4. report the remaining contents of `reinfer.md` and `reintern.md`

The plan is not complete until that final report reflects the real post-cleanup state.
