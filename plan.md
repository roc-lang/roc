# Remaining Explicit-Facts Plan

## Goal

Complete the remaining compiler cleanup in this exact order:

1. eliminate the remaining monotype fact recovery
2. eliminate the remaining `lambdamono` semantic-metadata recovery
3. eliminate the remaining `lambdamono` executable-type relowering
4. eliminate specialization-request relowering
5. eliminate the `ir/layout_facts` recovery layer
6. eliminate the leftover `keyId(...)` recanonicalization residue

Every phase must follow the same long-term architectural target:

- no workarounds
- no fallbacks
- no heuristics
- no dual systems
- every later stage consumes one explicit earlier fact instead of recovering,
  reconstructing, or re-lowering it

`~/code/cor` is the guide for the overall approach:

- solving owns semantic type facts
- specialization happens against one explicit function/request fact
- later lowering stages recursively consume already-owned facts
- later stages do not choose between competing facts or rediscover them from
  shape

## Phase 1. Monotype Explicit Semantic Facts

### Current offenders

- `src/monotype/lower.zig`
  - `lowerSolvedType(...)`
  - `lowerExprResultType(...)`
  - `lookupRecordFieldType(...)`
  - `lookupTagArgTypesOwned(...)`
  - `lookupTagArgTypesFromSolvedVarsOwned(...)`

### Target end state

- every monotype expr/pat/binding consumes one authoritative monotype type fact
- record destructuring/access consumes explicit earlier field-type facts
- tag construction/matching consumes explicit earlier payload-type facts
- monotype no longer lowers checker vars to monotype types at arbitrary use
  sites

### Work

1. identify the earliest stage that can own:
   - monotype expr result type facts
   - explicit record field-type facts
   - explicit tag payload-type facts
2. thread those facts into monotype AST/lowering inputs explicitly
3. make monotype lowering consume only those facts
4. delete:
   - `lowerSolvedType(...)`
   - `lowerExprResultType(...)`
   - `lookupRecordFieldType(...)`
   - `lookupTagArgTypesOwned(...)`
   - `lookupTagArgTypesFromSolvedVarsOwned(...)`
5. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
6. update `reinfer.md`
7. commit

## Phase 2. Lambdamono Explicit Semantic Metadata

### Current offenders

- `src/lambdamono/lower.zig`
  - `tagDiscriminantForUnion(...)`
  - `recordFieldIndex(...)`

### Target end state

- `lambdamono` receives explicit field indices and tag discriminants from
  earlier semantic facts
- `lambdamono` does not rediscover semantic ids from lowered executable type
  shape

### Work

1. decide the earlier stage that owns:
   - record field index facts
   - tag discriminant facts
2. attach those facts explicitly before `lambdamono` lowering
3. make `lambdamono` consume them directly
4. delete:
   - `tagDiscriminantForUnion(...)`
   - `recordFieldIndex(...)`
5. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
6. update `reintern.md`
7. commit

## Phase 3. Lambdamono Explicit Executable-Type Facts

### Current offenders

- scattered `lower_type.lowerType(...)` use sites in `src/lambdamono/lower.zig`

### Target end state

- every relevant `lambdamono` expr/pat/binding/capture carries one explicit
  executable type fact
- `lambdamono` does not re-lower solved types at arbitrary use sites

### Work

1. identify the canonical executable-type fact model for:
   - expressions
   - patterns
   - binders
   - captures
2. attach those executable-type facts once
3. convert all scattered `lower_type.lowerType(...)` use sites to consume them
4. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
5. update `reintern.md`
6. commit

## Phase 4. Explicit Specialization Request Facts

### Current offenders

- `src/lambdamono/specializations.zig`
  - per-request arg/ret/capture relowering

### Target end state

- each specialization request already carries one explicit canonical executable
  signature fact
- specialization lookup is direct key lookup, not request-time relowering

### Work

1. define one explicit specialization-request fact carrying the canonical
   executable signature
2. construct that fact before queue lookup
3. make specialization lookup consume only that fact
4. delete per-request arg/ret/capture relowering from specialization lookup
5. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
6. update `reintern.md`
7. commit

## Phase 5. Eliminate `ir/layout_facts` Recovery

### Current offenders

- `src/ir/layout_facts.zig`
  - type -> layout lowering
  - payload/field layout derivation
  - nominal wrapper peeling

### Target end state

- earlier stages own the logical executable layout facts
- IR consumes those explicit layout refs and payload/field layout facts
- `ir/layout_facts.zig` is deleted

### Work

1. decide which earlier stage owns:
   - logical executable layout refs
   - explicit field-layout facts
   - explicit payload-layout facts
2. thread those facts into IR AST/lowering inputs explicitly
3. make IR consume only those facts
4. delete `src/ir/layout_facts.zig`
5. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
6. update `reintern.md`
7. commit

## Phase 6. Eliminate Recanonicalization Residue

### Current offenders

- remaining `keyId(...)` recanonicalization sites in:
  - `src/monotype/lower.zig`
  - `src/monotype/specializations.zig`
  - `src/lambdamono/lower_type.zig`
  - `src/ir/lower_type.zig`

### Target end state

- these ids are canonical by construction
- no hot-path or stage-boundary recanonicalization remains

### Work

1. delete the remaining `keyId(...)` cleanup passes only after the earlier
   phases make the ids canonical by construction
2. verify:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
3. update `reinfer.md` and `reintern.md`
4. commit

## Finalization

When all phases are complete:

1. rerun:
   - `timeout 600s zig build test-eval -- --threads 1`
   - `timeout 600s zig build test-eval-host-effects -- --threads 1`
2. ensure only intended code changes are committed
3. push the branch
4. report the remaining contents of:
   - `reinfer.md`
   - `reintern.md`
