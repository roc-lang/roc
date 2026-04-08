# Exact Facts Cleanup Plan

## Goal

Finish the remaining "re-work" in strict upstream-to-downstream order:

1. make monotype publish one explicit semantic fact table per lowered scope
2. delete all remaining monotype late fact / shape recovery
3. make `lambdamono` publish explicit logical layout facts directly
4. delete the dedicated `lambdamono/layout_facts.zig` re-layout layer
5. make IR consume only explicit upstream semantic/layout facts
6. delete runtime-shape comparison escape hatches from `FromIr`, the
   interpreter, and the dev backend

`~/code/cor` is the guide for the overall architecture:

- solved expressions carry one authoritative fact
- monotype clones/lowers those facts once
- later stages consume explicit earlier facts
- stage boundaries publish exact facts, not approximate shape-equivalence

Compiler-wide rules for every phase:

- no workarounds
- no fallbacks
- no heuristics
- no dual systems
- no late repair of earlier-stage ids or shapes
- no downstream structural layout comparison to compensate for upstream
  ambiguity

## Phase 1. Monotype Explicit Semantic Fact Table

### Current offenders

- `src/monotype/lower.zig`
  - `lookupSourceFunctionSeedVar(...)`
  - `lookupCurriedFunctionArgVar(...)`
  - `lookupCurriedFunctionFinalRetVar(...)`
  - `lookupFunctionRetVar(...)`
  - `unifyAppliedFunctionResultVar(...)`
  - `functionArgCount(...)`
  - `lowerInstantiatedType(...)` late use sites
  - `materializePatternSourceType(...)`

### Target end state

- monotype owns one explicit fact table per lowered scope / body
- each expr has:
  - explicit result var
  - explicit monotype type
- each pattern has:
  - explicit solved var
  - explicit monotype type
  - explicit source type
- each call-like expression has:
  - explicit callee fact
  - explicit arg facts
  - explicit result fact
  - explicit curried application shape when relevant
- lowering only consumes those facts

### Work

1. define the explicit monotype fact tables needed for:
   - expr result vars
   - expr monotype types
   - pattern solved vars
   - pattern monotype types
   - pattern source types
   - call-shape facts
2. build those facts once from solved checker facts before recursive lowering
   consumes them
3. replace all remaining function-shape / seed-var recovery with direct fact
   lookup
4. replace late checker-var -> monotype lowering use sites with direct
   monotype fact lookup
5. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
6. update `reinfer.md`
7. commit

## Phase 2. Delete Remaining Monotype Shape Recovery

### Current offenders

- `src/monotype/lower.zig`
  - `lookupTupleElemTypesOwned(...)`
  - `lookupTupleElemType(...)`
  - `requireTagDiscriminantForType(...)`
  - `requireRecordFieldIndexForType(...)`
  - `requireListElemType(...)`
  - `callResultType(...)`

### Target end state

- monotype never asks a lowered type to rediscover:
  - tuple element types
  - list element type
  - tag discriminant
  - record field index
  - intermediate curried call result type
- all of those facts arrive through the explicit monotype fact table

### Work

1. add structural metadata to the monotype fact tables:
   - tuple element facts
   - list element facts
   - tag discriminants
   - record field indices
   - intermediate call result facts
2. delete the corresponding type-shape recovery helpers
3. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
4. update `reinfer.md` and `reintern.md`
5. commit

## Phase 3. Lambdamono Explicit Logical Layout Facts

### Current offender

- `src/lambdamono/layout_facts.zig`

### Target end state

- `lambdamono` publishes logical layout facts directly when constructing its
  result
- exprs / patterns / typed symbols / def returns already carry:
  - logical layout ref
  - field layout when needed
  - tag payload layout when needed
  - discriminant behavior when needed
- later stages never derive those by walking executable type shape or peeling
  nominals

### Work

1. identify every logical layout fact currently recovered in
   `layout_facts.zig`
2. attach those facts at `lambdamono` construction time
3. migrate `lambdamono` and IR consumers to direct fact access
4. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
5. update `reintern.md`
6. commit

## Phase 4. Delete `lambdamono/layout_facts.zig`

### Target end state

- no dedicated re-layout layer exists between `lambdamono` and IR
- logical layout facts are part of the published `lambdamono` result
- IR no longer asks `layout_facts` to lower or derive anything

### Work

1. remove `src/lambdamono/layout_facts.zig`
2. remove its storage and plumbing from the `lambdamono` result
3. update IR inputs to consume direct `lambdamono` layout facts
4. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
5. update `reintern.md`
6. commit

## Phase 5. IR Pure Consumption

### Current offenders

- `src/ir/lower.zig`
  - list element type / layout derivation
  - discriminant layout derivation
  - bool-vs-tag-union local shape checks in `lowerWhenExpr(...)`

### Target end state

- IR lowers only from explicit upstream semantic/layout facts
- IR does not inspect type shape to rediscover loop or `when` facts

### Work

1. add any still-missing `lambdamono -> IR` facts for:
   - loop element layout
   - discriminant layout behavior
   - bool direct-value vs tag-union discriminant handling
2. delete the remaining IR type-shape inspection helpers
3. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
4. update `reintern.md`
5. commit

## Phase 6. Exact Layout / Explicit Bridge Consumption

### Current offenders

- `src/lir/FromIr.zig`
- `src/eval/interpreter.zig`
- `src/backend/dev/LirCodeGen.zig`
- `src/layout/store.zig` `layoutsHaveSameRuntimeRepresentation(...)`

### Target end state

- downstream code accepts:
  - exact layout identity
  - or explicit earlier bridge facts
- downstream code never accepts "same runtime representation" as a substitute
  for exact earlier facts

### Work

1. identify every remaining bridge/coercion case currently decided by
   `layoutsHaveSameRuntimeRepresentation(...)`
2. move those decisions earlier into explicit bridge facts
3. delete the runtime-shape comparison branches from:
   - `src/lir/FromIr.zig`
   - `src/eval/interpreter.zig`
   - `src/backend/dev/LirCodeGen.zig`
4. keep only explicit exact-layout / explicit-bridge behavior
5. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
6. update `reintern.md`
7. commit

## Finalization

When all phases are complete:

1. rerun:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
2. do a fresh full audit of remaining live entries in:
   - `reinfer.md`
   - `reintern.md`
3. ensure both audit notes reflect the final state
4. commit the final code/audit-aligned cleanup slice
5. push
6. report exactly what remains on `reinfer.md` and `reintern.md`
