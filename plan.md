# Explicit Fact And Exact Layout Plan

## Goal

Complete the remaining cleanup in this exact order:

1. make monotype publish one explicit semantic fact set per lowered body
2. delete the remaining monotype late fact recovery helpers
3. make `lambdamono` publish explicit logical layout facts directly
4. delete the dedicated `lambdamono/layout_facts.zig` recovery layer
5. make IR consume only explicit upstream semantic/layout facts
6. delete runtime-shape comparison escape hatches from `FromIr`, the
   interpreter, and the dev backend

`~/code/cor` is the guide for the overall shape:

- solved expressions carry one authoritative fact
- monotype clones/lowers those facts once
- later stages consume explicit earlier facts
- stage boundaries publish exact facts, not approximate shape-equivalence

Every phase must follow the same architectural rules:

- no workarounds
- no fallbacks
- no heuristics
- no dual systems
- no late repair of earlier-stage ids or shapes
- no downstream shape comparison to compensate for upstream ambiguity

## Phase 1. Monotype Explicit Semantic Facts

### Current offenders

- `src/monotype/lower.zig`
  - source-seed / function-shape recovery
  - on-demand `lowerInstantiatedType(...)`
  - pattern source-type materialization
  - intermediate curried-call result reconstruction

### Target end state

- one explicit monotype fact table exists per lowered body / lowering scope
- each lowered expr has:
  - explicit result var
  - explicit monotype type
- each lowered pattern has:
  - explicit solved var
  - explicit monotype type
  - explicit source type
- each call-like thing has:
  - explicit callee fact
  - explicit arg facts
  - explicit result fact
- monotype lowering only consumes those facts

### Work

1. define the explicit monotype fact tables needed by lowering:
   - expr result vars
   - expr monotype types
   - pattern solved vars
   - pattern monotype types
   - pattern source types
   - call-shape facts
2. build those facts once from solved checker facts before recursive lowering
   consumes them
3. replace late source-seed / function-shape recovery with direct fact lookup
4. replace intermediate curried-call result reconstruction with explicit call
   result facts
5. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
6. update `reinfer.md`
7. commit

## Phase 2. Delete Remaining Monotype Shape Recovery

### Current offenders

- `src/monotype/lower.zig`
  - tuple element type recovery from type shape
  - list element type recovery from type shape
  - tag discriminant recovery from type shape
  - record field-index recovery from type shape
  - function arg/ret recovery from function-var structure

### Target end state

- monotype never asks a type or a function var to rediscover:
  - tuple element types
  - list element type
  - tag discriminant
  - record field index
  - curried function arg/ret facts
- all of those arrive as explicit monotype facts

### Work

1. add structural metadata to the explicit monotype fact tables:
   - tuple elem facts
   - list elem facts
   - tag discriminants
   - record field indices
   - function arg/ret facts
2. delete the corresponding recovery helpers
3. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
4. update `reinfer.md` and `reintern.md`
5. commit

## Phase 3. Lambdamono Explicit Logical Layout Facts

### Current offender

- `src/lambdamono/layout_facts.zig`

### Target end state

- `lambdamono` result objects already carry explicit logical layout refs
- exprs/pats/typed symbols/def returns already carry:
  - logical layout ref
  - field layout when needed
  - tag payload layout when needed
- later stages never derive those by walking type shape or peeling nominals

### Work

1. identify every logical layout fact currently derived by `layout_facts.zig`
2. attach those facts where `lambdamono` constructs the AST/result
3. make later `lambdamono` consumers read those attached facts directly
4. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
5. update `reintern.md`
6. commit

## Phase 4. Delete `lambdamono/layout_facts.zig`

### Target end state

- no dedicated recovery layer exists between `lambdamono` and IR
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
  - `requireListElemType(...)`
  - `discriminantLayoutForUnion(...)`
  - bool-vs-tag-union branch shape checks inside `lowerWhenExpr(...)`

### Target end state

- IR lowers only from explicit semantic/layout facts attached upstream
- IR does not inspect `lambdamono` type shape to rediscover loop or `when`
  facts

### Work

1. add any still-missing explicit `lambdamono -> IR` facts for:
   - loop element type/layout
   - match/when discriminant layout behavior
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

### Target end state

- downstream code accepts:
  - exact layout identity
  - or explicit earlier bridge facts
- downstream code never accepts "same runtime representation" as a substitute
  for exact earlier facts

### Work

1. identify every remaining bridge/coercion case currently decided by
   `layoutsHaveSameRuntimeRepresentation(...)`
2. move those decisions earlier into explicit facts
3. delete the `layoutsHaveSameRuntimeRepresentation(...)` branches from:
   - `src/lir/FromIr.zig`
   - `src/eval/interpreter.zig`
   - `src/backend/dev/LirCodeGen.zig`
4. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
5. update `reintern.md`
6. commit

## Finalization

When all phases are complete:

1. rerun:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 900s zig build test-eval-host-effects -- --threads 1`
2. keep `reinfer.md` and `reintern.md` untracked
3. push the branch
4. report what remains on `reinfer.md` and `reintern.md`
