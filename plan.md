# Exact Facts Final Cleanup Plan

## Goal

Finish the remaining active re-inference and re-work in strict
upstream-to-downstream order:

1. publish checker-adjacent semantic facts for monotype consumption
2. move logical layout fact ownership into `lambdamono` construction
3. make IR consume only explicit upstream semantic and layout facts
4. remove all downstream runtime-shape comparison escape hatches

`~/code/cor` remains the architectural guide:

- solve first
- attach one authoritative fact set once
- specialize whole function facts, not local fragments
- later stages only consume explicit earlier facts
- no fallbacks, no heuristics, no dual systems, no late repair

## Phase 1. Publish Checker-Adjacent Semantic Facts

### Remaining offenders in monotype today

- `src/monotype/lower.zig`
  - `lowerInstantiatedType(...)`

### Root problem

Monotype still owns one remaining category of fact that it should not own:

- source-root monotype type materialization still happens on demand

The completed tag/record structural-facts slice confirmed the correct
boundary: structural meaning has to come from explicit solved facts, not
lowered monotype type shape.

The remaining issue is narrower now: monotype still clones/lower-instantiates
checker vars on demand instead of consuming one published monotype fact set.

The latest failed experiment clarified one important boundary:

- explicit function/call vars can be published inside the current mutable
  monotype specialization flow
- explicit monotype types cannot

Publishing typed function facts inside `TypeCloneScope` is still too early,
because later specialization unifications can refine those vars after the fact
and make the cached types stale. So the remaining type publication work must
move earlier, to a stage where those facts are already stable, rather than
freezing them inside monotype’s current mutable cloning/unification loop.

That violates the intended `cor`-style contract:

- solve first
- publish semantic facts once
- lower later from those facts only

### Target end state

Introduce a dedicated semantic-facts artifact produced immediately after
checking and before monotype lowering. That artifact owns:

- source expr result vars
- source expr solved types
- source pattern solved types
- source call-root / call-application facts
- source tag discriminants
- source record field indices
- any remaining source-root type facts monotype still needs

Monotype may still build monomorphic types internally, but it must only consume
published source facts and builder-private finalized monotype facts. It may not
materialize fresh semantic meaning on demand while lowering expressions.

After that:

- the remaining late semantic uses of `lowerInstantiatedType(...)` are either
  gone or clearly builder-internal and excluded from the audit

### Work

1. introduce a checker-adjacent semantic-facts module and thread it into
   monotype entrypoints
2. move the remaining source-root type materialization out of expression
   lowering and into that artifact, using `cor`’s “solve first, publish once,
   lower later” approach
3. migrate monotype fact collection / lowering to consume those published
   semantic facts only
   - do not reintroduce typed function/call facts inside the current mutable
     `TypeCloneScope`; that boundary is not sound yet
4. re-audit the remaining `lowerInstantiatedType(...)` uses and either:
   - remove them from lowering logic
   - or classify them as builder-internal-only and exclude them explicitly
5. update:
   - `reinfer.md`
   - `reintern.md`
6. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 300s zig build test-eval-host-effects -- --threads 1`
7. commit

## Phase 2. Lambdamono Owns Logical Layout Facts

### Remaining offender

- `src/lambdamono/layout_facts.zig`

### Root problem

`lambdamono` still publishes executable types and then runs a separate logical
re-layout layer that derives:

- logical layout refs
- union payload layouts
- struct field layouts
- nominal-unwrapped union/struct physical shape

That is still late re-layouting.

### Target end state

`lambdamono` construction publishes logical layout facts directly:

- expr logical layout ref
- pattern logical layout ref
- typed-symbol logical layout ref
- def-return logical layout ref
- explicit field layout facts where needed
- explicit tag payload layout facts where needed
- explicit discriminant handling facts where needed

After that, `layout_facts.zig` is either trivial storage or gone.

### Work

1. identify every fact currently derived in `layout_facts.zig`
2. attach those facts while building `lambdamono` results
3. migrate all `lambdamono` and IR consumers to read direct facts
4. remove the recovery logic from `layout_facts.zig`
5. update:
   - `reintern.md`
6. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 300s zig build test-eval-host-effects -- --threads 1`
7. commit

## Phase 3. IR Pure Consumption

### Remaining offenders

- `src/ir/lower.zig`
  - `requireListElemType(...)`
  - `discriminantLayoutForUnion(...)`

### Root problem

IR still locally inspects type/layout shape for list/discriminant handling
instead of consuming exact upstream facts.

### Target end state

IR receives all required semantic/layout facts explicitly from `lambdamono`,
including:

- loop/list element layout behavior
- discriminant layout behavior
- bool direct-value vs tagged-union discriminant behavior

After that, IR lowering is pure consumption.

### Work

1. add any still-missing `lambdamono -> IR` facts
2. replace the remaining IR local derivation paths with direct fact reads
3. delete the remaining IR derivation helpers
4. update:
   - `reintern.md`
5. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 300s zig build test-eval-host-effects -- --threads 1`
6. commit

## Phase 4. Exact Layout Or Explicit Bridge Only

### Remaining offenders

- `src/lir/FromIr.zig`
- `src/eval/interpreter.zig`
- `src/backend/dev/LirCodeGen.zig`
- `src/layout/store.zig`
  - `layoutsHaveSameRuntimeRepresentation(...)`

### Root problem

Downstream code still sometimes accepts layout mismatch by structural runtime
shape comparison instead of requiring:

- exact layout identity
- or explicit earlier bridge facts

That is downstream compensation for earlier ambiguity, and it is forbidden by
the compiler rules.

### Target end state

Downstream code consumes only:

- exact layouts
- explicit earlier bridge/coercion facts

No backend, interpreter path, or `FromIr` bridge selection may ask whether two
layouts merely “have the same runtime representation.”

### Work

1. enumerate every live bridge/coercion case currently guarded by
   `layoutsHaveSameRuntimeRepresentation(...)`
2. move those decisions earlier into explicit bridge facts
3. make `FromIr` consume only exact layouts or explicit bridge facts
4. make the interpreter and dev backend consume only exact layouts or explicit
   bridge facts
5. delete:
   - `layoutsHaveSameRuntimeRepresentation(...)`
   - all downstream runtime-shape comparison escape hatches
6. update:
   - `reintern.md`
7. verify:
   - `timeout 900s zig build test-eval -- --threads 1`
   - `timeout 300s zig build test-eval-host-effects -- --threads 1`
8. commit

## Final Sweep

1. run a fresh from-scratch audit of remaining re-inference / re-work
2. update:
   - `reinfer.md`
   - `reintern.md`
3. verify both eval suites again
4. commit the final audit-synchronized code slices
5. push
6. report exactly what remains, if anything
