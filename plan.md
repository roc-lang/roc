# Monotype Settle/Freeze Split Plan

## Goal

Split monotype into the same two phases `~/code/cor` effectively already has:

1. settle specialization facts
2. lower from settled facts only

The remaining active monotype offender is still:

- [src/monotype/lower.zig](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig)
  - `lowerInstantiatedType(...)`

The recent failed experiment established the key architectural boundary:

- explicit vars can be published while monotype specialization is still mutable
- explicit monotype types cannot

So the remaining work is not “cache more types in `TypeCloneScope`.” The
remaining work is to move type publication to a later, frozen boundary and make
ordinary lowering consume only frozen facts.

## Cor Guidance

`~/code/cor` remains the model:

- clone / specialize first
- lower types from the specialized clone only once
- attach those lowered types to the lowered expression/pattern tree
- later lowering consumes them directly
- no fallback, no heuristic, no recovery path

The Zig equivalent should be:

- settle one specialization scope completely
- freeze one immutable specialization fact table
- lower from that table only

## Phase 1. Isolate Monotype Semantic Facts From Mutable Caches

### Problem

`TypeCloneScope` currently mixes:

- mutable specialization state
- type caches
- explicit semantic facts used by later lowering

That makes it too easy to publish facts before the specialization is actually
stable.

### Target

Introduce a dedicated monotype semantic-facts artifact for one specialization
scope. At minimum it must own:

- expr result vars
- expr monotype types
- pattern monotype types
- pattern source types
- explicit call facts
- source-function facts
- field/tag/list/pattern structural facts
- arithmetic facts

And it must be separable from:

- `var_map`
- `Instantiator`
- active/provisional type caches
- nominal cache scratch

### Work

1. define a dedicated semantic-facts type/module for monotype
2. move the current fact maps out of the mutable cache cluster and behind that
   dedicated type
3. keep behavior unchanged while the data ownership boundary is established
4. verify eval suites
5. commit

## Phase 2. Add An Explicit Settle-Then-Freeze Boundary

### Problem

Monotype still interleaves:

- collecting facts
- mutating cloned checker vars
- lowering AST nodes

That is what keeps `lowerInstantiatedType(...)` live in ordinary lowering.

### Target

For each specialization scope (top-level value, top-level specialization,
lambda/closure body, local function specialization):

1. run a settling pass that performs all specialization/unification work
2. freeze one immutable semantic-facts snapshot from the settled scope
3. lower the AST using only that frozen snapshot

The freeze step is the first place typed semantic facts may become
authoritative.

### Work

1. identify every current entrypoint that already has an implicit
   collect-then-lower shape
2. extract an explicit settle phase for those entrypoints
3. freeze immutable semantic facts after settling and before lowering
4. thread frozen facts into the matching lowering entrypoints
5. verify eval suites
6. commit

## Phase 3. Remove Lowering-Time `lowerInstantiatedType(...)`

### Problem

After Phase 2, any remaining lowering-time `lowerInstantiatedType(...)` use in
ordinary monotype lowering means the frozen fact table is still incomplete.

### Target

In ordinary monotype lowering:

- no expression lowering path calls `lowerInstantiatedType(...)`
- no pattern lowering path calls `lowerInstantiatedType(...)`
- no call / lambda / match / block lowering path materializes fresh semantic
  type meaning on demand

Allowed remaining uses:

- builder-internal monotype type construction
- freeze-time conversion from settled checker vars to immutable monotype types

### Work

1. replace remaining ordinary lowering-time uses with frozen fact reads
2. delete any now-dead fact recovery helpers
3. re-audit `lowerInstantiatedType(...)`
4. update:
   - [reinfer.md](/Users/rtfeldman/.codex/worktrees/1d55/roc/reinfer.md)
   - [reintern.md](/Users/rtfeldman/.codex/worktrees/1d55/roc/reintern.md)
5. verify eval suites
6. commit

## Phase 4. Resume Downstream Cleanup

Only after monotype is split correctly:

1. move logical layout fact ownership into `lambdamono` construction
2. make IR consume only explicit upstream semantic/layout facts
3. remove all downstream runtime-shape comparison escape hatches

Those remaining items stay tracked in:

- [reintern.md](/Users/rtfeldman/.codex/worktrees/1d55/roc/reintern.md)

## Final Sweep

1. run a fresh from-scratch audit
2. update:
   - [reinfer.md](/Users/rtfeldman/.codex/worktrees/1d55/roc/reinfer.md)
   - [reintern.md](/Users/rtfeldman/.codex/worktrees/1d55/roc/reintern.md)
3. verify eval suites
4. commit
5. push
6. report exactly what remains, if anything
