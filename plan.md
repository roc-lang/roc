## Checker-To-Monotype Typed CIR Plan

Status: in progress

Goal:
- Move Roc from the current `checked CIR + solved-var graph` checker→monotype boundary to the `cor` shape: an explicit typed solved artifact at the boundary.
- First publish immutable settled type facts from the checker.
- Then move from those side facts to a fully typed CIR/AST representation.
- Then delete every obsolete side table, freeze/materialization path, and alternate source of truth so the typed tree is the only possible source of truth.
- Keep correctness and runtime performance at the long-term ideal end state; implementation time does not matter.

Design bar:
- Long-term ideal and perfect is the goal.
- No heuristics, no workarounds, no fallbacks, no “good enough for now”.
- Every later stage must consume explicit facts produced earlier.
- When in doubt, use `~/code/cor` as the model.
- If we intentionally diverge from `cor`, that divergence must be justified as the long-term ideal design here rather than a convenience, and that justification must be reported at the end of implementation.

Testing rule:
- Every phase below must add or update tests that specifically prove the phase worked.
- Existing tests staying green is necessary but not sufficient.
- The final state must pass the full relevant test suites.

Reference model:
- `cor` monotype starts from an already-typed solved AST:
  - `/Users/rtfeldman/code/cor/experiments/lss/monotype/lower.ml`
- Roc should move toward that same stage-boundary shape in phases rather than preserving the current `CIR node id + solved var` contract forever.

Current problem:
- Roc monotype still starts from checked CIR ids plus solved checker vars:
  - `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig`
  - `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/semantic_facts.zig`
- That is why monotype still needs a settled-var-type materialization subphase instead of directly consuming an explicitly typed solved tree.

## Phase 1: Checker Publishes Immutable Settled Type Facts

Goal:
- Move settled type publication upstream to the checker boundary.
- Monotype must stop being the stage that first turns solved vars into authoritative typed facts.

Ideal end state:
- After checking is complete, there is one immutable settled-facts artifact keyed by solved CIR nodes/bindings.
- That artifact contains every type fact monotype needs:
  - expr types
  - pattern types
  - binding types
  - function arg/ret types
  - any other typed structural fact monotype currently depends on
- Monotype does not materialize those facts itself.

Implementation steps:
- Identify every type fact currently first materialized inside monotype.
- Introduce a checker-owned settled-facts artifact and publish those facts there.
- Make the checker own the publication timing: facts only appear once the relevant solved state is final.
- Make monotype receive that artifact explicitly as input.
- Delete the monotype boundary assumption that it is responsible for first materialization of settled types.

Testing:
- Add targeted tests that prove monotype can run entirely from checker-published settled type facts.
- Add regression tests for previously tricky cases:
  - polymorphic functions
  - higher-order functions
  - closures with captures
  - boxed erased lambdas
  - zero-arg functions

Completion criteria:
- Checker owns settled type publication.
- Monotype no longer first-publishes settled expr/pattern/function/call types.
- The checker→monotype boundary has one explicit immutable settled-type artifact.

Progress:
- Added a checker-owned solved wrapper layer in `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/solved_cir.zig`.
- Added a targeted wrapper test in `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/test/solved_cir_test.zig`.
- Monotype top-level def lowering and semantic-fact collection now consume that solved wrapper layer instead of starting directly from raw checked CIR lookups in the converted paths.

## Phase 2: Monotype Consumes Only Checker-Published Settled Facts

Goal:
- Remove the remaining monotype-side settled-var-type materializer as an architectural boundary concept.

Ideal end state:
- Monotype consumes only checker-published settled facts.
- Ordinary monotype lowering never asks “what monotype type does this solved var become?”
- `lowerInstantiatedType(...)` no longer serves as a stage-boundary materializer.

Implementation steps:
- Redirect every monotype type consumer to the checker-published artifact.
- Remove monotype entry subphases whose only job is settled-var-type publication.
- Collapse any remaining monotype-side typed fact freeze/publication code into plain consumption logic.
- Delete the now-dead monotype-side publication helpers and supporting side tables.

Testing:
- Add targeted tests that fail if monotype attempts to lower solved vars at boundary time.
- Keep full eval, host-effects, and cor-pipeline suites green.

Completion criteria:
- No monotype boundary materializer remains.
- Monotype typed facts are fully upstream-owned.
- Monotype entry is consumption-only with respect to settled types.

## Phase 3: Introduce Typed CIR Nodes

Goal:
- Change representation, not semantics: move from side facts keyed by CIR node ids to a typed CIR where types live on the nodes.

Ideal end state:
- Expr and pattern nodes carry their settled types directly.
- Binding/function nodes carry the type facts they semantically own.
- The typed tree itself visibly encodes the invariants that side tables currently hide.

Implementation steps:
- Extend CIR (or introduce a typed solved CIR layer) so every relevant node carries its settled type.
- Migrate the checker-published settled facts into typed-node construction.
- Make the typed CIR immutable and authoritative.
- Keep node-local explicit facts on the nodes rather than parallel side tables whenever the fact is semantically owned by that node.

Testing:
- Add targeted construction/round-trip tests for typed CIR publication.
- Add regressions covering:
  - nested closures
  - polymorphic call sites
  - pattern-heavy code
  - tag/record/list structure

Completion criteria:
- There is a typed solved CIR/AST layer analogous in role to `cor`’s typed solved AST.
- All essential settled type facts are directly reachable from the typed nodes.

Progress:
- The first typed solved layer now exists as a wrapper over checked CIR:
  - `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/solved_cir.zig`
- This is not yet the final typed CIR representation, but it establishes the explicit checker-owned typed boundary needed for the later full migration.

## Phase 4: Migrate Monotype To Typed CIR Consumption

Goal:
- Make monotype structurally match `cor`: consume typed solved nodes directly rather than `node id + side table`.

Ideal end state:
- Monotype lowering takes typed CIR/AST as input.
- The typed tree, not side tables, is the primary and normal source of type truth.
- The monotype API and internals become simpler and more obviously correct.

Implementation steps:
- Rewrite monotype lowering entrypoints to accept typed CIR/AST nodes.
- Replace node-id lookups and side-table fetches with direct typed-node consumption.
- Convert typed structural facts that belong on nodes to direct node reads.
- Keep only stage-local scratch state that is truly builder-internal and not a second source of truth.

Testing:
- Add targeted tests that would fail if monotype still depended on old side tables.
- Keep all existing suites green.

Completion criteria:
- Monotype input is typed CIR/AST.
- Monotype no longer uses checker-published parallel settled-type side tables as its normal input format.

Progress:
- Monotype top-level boundary and semantic-fact collection have begun migrating to the solved wrapper layer.
- Ordinary monotype lowering still uses the older internal node-id/side-table model beyond those converted entry paths, so this phase is not complete yet.

## Phase 5: Delete Obsolete Side Tables And Old Boundary Machinery

Goal:
- After typed CIR is in place and monotype consumes it, delete every obsolete source of truth so the typed tree is the only possible source of truth.

Ideal end state:
- No obsolete side-table path remains for settled expr/pattern/function/call types.
- No old freeze/materialization/publication helpers remain.
- No code path can accidentally keep using the pre-typed-CIR boundary.

Implementation steps:
- Delete old checker→monotype settled-type side tables that are now redundant.
- Delete obsolete monotype semantic-facts entries and helpers that only existed to bridge the old boundary.
- Delete unused APIs, caches, helper functions, and dead comments tied to the old model.
- Add invariant checks where useful so future code cannot quietly reintroduce parallel sources of truth.

Testing:
- Add targeted tests or assertions that fail if deleted machinery reappears or if typed-node facts are absent.
- Keep full regression suites green after deletion.

Completion criteria:
- The typed CIR/AST is the only source of truth for settled typing at the checker→monotype boundary.
- There is no remaining obsolete side-table or materialization machinery left in the code base for this boundary.

## Phase 6: Final Audit And Verification

Goal:
- Prove the migration is complete and there is no residue.

Steps:
- Run a fresh from-scratch audit focused on:
  - reinfer
  - reintern
  - redoing work
  - stale side tables
  - obsolete materializers
  - duplicate sources of truth
  - hacks, shortcuts, fallbacks, heuristics
- Search specifically for old boundary machinery names and categories.
- Verify that typed CIR/AST is now the only source of truth.
- Explicitly list any intentional divergence from `cor`, with the justification for why it is long-term ideal here.

Required verification:
- `zig build test-eval -- --threads 1`
- `zig build test-eval-host-effects -- --threads 1`
- `zig build test-cor-pipeline`
- any new targeted commands added during implementation for typed CIR publication and consumption

Success condition:
- The next fresh audit does not find any checker→monotype re-materialization or duplicate-source-of-truth residue.
- Monotype consumes typed CIR/AST directly.
- The old obsolete side tables and boundary machinery are gone.
- All relevant tests pass.
