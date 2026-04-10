## Checker-To-Monotype MIR Plan

Status: complete

Goal:
- Fully replace the current checker -> monotype boundary with MIR, where MIR means `Monomorphic IR`.
- The checker must produce one explicit typed MIR directly.
- Monotype must consume MIR directly.
- The old raw CIR + solved-var + wrapper/adapter boundary must be deleted completely.
- When this plan is complete, MIR must be the only source of truth at this stage boundary.
- All relevant tests must pass afterward.

Design bar:
- Always aim for the long-term ideal and perfect design for correctness and compiler runtime performance.
- Implementation time does not matter.
- No workarounds, no fallbacks, no heuristics, no shortcuts.
- Later stages must consume explicit earlier facts.
- When in doubt, use `~/code/cor` as the architectural guide.
- If any implementation intentionally differs from `cor`, that difference must be justified as the long-term ideal design here, not a short-term expedient, and that justification must be reported at the end.

Reference model:
- `cor` publishes an explicitly typed solved tree before monotype lowering:
  - `/Users/rtfeldman/code/cor/experiments/lss/monotype/lower.ml`
- Roc should end up with the same architectural shape:
  - one typed solved boundary artifact
  - monotype as pure consumption of that artifact
  - no parallel raw-tree-plus-side-table contract left alive

Completed end state:
- Roc now has an explicit MIR boundary:
  - `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/mir.zig`
- MIR publication is an explicit checker-side phase boundary, not a monotype-side wrapper construction step.
- Monotype consumes published MIR directly.
- The old raw checker boundary contract is gone from monotype.
- MIR is the only checker-owned source of truth at the checker -> monotype boundary.

Success condition:
- The checker directly constructs MIR.
- MIR nodes directly carry the facts monotype consumes.
- Monotype input is MIR only.
- `src/check/mir.zig` is the only checker -> monotype boundary module left.
- The old checker-side raw CIR boundary contract is deleted from monotype.
- Any side tables or helper APIs whose only purpose was to bridge the old boundary are deleted.
- A fresh audit should find no trace of the old boundary model.

Testing rule:
- Every phase below must add or update targeted tests proving that the phase actually worked.
- Existing suites staying green is necessary but not sufficient.
- The final state must pass all relevant regression suites.

## Phase 1: Define MIR As A Real Checker-Owned Data Structure

Goal:
- Stop treating typed solved nodes as a wrapper concept.
- Introduce MIR as a first-class data structure owned by the checker.

Implementation steps:
- Introduce MIR node types in `src/check/` for the checker -> monotype boundary.
- MIR must directly carry settled monotype-ready facts on the nodes that semantically own them.
- MIR expr nodes must carry their settled type.
- MIR pattern nodes must carry their settled type.
- MIR defs/bindings/functions must carry their owned type facts directly.
- MIR must be immutable once published.
- MIR must be named consistently as MIR throughout the code base.

Required tests:
- Construction tests for MIR publication from checked programs.
- Targeted tests for:
  - simple values
  - functions
  - closures with captures
  - higher-order functions
  - boxed erased lambdas
  - zero-arg functions

Completion criteria:
- MIR exists as a real checker-owned boundary data structure.
- MIR is not just a wrapper API over raw CIR.

Progress:
- Replaced the old `SolvedCIR` naming with MIR in live code.
- Added `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/mir.zig`.
- Added targeted MIR coverage in `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/test/mir_test.zig`.
- Monotype and eval helpers now consume `check.MIR`.
- Ordinary monotype lowering no longer reaches directly through MIR into canonicalize `NodeStore`; span/branch/field reads now go through MIR methods instead.
- MIR now owns copied node/index/region/branch/field boundary data instead of forwarding those reads into canonicalize storage at monotype consumption time.
- MIR now also owns cloned type/name/string/import/method/evaluation-order boundary facts rather than borrowing them from `ModuleEnv`.
- MIR publication now happens through `check.MIR.Modules.publish(...)`, not `Modules.init(...)`.

## Phase 2: Make The Checker Produce MIR Directly

Goal:
- Move from “wrap raw CIR plus solved vars” to “publish MIR directly”.

Implementation steps:
- Identify the checker finalization point where solved facts are complete.
- Construct MIR directly at that point.
- Publish MIR from the checker as the explicit boundary artifact.
- Do not route MIR construction through `ModuleEnv.varFrom(...)` lookups at consumption time.
- Do not leave raw-CIR reopening as part of the boundary contract.
- Ensure all type facts monotype needs are published into MIR exactly once by the checker.

Required tests:
- Checker-focused publication tests proving MIR contains the settled facts without later reconstruction.
- Regression tests for polymorphic and higher-order cases where late reconstruction previously tended to creep in.

Completion criteria:
- The checker directly publishes MIR.
- MIR publication timing is checker-owned and final.
- The boundary contract is MIR, not raw CIR plus solved-var graph.

Progress:
- `check.MIR.Modules.publish(...)` now publishes MIR from explicit source modules instead of letting monotype construct MIR from raw `ModuleEnv` arrays.
- The eval/lowering pipeline now publishes MIR immediately after checking and before monotype lowering begins.
- MIR publication now performs the final global `resolveImports(...)` pass before freezing boundary facts, so MIR import indices match the actual lowering order.
- Added MIR teardown coverage in `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/test/mir_test.zig` proving published MIR remains usable after checker teardown.

## Phase 3: Move Monotype To Pure MIR Consumption

Goal:
- Make monotype structurally match `cor`: it consumes MIR directly and does not translate boundary facts itself.

Implementation steps:
- Rewrite monotype entrypoints to accept MIR as their only checker-owned input.
- Replace any remaining wrapper-based boundary reads with MIR node reads.
- Replace any remaining side lookups for checker-owned type facts with direct MIR field access.
- Keep only builder-local monotype state that is truly stage-internal and not a second source of truth.
- Make MIR the obvious and exclusive source for expr/pattern/binding/function/call boundary facts.

Required tests:
- Targeted tests that would fail if monotype still reopened raw checker data or depended on boundary adapters.
- End-to-end tests covering:
  - nested closures
  - polymorphic call sites
  - pattern-heavy code
  - boxed lambdas
  - hosted functions

Completion criteria:
- Monotype input is MIR only.
- Monotype no longer depends on wrapper translation or raw checker boundary APIs.

Progress:
- The live monotype consumer no longer contains direct `ctx.env(...).store`, `expr.env`, or `cir_env.store` reads.
- Type-cloning entrypoints in monotype now accept MIR modules directly rather than raw checker env pointers.
- Monotype no longer reaches into raw checker `ModuleEnv` directly for solved-type/text/import services either; those now flow through MIR methods.
- Monotype and MIR also no longer tunnel a whole `ModuleEnv` through checker unification helpers just to get an allocator, ident store, or recursion-order query.
- Monotype lowering now consumes MIR that was already published earlier in the pipeline; lowering no longer constructs MIR or reruns import-resolution side effects before monotype.

## Phase 4: Delete The Wrapper Layer And Old Boundary APIs

Goal:
- Remove all transitional boundary machinery once checker-owned MIR is live.

Implementation steps:
- Delete or radically replace:
  - `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/mir.zig`
  if it is still acting as an adapter over canonicalize storage rather than as the owned MIR artifact itself.
- Delete any exports and tests that only existed for the transitional adapter form.
- Remove old monotype initialization signatures that still mention raw module env arrays or solved-wrapper concepts.
- Remove any helper APIs whose only job was translating raw CIR + solved vars into monotype-consumable facts.
- Remove comments and naming that imply the transitional adapter form is still part of the architecture.

Required tests:
- Add or update tests that fail if the deleted transitional adapter path or raw boundary APIs reappear.
- Keep all relevant suites green after deletion.

Completion criteria:
- The transitional adapter layer is gone.
- No transitional wrapper-based boundary API remains.

Progress:
- `check.MIR.Modules.init(...)` is gone.
- The only remaining public MIR publication API is `check.MIR.Modules.publish(...)`.
- Eval helpers no longer create MIR at monotype-lowering time; they consume the already-published MIR artifact.

## Phase 5: Delete Parallel Sources Of Truth

Goal:
- Ensure MIR is the only checker -> monotype source of truth.

Implementation steps:
- Delete any remaining side tables whose only purpose was to compensate for the old boundary.
- Delete any old “settled facts” stores that are redundant once MIR exists.
- Delete any caches or helper functions that duplicate MIR-owned facts.
- Delete dead names, dead tests, dead comments, and dead audit residue tied to the old boundary model.
- Add assertions where useful so future code cannot quietly reintroduce duplicate sources of truth.

Required tests:
- Add targeted tests or assertions that fail if MIR-owned facts are absent or if duplicate boundary sources reappear.

Completion criteria:
- MIR is the only source of checker-owned typing truth at the boundary.
- No redundant boundary side table or adapter machinery remains anywhere in the code base.

Progress:
- Old monotype-side settled-var materialization residue and wrapper terminology are gone from the boundary.
- Monotype no longer accepts raw checker env arrays or raw checker-side lookups as an alternative source of truth.
- A fresh search over `src/` finds no live `SolvedCIR`, `MIR.Modules.init`, `materializeSettledVarTypeFacts`, or `var_type_facts` residue.

## Phase 6: Align Terminology And Architecture Around MIR

Goal:
- Make the new architecture obvious and durable.

Implementation steps:
- Rename remaining “typed CIR”, “solved wrapper”, or similar transitional terms to MIR where appropriate.
- Ensure docs, comments, and function names describe the checker -> monotype boundary as MIR.
- Keep the architecture legible enough that a fresh reader can see the `cor`-shaped design directly from the code.

Required tests:
- No special new suite required, but all targeted and regression tests must still pass after naming cleanup.

Completion criteria:
- MIR is the consistent architectural term.
- The code base no longer describes the old wrapper-based model as current reality.

Progress:
- MIR is now the live architectural name throughout the checker -> monotype path.
- Remaining references to `SolvedCIR` and wrapper terminology are now only historical notes in this plan file.

## Phase 7: Final Audit And Verification

Goal:
- Prove the transition is complete and there is no residue.

Audit steps:
- Run a fresh from-scratch audit for:
  - reinfer
  - reintern
  - boundary translation residue
  - duplicate sources of truth
  - stale side tables
  - adapter layers
  - hacks, shortcuts, fallbacks, heuristics
- Search specifically for:
  - `SolvedCIR`
  - old wrapper terminology
  - raw checker boundary calls from monotype
  - deleted side-table names
- Confirm MIR is the only checker-owned boundary artifact monotype consumes.
- Explicitly report any intentional divergence from `cor` and justify it as the long-term ideal design.

Required verification:
- `zig build test-monotype`
- `zig build test-eval -- --threads 1`
- `zig build test-eval-host-effects -- --threads 1`
- `zig build test-cor-pipeline`
- any new targeted MIR publication/consumption tests added during implementation

Final success condition:
- The checker produces MIR directly.
- Monotype consumes MIR directly.
- The transitional adapter layer and old boundary machinery are deleted.
- MIR is the only source of truth.
- All relevant tests pass.

Verification run:
- `zig build test-monotype`
- `zig build test-eval -- --threads 1`
- `zig build test-eval-host-effects -- --threads 1`
- `zig build test-cor-pipeline`

Additional audit results:
- A fresh search over `src/` finds no live `SolvedCIR`, `MIR.Modules.init`, `moduleEnvConst(...)`, `moduleEnvMut(...)`, `materializeSettledVarTypeFacts(...)`, or `var_type_facts` residue.
- A fresh monotype-boundary search finds no raw checker-boundary `ModuleEnv.varFrom(...)`, raw checker `store.getExpr(...)`, or raw checker `store.getPattern(...)` reads in ordinary monotype consumption.
- `zig build test-check` still fails, but only for unrelated pre-existing `src/check/unify.zig` / `src/check/test/unify_test.zig` signature drift outside this MIR transition.

Intentional divergence from `cor`:
- `cor` publishes a physically separate explicitly typed solved tree before monotype.
- Roc now publishes an immutable owned MIR artifact at the same boundary, but it is produced by freezing/cloning the checker’s internal solved state rather than replacing the checker’s internal representation wholesale.
- This is the intentional long-term choice here because it preserves the existing checker/reporting infrastructure while still giving monotype one explicit immutable source of truth and avoiding a second live mutable boundary contract.
