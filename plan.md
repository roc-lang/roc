## Zero-Copy Typed CIR Boundary Plan

Status: in_progress

Progress so far:

- `src/check/mir.zig` no longer stores a duplicate published tree; it now wraps real `ModuleEnv` state directly.
- publication is zero-copy for checked modules in the active eval/test pipeline
- monotype now borrows the published boundary instead of owning and destroying it before later stages
- full `test-eval`, `test-monotype`, `test-cor-pipeline`, and `test-eval-host-effects` are green on this slice
- the remaining unfinished work is the monotype-side specialization refactor:
  - published source typing state is still mutable from monotype through `typeStoreMut()`
  - specialization data is still not fully monotype-owned

## Goal

Replace the current published-MIR boundary with the long-term ideal design:

- the checker publishes one owned typed CIR artifact directly
- publication is zero-copy ownership transfer, not clone/wrap/adapter construction
- typed CIR is the only source of source-level solved typing truth after checking
- monotype consumes typed CIR directly
- monotype specialization state lives entirely on the monotype side
- no duplicated MIR tree exists
- no checker-style solved type store is mutated by monotype
- all relevant tests pass afterward

This plan is explicitly about correctness and compiler runtime performance, not implementation convenience.
If a choice trades long-term architecture or performance for short-term ease, reject it.

When in doubt, use `~/code/cor` as the architectural guide:

- `cor` publishes one explicit solved typed tree before monotype
- monotype lowers from that tree
- specialization and later executable lowering are downstream concerns

Any intentional divergence from `cor` must be justified at the end as the long-term ideal design here, not as a short-term expedient.

## Why This Plan Exists

Current state is better than the old checker-internal leakage, but still not ideal:

- we still publish a separate `MIR` artifact in `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/mir.zig`
- that artifact still owns a cloned checker `types.Store`
- monotype still treats that solved checker-style store as part of its input
- monotype still performs specialization by cloning/instantiating solved vars rather than consuming a pure source boundary plus monotype-owned specialization artifacts

That means we are still paying for:

- an extra publication structure
- a clone of checker solved state
- conceptual duplication between typed CIR and MIR
- a muddled boundary where source typing facts and monotype specialization facts are not cleanly separated

The ideal design is stricter and simpler:

- checked typed CIR is published once
- ownership transfers out of the checker once
- monotype never mutates published typed CIR
- specialization results are stored separately as monotype artifacts keyed by specialization

## End State

When this plan is complete:

- `CIR` nodes are the published typed boundary; there is no second duplicated MIR tree
- every published expr/pattern/def/binding node carries or can directly index its settled source type facts
- the checker publishes that typed CIR by moving ownership, not cloning
- the checker has no remaining responsibility for that published artifact after publication
- monotype reads only published typed CIR and monotype-owned caches/artifacts
- monotype does not call any checker-style `typeStoreMut()` on boundary data
- monotype specialization artifacts are keyed by specialization and source identity, not by mutating the published source tree
- old MIR naming, files, helpers, tests, and comments are deleted or renamed away unless they truly remain as zero-copy publication helpers with no duplicate storage

## Non-Negotiable Invariants

These invariants must hold at every phase, and the final state must make them obvious in code:

1. Published typed CIR is immutable.
2. Published typed CIR is owned.
3. Publication is a one-way boundary: checker finishes, publishes, and does not keep using the published data as checker working state.
4. Published typed CIR contains only source-level solved facts, not monotype specialization results.
5. Monotype specialization state is monotype-owned only.
6. A source node has one source solved type identity, but may have many monotype specializations.
7. Later stages consume explicit earlier facts only.
8. No clones exist merely to defend against our own boundary bugs once ownership transfer is in place.
9. No debug-vs-release architectural split is allowed.
10. No fallback reconstruction of missing source typing facts is allowed anywhere in monotype or later stages.

## Important Architectural Clarifications

### Typed CIR Is Not Monomorphic

Typed CIR is the published solved source program.
It is not already monomorphic and must not pretend to be.

That means:

- source node idx -> source solved checker var/type fact
- `(specialization key, source node idx)` -> monotype instance

The published source tree stores the first thing.
Monotype-owned specialization artifacts store the second thing.

### Lambda Sets Are Not Part Of This Boundary

Lambda sets appear later, after monotype, just as they do in `cor`.
They are not a blocker for publishing typed CIR directly.

So this plan must not delay the typed-CIR transition waiting on lambda-set design.

### Zero-Copy Means Ownership Transfer, Not Borrowing

Acceptable:

- checker constructs data
- checker moves ownership of finalized typed CIR and associated solved type data into the published artifact
- checker stops using it

Unacceptable:

- checker publishes borrowed views into still-owned checker storage
- debug-only frozen-bit schemes replacing ownership transfer
- release-only zero-copy with debug-only clone

## Source Of Truth Layout After Transition

There should be exactly three memory domains:

1. Published typed CIR boundary.
   - source tree
   - source solved types
   - source names/imports/method facts
   - source evaluation-order facts
   - immutable

2. Monotype global artifacts.
   - monotype type interner
   - specialization table
   - proc-spec table
   - frozen monotype bodies
   - global work queues and caches

3. Per-specialization monotype scratch.
   - temporary arrays/maps keyed by source node id within one specialization
   - discarded after that specialization is frozen

Published typed CIR must never become a fourth-place dumping ground for monotype specialization state.

## Things To Watch Out For

These are the main ways this migration can go wrong:

- Accidentally keeping both typed CIR and duplicated MIR alive.
- Letting monotype continue to mutate a checker-style solved store after the new boundary lands.
- Smuggling monotype specialization results back onto source nodes.
- Preserving old APIs under new names.
- Keeping clone-based publication “temporarily” after ownership transfer is already feasible.
- Reintroducing debug-only architecture differences.
- Mixing reporting-only checker state with publishable boundary state in a way that blocks ownership transfer.
- Forgetting cross-module imported-var handling; imports must be explicit published facts, not a reason to keep checker state alive.
- Losing source regions/spans needed for diagnostics while moving ownership.
- Treating local-function specialization facts as source-level truth instead of specialization artifacts.

## Phase 0: Freeze Terminology And Success Criteria

Goal:

- Stop pretending the duplicate published tree is inherently “MIR” in the monomorphic sense.
- Define the target architecture in code and docs before touching storage again.

Implementation steps:

- Audit and enumerate every place where `MIR` currently means “published solved boundary”.
- Decide the final public term:
  - preferred: published typed CIR
  - acceptable only if it is not misleading: keep `MIR` as a thin publication module name with no duplicate tree inside it
- Update comments in:
  - `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/mir.zig`
  - `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig`
  - `/Users/rtfeldman/.codex/worktrees/1d55/roc/plan.md`
  so they state the real target:
  - one published typed CIR boundary
  - monotype specialization state lives elsewhere

Required tests:

- none specific beyond keeping the tree green

Completion criteria:

- the intended end state is described consistently
- comments no longer imply the published source tree is already monomorphic

## Phase 1: Define The Published Typed-CIR Ownership Boundary

Goal:

- Make it explicit which data must transfer out of the checker and which data must remain checker-internal.

Implementation steps:

- Split checker state conceptually into:
  - publishable typed-CIR boundary data
  - checker-only reporting/workspace data
- For each field currently cloned into `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/mir.zig`, classify it as one of:
  - must move to published typed CIR
  - must stay checker-only
  - should be recomputed nowhere and therefore must move
- Build a table in code/comments for:
  - CIR node storage
  - solved `types.Store`
  - string/ident stores
  - import resolution
  - method lookup ids
  - evaluation order
  - exposed items
  - any module indexing structures
- Identify exactly what diagnostics still need after publication.
- Refuse to keep publishable state checker-owned just because it is convenient.

Required tests:

- targeted ownership-boundary test additions proving published typed CIR survives checker teardown
- tests that prove reporting-only state can still be torn down independently after publication

Completion criteria:

- every currently cloned field has a planned move/retain/delete outcome
- no “we’ll see later” ownership ambiguity remains

## Phase 2: Make Publication A Real Move, Not A Clone

Goal:

- Replace clone-based publication with zero-copy transfer of finalized checker output.

Implementation steps:

- Redesign the publication API so checker finalization produces:
  - one owned typed-CIR artifact
  - one owned solved type store paired with it
- Remove clone helpers from the boundary path for:
  - `types.Store`
  - string/ident interners
  - evaluation order
  - exposed items
  - any remaining boundary-owned vectors or tables
- Make publication consume or move the finalized checker module state instead of borrowing it.
- After move, checker must not retain usable aliases to the transferred data.
- If necessary, split checker finalization into:
  - finish solving
  - publish/move typed CIR
  - discard checker-only temporary state

Required tests:

- teardown tests proving published typed CIR remains valid after full checker teardown
- tests proving there is no clone-based duplicate copy by checking pointer/ownership relationships where practical
- regression tests for imports, methods, and module order across publication

Completion criteria:

- clone-based publication path is gone from the boundary
- ownership is transferred, not copied
- checker teardown no longer threatens published typed CIR lifetime

## Phase 3: Stop Monotype From Mutating Published Source Typing State

Goal:

- Remove the remaining architectural blocker to a truly immutable published source boundary.

Implementation steps:

- Audit every `typeStoreMut()` and checker-style var unification path reached from `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig`.
- Categorize them into:
  - source-type lookup that should become read-only
  - specialization bookkeeping that should become monotype-local
  - import copying that should become explicit published facts or monotype-owned instantiation state
- Eliminate monotype mutation of published solved checker vars.
- Replace `TypeCloneScope` responsibilities that currently depend on mutating checker-style solved state with monotype-owned specialization data:
  - source var -> specialized mono type mapping
  - source node -> specialized mono type mapping
  - proc specialization key -> proc spec id
  - local-fn specialization mapping
- Make source type instantiation read from published typed CIR and write only monotype-owned artifacts.

Required tests:

- targeted tests for polymorphic top-level functions
- targeted tests for local polymorphic functions
- tests for imported polymorphic functions across modules
- tests for closures and higher-order calls where specialization multiplies source nodes

Completion criteria:

- monotype no longer mutates published source solved type state
- published typed CIR is actually immutable in practice, not just by convention

## Phase 4: Define Explicit Monotype Specialization Storage

Goal:

- Give specialization its own first-class storage model so it no longer parasitizes checker-style solved state.

Implementation steps:

- Introduce explicit monotype-owned data structures for:
  - `SpecKey`
  - `ProcSpec`
  - frozen monotype bodies
  - per-specialization expr type arrays
  - per-specialization pattern type arrays
  - symbol binding/state arrays
  - pending work queues for newly discovered specializations
- Ensure the specialization key includes every fact that can change generated monotype code.
- Keep per-specialization source-node mappings dense and source-id keyed, not hash-map-heavy, whenever ids are dense.
- Ensure a single source proc can yield many proc specializations without mutating the published source tree.
- Move any current specialization-time semantic fact storage out of checker-style structures into these monotype-owned artifacts.

Required tests:

- targeted tests exercising multiple specializations of the same source proc
- tests proving two specializations of one source node produce distinct monotype results without altering source facts
- performance-sensitive tests or assertions around dense node-indexed storage shape where practical

Completion criteria:

- specialization storage is explicit and monotype-owned
- no checker-style mutation remains necessary for specialization

## Phase 5: Make Monotype A Pure Consumer Of Published Typed CIR

Goal:

- Align Roc’s checker -> monotype boundary with `cor` in substance.

Implementation steps:

- Rewrite monotype entrypoints so their source-level input is only:
  - published typed CIR
  - monotype-owned specialization state
- Eliminate any remaining dependence on:
  - duplicated MIR node structures
  - wrapper-only node readers
  - checker-owned env pointers
  - clone-time translation caches whose only purpose was to compensate for the old boundary
- Ensure every source-level fact monotype needs is read directly from published typed CIR.
- Ensure every specialization-level fact monotype needs is read directly from monotype-owned artifacts.

Required tests:

- targeted tests that fail if monotype reaches outside published typed CIR for source facts
- end-to-end tests covering:
  - nested closures
  - higher-order polymorphism
  - boxed lambdas
  - patterns with many child nodes
  - multi-module import specialization

Completion criteria:

- monotype has a clean two-input model:
  - published typed CIR
  - monotype-owned specialization artifacts

## Phase 6: Delete The Duplicate MIR Structure Entirely

Goal:

- Remove the duplicate-tree idea completely.

Implementation steps:

- Delete the current duplicated publication structure in `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/mir.zig` if it still stores copied nodes.
- If a publication helper module is still useful, reduce it to boundary orchestration only:
  - publish typed CIR
  - transfer ownership
  - no duplicate tree storage
- Rename files/types as needed so the code no longer suggests there is a second source tree.
- Remove stale tests that were only asserting the duplicated-MIR transition shape.

Required tests:

- tests proving the checker publishes typed CIR directly
- no test should require a duplicate MIR node store to exist

Completion criteria:

- there is no duplicate published MIR tree left in memory
- there is only the published typed CIR source tree

## Phase 7: Delete Old Clone-Oriented And Wrapper-Oriented Machinery

Goal:

- Remove every remaining artifact that existed only because we used clone-based MIR publication.

Implementation steps:

- Delete obsolete clone helpers from the publication path.
- Delete wrapper/adapter APIs that only existed to make the old MIR look like checker data.
- Delete stale terminology:
  - `SolvedCIR`
  - duplicate-MIR comments
  - “publish by clone” descriptions
- Delete dead tests that encode obsolete boundary behavior.
- Search specifically for residue in:
  - `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/check/`
  - `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/`
  - `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/`

Required tests:

- targeted residue tests/search-based checks where practical
- all regression suites still green

Completion criteria:

- no clone-oriented boundary machinery remains
- no duplicate-tree wording remains
- no wrapper residue remains

## Phase 8: Performance And Invariant Hardening

Goal:

- Ensure the new architecture is not only cleaner, but actually the performance-oriented end state we want.

Implementation steps:

- Audit publication to confirm it is zero-copy in the steady state.
- Audit specialization storage for unnecessary hashing, copying, or per-node allocations.
- Prefer dense arrays keyed by source node id within a proc specialization.
- Ensure source-level interner/storage ownership is transferred once, not duplicated.
- Add assertions that enforce:
  - published typed CIR immutability
  - no monotype mutation of source solved type data
  - specialization artifacts never become a second source-level truth

Required tests:

- targeted tests for multi-specialization workloads
- any useful debug assertions that trip if source data is mutated or if specialization state leaks into source storage

Completion criteria:

- the implementation reflects the performance intent, not just the architectural shape

## Phase 9: Final Audit And Verification

Goal:

- Prove the transition is complete and no residue remains.

Audit checklist:

- Search for duplicate MIR-tree storage.
- Search for clone-based publication in the checker -> monotype boundary path.
- Search for monotype mutation of published source solved type state.
- Search for stale wrapper/adapter APIs.
- Search for comments claiming MIR is monomorphic when it is not.
- Search for any later-stage source-fact reconstruction that should have been explicit.

Required verification:

- `zig build test-monotype`
- `zig build test-eval -- --threads 1`
- `zig build test-eval-host-effects -- --threads 1`
- `zig build test-cor-pipeline`
- relevant checker tests
- all new targeted publication/specialization tests added during the migration

Final success condition:

- checker publishes typed CIR directly by ownership transfer
- no duplicate MIR tree exists
- typed CIR is the only source-level typing truth after checking
- monotype specialization state is entirely monotype-owned
- published typed CIR is immutable
- monotype consumes published typed CIR directly
- all relevant tests pass

## Expected Justification At The End

At the end of implementation, report:

- whether any divergence from `cor` remains
- why that divergence is long-term ideal here
- whether zero-copy publication is fully achieved
- whether any transitional boundary residue remains

If the answer to the last question is anything other than “no”, the plan is not complete.
