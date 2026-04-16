# Plan: Replace Roc's Post-Check Lowering Architecture With cor's Architecture

## Summary
Rebuild the post-check pipeline to match `~/code/cor/experiments/lss`, instead of continuing to repair Roc's current monotype/workspace/publication machinery.

The target end state is:

- monotype types are direct, immutable lowered types, not entries in a mutable span-backed store
- monotype lowering consumes explicit solved facts from typed CIR/lambdasolved directly
- specialization uses exact function identity only
- payload extraction stays explicit in the IR, never reconstructed via structural binding tricks
- no publication/canonicalization/workspace-remapping layer exists anywhere in the compiler
- none of the removed machinery is reintroduced in any form

`zig` must not be run until the final "fix what breaks under the new architecture" phase.

## Required Architecture Changes
- Replace [`src/monotype/type.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/type.zig) `Store`/`TypeId`/`TypeSpan` architecture with a cor-style monotype type representation:
  - immutable arena-owned type nodes
  - child payloads stored as owned slices on the node itself
  - no global `type_ids`/`tags`/`fields` append-only backing arrays
  - no `cloneTypeGraph`, `canonicalizeResolved`, `canonicalizePublished`, or equivalent replacement
  - only direct instantiated-type construction and exact per-instantiation caching, mirroring cor's clone/lower caches
- Replace [`src/monotype/lower.zig`](/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig) workspace/source-var remapping model with direct fact consumption:
  - delete `prepareScopedFunctionRoot`
  - delete `bindSourceVarToExistingWorkspace`
  - delete `lookupFunctionNodeRetVar`
  - delete `lookupCurriedFunctionFinalRetVar` recovery logic
  - delete `materializeAppliedFunctionResultVar`
  - delete any logic that aligns or reconstructs function structure by mutating a workspace graph
- Move function-shape truth to the solved input:
  - every top-level fn, anonymous lambda, closure, and curried lambda stage input must expose exact arg vars and exact result var directly
  - if typed CIR does not already provide those facts explicitly, extend typed CIR/checker output first
  - monotype must then read those facts directly and never recover them from type-store shape
- Keep match payload handling cor-style:
  - `tag_payload` remains an explicit node all the way down
  - no synthetic `runtime_error` fallback payload extraction
  - no "structural binding decls imply payload access" path for refutable payload patterns
- Keep specialization exact-symbol-driven:
  - compilation pipeline uses `lookupFnExact` only
  - any canonical-source lookup is removed from compilation stages entirely
  - no mixed API, no fallback, no ambiguous identity behavior
- Keep lambdamono cor-style:
  - lower executable types directly from instantiated types
  - no separate fact publication/recovery layer
  - no stage-internal reconstruction of information already known from solved input

## Execution Phases
### Phase 1: Lock in the cor architecture without running `zig`
- Read `~/code/cor/experiments/lss/{canonical_solved,monotype,monotype_lifted,lambdasolved,lambdamono,ir}` and mirror their architecture exactly, except where Roc-specific constructs require explicit extensions.
- For Roc-only constructs, preserve the same design rule:
  - explicit earlier-stage facts only
  - immutable lowered types
  - no publication/recovery/reconstruction layer
- Add invariant comments and a grep-based boundary script that forbids reintroducing:
  - `canonicalizePublished`
  - `canonicalizeResolved`
  - `cloneTypeGraph`
  - `prepareScopedFunctionRoot`
  - `bindSourceVarToExistingWorkspace`
  - `lookupFunctionNodeRetVar`
  - `materializeAppliedFunctionResultVar`
  - any mixed exact/canonical specialization lookup in compilation stages
- Commit after each completed deletion/refactor slice, even if the tree does not build.

### Phase 2: Delete the monotype type-store model
- Remove the mutable monotype store and replace all monotype type construction/traversal with direct immutable types.
- Convert all monotype/lambdamono/IR consumers to work on those direct types.
- Delete every remaining publication/canonicalization entrypoint and all callers.
- Do not add compatibility shims. Callers must be rewritten to the new model directly.

### Phase 3: Delete the workspace/source-var remapping model
- Rewrite monotype lowering so expression, pattern, lambda, and call lowering consume explicit solved vars and solved signature facts directly from typed CIR/lambdasolved.
- For anonymous and curried closures, make exact arg/result facts explicit in the solved input, then consume them directly.
- Delete all function-shape recovery logic instead of patching missing cases.
- If any missing fact is discovered, add it to the earlier solved IR; do not reconstruct it later.

### Phase 4: Align remaining stages to cor before any build/test run
- Ensure monotype-lifted, lambdasolved, lambdamono, and IR lowering all consume the new direct monotype representation without publication or recovery steps.
- Remove any remaining payload-extraction shortcuts, structural binding side channels, or defensive guards against earlier compiler bugs.
- Remove any remaining compiler-path use of canonical-source specialization lookup.
- Remove any remaining code whose only purpose is to defend against the deleted architecture.

### Phase 5: Only now run `zig`, then fix what breaks under the new architecture
- Start running builds/tests only after the cor-style architecture is fully in place.
- Fix breakage by adding missing explicit earlier-stage facts or correcting direct consumers.
- Never fix breakage by:
  - reintroducing publication/canonicalization
  - reintroducing workspace/source-var remapping
  - adding fallback/recovery/reconstruction logic
  - adding defensive post-hoc guards for earlier-stage bugs
- If a failure appears to require one of those, stop and move the missing fact earlier in the pipeline instead.

## Test and Acceptance Plan
- First targeted checks after `zig` is allowed:
  - `./zig-out/bin/eval-test-runner --filter 'tag union payload matching inside function single module'`
  - legacy lambda-closure eval repro
  - `zig build run -- --opt=interpreter test/echo/all_syntax_test.roc`
- Then broader suites:
  - `zig build test-eval`
  - `zig build test-repl`
  - `zig build test-cli`
  - `zig build`
- Add/keep invariant checks that fail if forbidden architecture is reintroduced.
- Acceptance criteria:
  - no publication/canonicalization/workspace-remapping machinery remains in compiler stages
  - monotype and lambdamono match cor's architecture in design, not just behavior
  - specialization is exact-symbol-only in compilation stages
  - payload extraction is explicit and branch-guarded
  - all targeted regressions and full suites pass

## Assumptions and Defaults
- `~/code/cor/experiments/lss` is the architectural source of truth.
- Zig may use arena-owned immutable structs/slices instead of OCaml refs/lists, but the semantics and pipeline shape must match cor.
- Roc-only features may extend cor's design, but only as explicit earlier-stage facts; they must not reintroduce any publication, recovery, fallback, heuristic, or workspace-remapping machinery.
- Never reintroduce any of the deleted architecture, even temporarily.
