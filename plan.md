## Monotype Specialization: cor-Style Plan

Status: done

Goal: fully switch monotype specialization to cor-style lowering from instantiated types, and delete the explicit semantic fact system entirely. After completion, there must be no trace of the old “fact” system anywhere in `src/`.

This plan prioritizes long-term ideal architecture and performance over short-term expedience. No fallbacks, heuristics, or recovery logic are allowed outside parsing/error reporting.

When in doubt, use `~/code/cor` as the architectural guide.

## Target Architecture (End State)

1. Typed CIR remains the sole checker output boundary.
2. Monotype specialization:
   - Clones only reachable source type vars for a specialization (cor-style `clone_inst` model).
   - Lowers directly from the instantiated type graph at the use site.
   - Does not publish or consume explicit semantic “fact” tables.
3. Method call resolution:
   - Occurs at the point of lowering by inspecting the instantiated receiver type.
   - Uses direct method table lookup from the instantiated nominal identity.
   - No intermediate dispatch-fact publication or consumption.
4. Field/tag indices and call arity:
   - Derived by inspecting the instantiated type graph where needed.
   - No explicit “fact” storage or caching system.
5. All `semantic_facts`-style machinery is deleted:
   - `src/monotype/semantic_facts.zig`
   - all fact maps, recording APIs, and consumers
   - any plan/test scaffolding tied to facts

## Phase 1: Remove Fact Publication/Consumption

1.1 Delete the semantic facts module.
- Remove `src/monotype/semantic_facts.zig`.
- Remove all imports and any `fact`-related fields from monotype structures.

1.2 Delete all explicit fact recording and lookup APIs.
- Remove `record*Fact`, `put*Fact`, `lookup*Fact`, and similar helpers in `src/monotype/lower.zig`.
- Remove any “facts_frozen” or “typed_facts_frozen” state.

1.3 Remove fact-driven lowering paths.
- Replace uses of explicit call/dispatch/field/tag facts with direct type inspection.
- Ensure every lowering path reads the instantiated type graph directly.

Completion criteria:
- `rg -n "fact|semantic_facts|ResolvedDispatchTargetFact|ExplicitCallFact|ExplicitFunctionFact" src/monotype` is empty.

## Phase 2: cor-Style Specialization Model

2.1 Implement cor-style instantiation cache.
- Per-specialization instantiation cache that clones only reachable vars.
- No global per-module type-store cloning.
- No specialization-local “facts” side tables.

2.2 Lower directly from instantiated types.
- For function calls: derive arg/ret types by inspecting instantiated function type.
- For record fields: derive field index by scanning instantiated record field list.
- For tag unions: derive tag index by scanning instantiated tag list.
- For nominal types: carry nominal identity in instantiated type so method lookup is direct.

2.3 Replace method call resolution with direct instantiated-type lookup.
- At call site, inspect instantiated receiver type.
- If nominal: use attached-method table lookup to resolve target.
- If not nominal: emit compiler error (checker already should have prevented this).

Completion criteria:
- No call-site dispatch resolution code uses earlier recorded facts.
- All call/field/tag lowering reads instantiated types directly.

## Phase 3: Delete Remaining Fact Residue and Tests

3.1 Remove tests that depend on fact publication APIs.
- Delete or rewrite any tests that assert on fact maps.
- Replace with behavior-level tests only.

3.2 Remove any caching structures that were introduced solely for facts.
- If a map is only used for fact publication/consumption, delete it.
- If a map became redundant after switching to cor-style lowering, delete it.

Completion criteria:
- No monotype fact terminology remains in `src/monotype`.
- No tests reference explicit monotype fact publication.

## Phase 4: Method Resolution Path Cleanup

4.1 Revisit typed-CIR dispatch metadata.
- If typed-CIR still exposes resolved dispatch sites, remove that output if it is no longer used.
- Preserve only data needed for direct instantiated-type lookup.

4.2 Ensure monotype does not depend on checker-resolved dispatch facts.
- All dispatch resolution happens in monotype from instantiated types.

Completion criteria:
- Typed-CIR no longer has “resolved dispatch site” storage unless required for non-specialized code paths.
- Monotype never consults checker-resolved dispatch tables.

## Phase 5: Verification & Audit

5.1 Run and pass:
- `zig build test-monotype`
- `zig build test-eval -- --threads 1`
- `zig build test-eval-host-effects -- --threads 1`
- `zig build test-cor-pipeline`

5.2 Audit:
- `rg -n "fact|semantic_facts|ResolvedDispatchTargetFact|ExplicitCallFact|ExplicitFunctionFact" src/monotype` must be empty.
- No “dispatch fact” or “fact publication” language remains in monotype source or docs.
- No monotype code path relies on earlier-stage semantic recovery (beyond type inspection of instantiated types).

## Definition of Done

This plan is complete only when:

- Monotype uses cor-style specialization and lowers directly from instantiated types.
- The semantic fact system is fully removed with no residue.
- Method call resolution is done at lowering from instantiated receiver types.
- All suites pass and the audit checks are clean.
