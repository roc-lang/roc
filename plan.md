# Plan: Make LIR the Only Non-Builtin Source of Ownership Semantics

## Summary

Goal state:

- Outside builtin/runtime implementations, **100% of ownership logic lives in LIR**.
- `interpreter`, `dev`, and `wasm` do **zero ownership reasoning** of their own.
- The only non-builtin ownership side effects they perform are executing explicit
  LIR `incref` / `decref` / `free` statements.
- All ordinary value construction, coercion, projection, unboxing, container
  shaping, and call lowering are plain data movement only.
- If ownership is needed, it is represented explicitly in LIR first and emitted
  by the dedicated LIR ownership pipeline.

This plan is intentionally ordered so that **enforcement mechanisms come
first**. The compiler must be structurally prevented from regressing before any
semantic migration work begins.

The plan also intentionally forbids using `zig` during the enforcement/setup
phase:

- **Until Phase 4 begins, running `zig` is strictly forbidden.**
- Phases 1-3 must be done entirely "in the dark", without compiler feedback.
- Only after the boundaries, module structure, lint enforcement, and
  permissions model are fully in place may we begin the correctness phase and
  run `zig`.

Commit policy:

- Commit as you go.
- Commit after every coherent milestone, even if the tree is temporarily
  incomplete or uncompilable.
- Do not wait for a working state to commit enforcement or boundary changes.

Design constraints:

- No workarounds.
- No fallbacks.
- No heuristics.
- Every stage consumes explicit outputs from earlier stages.
- Backends may not invent ownership policy.
- Builtin/runtime implementations are the only allowed place for internal RC
  behavior.
- When in doubt, follow `~/code/cor` architecturally. Any deliberate divergence
  must later be justified as more long-term ideal.

## Non-Negotiable Invariants

These invariants must be made mechanically enforceable, not merely documented.

1. Ordinary interpreter/backend code may not directly call generic RC helpers.
2. Ordinary interpreter/backend code may not branch on layout refcounted-ness
   in order to decide whether to retain/release/free.
3. Ordinary interpreter/backend code may not emit or perform RC as part of
   struct/tag/list/box construction, projection, coercion, unboxing, or
   indirect-call preparation.
4. Only explicit LIR RC statements may trigger non-builtin RC execution.
5. Builtin/runtime implementations may perform internal RC because that is part
   of their primitive semantics.
6. Ownership semantics for every non-builtin operation must be expressible in
   LIR and emitted from LIR.

## Phase 1: Boundary Hardening (No `zig` Allowed)

Objective:

- Reorganize modules and APIs so that ordinary backend/interpreter code cannot
  casually reach RC primitives.
- Create architectural choke points before touching semantics.

`zig` policy:

- Forbidden.

Milestone commits:

- Commit after each subphase below.

### 1.1 Introduce explicit ownership boundary modules

Create a strict split between:

- builtin/runtime RC internals
- ordinary LIR RC execution
- ordinary value lowering/evaluation/codegen

Target structure:

- A tiny, explicit RC-execution surface for non-builtin consumers.
- Private/internal RC helper modules for builtin/runtime use only.
- Separate non-builtin utility layers that only expose plain data movement.

Actions:

- Identify all RC entry points currently reachable from:
  - `src/eval/interpreter.zig`
  - `src/backend/dev/LirCodeGen.zig`
  - `src/backend/wasm/WasmCodeGen.zig`
- Refactor exports so ordinary code paths do not import low-level RC helper
  symbols directly.
- Introduce narrow interfaces for:
  - execute explicit RC stmt
  - call builtin/runtime helper
- Remove any "convenience" imports that let ordinary code paths call RC helpers
  directly.

Required output:

- Source tree layout makes the intended boundary obvious.
- Ordinary non-builtin code no longer has unrestricted access to RC helper
  symbols.

### 1.2 Define allowed call-site categories in code

Encode the allowed call-site classes explicitly in code comments/module headers:

- `builtin/runtime internal RC`
- `explicit LIR RC execution`
- `forbidden ordinary path`

Actions:

- Add short invariant headers to:
  - `src/eval/interpreter.zig`
  - `src/backend/dev/LirCodeGen.zig`
  - `src/backend/wasm/WasmCodeGen.zig`
  - `src/lir/Ownership.zig`
  - `src/lir/RcInsert.zig`
- Document exactly which functions are allowed to trigger RC and why.
- Mark the intended single RC choke points in each backend/interpreter.

Required output:

- No ambiguity remains about which functions are supposed to be the only legal
  RC execution/lowering entry points.

### 1.3 Introduce "forbidden import/use" enforcement scaffolding

Add source-level enforcement machinery before changing semantics.

Actions:

- Add a repo-local lint/check script that scans for forbidden patterns outside
  allowlisted files/functions.
- Add a checked-in allowlist with explicit, narrow exceptions.
- Scan for at least these classes:
  - direct RC helper calls from ordinary interpreter/backend code
  - `performRc` use outside explicit RC stmt execution
  - backend RC emission helpers used outside explicit RC stmt lowering or
    builtin lowering
  - `containsRefcounted`-style ownership decisions outside allowed RC planning
    layers

Important:

- The lint must be structural, not aspirational.
- The allowlist must be small and specific.
- Every allowlist entry must name the file and reason.

Required output:

- A deterministic boundary-enforcement script exists in the repo.
- The script is ready to fail CI later, even though it will not be run yet.

### 1.4 Add debug-only invariant hooks

Add assertion hooks that will later be enabled once correctness work begins.

Actions:

- Introduce debug-only assertion utilities for:
  - forbidden ordinary-path RC execution
  - forbidden backend RC emission outside explicit RC stmt lowering
  - forbidden interpreter-side RC outside explicit RC stmt handlers
- Wire these utilities into the designated choke points, but do not start using
  `zig` yet.
- Ensure the intended release behavior is:
  - panic in debug
  - `unreachable` in release

Required output:

- The repo contains debug-only invariant machinery ready to activate once
  correctness work starts.

### 1.5 Add structural test scaffolding without running it

Set up the future tests now, before semantics migration.

Actions:

- Add the files/harness hooks for:
  - source-grep invariant tests
  - structural LIR ownership tests
  - backend/interpreter boundary tests
- Stub the test files with clear TODO sections indicating what each suite will
  prove.
- Do not run anything yet.

Required output:

- The test scaffolding exists in the tree, even if temporarily incomplete.

## Phase 2: Enforce the Boundary in Source (Still No `zig`)

Objective:

- Finish every source-level enforcement mechanism before semantics migration.

`zig` policy:

- Still forbidden.

Milestone commits:

- Commit after each subphase below.

### 2.1 Replace ordinary RC helper access with choke-point wrappers

Actions:

- Replace direct ordinary-path calls with named choke-point wrappers.
- Make it impossible to accidentally add a new direct RC call without touching
  the choke-point modules.
- Ensure the wrappers clearly distinguish:
  - explicit RC stmt execution
  - builtin/runtime helper invocation

Required output:

- RC-capable ordinary code paths route through explicit choke points only.

### 2.2 Add and finalize static grep/lint rules

Actions:

- Finalize the grep/lint rules introduced in Phase 1.
- Cover all currently known violation shapes in:
  - interpreter
  - dev backend
  - wasm backend
- Add rules for newly created choke-point APIs so only allowlisted call sites
  may use them.

Required output:

- The source-level lint rules are complete enough that later migration work
  cannot regress the boundary silently.

### 2.3 Remove obviously forbidden ordinary-path RC calls where the source
boundary alone suffices

Actions:

- For code paths that are unambiguously wrong and mechanically removable without
  semantic redesign, move them behind the new choke points or delete them.
- Do not try to make the compiler work yet.
- This is still setup, not correctness.

Required output:

- The tree is aligned with the intended boundary even if temporarily broken.

### 2.4 Freeze the enforcement phase

This phase is complete only when all of the following are true in source:

- module/API boundaries exist
- narrow RC choke points exist
- lint/grep enforcement exists
- debug-only invariant hooks exist
- structural test scaffolding exists
- no additional enforcement mechanism remains to be added later

Only after that is Phase 3 allowed to begin.

## Phase 3: Centralize Ownership Semantics in LIR (Still No `zig`)

Objective:

- Move ownership semantics into LIR completely, but still do it without trying
  to make the build work yet.
- This phase is about representation and pipeline design, not feedback-driven
  debugging.

`zig` policy:

- Still forbidden.

Milestone commits:

- Commit after each subphase below.

### 3.1 Audit every remaining non-builtin ownership decision site

Build a complete migration inventory covering:

- interpreter ownership decisions
- dev backend ownership decisions
- wasm backend ownership decisions

For each site, record:

- what ownership action it performs
- what earlier fact it is compensating for
- what LIR statement/result semantics should encode that fact instead
- which future explicit RC stmt should replace it

Required output:

- A precise migration table for every known remaining violation site.

### 3.2 Make LIR result semantics complete enough to express all ownership facts

Actions:

- Review and extend `LIR.ResultSemantics`, `ProcResultContract`,
  projection/borrow tracking, and low-level op ownership metadata.
- Ensure every value-producing stmt can be classified without backend help:
  - fresh owner
  - alias
  - borrow
  - projected alias/borrow
- Ensure join points, loop-carried locals, projections, indirect calls, and
  method/desugared call shapes are all representable.

Required output:

- LIR is expressive enough to carry every non-builtin ownership fact.

### 3.3 Make low-level ops declare ownership centrally

Actions:

- Move all low-level ownership declarations into the LIR/low-level definition
  layer.
- Ensure each low-level op explicitly states:
  - arg ownership modes
  - alias/borrow/fresh result semantics
  - borrowed-arg-retained-by-result behavior where needed

Required output:

- No backend/interpreter is responsible for inventing low-level ownership
  policy.

### 3.4 Redesign RC insertion to be the sole ownership-effect materializer

Actions:

- Expand `src/lir/RcInsert.zig` to be the only non-builtin pass that materializes
  ownership transitions.
- Ensure the design covers:
  - last use
  - ownership passed into fresh aggregates
  - ownership retained by produced results
  - projections and joins
  - loops and early returns
  - indirect calls and polymorphic cases
- If current LIR is missing information, add it there rather than planning any
  backend workaround.

Required output:

- A source design where every future retain/release/free comes from `RcInsert`.

### 3.5 Delete semantic ownership policy from backend/interpreter design

This is still pre-correctness source work. Update the code structure so the
semantic ownership policy is plainly absent from:

- interpreter ordinary evaluation paths
- dev ordinary lowering paths
- wasm ordinary lowering paths

The code may still be uncompilable here. That is acceptable.

Required output:

- The design shape in source reflects the intended end state before any feedback
  loop begins.

## Phase 4: Correctness Phase Begins (Now `zig` Is Allowed)

Objective:

- Now that the invariants and design boundaries are in place, begin making the
  system actually work.

`zig` policy:

- Allowed starting here.

Milestone commits:

- Commit after each subphase below.

### 4.1 Make the tree compile again

Actions:

- Repair any breakage introduced during Phases 1-3.
- Do not relax invariants to get the build green.
- If a boundary is too strict for existing code, fix the code, not the
  invariant.

Verification:

- Start with the smallest targeted builds needed to regain compilation.

### 4.2 Activate and satisfy the structural enforcement checks

Actions:

- Run the new lint/grep boundary checks.
- Make them pass without broadening the allowlist improperly.
- If a violation appears, fix the offending code path rather than muting the
  check.

Required output:

- The enforcement suite is now live and green.

### 4.3 Make interpreter obey the invariant completely

Actions:

- Eliminate ordinary-path interpreter RC decisions.
- Keep builtin/runtime helper internal RC intact.
- Make all interpreter ownership side effects come from explicit RC stmts only.

Verification:

- Add/update focused interpreter regression tests for:
  - boxes
  - lists
  - structs/tags
  - projections
  - coercions
  - loops
  - early returns
  - boxed lambdas

### 4.4 Make dev backend obey the invariant completely

Actions:

- Eliminate ordinary-path dev RC emission.
- Keep builtin/runtime helper usage only where builtin ABI requires it.
- Ensure ordinary statement lowering only emits RC for explicit LIR RC stmts.

Verification:

- Run targeted eval/dev and CLI/dev cases that exercise ownership-sensitive
  programs.

### 4.5 Make wasm backend obey the invariant completely

Actions:

- Eliminate ordinary-path wasm RC emission.
- Keep builtin/runtime helper semantics only for builtin calls.
- Ensure wasm ordinary lowering only emits RC for explicit LIR RC stmts.

Verification:

- Run targeted eval/wasm cases mirroring dev/interpreter ownership-sensitive
  coverage.

## Phase 5: Correctness Expansion and Regression Coverage

Objective:

- Once the invariant is actually implemented, prove it across the pipeline.

Milestone commits:

- Commit after each coherent test/fix group.

### 5.1 Structural LIR tests

Add tests that verify ownership is explicit in LIR for representative source
programs:

- boxed lambda helper chains
- record/tag projections
- loop-carried aggregates
- early return in method argument evaluation
- polymorphic closure passing
- list/box nesting

These tests should inspect LIR artifacts directly and confirm expected explicit
`incref` / `decref` / `free` placement.

### 5.2 Boundary enforcement tests

Turn the Phase 1/2 scaffolding into real test coverage:

- static grep boundary test passes
- no forbidden RC helper use outside allowlisted locations
- no forbidden backend/interpreter ownership reasoning patterns remain

### 5.3 Semantic regression tests

Run and fix:

- `zig build test-eval`
- `zig build test-repl`
- `zig build test-cli`
- any focused glue/host tests impacted by ownership

Priority regression buckets:

- boxed lambdas through helper chains
- boxed lambdas through host boundaries
- list/box/tag nesting
- method argument early return
- loops with mutable accumulators
- polymorphic higher-order closure tests
- CLI cases that previously failed due to ownership mismatches

### 5.4 Differential backend parity checks

Ensure interpreter/dev/wasm agree on ownership-sensitive programs and outputs.

Any disagreement is treated as a bug in LIR ownership semantics or RC insertion,
not as a reason to add backend-specific behavior.

## Phase 6: Deletion and Lock-In

Objective:

- Remove all remnants of the old mixed-ownership world.

### 6.1 Delete obsolete ordinary-path RC logic

Remove:

- interpreter-side ordinary-path `performRc` decisions
- dev-side ordinary-path RC helper emission
- wasm-side ordinary-path RC emission
- stale comments implying backends may infer ownership
- stale allowlist entries no longer needed

### 6.2 Tighten allowlists to near-zero

After the migration:

- shrink the boundary allowlist as much as possible
- keep only genuinely necessary builtin/runtime exceptions

### 6.3 Final audit

Do a fresh audit specifically for:

- backend/interpreter ownership reasoning outside explicit RC stmts
- refcounted-layout conditional logic outside allowed planning layers
- hidden retains/releases in non-builtin code paths
- duplicated ownership policy across execution backends

The audit is complete only when no such remnants remain.

## Verification Matrix

The full migration is not done until all of the following are true:

1. Source-level boundary lints pass.
2. Structural LIR ownership tests pass.
3. Interpreter performs no non-builtin ownership reasoning outside explicit RC
   stmt execution.
4. Dev backend performs no non-builtin ownership reasoning outside explicit RC
   stmt lowering.
5. Wasm backend performs no non-builtin ownership reasoning outside explicit RC
   stmt lowering.
6. Builtin/runtime internal RC remains intact and is the only allowed
   exception.
7. `zig build test-eval` passes.
8. `zig build test-repl` passes.
9. `zig build test-cli` passes.
10. Final audit finds no trace of the old mixed model.

## Definition of Done

Done means:

- Outside builtin/runtime implementations, **all ownership semantics and all
  non-builtin RC effects originate in LIR and nowhere else**.
- `interpreter`, `dev`, and `wasm` are reduced to dumb consumers of explicit
  LIR RC statements plus builtin/runtime calls.
- The boundary is enforced structurally, linted, asserted in debug, and covered
  by tests.
- The old mixed ownership model has been deleted completely.
