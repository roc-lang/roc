# Eval Test Reset Plan

## Recursive Layout Architecture

Before any additional bring-up work, the compiler must follow this rule:

- recursive nominal data stays logical through `monotype`, `monotype_lifted`, `lambdasolved`, `lambdamono`, and `ir`
- recursive boxing is committed exactly once at the shared `IR -> LIR/layout` boundary
- after that commit, every backend and interpreter consumes only final explicit `box` / `tag_union` / `struct` layout facts

This means:

- no early recursive boxing in `IR` type/layout lowering
- no store-owned fallback resolver for recursive nominal boxing
- no backend-local recursive-structure recovery or repair logic
- no `RecursivePointer`-style backend-visible abstraction

The only place allowed to decide “this recursive edge becomes a box” is the shared
`LIR` layout commit path.

## Goal

Reset eval testing so that backend comparison follows one and only one observation model:

- input: raw Roc source text
- source wrapping: `Str.inspect((...))`
- compilation: the normal cor-style compiler pipeline
- execution: backend runs the compiled program
- output: raw returned `RocStr` bytes
- assertion: exact string comparison

There must be no host-side typed value readback, no host-side generic value formatting, and no secondary observation protocol for eval tests.

This plan is not complete until:

- all legacy eval-test observation machinery is gone
- the deletion has been audited and confirmed
- the new single-observation design is implemented
- eval tests pass except for cases that intentionally hit remaining `TODO` panics

## Global Rules

- Do not run Zig at all during phases 1, 2, or 3.
- Do not add workarounds, fallbacks, heuristics, or compatibility layers.
- Do not preserve any legacy eval-test infrastructure just because it already exists.
- Every backend-comparison eval test must ultimately observe only a returned `RocStr`.
- No eval test may assert on host-read raw non-string runtime values.

## Phase 1: Delete All Legacy Eval Testing Infrastructure

### Objective

Delete every eval-test path that observes results any way other than “program returns `Str`”.

### Delete These Concepts Entirely

1. Typed eval helpers that inspect native runtime values in host code.
   - Examples:
     - helpers returning `i64`, `i128`, `bool`, `f32`, `f64`, `dec`
     - focused pipeline helpers that assert on non-`Str` values

2. Generic host-side eval formatting for test comparison.
   - Examples:
     - `EvalValue`
     - `formatEvalValueString(...)`
     - stringification of arbitrary runtime values in the harness
     - “unsupported value formatting” panic paths for eval comparison

3. Any eval helper API that exposes both:
   - “run and return arbitrary value”
   - “run and return inspected string”
   The only remaining eval-comparison API must be the `RocStr` one.

4. Focused cor-pipeline tests that validate backend/runtime results through typed host readback.
   - Rewrite or delete them later in phase 3.
   - But first, delete the typed observation helpers they depend on.

5. Any test runner branching that distinguishes:
   - value-producing tests
   - inspect-string tests
   These must collapse to one model: compile `Str.inspect(...)`, run, compare raw string bytes.

### Files Likely In Scope

- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/helpers.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/parallel_runner.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/cor_pipeline_test.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/test/eval_tests.zig`
- any additional eval test/helper files found by audit

### Required End State For Phase 1

- no eval helper remains that exposes typed runtime-value observation for tests
- no host-side generic formatting path remains for eval test comparison
- no test runner mode remains except “compile inspected source, run backend, extract `RocStr`”

## Phase 2: Verify All Legacy Infrastructure Is Deleted

### Objective

Prove phase 1 was complete. If not complete, go back to phase 1 and continue deleting.

### Mandatory Audits

Run static audits only. Do not run Zig.

1. Search for typed eval-result helper surfaces.
   - Examples of forbidden residue:
     - `EvalValue`
     - `asI128`
     - `copyEvalValueFromValue`
     - typed `eval*Expr`
     - typed `expect*`
     - helper names ending in `Str` that are not raw-`RocStr` extraction from an inspected program

2. Search for host-side formatting / quoting infrastructure used for eval comparison.
   - Examples:
     - `formatEvalValueString`
     - `quoteString`
     - `unsupportedEvalValue`
     - generic runtime value printing / formatting in eval helpers

3. Search for multiple eval-comparison modes in the runner.
   - The runner must not distinguish “primitive value tests” from “inspect string tests”.

4. Search for focused tests still reading non-`Str` results directly from interpreter/dev backend runtime memory.

### Audit Rule

If any legacy artifact remains:

- the plan immediately returns to phase 1
- delete the remaining artifact
- repeat phase 2

Phase 2 ends only when the audit is clean.

## Phase 3: Commit, Then Implement The New Design Without Running Zig

### Objective

After the legacy system is fully deleted and audited clean, make one clean commit. Then implement the new design from scratch, still without running Zig.

### New Design

There is exactly one eval-observation protocol:

1. The harness receives raw Roc source text `expr_src`.
2. The harness constructs source:

   `main = Str.inspect((expr_src))`

3. The compiler runs its normal cor-style pipeline on that source.
4. `Str.inspect` is lowered in monotype based on the exact monomorphic type.
5. Downstream stages see only ordinary executable structure producing a `Str`.
6. Each backend executes the same lowered program.
7. The harness requires the root result layout/type to be `Str`.
8. The harness copies raw `RocStr` bytes into owned memory.
9. Tests compare those bytes exactly.

### Required Implementation Properties

1. `Str.inspect` is language-level observability, not host-side formatting.
   - No backend parity logic may inspect arbitrary runtime values in host code.

2. The harness must compile inspected source once and reuse the same lowered artifact per backend where architecture permits.
   - Parsing / checking / lowering are not to be duplicated per backend unless the backend API fundamentally forces it.

3. The helper surface must be minimal.
   - One source-compilation path for inspected programs.
   - One backend-execution path that returns raw string bytes.
   - No second helper family for typed observation.

4. Focused eval tests must also use the inspect protocol.
   - Even narrow cor-pipeline tests must observe only the final `Str`.
   - No exceptions.

5. `Str.inspect` lowering must remain in monotype.
   - It must not leak as a generic inspect node to later stages.

### Concrete Deliverables

1. New eval helper API:
   - compile inspected source
   - run lowered program expecting `Str`
   - return owned `[]u8`

2. New runner shape:
   - every backend-comparison test is an inspect-string test
   - no alternate comparison mode remains

3. Focused pipeline tests rewritten:
   - compare returned `Str`
   - no host typed result extraction

4. Clear invariants:
   - if a backend does not return `Str`, debug panic or explicit test error
   - if a caller tries to use old typed observation APIs, those APIs no longer exist

### Commit Boundary

Before phase 3 implementation:

- commit the completed phase 1+2 deletion state

After phase 3 implementation:

- commit the completed new-design state

## Phase 4: Commit, Then Proceed To Getting Tests To Pass

### Objective

After the new design is fully implemented and committed, run Zig again and bring eval tests up under the new architecture.

### Rules For This Phase

- Only now is Zig allowed.
- If a bug appears, fix it by preserving the single-observation design.
- Do not reintroduce typed host readback, generic host formatting, or legacy comparison paths.
- If a test fails because `Str.inspect` lowering is incomplete, implement the missing lowering in the compiler, not in the harness.
- If a test fails because a backend cannot yet execute the generated inspected program, fix the backend/compiler path, not the harness protocol.

### Success Criteria

Phase 4 is complete when:

- eval tests use only the inspected-string path
- all non-`TODO` eval tests pass
- the only remaining failures are explicit intentional `TODO` panics
- no legacy eval-observation artifacts were reintroduced during bring-up

## Final Completion Criteria

This plan is complete only when all of the following are true:

1. There is no typed eval-test observation infrastructure left.
2. There is no generic host-side eval value formatter left for backend parity.
3. Every eval test observes only a returned `RocStr`.
4. `Str.inspect((...))` is the sole backend-comparison protocol.
5. Phase-2 audits come back clean.
6. The deletion state was committed before rebuilding.
7. The new design state was committed before test bring-up.
8. Eval tests pass except for explicit intentional `TODO` panic cases.
