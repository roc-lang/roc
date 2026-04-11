## Eval Test Port Plan (Origin Main -> New Eval Style)

Status: complete (implemented and verified)

Goal: port every remaining eval test from `origin/main` into the new eval test
style on this branch, and make every ported test pass. This includes replacing
any `TODO implement` panics with real implementations. The plan prioritizes
long-term ideal architecture and performance over short-term expedience.

Constraints:
- No workarounds, heuristics, or fallbacks outside parsing/error reporting.
- Every stage must consume explicit outputs from earlier stages (no recovery).
- Backends must remain dumb; they only follow LIR `incref/decref`.
- When in doubt, use `~/code/cor` as an architectural guide. If we diverge, we
  must justify why it is long-term ideal.

Reference: missing tests audit summary from `origin/main` (378 tests).
Missing buckets:
- `eval_test.zig` (1)
- `interpreter_polymorphism_test.zig` (3)
- `interpreter_style_test.zig` (94)
- `low_level_interp_test.zig` (266)
- `mono_emit_test.zig` (14)

## Phase 1: Inventory + Categorize

1.1 Build a definitive list of missing tests with names and source snippets.
    - Capture per-file lists, include the Roc source for each test.
    - Keep this list updated as tests are ported.

1.2 Categorize missing tests by subsystem:
    - Interpreter semantics (expression tests, match, records, tags).
    - Debug/expect/crash behaviors (expect failure, dbg output).
    - Low-level builtins (Str.*, List.*, numeric ops).
    - Refcount behavior (list RC tests).
    - Mono emitter tests (emit/roundtrip/closure detection).

Output:
- A living checklist for each missing test with a target new test location.

## Phase 2: Port Tests Without New Compiler Work

2.1 Port tests that are pure semantic checks and should already pass.
    - Use new `parallel_runner.zig`/`eval_tests.zig` format.
    - Preserve original Roc source and expected output.

2.2 Port mono emitter tests to the new style (if they exist in new harness).
    - Keep behavior parity with `origin/main`.

Completion criteria:
- All missing tests are present in the new eval test files.
- Tests that fail do so only due to missing compiler/runtime behavior.

## Phase 3: Implement Missing Behavior (No TODO Panics)

For each failing test group:

3.1 Identify failure cause.
    - Find failing stage or `TODO implement` panic.
    - Determine whether the behavior belongs to interpreter, compiler, or runtime.

3.2 Design ideal fix.
    - Check `~/code/cor` and `origin/main` for the most principled approach.
    - Prefer explicit facts over reconstruction.
    - Prefer data-structure reuse over repeated traversal.

3.3 Implement and verify.
    - Update the relevant stage.
    - Add regression coverage (ported test is the regression).

Likely work areas:
- Interpreter layout/semantic handling for tags/records/tuples/lists.
- Low-level builtins mapping and evaluation semantics.
- Debug/expect/crash reporting.
- Mono emitter output parity.

## Phase 4: Low-Level Builtins Coverage

4.1 Port `low_level_interp_test.zig` cases to `eval_low_level_tests.zig`.
    - Preserve names and expected outputs.
    - Ensure consistent formatting of inspect outputs.

4.2 Implement missing low-level builtins or fix lowering/runtime gaps.
    - Follow builtins definitions from `Builtin.roc`.
    - Avoid any compiler-side special cases beyond irreducible primitives.

## Phase 5: Interpreter Semantics Coverage

5.1 Port `interpreter_style_test.zig` cases to `eval_tests.zig`.
5.2 Fix any interpreter/IR/LIR issues triggered by these tests.

## Phase 6: Polymorphism + RC Tests

6.1 Port `interpreter_polymorphism_test.zig` cases.
6.2 Ensure polymorphic behavior is correct in monotype/lambdasolved/lambdamono.
6.3 Port list refcount tests (if any gaps remain).

## Phase 7: Mono Emitter Tests

7.1 Port missing mono emitter tests.
7.2 Fix any emitter regression surfaced by those tests.

## Phase 8: Verification + Audit

8.1 Run:
- `zig build test-eval -- --threads 1`
- `zig build test-eval-host-effects -- --threads 1`
- Any targeted test filters for newly ported cases.

8.2 Audit:
- Ensure no `TODO implement` panics remain in covered paths.
- Ensure all missing tests are now present and passing.

## Definition of Done

- All missing tests are ported.
- All tests pass under the new eval test style.
- No TODO panics remain in the exercised paths.
- Any deviations from `~/code/cor` are explicitly justified as long-term ideal.
