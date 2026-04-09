## Final Audit Cleanup Plan

Status: completed

Goal:
- Eliminate every remaining non-TODO audit finding in the active pipeline so the next fresh audit finds none of them.
- Remove the issue at its source instead of papering over it.
- Leave no dead APIs, dead symbols, dead exports, dead enum entries, or dead helper paths behind.

Design bar:
- Long-term ideal and perfect is the goal.
- Correctness and explicit-fact ownership come first.
- Runtime performance matters just as much as architecture.
- No heuristics, no workarounds, no fallbacks, no “good enough for now” cleanup.
- Implementation time does not matter; only the long-term ideal end state matters.

Reference model:
- When in doubt, use `~/code/cor` as the architectural guide for how stage boundaries and explicit facts should work.
- If this compiler intentionally does something different from `cor`, that difference must be justified as the long-term ideal design for this compiler rather than a convenience.
- At the end of implementation, explicitly report every place where the final design still differs from `cor`, with the reason that difference is long-term ideal here.

Testing rule:
- Every phase below must add or update tests that specifically prove the cleanup worked.
- The goal is not just “existing tests still pass”; the goal is also “new targeted tests would fail before this cleanup and pass after it.”

Files currently implicated:
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/layout_facts.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/builtins/dev_wrappers.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/builtins/static_lib.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/object_reader.zig`

## Phase 1: Eliminate Monotype Freeze-Time Reification

Problem:
- Monotype still reifies solved vars into monotype types during freeze/publication in `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig`.
- This is no longer late fallback logic, but it is still a stage-internal solved-var -> monotype-type reconstruction step.

Current audit sites:
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:1615`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:1625`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:1659`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:6981`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:8814`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig:8820`

Ideal end state:
- Monotype lowering consumes a fully settled explicit monotype-facts artifact.
- The artifact already contains the monotype types needed by later monotype lowering code.
- Freeze/publication does not lower solved vars into monotype types on demand.
- `lowerInstantiatedType(...)` is not used by monotype fact publication or lowering.

Implementation steps:
- Identify every fact category currently frozen from solved vars in monotype:
  - expr types
  - pattern types
  - source types
  - call types
  - function arg/ret types
- Move ownership of those typed facts to the point where the specialization is considered settled.
- Introduce or refine a dedicated finalized monotype-facts artifact that is complete before ordinary monotype lowering consumes it.
- Change all monotype lowering consumers to read only from that finalized artifact.
- Delete the freeze/publication reification helpers and any remaining call sites.
- Remove any now-dead data plumbing used only to support the old freeze-time reification path.

Validation:
- Full compiler tests stay green.
- A fresh audit should find no solved-var -> monotype-type publication path in monotype.
- No remaining relevant uses of `lowerInstantiatedType(...)` remain in `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/monotype/lower.zig`.
- Add or update targeted tests that prove monotype consumes already-finalized typed facts rather than reifying them during lowering/publication.

Completion criteria:
- The six audit sites above are gone or no longer perform solved-var -> monotype reification.
- No replacement helper exists elsewhere doing the same work under a new name.
- Monotype has one explicit finalized typed-facts boundary and consumes only that.

Completed:
- Ordinary monotype typed-fact freezing no longer lowers solved vars independently at each expr/pattern/call/function/arithmetic fact site.
- Those consumers now read one explicit settled var-type artifact first.

## Phase 2: Eliminate Lambdamono Runtime-Representation Structural Comparison

Problem:
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/layout_facts.zig` still computes runtime-representation equivalence classes by structurally comparing layouts during publication.

Current audit sites:
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/layout_facts.zig:274`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/layout_facts.zig:292`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/layout_facts.zig:303`

Ideal end state:
- Runtime-representation class identity is explicit, not derived by structural comparison.
- `lambdamono` publishes explicit representation-class facts from earlier explicit layout construction facts.
- No structural “same runtime representation?” comparison logic remains in `layout_facts.zig`.

Implementation steps:
- Trace where runtime-representation class identity is actually needed downstream.
- Introduce an explicit representation-class identity or bridge-class fact at the point where executable layout facts are finalized.
- Make class identity flow as explicit data instead of being recomputed by shape comparison.
- Remove structural comparison helpers and their call sites from `layout_facts.zig`.
- Remove any now-dead helper types or caches that existed only to support that comparison pass.

Validation:
- Existing layout-sensitive tests stay green.
- Boxed erased-callable tests still pass.
- No structural runtime-representation comparison helper remains in `layout_facts.zig`.
- Add or update targeted tests that force the affected runtime-representation-class paths and prove the explicit class facts are sufficient without structural comparison.

Completion criteria:
- The three audit sites above are gone.
- There is no remaining “same runtime representation” comparison logic in `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/layout_facts.zig`.
- Runtime-representation class identity is explicit stage data, not recovered by comparison.

Completed:
- The runtime-representation equivalence-class pass was deleted.
- `FromIr` now uses already-committed physical element layouts directly for the remaining list reinterpret decision.

## Phase 3: Eliminate Interpreter Boxed-List Layout Peeling

Problem:
- The interpreter still peels boxed-list layouts recursively at runtime in `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig`.

Current audit site:
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:2155`

Ideal end state:
- The interpreter receives explicit resolved list-layout facts.
- Runtime execution does not inspect nested layout shape to rediscover list element/storage layout.
- Boxed-list handling consumes explicit pre-attached facts only.

Implementation steps:
- Trace all interpreter sites that currently depend on `resolveListLayout(...)` or equivalent layout peeling.
- Add the missing explicit list-layout fact to the earliest stage that already knows it precisely.
- Thread that fact through IR/LIR/execution metadata until the interpreter can consume it directly.
- Replace interpreter runtime peeling with direct use of the attached list-layout fact.
- Delete `resolveListLayout(...)` and any now-dead recursive peeling helpers.

Validation:
- List tests, boxed list tests, and boxed erased-callable tests stay green.
- No runtime recursive list-layout peeling remains in the interpreter.
- Add or update targeted tests that execute the affected boxed-list operations and prove the interpreter consumes explicit resolved list-layout facts.

Completion criteria:
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig:2155` is gone.
- No equivalent recursive boxed-list layout peeling logic remains elsewhere in the interpreter.
- Execution consumes only explicit resolved list-layout facts.

Completed:
- The interpreter runtime peeling helper was deleted.
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/layout/store.zig` now records explicit resolved list-layout facts that execution consumes directly.

## Phase 4: Remove Every Trace of the Old Safe-Append Cheating Path

Problem:
- Dead residue from the old `list_append_safe` cheating path still exists in the code base even though active lowering no longer uses it.

Current audit sites:
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/builtins/dev_wrappers.zig:1036`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/builtins/static_lib.zig:106`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:142`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/LirCodeGen.zig:233`
- `/Users/rtfeldman/.codex/worktrees/1d55/roc/src/backend/dev/object_reader.zig:889`

Ideal end state:
- The safe-append cheating path does not exist anywhere in the repo.
- There is no safe append wrapper, export, symbol entry, enum entry, object-reader entry, or dead comment residue related to that path.

Implementation steps:
- Delete the dead safe append wrapper from `dev_wrappers.zig`.
- Remove the corresponding export from `static_lib.zig`.
- Remove every corresponding builtin enum/mapping entry from `LirCodeGen.zig`.
- Remove the corresponding object-reader symbol table entry from `object_reader.zig`.
- Search the repo for all remaining `list_append_safe` and `roc_builtins_list_append_safe` references and delete them.
- Remove any comments that mention the old cheating path.

Validation:
- Dev backend tests stay green.
- Static lib still builds.
- Object reader still resolves the remaining builtin symbol set correctly.
- Add or update targeted tests that ensure only the unsafe append symbol path exists and that no safe append residue remains reachable.

Completion criteria:
- `rg "list_append_safe|roc_builtins_list_append_safe" /Users/rtfeldman/.codex/worktrees/1d55/roc/src` returns no matches.
- There is no dead residue of the old cheating path anywhere in the active source tree.

Completed:
- The dead safe-append wrapper, export, backend enum mapping, and object-reader symbol entry were deleted.
- The source tree no longer contains `list_append_safe` or `roc_builtins_list_append_safe`.

## Phase 5: Final Audit And Lock-In

Goal:
- Prove that all four current non-TODO audit findings are gone, with no renamed residue.

Steps:
- Run targeted searches for:
  - `lowerInstantiatedType`
  - runtime-representation comparison helpers in `layout_facts.zig`
  - interpreter list-layout peeling helpers
  - `list_append_safe`
- Re-run the full relevant test suites.
- Perform a fresh from-scratch audit of the active pipeline:
  - reinfer
  - reintern
  - redoing work
  - hacks
  - shortcuts
  - fallbacks
- Update this file to mark every phase complete only after the code and audit both confirm the issue is actually gone.

Required verification:
- `zig build test-eval -- --threads 1`
- `zig build test-eval-host-effects -- --threads 1`
- `zig build test-cor-pipeline`

Required targeted verification:
- Targeted searches that prove the old code paths are gone:
  - `rg "list_append_safe|roc_builtins_list_append_safe" /Users/rtfeldman/.codex/worktrees/1d55/roc/src`
  - `rg "resolveListLayout" /Users/rtfeldman/.codex/worktrees/1d55/roc/src/eval/interpreter.zig`
  - `rg "same runtime representation|runtime representation comparison|refsHaveSameRuntimeRepresentation" /Users/rtfeldman/.codex/worktrees/1d55/roc/src/lambdamono/layout_facts.zig`
- Any additional targeted test commands added during implementation for the four cleanup areas.

Success condition:
- The next fresh audit does not find any of the four issues listed above.
- There is no trace of their old implementation paths left in the code base.
- The final implementation report explicitly lists any intentional divergences from `~/code/cor`, with the justification for why each divergence is long-term ideal here.

Completed:
- Verification ran successfully:
  - `zig build test-eval -- --threads 1`
  - `zig build test-eval-host-effects -- --threads 1`
  - `zig build test-cor-pipeline`
