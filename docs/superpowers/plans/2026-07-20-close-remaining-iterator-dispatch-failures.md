# Close Remaining Iterator/Dispatch Failures Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Drive the "principled empty-row representation" branch to both gates green — every Zig test passes, then full `zig build minici` (62/62 phases) passes. Three pre-existing failures remain, all confirmed present on the pre-session working-copy snapshot (`2195efbd`) and therefore unfinished branch feature work, not regressions:

1. `run-test-cli`: `test/cli/DispatchCycleThreeGroupChain.roc` panics `compiler-generated method edge did not contain a dispatcher path` — a three-group dispatch chain whose back-edge produces a *pathless* (constraint-queue-reachable) evidence param that lowering cannot resolve.
2. `run-test-cli`: five derived-parser `FieldNames` tests return the wrong value (right structure) after the construction-site iterator rework regressed `Iter.next`/`FieldNames.for_size` traversal.
3. `run-test-eval-host-effects`: `rc balance: Json.parse releases runtime string input after success` hangs (killed at 30s) — same Json/iterator lowering area as (2).

**Architecture:** Static-dispatch evidence params are enumerated in canonical order by `src/check/dispatch_evidence.zig` (`enumerateEvidenceParams`), published as `EvidenceParamRecord`s by `src/check/checked_artifact.zig` (`appendEvidenceParams`), and consumed at lowering by `src/postcheck/monotype/lower.zig` (`methodTargetCalleeAtNode` → `projectEvidenceComponentNodes` / `synthesizeEvidenceAtComponentNodes`). Each param carries a semantic *path* from the scheme root to the dispatcher so compiler-generated call edges (which have no checked instantiation records) can resolve the obligation over the concrete monomorphic callable. A param whose dispatcher is reachable *only* through an earlier constraint's fn type has an **empty path** (`dispatch_evidence.zig` fn-var-queue drain, ~lines 138-141, `with_paths=false`; documented at `static_dispatch_registry.zig:935-936`). The empty-path param's dispatcher lives inside a previously-emitted param's constraint fn type, but that parent linkage and the relative path within the fn type are currently **discarded** by the enumerator and the publisher. Lowering therefore has no explicit upstream data to resolve them and raises an invariant. The principled fix (per `AGENTS.md`: consume explicit upstream data, never reconstruct/guess) is to make the checker publish the parent-param index plus the relative path from that parent's constraint fn type to the dispatcher, then have lowering resolve the parent's concrete callable target first and walk the relative path over it.

The iterator-representation rework (`minted` / `forced_dynamic` tiers, `design.md` §"SpecConstr And Loop Scalarization" ~1780-1930) is the second failure cluster: derived Json parsing iterates `FieldNames.for_size(...)` via `Iter.next`, and the wrong-value/hang symptoms point at a defect in the reworked iterator lowering rather than at dispatch evidence.

**Tech Stack:** Zig postcheck compiler pipeline, `dispatch_evidence.zig` enumerator, `CheckedArtifact` evidence serialization, Monotype `InstGraph`, Lambda Solved / LIR regression checks, jj scoped commits, token-efficient build wrappers.

---

## Commit Discipline

- The main thread owns all `jj` commands. Do not use worktrees.
- Before each task, run `jj describe -m "<message>"` on the current empty working-copy commit; implement only that task; verify it; request subagent spec/code-quality review; then `jj new`.
- If a targeted section fails, fix and rerun **that section** until it passes before returning to full MiniCI (`AGENTS.md`).
- Prefer the wrappers over raw `zig build`:
  - Test steps → `zig run .claude/zig-test-llm.zig -- <step>` (`run-test-zig`, `run-test-zig-module-postcheck`, `run-test-zig-module-check`, `run-test-zig-lir-inline`).
  - `roc` / snapshots / fmt → `zig run .claude/zig-llm.zig -- <step>`.
  - Multi-backend eval: `zig build run-test-eval --summary failures --color off` (do **not** use interpreter-only checks to declare a codegen fix done).
  - CLI/host-effects: run the specific `roc` invocation directly for fast iteration, then the wrapped step.
  - MiniCI: run **raw** `zig build minici` (or `nix shell nixpkgs#rustc nixpkgs#cargo -c zig build minici --summary failures --color off` when `rustc`/`cargo` are off `PATH`); on failure read `zig-out/minici/logs/<step>.txt`.

## Design Rules To Preserve

- No workarounds, fallbacks, or heuristics outside parsing and error reporting. Every stage consumes explicit data produced by earlier stages.
- An empty evidence path is a real published state; do not delete the `path.len == 0` guards until lowering has an explicit mechanism to resolve the pathless param (deleting them alone relocates the panic to `compiler-generated ownerless graph component had no checked structural or uninhabited evidence` at `lower.zig` ~27372, because the callable root is a `.func` with no method owner — verified during the prior session).
- Backends only follow explicit LIR `incref`/`decref`.
- Do not synthesize Roc runtime `crash` bodies for compiler-internal states.
- Iterator classification consumes the explicit `iterator_representation` field or checked `Builtin.Iter` identity; never re-derive a tier/site/depth from lowered type shape (`design.md` ~1842).
- Any change to `EvidenceParamRecord`'s layout or `ModuleEnv.Serialized` requires bumping `Constants.CACHE_VERSION` and replacing the golden bytes in `src/compile/cache_module.zig` (`MODULE_ENV_VERSION_HASH golden value` test, ~line 287-296) with the ones the assertion prints.

---

### Task 1: Baseline — Reproduce And Pin The Three Failures

**Files:**
- None (investigation + notes only).

- [ ] **Step 1: Describe the jj commit**

```bash
jj describe -m "chore: baseline remaining iterator/dispatch failures"
```

Expected: empty working copy, description set.

- [ ] **Step 2: Confirm each failure reproduces on the current working copy**

```bash
zig run .claude/zig-llm.zig -- roc
./zig-out/bin/roc --opt=interpreter --no-cache test/cli/DispatchCycleThreeGroupChain.roc
./zig-out/bin/roc test --no-cache test/cli/ParserRuntimeRenameFields.roc
```

Expected: (1) panics `compiler-generated method edge did not contain a dispatcher path` at `src/postcheck/monotype/lower.zig:~27293`; (2) the parser test reports `0 passed, 1 failed` with the `expect` block `result == Ok({ foo_bar: "runtime-renamed" })` failing (no panic).

- [ ] **Step 3: Enumerate the full failing set from a MiniCI-parity CLI run**

```bash
zig build run-test-cli --summary all --color off -- --stats-json zig-out/minici/raw/run-test-cli.json 2>&1 | tee /tmp/cli.log
```

Confirm exactly these fail: `dispatch cycle: three-group dispatch chain with late back-edge runs`, `roc check accepts expression break inside loop` (already fixed this session — verify it now passes), `roc test uses renamed FieldNames metadata in derived parser` (`ParserRenamedFieldsMetadata.roc`), `roc test uses renamed FieldNames name bounds in derived parser` (`ParserRenamedFieldBounds.roc`), `roc test supports userspace FieldNames.rename_fields` (`ParserRuntimeRenameFields.roc`), `roc test supports stored and runtime prepared parser fields` (`ParserStoredAndRuntimePreparedFields.roc`), `roc test supports top-level parser construction` (`ParserTopLevelConstructor.roc`). Record the live list; if the set differs, update Tasks 2/3 accordingly.

- [ ] **Step 4: Confirm the host-effects hang**

```bash
zig build run-test-eval-host-effects --summary all --color off 2>&1 | grep -iE 'HANG|passed|failed'
```

Expected: `1 hung` — `rc balance: Json.parse releases runtime string input after success` (`src/eval/test/host_effects_tests.zig:1053`).

- [ ] **Step 5: Split**

No file changes; do not create a commit (`jj describe` alone is fine to leave, then proceed to Task 2 without `jj new` if the working copy is still empty).

---

### Task 2: Carry The Constraint-Reachable Dispatcher As Explicit Checker Data

Make the checker publish, for every pathless (constraint-queue-reachable) evidence param, the **parent param index** whose constraint fn type contains the dispatcher, plus the **relative path** from that fn type to the dispatcher. This is the single load-bearing change; Task 3 consumes it.

**Files:**
- Modify: `src/check/dispatch_evidence.zig`
- Modify: `src/check/static_dispatch_registry.zig`
- Modify: `src/check/checked_artifact.zig`
- Modify: `src/compile/cache_module.zig` (golden bytes)

- [ ] **Step 1: Describe the jj commit**

```bash
jj describe -m "feat: publish constraint-reachable dispatcher evidence"
```

- [ ] **Step 2: Record the parent linkage and relative path in the enumerator**

In `src/check/dispatch_evidence.zig`:

- `EvidenceParam` (lines 61-76) currently drops all path info for queue-drained params (`emitConstraints` at 300-308 sets `path_start/len = 0` when `with_paths == false`). Add two fields to `EvidenceParam`: `parent: ?u32` (index, **within this scheme's param list**, of the param whose `constraint.fn_var` is the fn-var-queue entry currently being walked) and keep the *relative* path even when `with_paths == false`.
- The fn-var-queue drain (`enumerateEvidenceParams`, 138-141) walks each queued `constraint.fn_var`. Thread the *emitting param's index* into that walk so `emitConstraints` can stamp `parent` on every param it emits during that walk. The relationship is exact: the queued `fn_var` at queue position `q` was appended by `emitConstraints` for the param at some known `out` index (record `(fn_var → emitting_param_index)` when you append to `fn_var_queue` at line 307).
- Change `emitConstraints` (290-309) and `walk` (149-228) so that during a queue-drain walk the path IS accumulated (relative to the fn-var root), and stamp `parent` = the emitting param's index. Root-walk params (the initial `walk(root, true, ...)` at line 132) keep `parent = null` and their absolute path.
- Preserve the existing canonical order and the "parents precede children" guarantee (the queue drains in emission order, so a parent param is always emitted before the children found inside its constraint fn type — assert this in a debug check).

- [ ] **Step 3: Extend the published record**

In `src/check/static_dispatch_registry.zig`, extend `EvidenceParamRecord` (937-943) with the new explicit data, e.g.:

```zig
pub const EvidenceParamRecord = struct {
    method: canonical.MethodNameId,
    structural: ?StructuralKind = null,
    path: artifact_serialize.Span = .{},
    /// Index (within this scheme's param range) of the param whose constraint
    /// fn type contains this dispatcher, when `path` is empty. `path` is then
    /// the relative path from that constraint fn type to the dispatcher.
    /// `null` for root-reachable params (absolute `path`).
    parent: ?u32 = null,
};
```

Update the doc block (928-936) to state that a pathless *root* obligation is now published with `parent` set and a **relative** `path`, so lowering no longer sees a truly empty, unresolvable path. Keep `EvidencePathStep` unchanged.

- [ ] **Step 4: Publish the new fields**

In `src/check/checked_artifact.zig`:

- `appendEvidenceParams` (13667-13688) currently maps `EvidenceParam → EvidenceParamRecord` and drops `dispatcher_var`/parent info. Publish the new `parent` field and the now-populated relative `path` (the path interning loop at 13672-13680 already handles `record_field`/`tag_payload_tag` ident conversion — reuse it for relative paths).
- Update `evidence_params_pool` serialization: the `SerializedSlice(EvidenceParamRecord)` at `CheckedProcedureTemplateTable` (~15029, 15042) is a flat POD copy, so the added `?u32` rides along automatically, but confirm the struct stays `extern`-safe / POD (it is a plain struct; if adding `?u32` breaks the serialized-slice assumptions, store `parent` as a `u32` sentinel `maxInt(u32)` for "none" and document it).
- The verifier over `evidence_params_pool` (~24419-24423) must be extended to bounds-check `parent` (< the param's own index within its scheme range) so a corrupt artifact can't point a child past its parent.

- [ ] **Step 5: Bump the cache version and golden bytes**

The `EvidenceParamRecord` layout changed, which flows into `ModuleEnv.Serialized`. In `src/compile/cache_module.zig` bump `Constants.CACHE_VERSION` and replace the `golden` byte array (test at ~287-296) with the value the failing assertion prints.

- [ ] **Step 6: Verify checker + serialization**

```bash
zig run .claude/zig-test-llm.zig -- run-test-zig-module-check
zig run .claude/zig-test-llm.zig -- run-test-zig-module-postcheck
```

Expected: both pass (the postcheck lowering guards still fire for now — Task 3 removes them). Fix any serialization/round-trip test that the layout change breaks.

- [ ] **Step 7: Review and split**

Request spec + code-quality review. After approval: `jj new`.

---

### Task 3: Resolve Constraint-Reachable Dispatchers At Lowering

Consume Task 2's `parent` + relative path so `.synthesize` evidence resolves a pathless param by walking the relative path over its parent param's already-resolved concrete callable, instead of raising an invariant.

**Files:**
- Modify: `src/postcheck/monotype/lower.zig`
- Modify: `src/postcheck/structural_test.zig` (guards)

- [ ] **Step 1: Describe the jj commit**

```bash
jj describe -m "fix: resolve constraint-reachable dispatch evidence"
```

- [ ] **Step 2: Resolve pathless params from their parent component**

In `src/postcheck/monotype/lower.zig`:

- `projectEvidenceComponentNodes` (27282-27300): replace the `if (path.len == 0) invariant(...)` guard (27292-27294). Because Task 2 guarantees parents precede children, resolve params in order into `out[]`; for a param with `parent` set, start the walk at `out[param.parent]` (the parent's already-projected component node — the constraint fn type's concrete graph node) and walk the relative `path` with `walkEvidencePathNode` (27546). For a root param (`parent == null`) keep the current behavior: walk the absolute path over `target_root_node`.
- `synthesizeParamsEvidence` (27254-27276): apply the identical parent-relative logic to the durable-type twin, walking `walkEvidencePath` (27448) from the parent's `component_ty`. Remove its `path.len == 0` invariant (27267-27269).
- `synthesizeEvidenceAtComponentNodes` (27302-27324) and `synthesizeComponentEvidenceAtNode` (27346-27373) then receive a real component node (a named/nominal/builtin owner reachable through the parent's constraint fn type), so `methodOwnerFromNode` succeeds and the `ownerless graph component` invariant (27372) no longer fires. Confirm the parent's component node is itself a *resolved* dispatcher-owning type at the point the child is resolved (it must be — the parent obligation was synthesized just before). If the parent component is a `.func` (the constraint fn type itself), walk into it via the child's relative `fn_arg`/`fn_ret` steps to reach the owning type.

- [ ] **Step 3: Verify the dispatch cycle end-to-end**

```bash
zig run .claude/zig-llm.zig -- roc
./zig-out/bin/roc --opt=interpreter --no-cache test/cli/DispatchCycleThreeGroupChain.roc && echo OK
zig build run-test-eval --summary failures --color off -- --test-filter "dispatch"
```

Expected: the interpreter run exits success (the app computes `result == 11` and returns `Ok({})`, so `crash "…wrong value"` is never hit); no `invariant violated` / `trying to add var at rank` / `panic` on stderr. Run the compiled backends via `run-test-eval` too — this is a codegen path, not interpreter-only.

- [ ] **Step 4: Replace the retired lowering guards with structural guards**

The two `path.len == 0` invariants are gone. In `src/postcheck/structural_test.zig` add guards, in this same commit, asserting the parent-relative resolution is present and the old dead invariants are not:

```zig
test "Monotype resolves constraint-reachable dispatch evidence" {
    const lower_source = @embedFile("monotype/lower.zig");
    try expectNotContains(lower_source, "compiler-generated method edge did not contain a dispatcher path");
    try expectNotContains(lower_source, "synthesized evidence param's dispatcher has no path over the scheme's callable");
    try expectContains(lower_source, "param.parent");
}
```

Adjust the exact wording to match the field/name you chose in Task 2.

- [ ] **Step 5: Verify the whole CLI dispatch group**

```bash
zig build run-test-cli --summary all --color off -- --stats-json zig-out/minici/raw/run-test-cli.json 2>&1 | grep -iE 'dispatch|passed|failed'
```

Expected: the dispatch-cycle test passes; no other dispatch test regressed.

- [ ] **Step 6: Review and split**

Request spec + code-quality review. After approval: `jj new`.

---

### Task 4: Localize The Derived-Parser FieldNames Regression

The five parser tests return the wrong value (right structure). They pass on committed parent `f55f1439` and fail on the reworked working copy, so the construction-site iterator changes regressed derived parsing that iterates `FieldNames` via `Iter.next` / `FieldNames.for_size`. This task is **diagnosis**; Task 5 fixes.

**Files:**
- None (instrumentation only — revert all instrumentation before ending the task).

- [ ] **Step 1: Describe the jj commit**

```bash
jj describe -m "chore: localize derived-parser FieldNames regression"
```

- [ ] **Step 2: Read the failing behavior**

`test/cli/ParserRuntimeRenameFields.roc`: `find_field` (its body) runs `var $remaining = Encoding.FieldName.FieldNames.for_size(fields, …)` then a `while True { match Iter.next($remaining) { One … Skip … Done … } }`. The `expect` block wants `Ok({ foo_bar: "runtime-renamed" })`. Determine what it actually returns (add a `dbg` in the roc source, or run under `--opt=interpreter` and print). Establish whether the wrong value is: (a) the renamed field never matches (`find_field` returns `Err(NotFound)` → `Ok(Done(...))`), or (b) the field matches but the payload string is wrong. This bisects "iterator loses the renamed metadata" vs "string payload mislowered".

- [ ] **Step 3: Diff the iterator lowering against the committed parent**

Because there are no intermediate commits, use the sibling `roc-2` jj workspace (or a fresh checkout of `f55f1439`) to compare lowering output:

```bash
# In the parent checkout: dump the lowered LIR for find_field / the parser.
# In the working copy: same dump. Diff the two.
```

Focus the diff on `src/postcheck/monotype/lower.zig` iterator paths (`lowerIteratorFor`, `forcedDynamicIteratorType`, `generatedIteratorBackingType`, the `iterator_representation` selection ~16060-16330) and `src/postcheck/monotype_lifted/spec_constr.zig` loop/iterator scalarization (`scalarizeKnownLoops`, `cloneLoopInPlace`, `loopInitialIsOwnedConstruction`). The regression is a wrong tier decision (`minted` vs `forced_dynamic`), a wrong backing rewrite, or a scalarized loop that drops the renamed-field carrier.

- [ ] **Step 4: Instrument the iterator tier decision for this program**

Add temporary `std.debug.print` at the `iterator_representation` selection and at `Iter.next` lowering to log, for `FieldNames.for_size`'s iterator, the chosen tier, item type digest, and whether the renamed-name field survives into the step result. Compare working copy vs parent. Identify the exact divergent decision.

- [ ] **Step 5: Record the root cause; revert instrumentation**

Write the precise divergence (file, function, which explicit upstream datum is misread or dropped) into the task notes for Task 5. Remove **all** instrumentation:

```bash
grep -rn 'std.debug.print' src/postcheck/ | grep -v '.output'
```

Expected: no matches introduced by this task.

- [ ] **Step 6: Split**

If no product files changed, do not create a commit; proceed to Task 5.

---

### Task 5: Fix The Derived-Parser Iterator Lowering

Apply the principled fix Task 4 localized: the iterator lowering must consume the explicit checked construction-site / representation evidence and carry the renamed `FieldNames` metadata through the `Iter.next` loop unchanged.

**Files:**
- Modify: the file Task 4 identified (expected: `src/postcheck/monotype/lower.zig` and/or `src/postcheck/monotype_lifted/spec_constr.zig`).
- Modify: `src/postcheck/structural_test.zig` if a guard is warranted.

- [ ] **Step 1: Describe the jj commit**

```bash
jj describe -m "fix: preserve renamed FieldNames through iterator lowering"
```

- [ ] **Step 2: Apply the localized fix**

Consume the explicit upstream datum (the checked construction-site digest / representation tier / field metadata) rather than reconstructing it. No fallback or heuristic — if the loop carrier's shape is ambiguous, the fix is to thread the checker-authored evidence, not to guess.

- [ ] **Step 3: Verify all five parser tests**

```bash
zig run .claude/zig-llm.zig -- roc
for f in ParserRuntimeRenameFields ParserRenamedFieldsMetadata ParserRenamedFieldBounds ParserStoredAndRuntimePreparedFields ParserTopLevelConstructor; do
  echo "== $f =="; ./zig-out/bin/roc test --no-cache test/cli/$f.roc 2>&1 | grep -iE 'passed|failed|panic'
done
```

Expected: each reports all tests passed, no panic.

- [ ] **Step 4: Verify no iterator/eval regression**

```bash
zig run .claude/zig-test-llm.zig -- run-test-zig-lir-inline
zig build run-test-eval --summary failures --color off
```

Expected: `run-test-zig-lir-inline` OK; `run-test-eval` shows the previously-hung `Json.parse releases runtime string input after success` now resolved *or* still hung (Task 6 handles the hang if it persists), with no new failures/crashes.

- [ ] **Step 5: Review and split**

Request spec + code-quality review. After approval: `jj new`.

---

### Task 6: Resolve The Host-Effects Hang (If Not Already Fixed)

`rc balance: Json.parse releases runtime string input after success` (`src/eval/test/host_effects_tests.zig:1053`) hung at 30s in the same Json/iterator area. Fixing Task 5 may resolve it; confirm, and if not, isolate the loop.

**Files:**
- Modify only if the hang survives Task 5.

- [ ] **Step 1: Re-check the hang after Task 5**

```bash
zig build run-test-eval-host-effects --summary all --color off 2>&1 | grep -iE 'HANG|passed|failed'
```

If it passes, skip to Step 4.

- [ ] **Step 2: Describe the jj commit (only if a fix is needed)**

```bash
jj describe -m "fix: terminate Json.parse runtime-string iterator loop"
```

- [ ] **Step 3: Isolate the non-terminating loop**

Determine which backend hangs (interpreter vs dev vs wasm) and whether the loop is in postcheck lowering (an unbounded specialization/inline in `spec_constr.zig` — see `design.md` bounded-walk rules ~99-118) or in emitted code (an iterator `continue` edge that never reaches its break). Consume the explicit loop fixed-point evidence; do not add an iteration cap as a workaround. If the same construction-site datum from Task 5 governs termination, thread it here.

- [ ] **Step 4: Verify**

```bash
zig build run-test-eval-host-effects --summary all --color off 2>&1 | grep -iE 'HANG|passed|failed'
```

Expected: `0 hung`, all passed.

- [ ] **Step 5: Review and split (only if files changed)**

Request review; then `jj new`. If no files changed, do not create a commit.

---

### Task 7: Final Guards, Full Suite, And MiniCI

**Files:**
- Modify only files needed to satisfy existing checks (`fmt`, semantic audit, unused-suppression).

- [ ] **Step 1: Describe the jj commit if fixes are needed**

```bash
jj describe -m "test: enforce constraint-reachable dispatch invariants"
```

- [ ] **Step 2: Full Zig suite and targeted checks**

```bash
zig run .claude/zig-test-llm.zig -- run-test-zig
zig run .claude/zig-test-llm.zig -- run-test-zig-module-postcheck
zig build run-check-semantic-audit --summary failures --color off
zig build run-check-postcheck-architecture --summary failures --color off
zig build run-check-unused-suppression --summary failures --color off
zig run .claude/zig-llm.zig -- run-fmt-zig
```

Expected: all pass. Fix any semantic-audit wording violations introduced by new comments (see `ci/semantic_audit.pl` banned-term list — avoid `canonical`/`semantic`/`artifact`/`publish`/`facts`/`reconstruct`/`rebuild`/`value graph` in postcheck comments).

- [ ] **Step 3: Snapshots**

```bash
zig run .claude/zig-llm.zig -- run-snapshot-tool
```

Review the diff; a dispatch/parser behavior fix may legitimately change snapshots — confirm each change is intended.

- [ ] **Step 4: Full MiniCI**

```bash
nix shell nixpkgs#rustc nixpkgs#cargo -c zig build minici --summary failures --color off
```

Expected:

```text
MiniCI summary: 62/62 phases ran; 62 passed, 0 failed, 0 crashed, 0 skipped
```

- [ ] **Step 5: Handle failures by section**

If MiniCI fails in one section, stop using full MiniCI as the inner loop. Rerun that section (read `zig-out/minici/logs/<step>.txt`), fix, rerun that section until green, then return to full MiniCI. Note: `run-test-cli` and `run-test-eval-host-effects` need `rustc`/`cargo`; if RustGlue tests fail with `run spawn error: error.FileNotFound`, rerun through the `nix shell` wrapper.

- [ ] **Step 6: Final review**

Request final subagent review over the commit stack. Report: commit list; targeted checks; full MiniCI result; remaining risks.

---

## Risk Notes

- **Task 2/3 is the deep one.** The parent-relative resolution assumes the parent param's component node is resolved *before* its children — Task 2 must guarantee and assert this ordering. If a constraint fn type binds vars through *another* constraint (nested queue drains, e.g. `where [a.iter : a -> i, i.next : i -> …]`), `parent` chains transitively; resolve params strictly in published order so each child's parent is already in `out[]`.
- **Serialization/version hash.** Changing `EvidenceParamRecord` forces a `CACHE_VERSION` bump and golden-bytes update; missing this makes the cache test fail with a clear diff to paste back.
- **Tasks 4-6 may share one root cause.** The wrong-value parser bug and the Json hang are both in the reworked iterator lowering; fixing Task 5 may close Task 6. Keep Task 6 as a guarded follow-up rather than assuming independence.
- **Multi-backend.** The dispatch-cycle and parser fixes are codegen paths — always confirm via `run-test-eval` (interpreter + dev + wasm), never interpreter-only checks.
