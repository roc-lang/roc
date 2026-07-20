# Empty Tag Union Provenance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Preserve explicit provenance so a source-level empty tag union (`[]`) is not confused with an unconstrained Monotype slot that materialized without evidence.

**Architecture:** Keep ordinary solved empty tag unions closed and uninhabited, but mark builder-global/cache-derived Monotypes that were produced without body evidence as reopenable placeholders before they enter another specialization graph. Monotype import and Lambda Solved must consume that explicit provenance instead of inferring intent from `tag_union` with an empty span.

**Tech Stack:** Zig compiler code, Roc postcheck Monotype/Lambda Solved IR, `zig build` MiniCI sections.

---

## Context

Read `design.md` before code changes. The relevant invariant is in the Monotype specialization section around the rule:

```text
checked stage owns meaning and relations
Monotype instantiation owns monomorphic type cells
later stages consume closed Monotype types only
```

The current debt is concentrated in these files:

- `src/postcheck/monotype/solve.zig`
  - `InstNode.empty_tag_union` is a solving-time graph state.
  - `importMono` currently imports any finished zero-tag `tag_union` as unresolved row evidence.
  - `fillMono`, `GraphTypeFinals.sealContent`, and `materializeUnresolved` collapse solving-time empty/unresolved cases into the same final `Type.Content.tag_union = Type.Span.empty()`.
- `src/postcheck/lambda_solved/solve.zig`
  - `Solver.unify` currently lets any empty tag union yield to a concrete peer.
  - `isEmptyTagUnion` treats `tag_union` with zero tags as a provenance signal.
- `src/postcheck/monotype/lower.zig`
  - `Builder.unsolved_monos` already tracks Monotypes lowered without body evidence. This is the right explicit source for reopenable provenance.

Do not fix this by adding another heuristic over empty spans. The target state is that late stages only reopen/yield when explicit earlier-stage metadata says the type came from an unsolved Monotype cache entry.

---

## File Structure

- Modify: `src/postcheck/monotype/solve.zig`
  - Add explicit unsolved-import provenance through `importMono`.
  - Preserve ordinary explicit `[]` as closed `.empty_tag_union`.
  - Add unit tests that prove explicit empty tag unions do not reopen, while marked unsolved Monotypes still do.
- Modify: `src/postcheck/lambda_solved/solve.zig`
  - Remove blanket empty-tag-union yield behavior.
  - Replace it with exact unification only, or with provenance-aware metadata if Task 3 shows Lambda Solved still needs it.
- Modify: `src/postcheck/monotype/lower.zig`
  - Thread unsolved provenance into Monotype import requests if the current `unsolved_monos` map is not being used deeply enough.
  - Keep the ownership boundary in the Monotype builder; do not make backends or LIR aware of this.
- Modify tests:
  - `src/postcheck/monotype/solve.zig` for focused unit tests.
  - `src/postcheck/lambda_solved/solve.zig` for focused unit tests if the file already has local test helpers; otherwise use existing eval/CLI coverage.
  - `src/eval/test/eval_tests.zig` and `src/cli/test/parallel_cli_runner.zig` only for end-to-end regression coverage that cannot be expressed locally.

---

### Task 1: Baseline And Pin Current Debt With Failing Tests

**Files:**
- Modify: `src/postcheck/monotype/solve.zig`
- Test: `src/postcheck/monotype/solve.zig`

- [ ] **Step 1: Read the current implementation sites**

Run:

```bash
sed -n '2510,2565p' design.md
sed -n '120,230p' src/postcheck/monotype/solve.zig
sed -n '1228,1275p' src/postcheck/monotype/solve.zig
sed -n '1490,1548p' src/postcheck/monotype/solve.zig
sed -n '1800,2020p' src/postcheck/monotype/solve.zig
```

Expected: confirm that `importMono` reopens all zero-tag `tag_union` values and that materialization cannot distinguish explicit `[]` from defaulted/unconstrained slots.

- [ ] **Step 2: Add a failing unit test for explicit `[]` import**

Add this test near the existing Monotype graph tests in `src/postcheck/monotype/solve.zig`:

```zig
test "explicit empty tag union imports as closed uninhabited row" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
    defer graph.destroy();

    const explicit_empty = try type_store.add(.{ .tag_union = Type.Span.empty() });
    const imported = try graph.importMono(explicit_empty);

    try std.testing.expectEqual(InstNode.empty_tag_union, graph.content(imported));
}
```

- [ ] **Step 3: Run the focused test and confirm it fails**

Run:

```bash
zig build run-test-zig-module-postcheck --summary failures --color off -- --filter "explicit empty tag union imports as closed uninhabited row"
```

Expected: FAIL because `importMono` currently returns `.unresolved = InstVariable.row(.empty_tag_union)` for every zero-tag `tag_union`.

- [ ] **Step 4: Add a second unit test for marked unsolved import**

Add this test next to the previous one:

```zig
test "unsolved zero-tag Monotype imports as unresolved row evidence" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    var unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(gpa);
    defer unsolved_monos.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store, &unsolved_monos);
    defer graph.destroy();

    const unsolved_empty = try type_store.add(.{ .tag_union = Type.Span.empty() });
    try unsolved_monos.put(unsolved_empty, {});

    const imported = try graph.importMono(unsolved_empty);

    switch (graph.content(imported)) {
        .unresolved => |variable| try std.testing.expectEqual(checked.RowDefault.empty_tag_union, variable.row_default.?),
        else => return error.TestUnexpectedResult,
    }
}
```

- [ ] **Step 5: Run both tests and record the baseline**

Run:

```bash
zig build run-test-zig-module-postcheck --summary failures --color off -- --filter "tag union imports"
```

Expected: one test fails and one test passes before implementation. If the filter syntax does not match both tests, run the two exact filters separately.

---

### Task 2: Make Monotype Import Use Explicit Unsolved Provenance

**Files:**
- Modify: `src/postcheck/monotype/solve.zig`
- Test: `src/postcheck/monotype/solve.zig`

- [ ] **Step 1: Change zero-tag import to consult `unsolved_monos`**

In `InstGraph.importMono`, replace the current empty-span branch:

```zig
if (span.len == 0) break :blk .{ .unresolved = InstVariable.row(.empty_tag_union) };
```

with:

```zig
if (span.len == 0) {
    if (self.unsolved_monos.contains(ty)) {
        break :blk .{ .unresolved = InstVariable.row(.empty_tag_union) };
    }
    break :blk .empty_tag_union;
}
```

Keep the comment focused on provenance:

```zig
// Only builder-global Monotypes marked as unsolved may re-enter as row
// evidence. An ordinary finished zero-tag union is explicit/proven `[]`.
```

- [ ] **Step 2: Run the focused import tests**

Run:

```bash
zig build run-test-zig-module-postcheck --summary failures --color off -- --filter "explicit empty tag union imports as closed uninhabited row"
zig build run-test-zig-module-postcheck --summary failures --color off -- --filter "unsolved zero-tag Monotype imports as unresolved row evidence"
```

Expected: both pass.

- [ ] **Step 3: Run all postcheck module tests**

Run:

```bash
zig build run-test-zig-module-postcheck --summary failures --color off
```

Expected: pass. If it fails, fix this section before moving on.

- [ ] **Step 4: Commit the import provenance change**

Run:

```bash
git add src/postcheck/monotype/solve.zig
git commit -m "fix: preserve empty tag union provenance on monotype import"
```

---

### Task 3: Remove Lambda Solved Empty-Span Yielding

**Files:**
- Modify: `src/postcheck/lambda_solved/solve.zig`
- Test: `src/postcheck/lambda_solved/solve.zig` or `src/eval/test/eval_tests.zig`

- [ ] **Step 1: Read the yielding branch**

Run:

```bash
sed -n '1228,1430p' src/postcheck/lambda_solved/solve.zig
```

Expected: find the two `isEmptyTagUnion(...)` branches before structural unification.

- [ ] **Step 2: Remove the blanket yield behavior**

Delete this logic from `Solver.unify`:

```zig
if (isEmptyTagUnion(left) and !isEmptyTagUnion(right)) {
    self.program.types.set(a, .{ .link = b });
    return;
}
if (isEmptyTagUnion(right) and !isEmptyTagUnion(left)) {
    self.program.types.set(b, .{ .link = a });
    return;
}
```

Also delete `isEmptyTagUnion` if it is unused after the removal.

- [ ] **Step 3: Run focused Lambda Solved/postcheck tests**

Run:

```bash
zig build run-test-zig-module-postcheck --summary failures --color off
zig build run-test-zig-module-lir --summary failures --color off
```

Expected: pass, or fail with a concrete producer-side specialization mismatch. Do not reintroduce empty-span yielding to make failures go away.

- [ ] **Step 4: Run targeted eval regression that motivated the current Lambda Solved workaround**

Run:

```bash
zig build run-test-eval --summary failures --color off -- --filter "inspect: compile-time callable result reused through top-level data" --timeout 120000
```

Expected: pass. If it fails, inspect the Monotype producer path for the callable result and carry explicit unsolved provenance earlier. Do not add a Lambda Solved fallback.

- [ ] **Step 5: Commit the Lambda Solved cleanup**

Run:

```bash
git add src/postcheck/lambda_solved/solve.zig src/eval/test/eval_tests.zig
git commit -m "fix: stop yielding explicit empty tag unions in lambda solved"
```

If `src/eval/test/eval_tests.zig` was not changed, omit it from `git add`.

---

### Task 4: Audit Monotype Materialization And Sealing Comments

**Files:**
- Modify: `src/postcheck/monotype/solve.zig`
- Modify: `src/postcheck/monotype/lower.zig`
- Test: existing module tests

- [ ] **Step 1: Search for comments that still describe empty spans as reopenable**

Run:

```bash
rg -n "empty tag union|zero-tag|unresolved slots|slot no value reached|yield|supersede" src/postcheck/monotype src/postcheck/lambda_solved
```

Expected: identify comments in `solve.zig`, `lower.zig`, and `lambda_solved/solve.zig` that now need to distinguish explicit/proven `[]` from marked unsolved Monotypes.

- [ ] **Step 2: Update `InstGraph` field comment**

In `src/postcheck/monotype/solve.zig`, change the `unsolved_monos` comment to:

```zig
/// Monotypes lowered without body evidence by the builder-global type cache.
/// When one of these final TypeIds is imported into a specialization graph,
/// zero-tag unions inside that marked type may re-enter as unresolved row
/// evidence. Unmarked zero-tag unions are explicit/proven `[]`.
```

- [ ] **Step 3: Update `Builder.unsolved_monos` comment**

In `src/postcheck/monotype/lower.zig`, change the `unsolved_monos` comment to:

```zig
/// Monotypes owned by the builder-global type cache. They are lowered without
/// body evidence, so this map is explicit provenance for imports that may
/// reopen zero-tag unions as unresolved slots inside a later specialization.
```

- [ ] **Step 4: Run postcheck and formatting checks**

Run:

```bash
zig build run-test-zig-module-postcheck --summary failures --color off
zig build run-check-zig-format --summary failures --color off
```

Expected: both pass.

- [ ] **Step 5: Commit comments and audit cleanup**

Run:

```bash
git add src/postcheck/monotype/solve.zig src/postcheck/monotype/lower.zig src/postcheck/lambda_solved/solve.zig
git commit -m "docs: clarify empty tag union provenance"
```

Only include files that changed.

---

### Task 5: Run End-To-End Regressions That Exercise The Risky Paths

**Files:**
- Test only unless a failure identifies a producer-side bug.

- [ ] **Step 1: Run the Issue 9885 CLI regression**

Run:

```bash
nix shell nixpkgs#rustc nixpkgs#cargo -c zig build run-test-cli --summary failures --color off -- --filter "issue 9885"
```

Expected: pass. The unannotated reverse test must print the reversed string list and must not contain `uninhabited value` or `panic`.

- [ ] **Step 2: Run empty-tag-union CLI regressions**

Run:

```bash
nix shell nixpkgs#rustc nixpkgs#cargo -c zig build run-test-cli --summary failures --color off -- --filter "empty tag union"
```

Expected: pass. Parser/encoder empty-tag-union checks should report type errors, not panics.

- [ ] **Step 3: Run eval regressions around callable reuse and empty tag-union constants**

Run:

```bash
zig build run-test-eval --summary failures --color off -- --filter "compile-time callable result reused through top-level data" --timeout 120000
zig build run-test-eval --summary failures --color off -- --filter "empty tag-union constant is not planned as runtime payload" --timeout 120000
```

Expected: both pass.

- [ ] **Step 4: Run the broader impacted sections**

Run:

```bash
zig build run-test-zig-module-postcheck --summary failures --color off
zig build run-test-zig-module-lir --summary failures --color off
zig build run-test-eval --summary failures --color off -- --timeout 120000
nix shell nixpkgs#rustc nixpkgs#cargo -c zig build run-test-cli --summary failures --color off
```

Expected: all pass. If a section fails, follow the repo instruction: rerun the specific failing section until it passes before returning to full MiniCI.

- [ ] **Step 5: Commit any producer-side fixes revealed by targeted runs**

Run:

```bash
git status --short
git add <only-files-changed-for-the-fix>
git commit -m "fix: thread empty tag union provenance through specialization"
```

Skip this commit if no additional fixes were needed.

---

### Task 6: Full MiniCI Verification

**Files:**
- No code changes expected.

- [ ] **Step 1: Run full MiniCI with Rust available**

Run:

```bash
nix shell nixpkgs#rustc nixpkgs#cargo -c zig build minici --summary failures --color off
```

Expected: `61/61` phases pass. If one section fails, fix that issue and rerun that specific failing section until it passes; only then rerun full MiniCI.

- [ ] **Step 2: Capture final verification for PR description**

Record the final passing commands in the PR body:

```text
- `zig build run-test-zig-module-postcheck --summary failures --color off`
- `zig build run-test-eval --summary failures --color off -- --timeout 120000`
- `nix shell nixpkgs#rustc nixpkgs#cargo -c zig build run-test-cli --summary failures --color off`
- `nix shell nixpkgs#rustc nixpkgs#cargo -c zig build minici --summary failures --color off`
```

- [ ] **Step 3: Close the tracking issue from the PR**

In the PR body, include:

```text
Closes #10067
```

---

## Self-Review

- Spec coverage: the plan addresses the issue in #10067 by adding provenance-aware Monotype import, removing Lambda Solved empty-span yielding, and verifying the known inspect/recursive reverse/empty-union paths.
- Placeholder scan: no task relies on "best effort", heuristics, fallbacks, or unspecified error handling.
- Type consistency: the plan uses existing names from the codebase: `InstGraph`, `InstNode.empty_tag_union`, `InstVariable.row(.empty_tag_union)`, `Type.Span.empty()`, `unsolved_monos`, and `Solver.unify`.

Plan complete. Execution should start from an isolated worktree using `superpowers:using-git-worktrees`, then use either `superpowers:subagent-driven-development` or `superpowers:executing-plans`.
