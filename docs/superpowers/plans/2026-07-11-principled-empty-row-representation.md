# Principled Empty-Row Representation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the current empty-tag-union provenance bridge with a representation where unresolved rows never masquerade as durable `tag_union []`, and generic values with no runtime-construction evidence are not lowered as executable runtime values.

**Architecture:** Active Monotype specialization owns unresolved evidence in `InstGraph` nodes and draft type cells. Durable Monotype `TypeId` values are closed snapshots only; `tag_union []` means a proven closed uninhabited tag union. Expression lowering must be demand-aware: runtime-value demands require constructible values, while inspect-only demands may render values such as functions from type/callable identity without lowering their executable bodies.

**Tech Stack:** Zig postcheck compiler pipeline, Monotype `InstGraph`, Monotype draft body sealing, Lambda Solved/LIR regression checks, jj scoped commits.

---

## Commit Discipline

- The main thread owns all `jj` commands.
- Do not use worktrees.
- Before each task, run `jj describe -m "<message>"` on the current empty working-copy commit.
- Implement only that task, verify it, run subagent spec/code-quality review, then run `jj new`.
- If a targeted section fails, fix and rerun that section until it passes before returning to full MiniCI.

## Design Rules To Preserve

- Durable Monotype `tag_union []` means only a closed, uninhabited tag union.
- Unresolved row evidence is represented by `InstGraph` state until final sealing.
- Final sealing may produce `tag_union []` for a truly unconstrained checked variable.
- Active solving must not create or refill a durable `TypeId` with `tag_union []` to stand for an unresolved row.
- A generic value with no evidence for constructing a runtime value is not lowerable under a runtime-value demand.
- Inspect-only lowering may avoid constructing a runtime value when the result is determined by type/callable identity, for example rendering a function as `<function>`.
- Postcheck must not synthesize Roc runtime `crash` bodies for compiler-internal uninhabited or non-lowerable states.

---

### Task 1: Document The Principled Model

**Files:**
- Modify: `design.md`

- [ ] **Step 1: Describe the jj commit**

Run:

```bash
jj describe -m "docs: specify principled empty row lowering"
```

Expected: the working copy commit has that description and no file changes yet.

- [ ] **Step 2: Update the Monotype specialization design**

In `design.md`, update the section around the current text that says an unconstrained checked type variable lowers to the empty tag union. Make it state:

```text
During active Monotype specialization, unresolved checked variables and row
extensions remain instantiation graph nodes. They are not represented by
durable Monotype TypeIds.

The only time an unresolved checked variable with an empty-tag-union row
default may become durable `tag_union []` is final graph sealing, after every
checked relation and specialization demand for that body has been applied.
After sealing, `tag_union []` is closed and uninhabited.
```

- [ ] **Step 3: Add the demand-aware value rule**

In the same design area, add:

```text
Expression lowering is demand-aware. A runtime-value demand requires a
constructible monomorphic value. If a checked generic value remains
unconstrained and no runtime value can exist at its final type, lowering it
under a runtime-value demand is a compiler invariant violation.

An inspect-only demand may render results determined by type or callable
identity without lowering an executable runtime value. For example, inspecting
a standalone function value may produce `<function>` without lowering the
function body. A later call, export, dispatch target, or other executable
demand must request a concrete body specialization with sufficient type
evidence.
```

- [ ] **Step 4: Run checks**

Run:

```bash
zig build run-check-semantic-audit --summary failures --color off
zig build run-check-postcheck-architecture --summary failures --color off
```

Expected: both pass.

- [ ] **Step 5: Review and split**

Request spec and code-quality review from subagents. After approval:

```bash
jj new
```

---

### Task 2: Split The REPL Lambda Tests

**Files:**
- Modify: `src/cli/ReplSession.zig`

- [ ] **Step 1: Describe the jj commit**

Run:

```bash
jj describe -m "test: split unconstrained repl lambda rendering"
```

- [ ] **Step 2: Split the current combined test**

Replace the current test:

```zig
test "Repl - lambda renders as <function>" {
    try expectAllNative("|x| x + 1", "<function>");
    try expectAllNative("|x, y| x + y", "<function>");
}
```

with two tests:

```zig
test "Repl - lambda with defaulted literal renders as <function>" {
    try expectAllNative("|x| x + 1", "<function>");
}

test "Repl - unconstrained lambda function value renders as <function>" {
    try expectAllNative("|x, y| x + y", "<function>");
}
```

The first test documents that the numeric literal gives defaultable evidence. The second test documents the non-lowerable function-value behavior.

- [ ] **Step 3: Verify both tests still pass on the bridge implementation**

Run:

```bash
zig build run-test-zig-cli-main --summary all --color off -- --test-filter "Repl - lambda with defaulted literal renders as <function>"
zig build run-test-zig-cli-main --summary all --color off -- --test-filter "Repl - unconstrained lambda function value renders as <function>"
```

Expected: both pass before representation changes.

- [ ] **Step 4: Review and split**

Request subagent review. After approval:

```bash
jj new
```

---

### Task 3: Separate Active Graph State From Durable Monotype Types

**Files:**
- Modify: `src/postcheck/monotype/solve.zig`
- Modify: `src/postcheck/monotype/lower.zig`
- Modify: `src/postcheck/structural_test.zig`
- Modify tests in: `src/postcheck/monotype/solve.zig`

- [ ] **Step 1: Describe the jj commit**

Run:

```bash
jj describe -m "fix: keep unresolved rows in instantiation graph"
```

- [ ] **Step 2: Remove side-table authority**

Remove `unsolved_monos` from:

- `Builder`
- `InstGraph`
- `InstGraph.create`
- all `InstGraph.create(...)` call sites
- Monotype import tests that manually insert `tag_union []` into `unsolved_monos`

After this change, `importMono` must treat every durable `tag_union []` as closed empty.

- [ ] **Step 3: Remove reopen logic**

Delete:

- `markGraphBackedUnsolvedRows`
- `markGraphBackedUnsolvedRowsInner`
- `reopenUnsolvedEmptyTagUnionView`
- calls to those functions

Update `activeNodeFromType` to:

```zig
fn activeNodeFromType(self: *BodyContext, ty: Type.TypeId) Allocator.Error!NodeId {
    if (self.graph.monoViewNode(ty)) |node| return node;
    return try self.graph.importMono(ty);
}
```

- [ ] **Step 4: Add structural guards for removed side channels**

In `src/postcheck/structural_test.zig`, add guards in this same commit so the commit stays green:

```zig
test "Monotype lowering does not use unsolved_monos side table" {
    const lower_source = @embedFile("monotype/lower.zig");
    const solve_source = @embedFile("monotype/solve.zig");
    try expectNotContains(lower_source, "unsolved_monos");
    try expectNotContains(solve_source, "unsolved_monos");
}

test "Monotype instantiation does not reopen empty tag union views" {
    const solve_source = @embedFile("monotype/solve.zig");
    try expectNotContains(solve_source, "reopenUnsolvedEmptyTagUnionView");
}

test "Monotype active view materialization rejects unresolved rows" {
    const solve_source = @embedFile("monotype/solve.zig");
    try expectContains(solve_source, "active Monotype view requested for unresolved instantiation node");
}
```

Keep the existing Lambda Solved guard that rejects explicit empty-tag-union yielding.

- [ ] **Step 5: Split active view materialization from final sealing**

Change `InstGraph.fillMono` so it never writes a durable `tag_union []` for `.unresolved` graph nodes during active solving. If a caller attempts to create a durable active view of an unresolved root, raise:

```zig
Common.invariant("active Monotype view requested for unresolved instantiation node");
```

Keep `GraphTypeFinals.sealContent` as the final sealing path that may call `materializeUnresolved`.

- [ ] **Step 6: Update tests**

Replace the bridge tests with:

```zig
test "explicit empty tag union imports as closed uninhabited row" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const explicit_empty = try type_store.add(.{ .tag_union = Type.Span.empty() });
    const imported = try graph.importMono(explicit_empty);

    try std.testing.expectEqual(InstNode.empty_tag_union, graph.content(imported));
}

test "unresolved row graph node seals to closed empty tag union only at finalization" {
    const gpa = std.testing.allocator;

    var type_store = Type.Store.init(gpa);
    defer type_store.deinit();

    var name_store = names.NameStore.init(gpa);
    defer name_store.deinit();

    const graph = try InstGraph.create(gpa, &type_store, &name_store);
    defer graph.destroy();

    const node = try graph.newNode(.{ .unresolved = InstVariable.row(.empty_tag_union) });
    const sealed = try graph.sealNode(node);
    const content = type_store.get(sealed);

    try std.testing.expectEqual(Type.Span.empty(), content.tag_union);
}

```

- [ ] **Step 7: Verify targeted checks**

Run:

```bash
zig build run-test-zig-module-postcheck --summary failures --color off
zig build run-test-zig-module-lir --summary failures --color off
```

Expected: both pass, or fail only in call sites that still incorrectly request active unresolved `TypeId` views. Fix those call sites in this commit before continuing.

- [ ] **Step 8: Review and split**

Request spec and code-quality review. After approval:

```bash
jj new
```

---

### Task 4: Make Expression Lowering Demand-Aware

**Files:**
- Modify: `src/postcheck/monotype/lower.zig`
- Modify: `src/postcheck/structural_test.zig`
- Modify tests in: `src/cli/ReplSession.zig` if the split tests need filter/name adjustment

- [ ] **Step 1: Describe the jj commit**

Run:

```bash
jj describe -m "fix: avoid lowering non-runtime generic values for inspect"
```

- [ ] **Step 2: Introduce lowering demand**

Add a small internal enum near the body-lowering helpers:

```zig
const LoweringDemand = enum {
    runtime_value,
    inspect_only,
};
```

Do not expose this outside Monotype lowering.

- [ ] **Step 3: Add an inspect-only path for function values**

When lowering `Str.inspect` or REPL inspection for an expression whose resolved Monotype type is `.func` or `.erased`, return the existing string expression:

```zig
try self.stringExpr("<function>", str_ty)
```

without first lowering the operand expression or requesting its function body. This must be restricted to inspect-only demand. Runtime calls, exports, dispatch targets, and stored function values that require executable code still use the normal runtime-value demand.

- [ ] **Step 4: Treat unconstrained runtime-value lowering as an invariant**

If runtime-value demand reaches a checked expression whose final type is closed empty tag union and the expression is not a source/checker-authored runtime error path, raise an invariant instead of emitting a Roc `crash`.

Remove the branch in function lowering that emits:

```zig
runtimeCrashExpr(ret_ty, "called function with an uninhabited argument")
```

- [ ] **Step 5: Add the runtime-crash guard**

In `src/postcheck/structural_test.zig`, add this guard in the same commit that removes the branch:

```zig
test "Postcheck does not synthesize runtime crash for uninhabited lambda arguments" {
    const lower_source = @embedFile("monotype/lower.zig");
    try expectNotContains(lower_source, "called function with an uninhabited argument");
}
```

- [ ] **Step 6: Verify the split REPL behavior**

Run:

```bash
zig build run-test-zig-cli-main --summary all --color off -- --test-filter "Repl - lambda with defaulted literal renders as <function>"
zig build run-test-zig-cli-main --summary all --color off -- --test-filter "Repl - unconstrained lambda function value renders as <function>"
```

Expected:

- `|x| x + 1` passes through normal numeric defaulting.
- `|x, y| x + y` passes through inspect-only function rendering without lowering the body.

- [ ] **Step 7: Verify postcheck/LIR**

Run:

```bash
zig build run-test-zig-module-postcheck --summary failures --color off
zig build run-test-zig-module-lir --summary failures --color off
```

Expected: both pass.

- [ ] **Step 8: Review and split**

Request spec and code-quality review. After approval:

```bash
jj new
```

---

### Task 5: Preserve Attached-Method Dispatch Regressions

**Files:**
- Modify only if required by failures: `src/postcheck/monotype/lower.zig`

- [ ] **Step 1: Describe the jj commit**

Run:

```bash
jj describe -m "fix: preserve dispatch evidence without empty row views"
```

- [ ] **Step 2: Verify B050/B051 first**

Run:

```bash
zig build run-test-eval --summary failures --color off -- --test-filter "regression B050"
zig build run-test-eval --summary failures --color off -- --test-filter "regression B051"
zig build run-test-eval --summary failures --color off -- --test-filter "inspect: compile-time callable result reused through top-level data"
```

Expected: each reports `1 passed, 0 failed, 0 crashed`.

- [ ] **Step 3: Fix only if the regressions fail**

If any regression fails, keep the expected-return unification behavior in `instantiateTargetFromPlan`, but route expected return and plan return through graph nodes only. Do not reintroduce:

- `unsolved_monos`
- `reopenUnsolvedEmptyTagUnionView`
- materialized `tag_union []` as unresolved evidence

- [ ] **Step 4: Verify targeted sections**

Run:

```bash
zig build run-test-eval --summary failures --color off -- --test-filter "regression B050"
zig build run-test-eval --summary failures --color off -- --test-filter "regression B051"
zig build run-test-zig-module-postcheck --summary failures --color off
zig build run-test-zig-module-lir --summary failures --color off
```

Expected: all pass.

- [ ] **Step 5: Review and split**

If this task changed files, request review and then run `jj new`. If it changed no files, do not create a commit.

---

### Task 6: Final Architecture Guards And MiniCI

**Files:**
- Modify only files needed to satisfy existing checks.

- [ ] **Step 1: Describe the jj commit if fixes are needed**

If this task requires file changes, run:

```bash
jj describe -m "test: enforce principled empty row invariants"
```

- [ ] **Step 2: Run structural guards**

Run:

```bash
zig build run-test-zig-module-postcheck --summary failures --color off
zig build run-check-postcheck-architecture --summary failures --color off
zig build run-check-semantic-audit --summary failures --color off
```

Expected: all pass.

- [ ] **Step 3: Run full MiniCI through Rust-loaded Nix**

Run:

```bash
nix shell nixpkgs#rustc nixpkgs#cargo -c zig build minici --summary failures --color off
```

Expected:

```text
MiniCI summary: 61/61 phases ran; 61 passed, 0 failed, 0 crashed, 0 skipped
```

- [ ] **Step 4: Handle failures by section**

If MiniCI fails in one section, stop using full MiniCI as the inner loop. Rerun the failing section directly, fix it, and rerun that section until it passes. Only then return to full MiniCI.

- [ ] **Step 5: Final review**

Request final subagent review over the commit stack. Report:

- commit list;
- targeted checks;
- full MiniCI result;
- remaining risks, if any.
