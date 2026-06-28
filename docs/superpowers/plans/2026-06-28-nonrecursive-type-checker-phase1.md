# Non-Recursive Type Checker — Phase 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `checkExpr` depth-safe for `e_tuple_access` and `e_block` (incl. `checkBlockStatements`) by reifying the `checkExpr` stack frame onto a `Self`-owned work stack, behind a behavior-preserving incremental migration — stopping at a manual review gate.

**Architecture:** Approach B from the design doc (`docs/superpowers/specs/2026-06-28-nonrecursive-type-checker-design.md`). The current `checkExpr` is renamed `checkExprRecursive` (kept verbatim). A new driver `checkExprIter` runs a loop over an explicit `CheckFrame` stack allocated once at `Check.init`. Each migrated node kind is split into `enter` (shared prologue + schedule children) / optional resume steps / `exit` (per-kind unification + shared epilogue). Unmigrated kinds pass through to `checkExprRecursive` *before* the iterative prologue runs. A required two-pass module-level differential harness proves results are preserved.

**Tech Stack:** Zig; the Roc compiler `src/check/Check.zig`; test env `src/check/test/TestEnv.zig`; build via `zig build` steps.

**Scope:** This plan covers migration items 0 (`e_tuple_access` warm-up) and 1 (`e_block` + `checkBlockStatements`) only, plus the foundation, the differential harness, and the stress test. It STOPS at the manual review gate. Items 2–7 (`binop`, `call`, `if`, `match`, `list`/`tuple`/`record`, `dot_access`, remaining kinds) are a follow-up plan written after the gate.

**jj workflow (user rule):** Create AND describe each commit BEFORE doing its work: `jj new -m "<desc>"` is the first step of every task. The working copy *is* the commit (jj auto-snapshots edits), so there is no trailing "commit" step — verification is the last step, then the next task starts a new `jj new`. Never create merge commits; only rebase.

---

## File Structure

All changes are within `src/check/`:

- **`src/check/Check.zig`** (modify) — rename `checkExpr` → `checkExprRecursive`; add `checkExpr` wrapper + `checkExprIter` driver; add `CheckFrame`/`Step`/`KindState` types; add `check_frame_stack`, `force_recursive`, `check_recursion_depth` fields + init/deinit; flatten `e_tuple_access`, then `e_block` + `checkBlockStatements`.
- **`src/check/test/TestEnv.zig`** (modify) — add a `force_recursive` knob to checking and a `renderAllDefTypes` helper for the differential harness.
- **`src/check/test/differential_test.zig`** (create) — the two-pass differential harness + its corpus tests.
- **`src/check/test/deep_nesting_test.zig`** (create) — the stack-safety stress test.
- **`src/check/mod.zig`** (modify) — register the two new test files if the module aggregates tests explicitly (verify pattern in that file first).

---

## Task 1: Foundation — frame stack, passthrough driver, depth guard, force_recursive

**Files:**
- Modify: `src/check/Check.zig` — fields (`60–364`), `initAssumePrepared` (`1042–1148`), `deinit` (`1192–1268`), `checkExpr` (`9473`).

- [ ] **Step 1: Create & describe the commit**

```bash
jj new -m "check: add iterative checkExpr driver skeleton (full passthrough)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 2: Add the frame types and fields**

Add near the other private types in `Check.zig` (e.g. just above `fn checkExpr`, after the `Expected`/`AnnoVars` definitions):

```zig
/// Where in a frame's lifecycle the driver is. More variants are added as
/// node kinds are migrated (block adds its resume steps in a later task).
const CheckStep = enum {
    enter,
    exit,
};

/// Per-node-kind loop/scratch state for interleaving nodes. `none` for the
/// fixed-arity kinds migrated in this phase except where noted.
const CheckKindState = union(enum) {
    none,
    // block_loop is added in the e_block task.
};

/// A reified `checkExpr` stack frame. Must be cheap to memcpy (lives in an
/// ArrayList that may realloc): scalars + small structs only.
const CheckFrame = struct {
    expr_idx: CIR.Expr.Idx,
    env: *Env,
    expected: Expected,
    step: CheckStep,

    // Captured by the shared prologue (.enter), reused by the shared epilogue (.exit):
    expr_var: Var,
    expr_var_raw: Var,
    mb_anno_vars: ?AnnoVars,
    should_generalize: bool,
    hoist_guard: HoistFrameGuard,
    prev_instantiation_source: ?CIR.Expr.Idx,
    prev_discarded_binding_rhs_expr: ?CIR.Expr.Idx,
    is_binding_rhs: bool,
    binding_rhs_pattern: ?CIR.Pattern.Idx,
    child_expected: Expected,

    // Accumulated across children:
    does_fx: bool,

    kind_state: CheckKindState = .none,
};
```

Add these fields to the `Check` struct field block (`Check.zig:60–364`):

```zig
/// Work stack for the iterative checkExpr driver. Allocated once at init,
/// reused across calls via a saved base high-water-mark.
check_frame_stack: std.ArrayList(CheckFrame),
/// When true, checkExprIter escape-hatches every kind to checkExprRecursive
/// (i.e. exact pre-migration behavior). Used by the differential harness.
force_recursive: bool,
/// Native recursion depth through checkExprRecursive; converts a would-be
/// stack overflow into a clean diagnostic during migration.
check_recursion_depth: u32,
```

- [ ] **Step 3: Initialize and deinitialize the new fields**

In `initAssumePrepared` (`Check.zig:1042–1148`), alongside the other `.empty`/`.init` field initializers, add:

```zig
.check_frame_stack = try std.ArrayList(CheckFrame).initCapacity(gpa, 256),
.force_recursive = false,
.check_recursion_depth = 0,
```

In `deinit` (`Check.zig:1192–1268`), alongside the other `deinit` calls:

```zig
self.check_frame_stack.deinit(self.gpa);
```

- [ ] **Step 4: Rename the existing function and add the wrapper + driver skeleton**

Rename the existing `fn checkExpr` at `Check.zig:9473` to `checkExprRecursive` (signature otherwise unchanged):

```zig
fn checkExprRecursive(self: *Self, expr_idx: CIR.Expr.Idx, env: *Env, expected: Expected) std.mem.Allocator.Error!bool {
```

At the very top of `checkExprRecursive`'s body, add the depth guard (before the existing first line `const prev_instantiation_source = ...`):

```zig
    self.check_recursion_depth += 1;
    defer self.check_recursion_depth -= 1;
    if (self.check_recursion_depth > MAX_CHECK_RECURSION_DEPTH) return error.OutOfMemory;
```

Add the constant near the top of the file (with the other file-level consts):

```zig
/// Interim guard: until all unbounded-depth kinds are migrated, native recursion
/// through checkExprRecursive is bounded here so a pathological input yields a
/// clean error instead of a native stack overflow. Generous; only pathological
/// inputs approach it.
const MAX_CHECK_RECURSION_DEPTH: u32 = 8192;
```

Add the new public-facing `checkExpr` (same name/signature callers already use) and the driver, immediately after `checkExprRecursive`:

```zig
fn checkExpr(self: *Self, expr_idx: CIR.Expr.Idx, env: *Env, expected: Expected) std.mem.Allocator.Error!bool {
    return self.checkExprIter(expr_idx, env, expected);
}

fn checkExprIter(self: *Self, root_idx: CIR.Expr.Idx, root_env: *Env, root_expected: Expected) std.mem.Allocator.Error!bool {
    if (self.force_recursive) return self.checkExprRecursive(root_idx, root_env, root_expected);

    const base = self.check_frame_stack.items.len;
    var root_does_fx = false;

    try self.check_frame_stack.append(self.gpa, .{
        .expr_idx = root_idx,
        .env = root_env,
        .expected = root_expected,
        .step = .enter,
        .expr_var = undefined,
        .expr_var_raw = undefined,
        .mb_anno_vars = null,
        .should_generalize = undefined,
        .hoist_guard = undefined,
        .prev_instantiation_source = undefined,
        .prev_discarded_binding_rhs_expr = undefined,
        .is_binding_rhs = undefined,
        .binding_rhs_pattern = undefined,
        .child_expected = undefined,
        .does_fx = false,
    });

    while (self.check_frame_stack.items.len > base) {
        const top = self.check_frame_stack.items.len - 1;
        const expr_idx = self.check_frame_stack.items[top].expr_idx;

        // Escape hatch BEFORE the iterative prologue: unmigrated kinds run the
        // whole node natively, then the frame is finished as a unit.
        if (self.check_frame_stack.items[top].step == .enter and !isMigratedKind(self.cir.store.getExpr(expr_idx))) {
            const f = self.check_frame_stack.items[top];
            const fx = try self.checkExprRecursive(f.expr_idx, f.env, f.expected);
            self.finishFrameAndPropagate(top, base, fx, &root_does_fx);
            continue;
        }

        // Migrated kinds: dispatched by step. No kinds are migrated yet, so this
        // is unreachable until the warm-up task adds the first one.
        unreachable;
    }

    return root_does_fx;
}

/// Pop the frame at `top` and OR its `does_fx` into the new top frame (or into
/// `root_does_fx` if the stack is back at `base`). The single place frames are
/// popped + their effect propagated, used by every kind's completion path.
fn finishFrameAndPropagate(self: *Self, top: usize, base: usize, fx: bool, root_does_fx: *bool) void {
    self.check_frame_stack.items.len = top; // pop
    if (self.check_frame_stack.items.len > base) {
        const parent = &self.check_frame_stack.items[self.check_frame_stack.items.len - 1];
        parent.does_fx = parent.does_fx or fx;
    } else {
        root_does_fx.* = fx;
    }
}

/// True for expr kinds handled by the iterative driver. Grows as kinds migrate.
/// Empty in this task → every kind passes through to checkExprRecursive.
fn isMigratedKind(expr: CIR.Expr) bool {
    return switch (expr) {
        else => false,
    };
}
```

- [ ] **Step 5: Build the check module**

Run: `zig build run-test-zig-module-check`
Expected: PASS (compiles; behavior identical — every kind still passes through to `checkExprRecursive`). If `error.OutOfMemory` is a poor fit for the depth guard, confirm the project's error set; `std.mem.Allocator.Error` only has `OutOfMemory`, so reuse it (a dedicated diagnostic is added when the guard becomes meaningful post-migration).

- [ ] **Step 6: Run the full suite + snapshots to confirm zero behavior change**

Run: `zig build run-check-snapshots && zig build run-test-zig`
Expected: PASS with no snapshot diffs (the driver is a pure passthrough).

---

## Task 2: Differential harness (REQUIRED, two-pass module-level)

**Files:**
- Modify: `src/check/test/TestEnv.zig` (`303–411` init, `526–603` type rendering).
- Create: `src/check/test/differential_test.zig`.

- [ ] **Step 1: Create & describe the commit**

```bash
jj new -m "check/test: two-pass differential harness for iterative checker

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 2: Add a force_recursive knob to TestEnv**

Locate where `TestEnv.init` constructs the `Check` and calls `checkFile` (in `TestEnv.zig` around `303–411`). Add a parameterized init that sets the flag before checking:

```zig
/// Like `init`, but forces the checker into pure-recursive mode when
/// `force_recursive` is true (escape-hatches every kind in checkExprIter).
pub fn initWithMode(module_name: []const u8, source: []const u8, force_recursive: bool) Allocator.Error!TestEnv {
    // (Copy the body of `init`, but set `checker.force_recursive = force_recursive;`
    //  on the Check instance BEFORE the `try checker.checkFile()` call.)
}
```

Then make the existing `init` delegate: `return initWithMode(module_name, source, false);` (so existing tests are unchanged).

- [ ] **Step 3: Add a render-all-def-types helper**

Next to `assertDefType` (`TestEnv.zig:526–603`), add a helper that renders every top-level def's type to an owned `[]const u8` using the same type writer `assertDefType` uses:

```zig
/// Render the type of every top-level def, in def order, joined by '\n'.
/// Caller owns the returned slice. Reuses the same type-writing path as
/// `assertDefType`.
pub fn renderAllDefTypes(self: *TestEnv) Allocator.Error![]u8 {
    // Iterate self.module_env defs in order; for each, write its root type var
    // with the existing type writer (the one assertDefType calls), appending
    // "<def_name> : <rendered>\n" to a std.ArrayList(u8). Return toOwnedSlice().
}
```

(Read `assertDefType`'s body to reuse its exact rendering call; do not invent a new renderer.)

- [ ] **Step 4: Write the differential harness + a first failing-by-construction test**

Create `src/check/test/differential_test.zig`:

```zig
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

/// Type-check `source` twice — once forced fully-recursive, once with the
/// iterative driver — and assert identical diagnostics and identical rendered
/// def types. This is the REQUIRED results-preservation guarantee.
pub fn expectIterMatchesRecursive(source: []const u8) !void {
    var rec = try TestEnv.initWithMode("Diff", source, true);
    defer rec.deinit();
    var itr = try TestEnv.initWithMode("Diff", source, false);
    defer itr.deinit();

    const rec_types = try rec.renderAllDefTypes();
    defer rec.gpa.free(rec_types);
    const itr_types = try itr.renderAllDefTypes();
    defer itr.gpa.free(itr_types);
    try std.testing.expectEqualStrings(rec_types, itr_types);

    const rec_diags = try rec.module_env.getDiagnostics();
    defer rec.gpa.free(rec_diags);
    const itr_diags = try itr.module_env.getDiagnostics();
    defer itr.gpa.free(itr_diags);
    try std.testing.expectEqual(rec_diags.len, itr_diags.len);
}

test "differential: simple module matches across recursive/iterative" {
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = 1
        \\    y = (x, 2)
        \\    y.0
        \\}
    );
}
```

- [ ] **Step 5: Register the test file**

Check how `src/check/mod.zig` aggregates test files (grep for an existing `_test` import or `refAllDecls`). Add `differential_test.zig` following that exact pattern.

- [ ] **Step 6: Run the harness test**

Run: `zig build run-test-zig-module-check -- --test-filter "differential"`
Expected: PASS (both passes are identical today — every kind still passes through; this confirms the harness itself is sound before any kind migrates).

---

## Task 3: Deep-nesting stress test (executable definition of stack-safety done)

**Files:**
- Create: `src/check/test/deep_nesting_test.zig`.

- [ ] **Step 1: Create & describe the commit**

```bash
jj new -m "check/test: deep-nesting stack-safety stress test

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 2: Write a generator + the stress test (expected to FAIL today via the depth guard)**

Create `src/check/test/deep_nesting_test.zig`:

```zig
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

/// Build `main! = |_| { x0 = { x1 = { ... { 0 } ... } } }` nested `depth` deep,
/// purely via nested blocks — the e_block spine this phase makes safe.
fn nestedBlocks(gpa: std.mem.Allocator, depth: usize) ![]u8 {
    var buf = std.ArrayList(u8){};
    errdefer buf.deinit(gpa);
    try buf.appendSlice(gpa, "main! = |_args| {\n");
    var i: usize = 0;
    while (i < depth) : (i += 1) try buf.appendSlice(gpa, "{\n");
    try buf.appendSlice(gpa, "0\n");
    i = 0;
    while (i < depth) : (i += 1) try buf.appendSlice(gpa, "}\n");
    try buf.appendSlice(gpa, "}\n");
    return buf.toOwnedSlice(gpa);
}

test "deep nesting: 50k nested blocks type-check without native stack overflow" {
    const gpa = std.testing.allocator;
    const src = try nestedBlocks(gpa, 50_000);
    defer gpa.free(src);

    var test_env = try TestEnv.init("Deep", src);
    defer test_env.deinit();
    // Success criterion: no crash. We don't assert a specific type — only that
    // checking completes. Before e_block migration this trips MAX_CHECK_RECURSION_DEPTH
    // (clean error) or overflows; after migration it completes.
    _ = try test_env.module_env.getDiagnostics();
}
```

- [ ] **Step 3: Register the test file**

Add `deep_nesting_test.zig` to `src/check/mod.zig` following the same aggregation pattern used in Task 2 Step 5.

- [ ] **Step 4: Run it and confirm it FAILS today**

Run: `zig build run-test-zig-module-check -- --test-filter "deep nesting"`
Expected: FAIL — either a native stack overflow (crash) or, if the depth guard triggers first inside `checkExprRecursive`, a returned error. Record which. This failing test is the executable definition of done for the block task; it must pass after Task 5.

(If 50k overflows the *test harness's own* parse/canon recursion before reaching the checker, reduce to the largest depth that reaches `checkFile` and still fails only in the checker; note that parse/canon depth-safety is out of scope. Pick the depth empirically.)

---

## Task 4: Warm-up migration — `e_tuple_access`

**Files:**
- Modify: `src/check/Check.zig` — `isMigratedKind`, `checkExprIter` dispatch, `e_tuple_access` arm (`9909–9947`), the shared prologue (`9477–9582`) and epilogue (`11260–11362`).

- [ ] **Step 1: Create & describe the commit**

```bash
jj new -m "check: migrate e_tuple_access to iterative driver

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 2: Factor the shared prologue and epilogue into helpers**

To avoid duplicating the subtle generalization/rank/hoist logic, extract two helpers from `checkExprRecursive` that operate on a `*CheckFrame` (read the exact code at `9477–9582` for prologue and `11260–11362` for epilogue and move it verbatim, substituting frame fields for the locals):

```zig
/// Runs the shared prologue: sets instantiation source, consumes call-arg /
/// binding-rhs flags, decides should_generalize, pushes rank, generates the
/// annotation var, begins the hoist frame. Fills the prologue-captured fields.
/// Mirrors checkExprRecursive lines 9477–9582 exactly.
fn checkEnterPrologue(self: *Self, frame: *CheckFrame) std.mem.Allocator.Error!void { ... }

/// Runs the shared epilogue: annotation reconciliation, error tracking,
/// static-dispatch constraints, generalization (incl. cycle handling), rank pop,
/// hoist_guard.finish(frame.does_fx). Mirrors lines 11260–11362 + the deferred
/// rank pop at 9520–9537 exactly.
fn checkExitEpilogue(self: *Self, frame: *CheckFrame) std.mem.Allocator.Error!void { ... }
```

Then make `checkExprRecursive` call these two helpers in place of the inlined prologue/epilogue, passing a stack-local `CheckFrame`. Run `zig build run-check-snapshots && zig build run-test-zig` after this refactor alone — it must be a pure no-op (behavior identical) before migrating any kind. This isolates the risky extraction from the migration.

- [ ] **Step 3: Add `e_tuple_access` to the migrated set and dispatch**

In `isMigratedKind`:

```zig
fn isMigratedKind(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_tuple_access => true,
        else => false,
    };
}
```

Replace the `unreachable;` in `checkExprIter`'s loop with the dispatch:

```zig
        const frame = &self.check_frame_stack.items[top];
        switch (frame.step) {
            .enter => {
                try self.checkEnterPrologue(frame);
                switch (self.cir.store.getExpr(frame.expr_idx)) {
                    .e_tuple_access => |ta| {
                        // Schedule the single child, then resume at .exit.
                        frame.step = .exit;
                        try self.check_frame_stack.append(self.gpa, makeEnterFrame(ta.tuple, frame.env, frame.child_expected));
                    },
                    else => unreachable, // isMigratedKind gates this
                }
            },
            .exit => {
                // The child already OR'd its does_fx into frame.does_fx when it
                // popped (see finishFrameAndPropagate), matching the original
                // `does_fx = checkExpr(child) or does_fx` at line 9911.
                switch (self.cir.store.getExpr(frame.expr_idx)) {
                    .e_tuple_access => |ta| {
                        // Per-kind result unification: move lines 9913–9947 here
                        // VERBATIM, reading `frame.expr_var`, `frame.env`, and
                        // `ta` instead of the switch-arm locals `expr_var`, `env`,
                        // `tuple_access`. (These lines contain no checkExpr calls,
                        // so they move unchanged.)
                        _ = ta;
                    },
                    else => unreachable,
                }
                try self.checkExitEpilogue(frame);
                self.finishFrameAndPropagate(top, base, frame.does_fx, &root_does_fx);
            },
        }
```

Add the small constructor helper near `checkExprIter`:

```zig
fn makeEnterFrame(expr_idx: CIR.Expr.Idx, env: *Env, expected: Expected) CheckFrame {
    return .{
        .expr_idx = expr_idx, .env = env, .expected = expected, .step = .enter,
        .expr_var = undefined, .expr_var_raw = undefined, .mb_anno_vars = null,
        .should_generalize = undefined, .hoist_guard = undefined,
        .prev_instantiation_source = undefined, .prev_discarded_binding_rhs_expr = undefined,
        .is_binding_rhs = undefined, .binding_rhs_pattern = undefined,
        .child_expected = undefined, .does_fx = false,
    };
}
```

Note on `child_expected`: the original `e_tuple_access` arm checks its child with `child_expected` (`= expected.forStatement()`, computed at `9578`). `checkEnterPrologue` must compute and store `frame.child_expected` exactly as the original did, so the scheduled child receives the identical `Expected`.

- [ ] **Step 4: Add a focused differential test for tuple access**

Append to `src/check/test/differential_test.zig`:

```zig
test "differential: tuple access matches across recursive/iterative" {
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    t = (1, "two", 3)
        \\    a = t.0
        \\    b = t.1
        \\    (a, b)
        \\}
    );
}
```

- [ ] **Step 5: Run focused tests**

Run: `zig build run-test-zig-module-check -- --test-filter "differential"`
Expected: PASS (iterative `e_tuple_access` produces identical types to recursive).

- [ ] **Step 6: Run full suite + snapshots**

Run: `zig build run-check-snapshots && zig build run-test-zig`
Expected: PASS. Snapshot type output must be unchanged; eyeball any diagnostic-ordering churn to confirm it is pure reordering (allowed), never a changed/added/dropped type or error.

---

## Task 5: Migrate `e_block` + flatten `checkBlockStatements`

This is the large task. It is split into 5a (block arm) and 5b (statement loop). `e_block` is only depth-safe once **both** are flattened.

**Files:**
- Modify: `src/check/Check.zig` — `CheckStep`, `CheckKindState`, `isMigratedKind`, `checkExprIter` dispatch, `e_block` arm (`10372–10395`), `checkBlockStatements` (`12180–12550`).

### Task 5a: `e_block` arm onto the stack (statements still via recursive `checkBlockStatements`)

- [ ] **Step 1: Create & describe the commit**

```bash
jj new -m "check: migrate e_block arm to iterative driver (statements still recursive)

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 2: Add block steps and block loop state**

Extend the enums:

```zig
const CheckStep = enum {
    enter, exit,
    block_after_final,  // resume after final_expr child, do the block's result unify
    // block_stmt_after_expr is added in Task 5b when statements move onto the stack.
};

const CheckKindState = union(enum) {
    none,
    block_loop: struct {
        hoist_scope: HoistLexicalScope,
        stmt_result: BlockStatementsResult,
        // stmt cursor + per-statement state added in Task 5b:
        stmt_cursor: u32 = 0,
        does_fx_acc: bool = false,
        diverges: bool = false,
        blocks_later_hoists: bool = false,
    },
};
```

- [ ] **Step 3: Wire `e_block` into the dispatch (final_expr on the stack, statements recursive for now)**

In `isMigratedKind` add `.e_block => true`. In `checkExprIter`, handle the new steps. The `.enter` arm for `e_block`:

```zig
.e_block => |block| {
    // Mirrors checkExprRecursive lines 10372–10385, but final_expr is scheduled
    // as a child frame and the block's result unify happens in block_after_final.
    const hoist_scope = self.beginHoistLexicalScope();
    // Statements stay recursive in 5a:
    const stmt_result = try self.checkBlockStatements(block.stmts, frame.env, undefined, frame.expected.forStatement());
    frame.does_fx = stmt_result.does_fx or frame.does_fx;
    frame.kind_state = .{ .block_loop = .{ .hoist_scope = hoist_scope, .stmt_result = stmt_result } };
    frame.step = .block_after_final;
    const child_expected = frame.expected; // block final_expr uses the block's own expected
    if (stmt_result.blocks_later_hoists) {
        // replicate checkExprWithHoistSelectionSuppressed: bump suppressed depth,
        // restore it when the child's frame is popped (see note below).
        self.hoist_selection_suppressed_depth += 1;
        // Mark on the state so block_after_final knows to decrement.
        // (Add a bool field `suppressed_final` to block_loop.)
    }
    try self.check_frame_stack.append(self.gpa, makeEnterFrame(block.final_expr, frame.env, child_expected));
},
```

Add `suppressed_final: bool = false` to `block_loop`. The `block_after_final` step:

```zig
.block_after_final => switch (self.cir.store.getExpr(frame.expr_idx)) {
    .e_block => |block| {
        const st = frame.kind_state.block_loop;
        if (st.suppressed_final) self.hoist_selection_suppressed_depth -= 1;
        // Mirrors checkExprRecursive lines 10387–10394:
        if (st.stmt_result.diverges) {
            try self.unifyWith(frame.expr_var, .{ .flex = Flex.init() }, frame.env);
        } else {
            _ = try self.unify(frame.expr_var, ModuleEnv.varFrom(block.final_expr), frame.env);
        }
        self.endHoistLexicalScope(st.hoist_scope);
        try self.checkExitEpilogue(frame);
        self.finishFrameAndPropagate(top, base, frame.does_fx, &root_does_fx);
    },
    else => unreachable,
},
```

**Note on `child_expected` vs the suppressed path:** the original chooses `checkExprWithHoistSelectionSuppressed` vs `checkExpr` for `final_expr` (`10381–10384`). Both call `checkExpr(block.final_expr, env, expected)` with the *block's own* `expected` (not `child_expected`/`forStatement`). The only difference is the suppressed-depth bump, replicated above. Confirm against `10381–10384`.

- [ ] **Step 4: Differential test for blocks (final_expr path)**

Append to `differential_test.zig`:

```zig
test "differential: nested blocks match across recursive/iterative" {
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    a = {
        \\        b = 1
        \\        c = { d = 2; d }
        \\        (b, c)
        \\    }
        \\    a.1
        \\}
    );
}
```

(Use the project's actual block syntax — confirm whether statements are newline- or `;`-separated by checking an existing snapshot; adjust the literal accordingly.)

- [ ] **Step 5: Run focused + full suite**

Run: `zig build run-test-zig-module-check -- --test-filter "differential"` then `zig build run-check-snapshots && zig build run-test-zig`
Expected: PASS, no snapshot type changes. The deep-nesting test still FAILS (statements not yet flattened) — that's expected; 5b fixes it.

### Task 5b: Flatten `checkBlockStatements` onto the stack

The statement loop (`12189–12544`) has 8 statement kinds with ~11 recursive `checkExpr`/`checkPattern` sites. Flatten the loop so each statement's expr-check is a scheduled child frame, while preserving the per-statement accumulator updates that happen *after* each child.

- [ ] **Step 1: Create & describe the commit**

```bash
jj new -m "check: flatten checkBlockStatements onto the work stack

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

- [ ] **Step 2: Add the per-statement resume step + state**

Extend `CheckStep` with `block_stmt_after_expr` (the resume point after each statement's expr child). Extend `block_loop` state to carry the loop position and the pending post-child work:

```zig
// in block_loop:
stmts: CIR.Statement.Span,
stmt_cursor: u32,
statement_expected: Expected,
// fields needed to finish the CURRENT statement after its expr child returns:
cur_stmt_idx: CIR.Statement.Idx,
cur_kind: enum { none, decl, var_, reassign, while_cond, while_body, expr, dbg, ret } = .none,
cur_saved_func_name: ?Ident.Idx = null,
cur_statement_blocks_later_hoists: bool = false,
```

- [ ] **Step 3: Restructure the block `.enter`/resume to drive statements via the stack**

Replace the 5a approach (calling `checkBlockStatements` recursively) with an iterative pump. The driver, on `block_after_stmts` / `block_stmt_after_expr`, advances `stmt_cursor`:

- For each statement, replicate the **pre-child** portion of its case verbatim from the cited lines (e.g. `s_decl`: lines `12211–12242` — pattern check via the still-recursive `checkPattern`, `enclosing_func_name` save, `expectation` computation, `checking_binding_rhs` flag set), then schedule the statement's expr child and set `step = block_stmt_after_expr` with `cur_kind` recording which statement is in flight.
- On `block_stmt_after_expr`, run the **post-child** portion verbatim (e.g. `s_decl`: lines `12244–12252` — `does_fx` OR, `statement_blocks_later_hoists = checkedExprBlocksLaterHoists(...)`, `recordHoistBindingCandidate`, restore `enclosing_func_name`), fold `statement_blocks_later_hoists` into `blocks_later_hoists` (line `12543`), then advance `stmt_cursor`.
- Statements with **two** expr children (`s_while`: cond at `12398`, body at `12405`, with a `unify(bool_var, cond_var)` between them at `12403`) need an intermediate `while_cond`→`while_body` sub-state, carrying `cond` between them — mirror exactly lines `12398–12407`.
- `s_for` (`12383`) calls `checkIteratorForLoop`, which recurses internally. **Keep it recursive** in this phase (it is bounded by loop nesting, which `for` does not make input-unbounded in the block-spine sense; flatten in a follow-up). Schedule nothing; run it inline and continue. Document this as a scoped gap (consistent with the design's "follow-up" stance).
- `s_crash` / `s_infinite_loop` / other non-expr statement kinds: replicate verbatim (no child).

Build a per-statement-kind mapping table in a comment block at the top of the flattened code, listing for each kind its source line range, its child expr(s), and its post-child accumulator updates, e.g.:

```
// s_decl     : pre 12211-12242 | child 12243 expr | post 12244-12252
// s_var      : pre 12283-12302 | child 12303 expr | post 12304-12310
// s_reassign : pre 12348-12355 | child 12356 expr | post 12357-12363
// s_while    : pre 12395-12397 | child 12398 cond, 12405 body (unify between 12403) | post 12406-12407
// s_expr     : pre (none)      | child 12447 expr | post 12448-12459
// s_dbg      : pre 12464-12466 | child 12467 expr | post 12468-12470
// s_return   : pre 12494-12497 | child 12498 expr | post 12499-12514
// s_for      : 12379-12394 — keep checkIteratorForLoop recursive (scoped gap)
// (replicate any remaining kinds verbatim with no child)
```

When `stmt_cursor` exhausts `stmts`, set `frame.kind_state.block_loop.stmt_result = .{ .does_fx, .diverges, .blocks_later_hoists }` from the accumulators, then proceed to schedule `final_expr` exactly as in Task 5a Step 3 (`step = block_after_final`).

Keep `checkBlockStatements` itself in the file (now only reachable when `force_recursive` is true, via `checkExprRecursive`'s own `.e_block` arm) — do NOT delete it; the differential harness needs the recursive path intact.

- [ ] **Step 4: Run the deep-nesting stress test — it must now PASS**

Run: `zig build run-test-zig-module-check -- --test-filter "deep nesting"`
Expected: PASS (50k nested blocks complete; native depth is now O(1)). If it still fails, the statement-expr children are not actually going through the stack — verify the `s_decl` RHS child is scheduled, not checked recursively.

- [ ] **Step 5: Differential tests with statement-heavy blocks**

Append to `differential_test.zig`:

```zig
test "differential: statement-heavy block matches across recursive/iterative" {
    try expectIterMatchesRecursive(
        \\main! = |_args| {
        \\    x = 1
        \\    var y = 2
        \\    y = y + x
        \\    _ = y
        \\    z = { inner = x; inner }
        \\    (x, z)
        \\}
    );
}
```

(Adjust syntax to the project's actual `var`/reassign/decl forms — confirm against an existing snapshot that uses statements.)

- [ ] **Step 6: Full suite + snapshots**

Run: `zig build run-check-snapshots && zig build run-test-zig`
Expected: PASS. No snapshot type changes; diagnostic-order churn (if any) verified as pure reordering.

---

## REVIEW GATE — STOP HERE

Do not migrate any further kind. Hand off to the user (Jared) for manual review with this evidence:

- [ ] `zig build run-check-snapshots && zig build run-test-zig` is green; report any diagnostic-order churn and confirm it is pure reordering (no type/error changes).
- [ ] The deep-nesting stress test passes (was failing before Task 5b).
- [ ] All `differential_test.zig` cases pass (recursive ≡ iterative).
- [ ] Summarize the `enter`/resume/`exit` decomposition for `e_block` + `checkBlockStatements` and the `checkEnterPrologue`/`checkExitEpilogue` extraction for the reviewer to read.

Only after sign-off does the follow-up plan migrate items 2–7 (`binop`, `call`, `if`, `match`, `list`/`tuple`/`record`, `dot_access`, remaining kinds), reusing this exact pattern.

---

## Notes & Risks

- **`checkEnterPrologue`/`checkExitEpilogue` extraction (Task 4 Step 2) is the highest-risk refactor.** It moves the subtlest code (generalization rank, cycle handling, hoist frame) into helpers. Verify it as a pure no-op (full suite + snapshots green) *before* migrating any kind, so a later snapshot diff can't be confused with the extraction.
- **`HoistFrameGuard` in `CheckFrame`:** confirm it is memcpy-safe (no self-referential pointers). If it holds a pointer back into `Self`, store only its plain data fields and reconstruct, or keep the guard's `finish` operating on `Self` + the saved `candidate_start`/`deferred_dependency_start` indices. Read `881–897` before Task 4.
- **`env: *Env` per frame:** blocks reuse the parent `env` (no new `Env`); this holds for all Phase-1 kinds. Lambda (which *does* pull from `env_pool`) is not migrated here.
- **Out of scope (depth guard covers them):** `checkPattern`, type-annotation generation, `checkIteratorForLoop`, and recursive `unify`/`occurs`.
