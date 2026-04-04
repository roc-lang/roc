# Polarity Ext Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Promote `polarity_open` from a `Rigid.Name` variant to a first-class `Content.polarity_ext` variant, and rename `polarity_deferred` to `polarity_pending`.

**Architecture:** Add `polarity_ext` to the `Content` union (no payload). Move unification rules from special cases in `unifyRigid`/`unifyStructure` to a dedicated `unifyPolarityExt` function. Rename `polarity_deferred` → `polarity_pending` in `Rigid.Name`. Update all downstream `Content` switches — meaningful handling for type infrastructure files, `unreachable` for phases where `polarity_ext` should be resolved away.

**Tech Stack:** Zig, Roc compiler type checker

**Build/Test commands:**
- `zig build test-check` — type checking tests (fast feedback)
- `zig build test` — full test suite (final verification)

**Version control:** This repo uses Jujutsu (jj), NOT git. Use `jj` commands for all VCS operations.

**IMPORTANT:** Adding a new `Content` variant breaks ALL exhaustive switches on `Content` across the codebase. Tasks 2-9 must ALL be completed before the project will compile. Do not attempt to build between these tasks. Task 10 is the first build checkpoint.

---

### Task 1: Add composed-return test (pre-refactor baseline)

**Files:**
- Modify: `src/check/test/polarity_test.zig`

This test exercises two open-return functions composed in an if-else, establishing baseline behavior before the refactor.

- [ ] **Step 1: Create jj commit**

```bash
jj new && jj desc -m "Add composed open-return polarity test"
```

- [ ] **Step 2: Add the test**

Find the end of the polarity test file and add a new test. Look for the last `test` declaration and add after it:

```zig
test "check type - polarity - composed open returns" {
    try checkTypesModule(
        \\foo : Str -> [A, B]
        \\foo = \s -> if Bool.true then A else B
        \\
        \\bar : Str -> [A, B]
        \\bar = \s -> if Bool.true then A else B
        \\
        \\baz = \s -> if Bool.true then foo s else bar s
    , .{ .pass = .{ .def = "baz" } },
        \\Str -> [A, B, ..]
    );
}
```

- [ ] **Step 3: Run test to verify it passes**

Run: `zig build test-check 2>&1 | tail -30`
Expected: PASS (both `polarity_ext` vars become flex after instantiation, unify normally)

- [ ] **Step 4: Verify jj commit**

```bash
jj st
```

---

### Task 2: Modify Content union and Rigid.Name (`src/types/types.zig`)

**Files:**
- Modify: `src/types/types.zig:144-302`

- [ ] **Step 1: Create jj commit**

```bash
jj new && jj desc -m "Promote polarity_open to Content.polarity_ext, rename polarity_deferred to polarity_pending"
```

- [ ] **Step 2: Add `polarity_ext` to the Content union**

At `src/types/types.zig:144`, add `polarity_ext` between `rigid` and `alias`:

```zig
pub const Content = union(enum) {
    const Self = @This();

    flex: Flex,
    rigid: Rigid,
    polarity_ext,
    alias: Alias,
    structure: FlatType,
    err,
```

- [ ] **Step 3: Update Rigid.Name — remove `polarity_open`, rename `polarity_deferred`**

Replace the `Rigid` struct at lines 264-302 with:

```zig
pub const Rigid = struct {
    name: Name,
    constraints: StaticDispatchConstraint.SafeList.Range,

    /// Classifies the origin of this rigid variable.
    pub const Name = union(enum) {
        /// Pending polarity resolution — resolved at alias use-site
        /// during instantiation based on polarity context.
        polarity_pending,

        /// User-written named rigid variable (e.g., `a` in `[A, B, ..a]`).
        name: Ident.Idx,
    };

    pub fn init(ident: Ident.Idx) Rigid {
        return .{ .name = .{ .name = ident }, .constraints = StaticDispatchConstraint.SafeList.Range.empty() };
    }

    pub fn initPolarityPending() Rigid {
        return .{ .name = .polarity_pending, .constraints = StaticDispatchConstraint.SafeList.Range.empty() };
    }

    pub fn withConstraints(self: Rigid, constraints: StaticDispatchConstraint.SafeList.Range) Rigid {
        return .{ .name = self.name, .constraints = constraints };
    }
};
```

This removes `initPolarityOpen()`, `initPolarityDeferred()`, and the `polarity_open` variant.

---

### Task 3: Update unification (`src/check/unify.zig`)

**Files:**
- Modify: `src/check/unify.zig:371-646`

- [ ] **Step 1: Add `polarity_ext` dispatch in `unifyVars`**

At line ~371, in the `unifyVars` function, add a `.polarity_ext` branch to the switch on `vars.a.desc.content`. Insert it between the `.rigid` and `.alias` branches:

```zig
.polarity_ext => {
    try self.unifyPolarityExt(vars, vars.b.desc.content);
},
```

- [ ] **Step 2: Add the `unifyPolarityExt` function**

Add this function right after `unifyRigid` (after line ~481):

```zig
/// Unify when `a` was a polarity_ext (open tag union extension).
/// polarity_ext can unify with: flex (polarity_ext wins), polarity_ext (merge),
/// empty_tag_union (closes to empty), and err. Everything else is TypeMismatch.
fn unifyPolarityExt(self: *Self, vars: *const ResolvedVarDescs, b_content: Content) Error!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    switch (b_content) {
        .flex => |b_flex| {
            try self.recordDeferredConstraint(vars, b_flex.constraints);
            self.merge(vars, .polarity_ext);
        },
        .polarity_ext => {
            // Two polarity_ext vars: both say "maybe more tags." Merge.
            self.merge(vars, .polarity_ext);
        },
        .structure => |b_flat_type| {
            if (b_flat_type == .empty_tag_union) {
                // Closed body satisfies open annotation.
                self.merge(vars, Content{ .structure = .empty_tag_union });
            } else {
                return error.TypeMismatch;
            }
        },
        .rigid, .alias => return error.TypeMismatch,
        .err => self.merge(vars, .err),
    }
}
```

- [ ] **Step 3: Clean up `unifyRigid` — remove polarity_open special case**

At line ~468-477, replace the `.structure` branch in `unifyRigid` with a simple `TypeMismatch`:

```zig
.structure => return error.TypeMismatch,
```

This removes the `if (a_rigid.name == .polarity_open ...)` check entirely.

- [ ] **Step 4: Add `polarity_ext` branch to `unifyFlex`**

Find the `unifyFlex` function (before `unifyRigid`). It switches on `b_content`. Add a `.polarity_ext` branch. When flex meets polarity_ext, polarity_ext wins (same as rigid absorbing flex):

```zig
.polarity_ext => {
    try self.recordDeferredConstraint(vars, flex.constraints);
    self.merge(vars, .polarity_ext);
},
```

- [ ] **Step 5: Add `polarity_ext` branch to `unifyAlias`**

Find the `unifyAlias` function. Add a `.polarity_ext` branch that returns TypeMismatch:

```zig
.polarity_ext => return error.TypeMismatch,
```

- [ ] **Step 6: Clean up `unifyStructure` — remove polarity_open special case, add polarity_ext branch**

At line ~610-616, replace the `.rigid` branch in `unifyStructure` with a simple `TypeMismatch`:

```zig
.rigid => return error.TypeMismatch,
```

Then add a `.polarity_ext` branch (symmetric case):

```zig
.polarity_ext => {
    if (a_flat_type == .empty_tag_union) {
        self.merge(vars, Content{ .structure = .empty_tag_union });
    } else {
        return error.TypeMismatch;
    }
},
```

---

### Task 4: Update instantiation (`src/types/instantiate.zig`)

**Files:**
- Modify: `src/types/instantiate.zig:78-212`

- [ ] **Step 1: Add `polarity_ext` branch in `instantiateVar`**

In the main switch on `resolved.desc.content` (around line 95), add a `.polarity_ext` branch before the `.rigid` branch or in the `else` catch-all. Since `polarity_ext` has no sub-variables, it just creates a fresh copy:

```zig
.polarity_ext => {
    const fresh_var = try self.store.freshFromContentWithRank(.polarity_ext, self.current_rank);
    try self.var_map.put(resolved_var, fresh_var);
    return fresh_var;
},
```

If the current code uses `.rigid => |rigid| { ... }, else => { ... }`, the `polarity_ext` branch must be added explicitly before the `else` to avoid falling into the generic content handler.

- [ ] **Step 2: Rename `polarity_deferred` to `polarity_pending` in the rigid branch**

At line ~97-98, change:

```zig
if (rigid.name == .polarity_deferred) {
```

to:

```zig
if (rigid.name == .polarity_pending) {
```

- [ ] **Step 3: Update the polarity resolution content creation**

At line ~100-103, change `Rigid.initPolarityOpen()` to `.polarity_ext` and `Rigid.initPolarityDeferred()` to `Rigid.initPolarityPending()`:

```zig
const fresh_content: Content = switch (pol) {
    .pos => .polarity_ext,
    .neg, .in_opaque => .{ .structure = .empty_tag_union },
    .in_alias => .{ .rigid = Rigid.initPolarityPending() },
};
```

- [ ] **Step 4: Update `substitute_rigids` path**

At line ~143, change `.polarity_open, .polarity_deferred =>` to just `.polarity_pending =>`:

```zig
.polarity_pending => {
    // polarity_pending vars should not appear in substitute_rigids context
    // (they're anonymous, not user-named rigids)
    break :inner_blk try self.store.freshFromContentWithRank(
        .{ .flex = Flex.init() },
        self.current_rank,
    );
},
```

- [ ] **Step 5: Update rigid-to-flex name conversion**

At line ~172-174, change:

```zig
.polarity_open, .polarity_deferred => null,
```

to:

```zig
.polarity_pending => null,
```

---

### Task 5: Update annotation generation and Check.zig

**Files:**
- Modify: `src/check/Check.zig`

- [ ] **Step 1: Update tag union extension creation**

At line ~2920-2922, in `generateAnnoTypeInPlace`'s `.tag_union` case, change the polarity-based extension creation:

```zig
const pol_ext = switch (polarity) {
    .pos => try self.freshFromContent(.polarity_ext, env, anno_region),
    .neg, .in_opaque => try self.freshFromContent(.{ .structure = .empty_tag_union }, env, anno_region),
    .in_alias => try self.freshFromContent(.{ .rigid = types_mod.Rigid.initPolarityPending() }, env, anno_region),
};
```

- [ ] **Step 2: Update Rigid.Name switches for alias arg extraction**

At lines ~2697-2699 and ~2781-2783, change:

```zig
.polarity_open, .polarity_deferred => unreachable,
```

to:

```zig
.polarity_pending => unreachable,
```

(Two locations — both in alias argument extraction for `substitute_rigids`.)

- [ ] **Step 3: Search for any other `polarity_open` or `polarity_deferred` references**

Search the file for any remaining references to `polarity_open`, `polarity_deferred`, `initPolarityOpen`, or `initPolarityDeferred` and update them.

---

### Task 6: Update TypeWriter display (`src/types/TypeWriter.zig`)

**Files:**
- Modify: `src/types/TypeWriter.zig`

- [ ] **Step 1: Update top-level content display**

At line ~404, add a `.polarity_ext` branch to the main content switch. `polarity_ext` as a standalone type is anonymous — in `explicit` mode show nothing meaningful (it only makes sense as a tag union/record extension), in `user_facing` mode elide:

```zig
.polarity_ext => {
    // polarity_ext is only meaningful as a tag union/record extension.
    // As a standalone content, display nothing (similar to empty rigid).
},
```

- [ ] **Step 2: Update record extension display**

The `gatherRecordFields` function (line ~725) switches on `resolved.desc.content` and returns a tagged union with variants like `.flex`, `.rigid`, `.unbound`, `.invalid`, `.empty_record`. Three changes needed:

First, add a `.polarity_ext` variant to that return type enum.

Second, in `gatherRecordFields`, add a `.polarity_ext` branch to the content switch:

```zig
.polarity_ext => {
    return .polarity_ext;
},
```

Third, in the record writing code that switches on the gathered extension result, add a `.polarity_ext` branch (same display logic as the old `polarity_open` rigid case):

```zig
.polarity_ext => {
    if (self.display_mode == .explicit) {
        if (num_fields > 0) _ = try writer.write(", ");
        _ = try writer.write("..");
    }
},
```

- [ ] **Step 3: Update tag union extension display**

Same pattern as records. In `gatherTags` (line ~782), add:

```zig
.polarity_ext => {
    return .polarity_ext;
},
```

In the tag union writing code, add a `.polarity_ext` branch:

```zig
.polarity_ext => {
    if (self.display_mode == .explicit) {
        if (num_tags > 0) _ = try writer.write(", ");
        _ = try writer.write("..");
    }
},
```

- [ ] **Step 4: Update `polarity_deferred` references to `polarity_pending`**

In the `.rigid` branches that still exist (for named rigids and `polarity_pending`), rename all `polarity_deferred` references to `polarity_pending`.

- [ ] **Step 5: Update `countVarOccurrences` content switch**

At line ~1068, add a `.polarity_ext` branch. `polarity_ext` has no sub-variables, so no recursion needed:

```zig
.polarity_ext => {},
```

- [ ] **Step 6: Update DisplayMode doc comment**

At line ~42-48, update the comment to reference `polarity_ext` instead of `polarity_open`:

```zig
pub const DisplayMode = enum {
    /// User-facing: elide polarity_ext extensions in Pos position.
    /// Open types show as [A, B], closed types show as [A, B].
    user_facing,
    /// Test/debug: always show openness explicitly.
    /// Open types show as [A, B, ..], closed types show as [A, B].
    explicit,
};
```

---

### Task 7: Update generalize.zig

**Files:**
- Modify: `src/types/generalize.zig:290-407`

- [ ] **Step 1: Add `polarity_ext` branch in `adjustRankContent`**

At line ~290, add a `.polarity_ext` branch to the content switch. `polarity_ext` has no sub-variables — return `group_rank` (same as `flex` and `rigid`):

```zig
.polarity_ext => {
    return group_rank;
},
```

---

### Task 8: Update meaningful downstream files

**Files:**
- Modify: `src/check/snapshot.zig`
- Modify: `src/check/snapshot/diff.zig`
- Modify: `src/check/copy_import.zig`
- Modify: `src/check/report.zig`
- Modify: `src/docs/extract.zig`
- Modify: `src/lsp/type_utils.zig`
- Modify: `src/lsp/syntax.zig`
- Modify: `src/lsp/completion/builder.zig`
- Modify: `src/types/store.zig`
- Modify: `src/check/occurs.zig`

For each file, find every `Content` switch and add a `.polarity_ext` branch. Also find every `Rigid.Name` switch and rename `polarity_open, .polarity_deferred` to `polarity_pending`.

- [ ] **Step 1: `src/check/snapshot.zig`**

Line ~273: `deepCopyContent` recursive name detection — add `.polarity_ext => null` to the `recursive_name` switch (no name to extract).

Line ~327: `deepCopyRigid` — rename `polarity_deferred` references to `polarity_pending` if present.

For any other `Content` switches in this file: add `.polarity_ext` that serializes as a distinct snapshot value (e.g., copies as `.polarity_ext`).

- [ ] **Step 2: `src/check/snapshot/diff.zig`**

Line ~666: `extractTagUnionExt` — the `.rigid` branch maps polarity variants to `TagExt.other`. Now add a `.polarity_ext` branch:

```zig
.polarity_ext => {
    ext = TagExt.other;
    break;
},
```

- [ ] **Step 3: `src/check/copy_import.zig`**

Line ~123: `copyRigid` — rename `polarity_deferred` to `polarity_pending`, remove `polarity_open` arm.

For any `Content` switches: add `.polarity_ext` that copies as `.polarity_ext` (no payload to translate).

- [ ] **Step 4: `src/check/report.zig`**

Find `Content` switches and add `.polarity_ext` — treat as anonymous extension (similar to how `err` is handled, or how unnamed rigids are displayed).

- [ ] **Step 5: `src/docs/extract.zig`**

Lines ~1245, ~1476, ~1645: Three `Rigid.Name` switches that map `polarity_open, .polarity_deferred` to empty string. Change to `.polarity_pending => ""`.

For any `Content` switches: add `.polarity_ext` that maps to empty type var name `""`.

- [ ] **Step 6: `src/lsp/type_utils.zig`, `src/lsp/syntax.zig`, `src/lsp/completion/builder.zig`**

For each file, find `Content` switches and add `.polarity_ext` as anonymous type var (similar to how `err` or unnamed flex vars are handled).

- [ ] **Step 7: `src/types/store.zig`**

Find `Content` switches (content iteration). `polarity_ext` has no sub-variables — skip/empty handling.

- [ ] **Step 8: `src/check/occurs.zig`**

`polarity_ext` has no sub-variables. Add `.polarity_ext => return false` or equivalent (no occurrence possible).

---

### Task 9: Update unreachable downstream files

**Files:**
- Modify: `src/check/exhaustive.zig`
- Modify: `src/layout/store.zig`
- Modify: `src/layout/type_layout_resolver.zig`
- Modify: `src/mir/Monomorphize.zig`
- Modify: `src/mir/Lower.zig`
- Modify: `src/mir/Monotype.zig`
- Modify: `src/eval/interpreter.zig`
- Modify: `src/eval/comptime_evaluator.zig`
- Modify: `src/eval/render_helpers.zig`
- Modify: `src/eval/test/helpers.zig`
- Modify: `src/glue/glue.zig`
- Modify: `src/repl/eval.zig`
- Modify: `src/interpreter_layout/store.zig`
- Modify: `src/snapshot_tool/main.zig`

`polarity_ext` should be resolved away before reaching these phases. Use `unreachable` for loud failure if it leaks through.

- [ ] **Step 1: For each file, find every `Content` switch and add `.polarity_ext => unreachable`**

The approach for each file: search for patterns like `.flex =>`, `.rigid =>`, `.structure =>`, `.err =>` to find Content switches. Add `.polarity_ext => unreachable,` to each one.

Also search for any `Rigid.Name` switches and rename `polarity_open, .polarity_deferred` to `.polarity_pending` (or just `.polarity_pending`).

Files with `Rigid.Name` switches that need renaming:
- `src/eval/interpreter.zig` (line ~9718-9724): rename `polarity_open` → remove, `polarity_deferred` → `polarity_pending`
- `src/mir/Monotype.zig` (lines ~493, ~921, ~1042): `if (rigid.name == .name)` pattern — these implicitly skip polarity variants, no change needed unless they explicitly match on `polarity_open`/`polarity_deferred`
- `src/mir/Monomorphize.zig` (~lines 7200, 7213, 10328, 10340): same pattern
- `src/mir/Lower.zig` (~lines 1546, 1559): same pattern

- [ ] **Step 2: Verify no references to `polarity_open` or `polarity_deferred` remain**

Search the entire `src/` directory for any remaining references to `polarity_open`, `polarity_deferred`, `initPolarityOpen`, or `initPolarityDeferred`. All should be gone.

---

### Task 10: Build and fix compile errors

- [ ] **Step 1: Attempt to build**

Run: `zig build test-check 2>&1 | head -50`

If there are compile errors, they will be exhaustive-switch errors for `Content` switches that were missed in Tasks 3-9. Fix each one by adding the appropriate `.polarity_ext` branch.

- [ ] **Step 2: Iterate until clean build**

Keep fixing and rebuilding until compilation succeeds with no errors.

---

### Task 11: Run type-check tests

- [ ] **Step 1: Run the type checking test suite**

Run: `zig build test-check 2>&1 | tail -30`

Expected: All tests pass, including the new composed-return test from Task 1.

- [ ] **Step 2: If any tests fail, investigate and fix**

The refactor is representational — all existing tests should produce identical output. Any failures indicate a semantic change was accidentally introduced.

---

### Task 12: Run full test suite

- [ ] **Step 1: Run the complete test suite**

Run: `zig build test 2>&1 | tail -30`

This validates that `polarity_ext` doesn't leak into MIR/interpreter/layout phases (the `unreachable` branches would crash).

Expected: All tests pass.

- [ ] **Step 2: If any tests fail, investigate and fix**

If an `unreachable` is hit, it means `polarity_ext` is leaking into a downstream phase. Investigate why it wasn't resolved during type checking and fix the root cause — do NOT change the `unreachable` to permissive handling.

---

### Task 13: Squash into single commit

- [ ] **Step 1: Squash all work into the Task 2 commit**

The implementation changes (Tasks 2-12) should be squashed into a single commit since they're one atomic refactor. Task 1 (new test) stays as its own commit.

```bash
jj squash
```

- [ ] **Step 2: Verify final state**

```bash
jj log --limit 5
jj st
```
