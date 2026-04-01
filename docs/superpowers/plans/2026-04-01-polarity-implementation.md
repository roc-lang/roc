# Polarity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement polarity tracking in the Zig type checker so that tag union extensions in annotations are implicitly open in output position and closed in input position.

**Architecture:** Extend the existing `Polarity` enum in `types.zig` with `in_alias`/`in_opaque` variants. Change `Rigid.name` from `Ident.Idx` to a tagged union `Rigid.Name` with `polarity_open`, `polarity_deferred`, and `name` variants. Thread polarity through `GenTypeAnnoCtx` in `Check.zig` annotation generation. Handle `polarity_deferred` rigids during alias instantiation. Add `DisplayMode` to TypeWriter for explicit vs user-facing output.

**Tech Stack:** Zig, `zig build test-check` for type checking tests

**Spec:** `docs/superpowers/specs/2026-03-31-polarity-design.md`

---

### Task 1: Extend the Polarity Enum

**Files:**
- Modify: `src/types/types.zig:842-850`

- [ ] **Step 1: Create commit for this task**

```bash
jj new -m "Add in_alias and in_opaque variants to Polarity enum with flip method"
```

- [ ] **Step 2: Extend the existing Polarity enum with `in_alias` and `in_opaque` variants and a `flip` method**

Replace the existing `Polarity` enum (lines 842-850):

```zig
/// Polarity of a type, or roughly, what side of an arrow it appears on.
pub const Polarity = enum {
    /// A type that appears in negative/input position
    neg,
    /// A type that appears in positive/output position
    pos,

    pub const lhs = Polarity.neg;
    pub const rhs = Polarity.pos;
};
```

With:

```zig
/// Polarity of a type, or roughly, what side of an arrow it appears on.
pub const Polarity = enum {
    /// A type that appears in negative/input position
    neg,
    /// A type that appears in positive/output position
    pos,
    /// Deferred - creates polarity_deferred rigid, resolved at alias use site
    in_alias,
    /// Always closed - EmptyTagUnion
    in_opaque,

    pub const lhs = Polarity.neg;
    pub const rhs = Polarity.pos;

    /// Flip polarity for function argument positions (standard contravariance).
    /// in_alias and in_opaque are sticky and unaffected by flipping.
    pub fn flip(self: Polarity) Polarity {
        return switch (self) {
            .pos => .neg,
            .neg => .pos,
            .in_alias => .in_alias,
            .in_opaque => .in_opaque,
        };
    }
};
```

- [ ] **Step 3: Run tests to verify no regressions**

Run: `zig build test-check`
Expected: All existing tests pass. The new variants are not yet used anywhere.

---

### Task 2: Change Rigid.name to Rigid.Name Tagged Union

**Files:**
- Modify: `src/types/types.zig:264-281`

- [ ] **Step 1: Create commit for this task**

```bash
jj new -m "Refactor Rigid.name from Ident.Idx to Rigid.Name tagged union"
```

- [ ] **Step 2: Replace the Rigid struct with the new Name tagged union**

Replace the existing `Rigid` struct (lines 264-281):

```zig
pub const Rigid = struct {
    name: Ident.Idx,
    constraints: StaticDispatchConstraint.SafeList.Range,

    pub fn init(name: Ident.Idx) Rigid {
        return .{
            .name = name,
            .constraints = StaticDispatchConstraint.SafeList.Range.empty(),
        };
    }

    pub fn withConstraints(self: Rigid, constraints: StaticDispatchConstraint.SafeList.Range) Rigid {
        return .{
            .name = self.name,
            .constraints = constraints,
        };
    }
};
```

With:

```zig
pub const Rigid = struct {
    /// The name/kind of this rigid variable.
    /// This is purely metadata for display and alias instantiation logic.
    /// It does NOT affect unification behavior - all rigid variants unify
    /// identically (can only unify with themselves, never with concrete types).
    name: Name,
    constraints: StaticDispatchConstraint.SafeList.Range,

    /// Classifies the origin of this rigid variable.
    /// Does not affect unification behavior; purely metadata for
    /// display, diagnostics, and alias instantiation logic.
    pub const Name = union(enum) {
        /// Created by polarity system in Pos position - anonymous open extension.
        polarity_open,

        /// Created by polarity system in alias bodies - must be resolved to
        /// polarity_open or EmptyTagUnion at the alias use site based on polarity.
        polarity_deferred,

        /// User-written named rigid variable (e.g., `a` in `[A, B, ..a]`).
        name: Ident.Idx,
    };

    pub fn init(ident: Ident.Idx) Rigid {
        return .{ .name = .{ .name = ident }, .constraints = StaticDispatchConstraint.SafeList.Range.empty() };
    }

    pub fn initPolarityOpen() Rigid {
        return .{ .name = .polarity_open, .constraints = StaticDispatchConstraint.SafeList.Range.empty() };
    }

    pub fn initPolarityDeferred() Rigid {
        return .{ .name = .polarity_deferred, .constraints = StaticDispatchConstraint.SafeList.Range.empty() };
    }

    pub fn withConstraints(self: Rigid, constraints: StaticDispatchConstraint.SafeList.Range) Rigid {
        return .{ .name = self.name, .constraints = constraints };
    }
};
```

- [ ] **Step 3: Attempt to build to find all compile errors from the type change**

Run: `zig build test-check 2>&1 | head -80`
Expected: Multiple compile errors in files that access `rigid.name` as `Ident.Idx`. This identifies all locations that need updating.

---

### Task 3: Update All rigid.name Usages Across the Codebase

This task fixes all the compile errors introduced by changing `Rigid.name` from `Ident.Idx` to `Rigid.Name`. Every site that accesses `rigid.name` expecting `Ident.Idx` must now switch on the tagged union or access `.name` to get the inner `Ident.Idx`.

**Files:**
- Modify: `src/types/instantiate.zig:111,140-141`
- Modify: `src/types/TypeWriter.zig:423,646,871`
- Modify: `src/check/Check.zig:2248,2429,2642,2714`
- Modify: `src/check/copy_import.zig:133`
- Modify: `src/check/snapshot.zig:276,326`
- Modify: `src/check/snapshot/diff.zig:667`
- Modify: `src/mir/Monotype.zig:494,919,1040`
- Modify: `src/mir/Lower.zig:1546,1559`
- Modify: `src/mir/Monomorphize.zig:7200,7213,10328,10340`
- Modify: `src/docs/extract.zig:1246,1474,1640,1645`
- Modify: `src/eval/interpreter.zig:904,8742,9695,9707,20447`

**Important context:** All rigid variables created from user-written type annotations (header args, explicit `..a` extensions) always have `.name = .{ .name = ident_idx }`. The new `polarity_open` and `polarity_deferred` variants only appear in implicit tag union extensions. Therefore, most call sites can safely extract the inner `Ident.Idx` via `rigid.name.name` — but some need to handle the new variants (TypeWriter, instantiate, snapshot).

- [ ] **Step 1: Create commit for this task**

```bash
jj new -m "Update all rigid.name usages across the codebase for Rigid.Name tagged union"
```

- [ ] **Step 2: Update `src/types/instantiate.zig`**

**Line 111** — `.substitute_rigids` branch. Change:
```zig
if (rigid_subs.get(rigid.name)) |existing_flex| {
```
To:
```zig
if (rigid.name == .name) {
    if (rigid_subs.get(rigid.name.name)) |existing_flex| {
        break :inner_blk existing_flex;
    } else {
        std.debug.assert(false);
        break :inner_blk try self.store.freshFromContentWithRank(
            .err,
            self.current_rank,
        );
    }
} else {
    // polarity_open and polarity_deferred rigids should never appear
    // in the substitute_rigids path.
    std.debug.assert(false);
    break :inner_blk try self.store.freshFromContentWithRank(
        .err,
        self.current_rank,
    );
}
```

More specifically, replace the full `inner_blk` block (lines 110-119):
```zig
                            const existing_var = inner_blk: {
                                if (rigid_subs.get(rigid.name)) |existing_flex| {
                                    break :inner_blk existing_flex;
                                } else {
                                    std.debug.assert(false);
                                    break :inner_blk try self.store.freshFromContentWithRank(
                                        .err,
                                        self.current_rank,
                                    );
                                }
                            };
```
With:
```zig
                            const existing_var = inner_blk: {
                                switch (rigid.name) {
                                    .name => |ident_idx| {
                                        if (rigid_subs.get(ident_idx)) |existing_flex| {
                                            break :inner_blk existing_flex;
                                        } else {
                                            std.debug.assert(false);
                                            break :inner_blk try self.store.freshFromContentWithRank(
                                                .err,
                                                self.current_rank,
                                            );
                                        }
                                    },
                                    .polarity_open, .polarity_deferred => {
                                        // Anonymous polarity rigids should never appear in substitute_rigids path.
                                        std.debug.assert(false);
                                        break :inner_blk try self.store.freshFromContentWithRank(
                                            .err,
                                            self.current_rank,
                                        );
                                    },
                                }
                            };
```

**Lines 140-141** — rigid-to-flex conversion. Change:
```zig
                    .flex => Content{ .flex = Flex{ .name = rigid.name, .constraints = fresh_constraints } },
                    .rigid => Content{ .rigid = Rigid{ .name = rigid.name, .constraints = fresh_constraints } },
```
To:
```zig
                    .flex => Content{ .flex = Flex{ .name = switch (rigid.name) {
                        .name => |ident_idx| ident_idx,
                        .polarity_open => null,
                        .polarity_deferred => unreachable, // polarity_deferred handled before reaching here
                    }, .constraints = fresh_constraints } },
                    .rigid => Content{ .rigid = Rigid{ .name = rigid.name, .constraints = fresh_constraints } },
```

- [ ] **Step 3: Update `src/types/TypeWriter.zig`**

**Line 423** — rigid var display. Change:
```zig
                _ = try writer.write(self.getIdent(rigid.name));
```
To:
```zig
                switch (rigid.name) {
                    .name => |ident_idx| {
                        _ = try writer.write(self.getIdent(ident_idx));
                    },
                    .polarity_open => {
                        // polarity_open rigids are anonymous; display nothing
                    },
                    .polarity_deferred => {
                        // polarity_deferred should be resolved before display
                        std.debug.assert(false);
                        _ = try writer.write("<unresolved>");
                    },
                }
```

**Line 646** — record extension rigid display. Change:
```zig
            const name = self.getIdent(rigid.name);
```
To:
```zig
            const name = switch (rigid.name) {
                .name => |ident_idx| self.getIdent(ident_idx),
                .polarity_open => "",
                .polarity_deferred => blk: {
                    std.debug.assert(false);
                    break :blk "<unresolved>";
                },
            };
```

**Line 871** — tag union extension rigid display. Change:
```zig
            const name = self.getIdent(rigid.name);
```
To:
```zig
            const name = switch (rigid.name) {
                .name => |ident_idx| self.getIdent(ident_idx),
                .polarity_open => "",
                .polarity_deferred => blk: {
                    std.debug.assert(false);
                    break :blk "<unresolved>";
                },
            };
```

- [ ] **Step 4: Update `src/check/Check.zig`**

**Line 2248** — header arg rigid creation. This is already correct (`Rigid.init(rigid.name)` where `rigid.name` here is from a CIR annotation, which is an `Ident.Idx`). No change needed — the CIR `rigid_var` variant has its own `.name` field which is `Ident.Idx`, not `Rigid.Name`.

**Line 2429** — rigid var with where clause constraints. Change:
```zig
            try self.unifyWith(anno_var, .{ .rigid = Rigid{
                .name = rigid.name,
                .constraints = static_dispatch_constraints_range,
            } }, env);
```
To:
```zig
            try self.unifyWith(anno_var, .{ .rigid = Rigid{
                .name = .{ .name = rigid.name },
                .constraints = static_dispatch_constraints_range,
            } }, env);
```

**Line 2642** — apply case, local rigid substitution map. Change:
```zig
                        try self.rigid_var_substitutions.put(self.gpa, decl_arg_rigid.name, anno_arg_var);
```
To:
```zig
                        try self.rigid_var_substitutions.put(self.gpa, decl_arg_rigid.name.name, anno_arg_var);
```

**Line 2714** — apply case, external rigid substitution map. Change:
```zig
                            try self.rigid_var_substitutions.put(self.gpa, decl_arg_rigid.name, anno_arg_var);
```
To:
```zig
                            try self.rigid_var_substitutions.put(self.gpa, decl_arg_rigid.name.name, anno_arg_var);
```

- [ ] **Step 5: Update `src/check/copy_import.zig`**

**Line 133** — copy rigid across modules. Change:
```zig
    const name_bytes = source_idents.getText(source_rigid.name);
```
To:
```zig
    const name_bytes = switch (source_rigid.name) {
        .name => |ident_idx| source_idents.getText(ident_idx),
        .polarity_open => "",
        .polarity_deferred => "",
    };
```

The function after this line creates a `translated_name` and builds a new Rigid with it. For polarity_open/polarity_deferred, we need to preserve the name variant directly instead of translating. Wrap the existing logic in a switch:

```zig
fn copyRigid(
    source_rigid: types.Rigid,
    source_idents: *const Ident.Store,
    dest_idents: *Ident.Store,
    // ... other params
) ... {
    const translated_name: types.Rigid.Name = switch (source_rigid.name) {
        .name => |ident_idx| blk: {
            const name_bytes = source_idents.getText(ident_idx);
            const new_ident = try dest_idents.insert(allocator, base.Ident.for_text(name_bytes));
            break :blk .{ .name = new_ident };
        },
        .polarity_open => .polarity_open,
        .polarity_deferred => .polarity_deferred,
    };
    // ... build Rigid with translated_name ...
}
```

Read the full function to confirm the exact structure and adapt accordingly.

- [ ] **Step 6: Update `src/check/snapshot.zig`**

**Line 276** — recursive name extraction. Change:
```zig
                .rigid => |rigid| rigid.name,
```
To:
```zig
                .rigid => |rigid| switch (rigid.name) {
                    .name => |ident_idx| ident_idx,
                    .polarity_open, .polarity_deferred => null,
                },
```

**Line 326** — deepCopyRigid. The `SnapshotRigid.name` field is `Ident.Idx`. Change it to `Rigid.Name`:

In `src/check/snapshot.zig`, find the `SnapshotRigid` struct (line ~45):
```zig
pub const SnapshotRigid = struct {
    name: Ident.Idx,
    constraints: SnapshotStaticDispatchConstraintSafeList.Range,
};
```
Change to:
```zig
pub const SnapshotRigid = struct {
    name: types.Rigid.Name,
    constraints: SnapshotStaticDispatchConstraintSafeList.Range,
};
```

Then line 326 (`deepCopyRigid`) requires no change since `rigid.name` is already `Rigid.Name`.

Check `src/check/snapshot/diff.zig` line 667 — it assigns `rigid.name` into a `TagExt`. Check what `TagExt.rigid` field type is and update accordingly.

- [ ] **Step 7: Update MIR files**

**`src/mir/Monotype.zig`** — lines 494, 919, 1040. These use `rigid.name` expecting `Ident.Idx`. The function `lookupNamedSpecialization` takes `Ident.Idx`. For polarity rigids (which should not appear in monomorphization), return null/skip. Change each:

Line 494:
```zig
                if (lookupNamedSpecialization(scratches, rigid.name)) |specialized| return specialized;
```
To:
```zig
                if (rigid.name == .name) {
                    if (lookupNamedSpecialization(scratches, rigid.name.name)) |specialized| return specialized;
                }
```

Line 919:
```zig
            .rigid => |rigid| lookupNamedSpecialization(scratches, rigid.name),
```
To:
```zig
            .rigid => |rigid| if (rigid.name == .name) lookupNamedSpecialization(scratches, rigid.name.name) else null,
```

Line 1040:
```zig
            .rigid => |rigid| module_env.getIdent(rigid.name),
```
To:
```zig
            .rigid => |rigid| if (rigid.name == .name) module_env.getIdent(rigid.name.name) else null,
```

**`src/mir/Lower.zig`** — lines 1546, 1559. These extract `rigid.name` to compare with platform rigid names. Change:

Line 1546:
```zig
        .rigid => |rigid| rigid.name,
```
To:
```zig
        .rigid => |rigid| if (rigid.name == .name) rigid.name.name else return,
```

Line 1559:
```zig
                .rigid => |rigid| rigid.name,
```
To:
```zig
                .rigid => |rigid| if (rigid.name == .name) rigid.name.name else continue,
```

**`src/mir/Monomorphize.zig`** — lines 7200, 7213, 10328, 10340. Same pattern as Lower.zig:

Line 7200:
```zig
            .rigid => |rigid| rigid.name,
```
To:
```zig
            .rigid => |rigid| if (rigid.name == .name) rigid.name.name else return,
```

Line 7213:
```zig
                    .rigid => |rigid| rigid.name,
```
To:
```zig
                    .rigid => |rigid| if (rigid.name == .name) rigid.name.name else continue,
```

Line 10328:
```zig
            .rigid => |rigid| rigid.name,
```
To:
```zig
            .rigid => |rigid| if (rigid.name == .name) rigid.name.name else null,
```

Line 10340:
```zig
                    .rigid => |rigid| rigid.name,
```
To:
```zig
                    .rigid => |rigid| if (rigid.name == .name) rigid.name.name else continue,
```

- [ ] **Step 8: Update docs and interpreter files**

**`src/docs/extract.zig`** — lines 1246, 1474, 1640, 1645. These call `idents.getText(rigid.name)`. For polarity rigids, use a placeholder or skip. Change each to switch on `rigid.name`:

Line 1246:
```zig
            const var_name = idents.getText(rigid.name);
```
To:
```zig
            const var_name = switch (rigid.name) {
                .name => |ident_idx| idents.getText(ident_idx),
                .polarity_open, .polarity_deferred => "",
            };
```

Apply same pattern to lines 1474, 1640, 1645.

**`src/eval/interpreter.zig`** — lines 904, 8742, 9695, 9707, 20447. These access `rigid.name` expecting `Ident.Idx`. Change to extract via `.name`:

Line 904:
```zig
                const rigid_name = resolved.desc.content.rigid.name;
```
To:
```zig
                const rigid_name = if (resolved.desc.content.rigid.name == .name) resolved.desc.content.rigid.name.name else continue;
```

(Check surrounding context for the right early-exit — `continue` vs `return` vs `break`.)

Line 8742: Same pattern. Check context for correct early-exit.

Line 9695, 9707: Same pattern. Extract `.name` from `rigid.name.name`.

Line 20447 — test assertion. Change:
```zig
    try std.testing.expectEqual(name_a, resolved.desc.content.rigid.name);
```
To:
```zig
    try std.testing.expectEqual(Rigid.Name{ .name = name_a }, resolved.desc.content.rigid.name);
```

- [ ] **Step 9: Build and run all type checking tests**

Run: `zig build test-check`
Expected: All tests pass with no behavioral change. The `Rigid.Name` union is structurally equivalent for existing code paths (all existing rigids use `.name` variant).

---

### Task 4: Thread Polarity Through Annotation Generation

**Files:**
- Modify: `src/check/Check.zig:2282-2291` (GenTypeAnnoCtx)
- Modify: `src/check/Check.zig:2317` (generateAnnotationType)
- Modify: `src/check/Check.zig:1547` (processRequiresTypes)
- Modify: `src/check/Check.zig:2226,2335,2341,2346` (other generateAnnoTypeInPlace call sites)
- Modify: `src/check/Check.zig:2740-2757` (.fn case)

- [ ] **Step 1: Create commit for this task**

```bash
jj new -m "Thread polarity through GenTypeAnnoCtx in annotation generation"
```

- [ ] **Step 2: Add Polarity import to Check.zig**

Check if `types_mod.Polarity` or `Polarity` is already accessible in Check.zig. The existing local `Polarity` enum at line 2960 (`enum { open, closed }`) is used for patterns, not annotations. Our annotation polarity comes from `types.zig`. Add an alias near the top imports (or use fully-qualified `types_mod.Polarity`).

At the top of Check.zig where imports are, check if there's already a `const types_mod = @import("types");` or similar. If so, use `types_mod.Polarity` in the code. Since there's already a local `Polarity` at line 2960 used for patterns, we should NOT shadow it. Use the qualified name `types_mod.Polarity` in the annotation code.

- [ ] **Step 3: Change GenTypeAnnoCtx to carry Polarity**

Replace (lines 2282-2291):
```zig
const GenTypeAnnoCtx = union(enum) {
    annotation,
    type_decl: struct {
        idx: CIR.Statement.Idx,
        name: Ident.Idx,
        type_: enum { nominal, alias },
        backing_var: Var,
        num_args: u32,
    },
};
```

With:
```zig
const GenTypeAnnoCtx = union(enum) {
    annotation: types_mod.Polarity,
    type_decl: struct {
        idx: CIR.Statement.Idx,
        name: Ident.Idx,
        type_: enum { nominal, alias },
        backing_var: Var,
        num_args: u32,
    },
};
```

- [ ] **Step 4: Update all `.annotation` creation sites to pass Polarity**

Line 2317 — `generateAnnotationType`:
```zig
    try self.generateAnnoTypeInPlace(annotation.anno, env, .annotation);
```
To:
```zig
    try self.generateAnnoTypeInPlace(annotation.anno, env, .{ .annotation = .pos });
```

Line 1547 — `processRequiresTypes`:
```zig
        try self.generateAnnoTypeInPlace(required_type.type_anno, env, .annotation);
```
To:
```zig
        try self.generateAnnoTypeInPlace(required_type.type_anno, env, .{ .annotation = .pos });
```

Line 2226 — another call site:
```zig
    try self.generateAnnoTypeInPlace(type_anno.anno, env, .annotation);
```
To:
```zig
    try self.generateAnnoTypeInPlace(type_anno.anno, env, .{ .annotation = .pos });
```

Lines 2335, 2341, 2346 — where clause method processing:
```zig
            try self.generateAnnoTypeInPlace(method.var_, env, .annotation);
```
To:
```zig
            try self.generateAnnoTypeInPlace(method.var_, env, .{ .annotation = .pos });
```
(Apply to all three lines.)

- [ ] **Step 5: Update all `switch (ctx) { .annotation =>` branches**

Every place in `generateAnnoTypeInPlace` that does `switch (ctx) { .annotation => { ... }` needs updating since `.annotation` now has a payload. Search for `.annotation =>` in the function and change to `.annotation => |_| {` (discarding the polarity for now). These are at lines ~2413, ~2492, ~2592.

Line 2413:
```zig
                .annotation => {
```
To:
```zig
                .annotation => |_| {
```

Apply the same pattern to all other `.annotation =>` branches that don't use the polarity value.

- [ ] **Step 6: Add polarity flipping to the `.fn` case**

Replace the `.fn` case (lines 2740-2757):
```zig
        .@"fn" => |func| {
            const args_anno_slice = self.cir.store.sliceTypeAnnos(func.args);
            for (args_anno_slice) |arg_anno_idx| {
                try self.generateAnnoTypeInPlace(arg_anno_idx, env, ctx);
            }
            const args_var_slice: []Var = @ptrCast(args_anno_slice);

            try self.generateAnnoTypeInPlace(func.ret, env, ctx);

            const fn_type = inner_blk: {
                if (func.effectful) {
                    break :inner_blk try self.types.mkFuncEffectful(args_var_slice, ModuleEnv.varFrom(func.ret));
                } else {
                    break :inner_blk try self.types.mkFuncPure(args_var_slice, ModuleEnv.varFrom(func.ret));
                }
            };
            try self.unifyWith(anno_var, fn_type, env);
        },
```

With:
```zig
        .@"fn" => |func| {
            // Args: flip polarity (standard contravariance)
            const arg_ctx: GenTypeAnnoCtx = switch (ctx) {
                .annotation => |pol| .{ .annotation = pol.flip() },
                .type_decl => ctx, // sticky in_alias/in_opaque
            };
            const args_anno_slice = self.cir.store.sliceTypeAnnos(func.args);
            for (args_anno_slice) |arg_anno_idx| {
                try self.generateAnnoTypeInPlace(arg_anno_idx, env, arg_ctx);
            }
            const args_var_slice: []Var = @ptrCast(args_anno_slice);

            // Return: preserve parent polarity
            try self.generateAnnoTypeInPlace(func.ret, env, ctx);

            const fn_type = inner_blk: {
                if (func.effectful) {
                    break :inner_blk try self.types.mkFuncEffectful(args_var_slice, ModuleEnv.varFrom(func.ret));
                } else {
                    break :inner_blk try self.types.mkFuncPure(args_var_slice, ModuleEnv.varFrom(func.ret));
                }
            };
            try self.unifyWith(anno_var, fn_type, env);
        },
```

- [ ] **Step 7: Run tests**

Run: `zig build test-check`
Expected: All tests pass. No behavioral change yet — polarity is threaded but not used in tag union ext creation.

---

### Task 5: Polarity-Based Tag Union Extension Creation

**Files:**
- Modify: `src/check/Check.zig:2795-2802` (.tag_union ext creation)

- [ ] **Step 1: Create commit for this task**

```bash
jj new -m "Implement polarity-based tag union extension creation"
```

- [ ] **Step 2: Write the first failing test — basic Pos open return**

Add to `src/check/test/type_checking_integration.zig` at the end (before the final closing `}`):

```zig
test "check type - polarity - basic pos open return" {
    const source =
        \\foo : Str -> [Ok U64, Err Str]
        \\foo = |_| Ok(1)
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "Str -> [Err Str, Ok U64, ..]");
}
```

- [ ] **Step 3: Run test to verify it fails**

Run: `zig build test-check -- "check type - polarity - basic pos open return" 2>&1 | tail -20`
Expected: FAIL — currently the return type will be `Str -> [Err Str, Ok U64]` (closed, no `..`).

- [ ] **Step 4: Implement polarity-based extension creation in tag_union case**

Replace the ext_var block in `.tag_union` (lines ~2795-2802):
```zig
            // Process the ext if it exists. Absence means it's a closed union
            const ext_var = inner_blk: {
                if (tag_union.ext) |ext_anno_idx| {
                    try self.generateAnnoTypeInPlace(ext_anno_idx, env, ctx);
                    break :inner_blk ModuleEnv.varFrom(ext_anno_idx);
                } else {
                    break :inner_blk try self.freshFromContent(.{ .structure = .empty_tag_union }, env, anno_region);
                }
            };
```

With:
```zig
            // Process the ext based on polarity
            const ext_var = inner_blk: {
                if (tag_union.ext) |ext_anno_idx| {
                    // User wrote explicit extension — process as before
                    try self.generateAnnoTypeInPlace(ext_anno_idx, env, ctx);
                    break :inner_blk ModuleEnv.varFrom(ext_anno_idx);
                } else {
                    // No explicit extension — use polarity to determine openness
                    const polarity: types_mod.Polarity = switch (ctx) {
                        .annotation => |pol| pol,
                        .type_decl => |decl| switch (decl.type_) {
                            .alias => .in_alias,
                            .nominal => .in_opaque,
                        },
                    };
                    break :inner_blk switch (polarity) {
                        .pos => try self.freshFromContent(.{ .rigid = types_mod.Rigid.initPolarityOpen() }, env, anno_region),
                        .neg, .in_opaque => try self.freshFromContent(.{ .structure = .empty_tag_union }, env, anno_region),
                        .in_alias => try self.freshFromContent(.{ .rigid = types_mod.Rigid.initPolarityDeferred() }, env, anno_region),
                    };
                }
            };
```

- [ ] **Step 5: Run the test to verify it passes**

Run: `zig build test-check -- "check type - polarity - basic pos open return" 2>&1 | tail -20`
Expected: PASS

- [ ] **Step 6: Run all tests to check for regressions**

Run: `zig build test-check`
Expected: May have some failures in existing tests that now behave differently due to return types becoming open. Identify which tests fail.

---

### Task 6: Update Existing Tests for New Polarity Semantics

**Files:**
- Modify: `src/check/test/type_checking_integration.zig:6127-6156` (ext hints 1)
- Modify: `src/check/test/type_checking_integration.zig:6158-6184` (ext hints 2)

Two existing tests are expected to change behavior. Any other failures found in Task 5 Step 6 should also be addressed here.

- [ ] **Step 1: Create commit for this task**

```bash
jj new -m "Update existing tests for new polarity semantics"
```

- [ ] **Step 2: Update "ext hints 1" test**

The test at line ~6127 tests `bar : [A, B] -> [X, Y]` where `foo` calls `bar` and expects `[X, Y, ..]`. Previously `bar`'s return was closed, causing a mismatch. Now `bar`'s return is open (polarity_open rigid), so `bar(tag)` returns an open type that unifies with `foo`'s open return.

Read the test carefully and determine the new expected behavior. If it now passes, change from `fail_with` to `pass`:

```zig
test "check type - tag union - ext hints 1" {
    const source =
        \\bar : [A, B] -> [X, Y]
        \\bar = |_| X
        \\
        \\foo : [A, B] -> [X, Y, ..]
        \\foo = |tag| bar(tag)
    ;
    try checkTypesModuleDefs(source, &.{
        .{ .def = "bar", .expected = "([A, B] -> [X, Y, ..])" },
        .{ .def = "foo", .expected = "([A, B] -> [X, Y, ..])" },
    });
}
```

Note: The exact expected types depend on how the rigid variables unify. Run the test with a diagnostic print to see the actual inferred types, then set the correct expectations.

- [ ] **Step 3: Update "ext hints 2" test**

The test at line ~6158 tests `foo : [A, B, ..] -> [A, B]` where body is `|a| a`. With polarity, the arg `[A, B, ..]` is in Neg position (closed since user wrote explicit `..`, which creates a flex extension), and the return `[A, B]` is in Pos position (open via polarity_open rigid). The body `a` has the arg type (closed flex) which can't unify with the return's rigid ext. The test should still fail but with a different error message.

Read the actual error output and update the expected message accordingly.

- [ ] **Step 4: Fix any other failing tests identified in Task 5**

Run `zig build test-check` and fix any other tests that are affected by the new polarity behavior. Each fix should reflect the correct new semantics.

- [ ] **Step 5: Run all tests**

Run: `zig build test-check`
Expected: All tests pass.

---

### Task 7: Add instantiateVarWithPolarity Helper

**Files:**
- Modify: `src/types/instantiate.zig:36-44` (Instantiator struct)
- Modify: `src/check/Check.zig` (add helper, update alias use sites)

- [ ] **Step 1: Create commit for this task**

```bash
jj new -m "Add polarity-aware alias instantiation via instantiateVarWithPolarity"
```

- [ ] **Step 2: Add polarity field to Instantiator**

In `src/types/instantiate.zig`, add `polarity` field to the Instantiator struct (after line 43):

Change (lines 36-44):
```zig
pub const Instantiator = struct {
    // not owned
    store: *TypesStore,
    idents: *const base.Ident.Store,
    var_map: *std.AutoHashMap(Var, Var),

    current_rank: Rank,
    rigid_behavior: RigidBehavior,
    rank_behavior: RankBehavior = .respect_rank,
```

To:
```zig
pub const Instantiator = struct {
    // not owned
    store: *TypesStore,
    idents: *const base.Ident.Store,
    var_map: *std.AutoHashMap(Var, Var),

    current_rank: Rank,
    rigid_behavior: RigidBehavior,
    rank_behavior: RankBehavior = .respect_rank,
    polarity: ?types.Polarity = null,
```

Also add `const types = @import("types.zig");` at the top if not already imported (check existing imports).

- [ ] **Step 3: Handle polarity_deferred in the rigid branch of instantiateVar**

In `src/types/instantiate.zig`, at the start of the `.rigid` branch (line ~95), before the `fresh_type` block, add a polarity_deferred check:

Insert before the existing `const fresh_type: enum { flex, rigid } = blk: {` line:

```zig
                // Handle polarity_deferred rigids: resolve based on polarity context
                if (rigid.name == .polarity_deferred) {
                    if (self.polarity) |pol| {
                        const fresh_content: Content = switch (pol) {
                            .pos => .{ .rigid = types.Rigid.initPolarityOpen() },
                            .neg, .in_opaque => .{ .structure = .empty_tag_union },
                            .in_alias => .{ .rigid = types.Rigid.initPolarityDeferred() },
                        };
                        const fresh_var = try self.store.freshFromContentWithRank(fresh_content, self.current_rank);
                        try self.var_map.put(resolved_var, fresh_var);
                        return fresh_var;
                    } else {
                        // Deferred rigids must always be instantiated with polarity context.
                        std.debug.assert(false);
                        const fresh_var = try self.store.freshFromContentWithRank(.err, self.current_rank);
                        try self.var_map.put(resolved_var, fresh_var);
                        return fresh_var;
                    }
                }
```

- [ ] **Step 4: Add instantiateVarWithPolarity helper to Check.zig**

Add after the existing `instantiateVarWithSubs` function (~line 675):

```zig
fn instantiateVarWithPolarity(
    self: *Self,
    var_to_instantiate: Var,
    env: *Env,
    polarity: types_mod.Polarity,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
    const trace = tracy.trace(@src());
    defer trace.end();

    var instantiate_ctx = Instantiator{
        .store = self.types,
        .idents = self.cir.getIdentStoreConst(),
        .var_map = &self.var_map,

        .current_rank = env.rank(),
        .rigid_behavior = .fresh_flex,
        .polarity = polarity,
    };
    return self.instantiateVarHelp(var_to_instantiate, &instantiate_ctx, env, region_behavior);
}
```

- [ ] **Step 5: Update alias use sites in generateAnnoTypeInPlace to use polarity instantiation**

In the `.lookup` / `.local` case (line ~2504), change:
```zig
                        const instantiated_var = try self.instantiateVar(local_decl_var, env, .{ .explicit = anno_region });
```
To:
```zig
                        const polarity: types_mod.Polarity = switch (ctx) {
                            .annotation => |pol| pol,
                            .type_decl => |decl| switch (decl.type_) {
                                .alias => .in_alias,
                                .nominal => .in_opaque,
                            },
                        };
                        const instantiated_var = try self.instantiateVarWithPolarity(local_decl_var, env, polarity, .{ .explicit = anno_region });
```

Do the same for the `.external` case (line ~2521):
```zig
                        const ext_instantiated_var = try self.instantiateVar(
                            ext_ref.local_var,
                            env,
                            .{ .explicit = anno_region },
                        );
```
To:
```zig
                        const polarity: types_mod.Polarity = switch (ctx) {
                            .annotation => |pol| pol,
                            .type_decl => |decl| switch (decl.type_) {
                                .alias => .in_alias,
                                .nominal => .in_opaque,
                            },
                        };
                        const ext_instantiated_var = try self.instantiateVarWithPolarity(
                            ext_ref.local_var,
                            env,
                            polarity,
                            .{ .explicit = anno_region },
                        );
```

Also update the `.apply` case — both local (line ~2648) and external (line ~2720) `instantiateVarWithSubs` calls. These use `substitute_rigids`, not `fresh_flex`, so polarity won't affect named rigids. But we still need to pass it for polarity_deferred rigids that aren't substituted. Add polarity to these Instantiator creations too. The cleanest approach: add a `instantiateVarWithSubsAndPolarity` helper, or modify `instantiateVarWithSubs` to accept optional polarity.

Actually, the simpler approach: just set `polarity` on the existing `instantiateVarWithSubs` call. Since we added `polarity: ?types.Polarity = null` as a default field, the Instantiator in `instantiateVarWithSubs` won't have polarity set by default. We need to either:
- Accept it: substitute_rigids path with named rigids won't encounter polarity_deferred (they're in the same alias body, so deferred rigids only appear in the alias' own body, not in its args). The debug assert in the substitute_rigids branch will catch any bugs.
- Or add polarity to `instantiateVarWithSubs` too.

For safety, leave `instantiateVarWithSubs` as-is (no polarity). The debug assert in the substitute_rigids `.polarity_open, .polarity_deferred` branch will catch any incorrect usage.

- [ ] **Step 6: Write failing test for alias in pos (deferred resolves to open)**

Add test:
```zig
test "check type - polarity - alias in pos resolves to open" {
    const source =
        \\MyResult a : [Ok a, Err Str]
        \\
        \\foo : Str -> MyResult U64
        \\foo = |_| Ok(1)
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "Str -> MyResult U64");
}
```

Note: The expected output depends on how the type writer displays alias types. The outer type is the alias `MyResult U64`, but its backing type should be open. Adjust expectation based on actual output.

- [ ] **Step 7: Run test**

Run: `zig build test-check -- "check type - polarity - alias in pos" 2>&1 | tail -20`
Expected: PASS (the alias instantiation resolves polarity_deferred to polarity_open in pos position)

- [ ] **Step 8: Write failing test for alias in neg (deferred resolves to closed)**

```zig
test "check type - polarity - alias in neg resolves to closed" {
    const source =
        \\MyResult a : [Ok a, Err Str]
        \\
        \\bar : MyResult U64 -> Bool
        \\bar = |result|
        \\    when result is
        \\        Ok(_) -> Bool.true
        \\        Err(_) -> Bool.false
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "bar" } }, "MyResult U64 -> Bool");
}
```

- [ ] **Step 9: Run test**

Run: `zig build test-check -- "check type - polarity - alias in neg" 2>&1 | tail -20`
Expected: PASS

- [ ] **Step 10: Run all tests**

Run: `zig build test-check`
Expected: All tests pass.

---

### Task 8: Add DisplayMode to TypeWriter

**Files:**
- Modify: `src/types/TypeWriter.zig:44-68` (struct), `src/types/TypeWriter.zig:117-141` (init), `src/types/TypeWriter.zig:868-887` (tag union ext display)
- Modify: `src/canonicalize/ModuleEnv.zig:3116-3118` (initTypeWriter)
- Modify: `src/check/test/type_checking_integration.zig` (test helpers)
- Modify: `src/check/test/TestEnv.zig`

- [ ] **Step 1: Create commit for this task**

```bash
jj new -m "Add DisplayMode to TypeWriter with explicit/user_facing modes"
```

- [ ] **Step 2: Add DisplayMode enum to TypeWriter**

Add near the top of TypeWriter.zig (after the existing TypeContext enum, around line 40):

```zig
pub const DisplayMode = enum {
    /// User-facing: elide polarity_open extensions in Pos position.
    /// Open types show as [A, B], closed types show as [A, B].
    user_facing,
    /// Test/debug: always show openness explicitly.
    /// Open types show as [A, B, ..], closed types show as [A, B].
    explicit,
};
```

- [ ] **Step 3: Add display_mode field to TypeWriter struct**

Add to the struct fields (after `default_numerals_to_dec`):
```zig
/// Controls how polarity_open rigid extensions are displayed.
display_mode: DisplayMode = .user_facing,
```

- [ ] **Step 4: Update tag union extension display for polarity_open**

In the tag union ext writing code (line ~868), the `.rigid` branch currently shows `..` unconditionally. Update to check `display_mode`:

Replace the `.rigid` branch (lines ~868-878):
```zig
            .rigid => |rigid| {
                if (num_tags > 0) _ = try writer.write(", ");
                _ = try writer.write("..");
                const name = switch (rigid.name) {
                    .name => |ident_idx| self.getIdent(ident_idx),
                    .polarity_open => "",
                    .polarity_deferred => blk: {
                        std.debug.assert(false);
                        break :blk "<unresolved>";
                    },
                };
                // Suppress internal names (e.g. #open_ext_0 from anonymous `..`)
                if (name.len == 0 or name[0] != '#') {
                    _ = try writer.write(name);
                }

                for (self.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(tag_union.ext, constraint);
                }
            },
```

With:
```zig
            .rigid => |rigid| {
                switch (rigid.name) {
                    .polarity_open => {
                        // In explicit mode, show ".." for open tag unions
                        // In user_facing mode, elide the extension
                        if (self.display_mode == .explicit) {
                            if (num_tags > 0) _ = try writer.write(", ");
                            _ = try writer.write("..");
                        }
                    },
                    .polarity_deferred => {
                        std.debug.assert(false);
                        if (num_tags > 0) _ = try writer.write(", ");
                        _ = try writer.write("..<unresolved>");
                    },
                    .name => |ident_idx| {
                        if (num_tags > 0) _ = try writer.write(", ");
                        _ = try writer.write("..");
                        const name = self.getIdent(ident_idx);
                        // Suppress internal names (e.g. #open_ext_0 from anonymous `..`)
                        if (name.len == 0 or name[0] != '#') {
                            _ = try writer.write(name);
                        }
                    },
                }

                for (self.types.sliceStaticDispatchConstraints(rigid.constraints)) |constraint| {
                    try self.appendStaticDispatchConstraint(tag_union.ext, constraint);
                }
            },
```

Apply the same pattern to the record extension rigid display (~line 643).

- [ ] **Step 5: Update initTypeWriter to accept DisplayMode**

Update `initFromParts` (lines 117-141) to accept a `display_mode` parameter:

```zig
pub fn initFromParts(
    gpa: std.mem.Allocator,
    types_store: *const TypesStore,
    idents: *const Ident.Store,
    import_mapping: ?*const import_mapping_mod.ImportMapping,
    display_mode: DisplayMode,
) std.mem.Allocator.Error!TypeWriter {
    return .{
        // ... existing fields ...
        .display_mode = display_mode,
    };
}
```

Update `initFromParts` call in `ModuleEnv.zig` line 3117:
```zig
    return TypeWriter.initFromParts(self.gpa, &self.types, self.getIdentStore(), null, .user_facing);
```

Search for all other callers of `initFromParts` and update them. They should default to `.user_facing`.

- [ ] **Step 6: Update test infrastructure to use explicit mode**

In `src/check/test/TestEnv.zig`, update the two places where `initTypeWriter()` is called (lines 265, 375). Since `initTypeWriter` calls `initFromParts`, the test infrastructure needs to use `.explicit` mode.

Add a parameter to `initTypeWriter` in ModuleEnv.zig:
```zig
pub fn initTypeWriter(self: *Self) std.mem.Allocator.Error!TypeWriter {
    return TypeWriter.initFromParts(self.gpa, &self.types, self.getIdentStore(), null, .user_facing);
}

pub fn initTypeWriterExplicit(self: *Self) std.mem.Allocator.Error!TypeWriter {
    return TypeWriter.initFromParts(self.gpa, &self.types, self.getIdentStore(), null, .explicit);
}
```

Update TestEnv.zig to use `initTypeWriterExplicit()` so all existing tests use explicit mode (showing `..` for open types):

Line 265 and 375:
```zig
    var type_writer = try module_env.initTypeWriterExplicit();
```

- [ ] **Step 7: Run all tests with explicit mode**

Run: `zig build test-check`
Expected: Tests that were passing before now show `..` for polarity_open returns in explicit mode. Existing tests that compare against closed types should still pass. The polarity tests from Task 5 should pass since they already expect `..`.

- [ ] **Step 8: Add checkTypesModuleUserFacing helper**

In `src/check/test/type_checking_integration.zig`, add after `checkTypesModuleDefs`:

```zig
fn checkTypesModuleUserFacing(
    comptime source_expr: []const u8,
    comptime expectation: ModuleExpectation,
    comptime expected: []const u8,
) !void {
    var test_env = try TestEnv.initUserFacing("Test", source_expr);
    defer test_env.deinit();

    switch (expectation) {
        .pass => |def_expectation| {
            switch (def_expectation) {
                .last_def => {
                    return test_env.assertLastDefType(expected);
                },
                .def => |def_name| {
                    return test_env.assertDefType(def_name, expected);
                },
            }
        },
        .fail => {
            return test_env.assertOneTypeError(expected);
        },
        .fail_with => {
            return test_env.assertOneTypeErrorMsg(expected);
        },
        .fail_first => {
            return test_env.assertFirstTypeError(expected);
        },
    }
}
```

This requires adding a `TestEnv.initUserFacing` in `src/check/test/TestEnv.zig`. Copy the existing `TestEnv.init` function and change only the TypeWriter initialization line:

```zig
pub fn initUserFacing(comptime module_name: []const u8, comptime source_expr: []const u8) !TestEnv {
    // ... identical to init() except:
    var type_writer = try module_env.initTypeWriter(); // user_facing mode (default)
    // ... rest identical to init() ...
}
```

The key difference: `initTypeWriterExplicit()` is used in `init()` (for existing tests), `initTypeWriter()` (user_facing default) is used in `initUserFacing()`.

---

### Task 9: Add Remaining Polarity Tests

**Files:**
- Modify: `src/check/test/type_checking_integration.zig`

- [ ] **Step 1: Create commit for this task**

```bash
jj new -m "Add comprehensive polarity tests including pass, fail, and user-facing display"
```

- [ ] **Step 2: Add basic neg test (closed arg)**

```zig
test "check type - polarity - basic neg closed arg" {
    const source =
        \\foo : [A, B] -> Str
        \\foo = |tag|
        \\    when tag is
        \\        A -> "a"
        \\        B -> "b"
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "[A, B] -> Str");
}
```

- [ ] **Step 3: Run test**

Run: `zig build test-check -- "check type - polarity - basic neg closed arg"`
Expected: PASS (neg position = EmptyTagUnion = closed, same as before)

- [ ] **Step 4: Add higher-order flip test**

```zig
test "check type - polarity - higher order flip" {
    const source =
        \\foo : ([A, B] -> [X, Y]) -> Str
        \\foo = |callback|
        \\    when callback(A) is
        \\        X -> "x"
        \\        Y -> "y"
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "([A, B, ..] -> [X, Y]) -> Str");
}
```

In this test: the callback arg `[A, B]` is in neg-of-neg = pos (flipped twice) = open. The callback return `[X, Y]` is in neg (flipped once from outer pos) = closed.

- [ ] **Step 5: Run test**

Run: `zig build test-check -- "check type - polarity - higher order flip"`
Expected: PASS

- [ ] **Step 6: Add opaque always-closed test**

```zig
test "check type - polarity - opaque always closed" {
    const source =
        \\Color := [Red, Green, Blue]
        \\
        \\foo : Str -> Color
        \\foo = |_| @Color(Red)
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "Str -> Color");
}
```

- [ ] **Step 7: Run test**

Run: `zig build test-check -- "check type - polarity - opaque always closed"`
Expected: PASS

- [ ] **Step 8: Add failing test — body returns closed value to open return**

```zig
test "check type - polarity - closed body to open return fails" {
    const source =
        \\closedBody : [Ok U64, Err Str] -> [Ok U64, Err Str]
        \\closedBody = |input| input
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}
```

The arg `[Ok U64, Err Str]` is neg = closed. The return `[Ok U64, Err Str]` is pos = open (polarity_open rigid). Body `input` has the closed type. Unifying closed with polarity_open rigid fails.

- [ ] **Step 9: Run test**

Run: `zig build test-check -- "check type - polarity - closed body to open return fails"`
Expected: PASS (the test expects failure, and the type checker should produce a TYPE MISMATCH)

- [ ] **Step 10: Add failing test — body adds undeclared tag**

```zig
test "check type - polarity - undeclared tag fails" {
    const source =
        \\sneaky : Str -> [Ok U64, Err Str]
        \\sneaky = |s| Bad(s)
    ;
    try checkTypesModule(source, .fail, "TYPE MISMATCH");
}
```

- [ ] **Step 11: Run test**

Run: `zig build test-check -- "check type - polarity - undeclared tag fails"`
Expected: PASS

- [ ] **Step 12: Add user-facing display tests**

```zig
test "check type - polarity - user facing pos return elided" {
    const source =
        \\foo : Str -> [Ok U64, Err Str]
        \\foo = |_| Ok(1)
    ;
    try checkTypesModuleUserFacing(source, .{ .pass = .{ .def = "foo" } }, "Str -> [Err Str, Ok U64]");
}

test "check type - polarity - user facing neg arg with flex ext shown" {
    const source =
        \\foo : [A, B, ..] -> Str
        \\foo = |_| "hello"
    ;
    try checkTypesModuleUserFacing(source, .{ .pass = .{ .def = "foo" } }, "[A, B, ..] -> Str");
}
```

- [ ] **Step 13: Run user-facing tests**

Run: `zig build test-check -- "check type - polarity - user facing"`
Expected: PASS

- [ ] **Step 14: Run all tests**

Run: `zig build test-check`
Expected: All tests pass.

---

### Task 10: Add Redundant `..` Warning

**Files:**
- Modify: `src/check/problem/types.zig:32-53` (Problem union)
- Modify: `src/check/Check.zig` (tag_union ext creation)
- Modify: `src/check/test/type_checking_integration.zig` (diagnostic tests)

- [ ] **Step 1: Create commit for this task**

```bash
jj new -m "Add unnecessary_wildcard_ext warning for redundant .. in Pos position"
```

- [ ] **Step 2: Add unnecessary_wildcard_ext to Problem union**

In `src/check/problem/types.zig`, add to the Problem union (after `unmatchable_pattern`):
```zig
    unnecessary_wildcard_ext: UnnecessaryWildcardExt,
```

Add the struct definition after the existing problem structs:
```zig
/// Warning for when a user writes an explicit `..` extension in a position
/// where the tag union is already implicitly open due to polarity.
pub const UnnecessaryWildcardExt = struct {
    region: base.Region,
};
```

- [ ] **Step 3: Add warning emission in tag_union case**

In `src/check/Check.zig`, in the `.tag_union` case of `generateAnnoTypeInPlace`, when the ext is present (`tag_union.ext != null`), add a polarity check before processing:

In the ext_var block, update the `if (tag_union.ext) |ext_anno_idx|` branch:

```zig
                if (tag_union.ext) |ext_anno_idx| {
                    // Check if the explicit extension is redundant in Pos position
                    const polarity: types_mod.Polarity = switch (ctx) {
                        .annotation => |pol| pol,
                        .type_decl => |decl| switch (decl.type_) {
                            .alias => .in_alias,
                            .nominal => .in_opaque,
                        },
                    };
                    if (polarity == .pos) {
                        _ = try self.problems.appendProblem(self.gpa, .{ .unnecessary_wildcard_ext = .{
                            .region = anno_region,
                        } });
                    }
                    // Process the ext as before
                    try self.generateAnnoTypeInPlace(ext_anno_idx, env, ctx);
                    break :inner_blk ModuleEnv.varFrom(ext_anno_idx);
                }
```

- [ ] **Step 4: Write diagnostic tests**

```zig
test "check type - polarity - redundant wildcard ext in pos" {
    const source =
        \\foo : Str -> [Ok U64, Err Str, ..]
        \\foo = |_| Ok(1)
    ;
    // This should pass but emit a warning about unnecessary ..
    // The test infrastructure may need to check for warnings specifically.
    // For now, test that the type is correct.
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "Str -> [Err Str, Ok U64, ..]");
}

test "check type - polarity - non-redundant wildcard ext in neg" {
    const source =
        \\foo : [A, B, ..] -> Str
        \\foo = |_| "hello"
    ;
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "[A, B, ..] -> Str");
}

test "check type - polarity - redundant wildcard ext from flip" {
    const source =
        \\foo : ([A, B, ..] -> Str) -> Str
        \\foo = |callback| callback(A)
    ;
    // The callback arg [A, B, ..] is in neg-of-neg = pos, so .. is redundant
    try checkTypesModule(source, .{ .pass = .{ .def = "foo" } }, "([A, B, ..] -> Str) -> Str");
}
```

Note: Testing that the warning is actually emitted requires checking the problems list. Add a test helper or use an existing one that checks problem counts. If no such helper exists, add one:

```zig
fn assertProblemCount(test_env: *TestEnv, expected_count: usize) !void {
    const actual_count = test_env.checker.problems.count();
    try std.testing.expectEqual(expected_count, actual_count);
}
```

Then in the redundant `..` test, verify that a problem was emitted.

- [ ] **Step 5: Run tests**

Run: `zig build test-check -- "check type - polarity - redundant"`
Expected: PASS

- [ ] **Step 6: Run all tests**

Run: `zig build test-check`
Expected: All tests pass. Check that the new warning doesn't cause any existing tests with explicit `..` to break (the warning is non-fatal).

---

### Task 11: Final Verification

**Files:** None (verification only)

- [ ] **Step 1: Create commit for this task**

```bash
jj new -m "Final polarity implementation cleanup and verification"
```

- [ ] **Step 2: Run the full type checking test suite**

Run: `zig build test-check`
Expected: All tests pass.

- [ ] **Step 3: Run a broader build to check for issues in downstream code**

Run: `zig build 2>&1 | head -30`
Expected: No new build errors.

- [ ] **Step 4: Review the changes holistically**

Check:
- All `rigid.name` usages are properly handling the new `Name` union
- No polarity_deferred rigids leak to display (debug asserts catch this)
- No polarity_open/polarity_deferred rigids appear in substitute_rigids (debug asserts catch this)
- The existing `Polarity` enum in Check.zig (line 2960, `open/closed` for patterns) is unaffected

- [ ] **Step 5: Squash into parent if no cleanup needed, or describe changes**

If no cleanup was needed, abandon this empty commit:
```bash
jj abandon @
```

If cleanup was made, the commit message from Step 1 already describes it.
