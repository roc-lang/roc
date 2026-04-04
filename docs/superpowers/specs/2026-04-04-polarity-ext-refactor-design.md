# Polarity Ext Refactor: First-Class Content Variant

## Goal

Promote `polarity_open` from a `Rigid.Name` metadata variant to a first-class `Content` variant (`polarity_ext`), and rename `polarity_deferred` to `polarity_pending`. This makes the type representation honest about unification semantics — `polarity_ext` has non-rigid unification rules and should not masquerade as a rigid variable.

## Motivation

The current implementation encodes `polarity_open` as a `Rigid.Name` variant with comments claiming it "does NOT affect unification behavior." This is false — `unify.zig` has two special cases where `polarity_open` rigids can unify with `empty_tag_union`, which real rigids cannot do. The result is:

1. **Misleading comments** that will confuse future contributors
2. **Fragile special-casing** in unification that's easy to miss during refactors
3. **Semantic dishonesty** — code matching on `.rigid` expects rigid behavior but `polarity_open` doesn't follow those rules

`polarity_deferred` (renamed to `polarity_pending`) remains as a `Rigid.Name` variant because it genuinely behaves as a rigid — it should never reach unification (resolved during instantiation), and `TypeMismatch` is correct if it does.

## Part 1: Type Representation (`src/types/types.zig`)

### Content Union

Add `polarity_ext` as a new variant with no payload:

```zig
pub const Content = union(enum) {
    flex: Flex,
    rigid: Rigid,
    polarity_ext,       // open tag union extension (positive position)
    alias: Alias,
    structure: FlatType,
    err,
};
```

### Rigid.Name

Remove `polarity_open`, rename `polarity_deferred` to `polarity_pending`:

```zig
pub const Rigid = struct {
    name: Name,
    constraints: StaticDispatchConstraint.SafeList.Range,

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

Remove `initPolarityOpen()`.

## Part 2: Unification (`src/check/unify.zig`)

### New `unifyPolarityExt` Function

```zig
fn unifyPolarityExt(self: *Self, vars: *const ResolvedVarDescs, b_content: Content) Error!void {
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

### Symmetric Case in `unifyStructure`

Add a `.polarity_ext` branch:

```zig
.polarity_ext => {
    if (a_flat_type == .empty_tag_union) {
        self.merge(vars, Content{ .structure = .empty_tag_union });
    } else {
        return error.TypeMismatch;
    }
},
```

### Cleanup

- Remove the `polarity_open` check from `unifyRigid` (`.structure` branch reverts to unconditional `TypeMismatch`)
- Remove the `polarity_open` check from `unifyStructure` (`.rigid` branch reverts to unconditional `TypeMismatch`)

### Constraint Strength Ordering

`rigid` > `polarity_ext` > `flex`. When `polarity_ext` meets `flex`, `polarity_ext` wins (absorbs the flex). This preserves the annotation's constraint that the body can close the extension but cannot add undeclared tags.

## Part 3: Instantiation (`src/types/instantiate.zig`)

### `polarity_pending` Resolution

In the `.rigid` branch of `instantiateVar`, the `polarity_deferred` check becomes `polarity_pending`:

```zig
if (rigid.name == .polarity_pending) {
    if (self.polarity) |pol| {
        const fresh_content: Content = switch (pol) {
            .pos => .polarity_ext,
            .neg, .in_opaque => .{ .structure = .empty_tag_union },
            .in_alias => .{ .rigid = Rigid.initPolarityPending() },
        };
        // ... create fresh var, put in var_map, return
    }
}
```

### New `polarity_ext` Branch

```zig
.polarity_ext => {
    const fresh_var = try self.store.freshFromContentWithRank(.polarity_ext, self.current_rank);
    try self.var_map.put(resolved_var, fresh_var);
    return fresh_var;
},
```

### `substitute_rigids` Simplification

The `polarity_open, .polarity_deferred` arm becomes just `.polarity_pending` (since `polarity_ext` is no longer a rigid and won't enter the rigid path).

### Rigid-to-Flex Conversion

The `flex_name` switch simplifies:

```zig
const flex_name: ?Ident.Idx = switch (rigid.name) {
    .name => |ident_idx| ident_idx,
    .polarity_pending => null,
};
```

## Part 4: Annotation Generation (`src/check/Check.zig`)

In `generateAnnoTypeInPlace` for `.tag_union`, when no explicit extension is written:

| Polarity | Extension created |
|---|---|
| `.pos` | `Content.polarity_ext` |
| `.neg` | `Content{ .structure = .empty_tag_union }` |
| `.in_alias` | `Content{ .rigid = Rigid.initPolarityPending() }` |
| `.in_opaque` | `Content{ .structure = .empty_tag_union }` |

All `polarity_deferred` references renamed to `polarity_pending`.

## Part 5: Downstream Content Switch Updates

Every `Content` switch needs a `.polarity_ext` branch. Behavior by category:

### Unreachable (loud crash if reached)

`polarity_ext` should be resolved away before these phases. Use `unreachable` to catch leaks:

| File | Switches |
|---|---|
| `src/check/exhaustive.zig` | ~13 |
| `src/check/occurs.zig` | 1 |
| `src/layout/store.zig` | ~7 |
| `src/layout/type_layout_resolver.zig` | ~4 |
| `src/mir/Monomorphize.zig` | ~29 |
| `src/mir/Lower.zig` | ~13 |
| `src/mir/Monotype.zig` | ~5 |
| `src/eval/interpreter.zig` | ~41 |
| `src/eval/comptime_evaluator.zig` | ~2 |
| `src/eval/render_helpers.zig` | ~5 |
| `src/glue/glue.zig` | ~3 |
| `src/repl/eval.zig` | ~3 |
| `src/interpreter_layout/store.zig` | ~7 |
| `src/snapshot_tool/main.zig` | ~1 |
| `src/eval/test/helpers.zig` | ~1 |
| `src/backend/llvm/Builder.zig` | if present |

### Trivial but Meaningful

| File | Behavior |
|---|---|
| `src/types/TypeWriter.zig` | `explicit` mode: write `..`. `user_facing` mode: elide. |
| `src/types/generalize.zig` | `adjustRankContent`: return variable's current rank (no sub-vars). |
| `src/types/store.zig` | Content iteration: no sub-variables, skip. |
| `src/check/snapshot.zig` | Serialize as distinct tag. |
| `src/check/snapshot/diff.zig` | Classify as `TagExt.other`. |
| `src/check/copy_import.zig` | Copy as `polarity_ext` (no payload). |
| `src/check/report.zig` | Treat as anonymous extension. |
| `src/docs/extract.zig` | Map to empty type var name. |
| `src/lsp/type_utils.zig` | Treat as anonymous type var. |
| `src/lsp/syntax.zig` | Treat as anonymous type var. |
| `src/lsp/completion/builder.zig` | Treat as anonymous type var. |

### Logic Changes (covered in Parts 2-4)

| File | Change |
|---|---|
| `src/check/unify.zig` | New `unifyPolarityExt`, symmetric case, remove old special cases. |
| `src/types/instantiate.zig` | New branch, `polarity_pending` resolution, simplify rigid paths. |
| `src/check/Check.zig` | Extension creation, rename references. |

## Part 6: Test Plan

### Existing Tests

All 1877 lines in `src/check/test/polarity_test.zig` must pass unchanged. The refactor is representational — same semantics, cleaner encoding.

### New Test: Composed Open Returns

Exercises the `polarity_ext` vs `polarity_ext` unification path:

```
foo : Str -> [A, B]
bar : Str -> [A, B]
baz = \s -> if Bool.true then foo s else bar s
```

After instantiation of `foo` and `bar`, both `polarity_ext` extensions become flex vars, so this likely works today via the flex path. The test confirms the behavior is preserved and validates the new `polarity_ext` vs `polarity_ext` merge path if it's ever reached directly.

### Integration Validation

Running `zig build test` (full test suite including MIR/interpreter) with `unreachable` in the new branches serves as comprehensive validation that `polarity_ext` doesn't leak into downstream phases.

## Files Modified

| File | Nature of change |
|---|---|
| `src/types/types.zig` | Add `Content.polarity_ext`, simplify `Rigid.Name` |
| `src/check/unify.zig` | New unification function, remove old special cases |
| `src/types/instantiate.zig` | New branch, rename, simplify |
| `src/check/Check.zig` | Extension creation, rename |
| `src/types/TypeWriter.zig` | Display handling |
| `src/types/generalize.zig` | Trivial rank branch |
| `src/types/store.zig` | Content iteration |
| `src/check/snapshot.zig` | Serialization |
| `src/check/snapshot/diff.zig` | Diff classification |
| `src/check/copy_import.zig` | Cross-module copy |
| `src/check/report.zig` | Error reporting |
| `src/docs/extract.zig` | Doc extraction |
| `src/lsp/*.zig` | LSP support (3 files) |
| `src/check/exhaustive.zig` | Unreachable branch |
| `src/check/occurs.zig` | Unreachable branch |
| `src/layout/*.zig` | Unreachable branches (2 files) |
| `src/mir/*.zig` | Unreachable branches (3 files) |
| `src/eval/*.zig` | Unreachable branches (4 files) |
| `src/glue/glue.zig` | Unreachable branch |
| `src/repl/eval.zig` | Unreachable branch |
| `src/interpreter_layout/store.zig` | Unreachable branch |
| `src/snapshot_tool/main.zig` | Unreachable branch |
| `src/check/test/polarity_test.zig` | New composed-return test |

## What Does NOT Change

- **Polarity enum** (`types.zig`): unchanged
- **Polarity threading** through `GenTypeAnnoCtx`: unchanged
- **Generalization semantics**: `polarity_ext` generalizes like any other content (no special handling needed beyond `adjustRankContent`)
- **Exhaustiveness checking**: unchanged (works on tag union structure, not extension content)
- **User-visible behavior**: all existing tests produce identical output
