# Polarity Implementation Design

## Goal

Implement polarity tracking in the Zig type checker so that tag union extensions in annotations are implicitly open in output position and closed in input position. This eliminates the annotation ambiguity problem where identical annotations produce different caller-visible types depending on the function body.

## Key Simplification from Original Spec

The original POLARITY_SPEC.md proposed an `Openness` restriction mechanism with synthetic rigid injection during generalization. After confirming with the language creator that returning a closed tag union to an open-annotated return type can be a type error, the design simplifies to: **use a standard anonymous rigid variable in output position**. No new unification rules, no post-generalization fixup.

## Part 1: Polarity Enum

**File**: `src/types/types.zig`

```zig
pub const Polarity = enum {
    /// output position - implicit ext becomes anonymous rigid (open)
    pos,
    /// input position - implicit ext becomes EmptyTagUnion (closed)
    neg,
    /// deferred - creates polarity_deferred rigid, resolved at use site
    in_alias,
    /// always closed - EmptyTagUnion
    in_opaque,

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

Propagation uses standard contravariance:
- Function arguments: flip parent polarity
- Function return: preserve parent polarity
- Record fields, tag variant payloads, generic type args: preserve parent polarity

## Part 2: Rigid Name Enum

**File**: `src/types/types.zig`

Replace `Rigid.name: Ident.Idx` with a tagged union:

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

All existing code that reads `rigid.name` as `Ident.Idx` must be updated to switch on the `Name` enum.

## Part 3: Threading Polarity Through Annotation Generation

**File**: `src/check/Check.zig`

### GenTypeAnnoCtx

Add polarity to the annotation context:

```zig
const GenTypeAnnoCtx = union(enum) {
    annotation: Polarity,
    type_decl: struct {
        idx: CIR.Statement.Idx,
        name: Ident.Idx,
        type_: enum { nominal, alias },
        backing_var: Var,
        num_args: u32,
    },
};
```

### Entry Points

| Call site | New ctx |
|---|---|
| `generateAnnotationType` (value annotations) | `.{ .annotation = .pos }` |
| `processRequiresTypes` | `.{ .annotation = .pos }` |
| `checkAliasDecl` | `.{ .type_decl = ... .alias }` (unchanged) |
| `checkNominalDecl` | `.{ .type_decl = ... .nominal }` (unchanged) |

### Function Type Propagation

In the `.fn` case of `generateAnnoTypeInPlace`:

```zig
.@"fn" => |func| {
    // Args: flip polarity (standard contravariance)
    const arg_ctx: GenTypeAnnoCtx = switch (ctx) {
        .annotation => |pol| .{ .annotation = pol.flip() },
        .type_decl => ctx, // sticky in_alias/in_opaque
    };
    for (args_anno_slice) |arg_anno_idx| {
        try self.generateAnnoTypeInPlace(arg_anno_idx, env, arg_ctx);
    }

    // Return: preserve parent polarity
    try self.generateAnnoTypeInPlace(func.ret, env, ctx);
    ...
}
```

All other type constructors (records, tag variant payloads, generic type args) pass `ctx` through unchanged.

## Part 4: Tag Union Extension Creation

**File**: `src/check/Check.zig`

In `generateAnnoTypeInPlace`, the `.tag_union` case. When no explicit extension is written (`tag_union.ext == null`):

| Polarity | Extension created |
|---|---|
| `pos` | `Rigid.initPolarityOpen()` - open, body can't close or widen |
| `neg` | `EmptyTagUnion` - closed (same as current behavior) |
| `in_alias` | `Rigid.initPolarityDeferred()` - resolved at alias use site |
| `in_opaque` | `EmptyTagUnion` - always closed |

When explicit extension IS present (`tag_union.ext != null`): process as before (user controls extension), but emit a warning if polarity is `pos` (see Part 7).

## Part 5: Alias Instantiation with Polarity

**File**: `src/types/instantiate.zig` and `src/check/Check.zig`

### Instantiator Changes

Add optional polarity field to `Instantiator`:

```zig
pub const Instantiator = struct {
    // ... existing fields ...
    polarity: ?Polarity = null, // only set for alias instantiation
};
```

In the `.rigid` branch of `instantiateVar`, handle `polarity_deferred` before normal rigid logic:

```zig
.rigid => |rigid| {
    if (rigid.name == .polarity_deferred) {
        if (self.polarity) |pol| {
            const fresh_content: Content = switch (pol) {
                .pos => .{ .rigid = Rigid.initPolarityOpen() },
                .neg, .in_opaque => .{ .structure = .empty_tag_union },
                .in_alias => .{ .rigid = Rigid.initPolarityDeferred() },
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
    // ... existing rigid handling ...
}
```

Update `substitute_rigids` to handle the new `Name` variants:

```zig
.substitute_rigids => |rigid_subs| {
    switch (rigid.name) {
        .name => |ident_idx| {
            // Named rigid - look up in substitution map (existing logic)
            if (rigid_subs.get(ident_idx)) |existing_var| { ... }
            else { std.debug.assert(false); /* create .err */ }
        },
        .polarity_open, .polarity_deferred => {
            // Anonymous polarity rigids should never appear in substitute_rigids path.
            std.debug.assert(false);
            const fresh_var = try self.store.freshFromContentWithRank(.err, self.current_rank);
            try self.var_map.put(resolved_var, fresh_var);
            return fresh_var;
        },
    }
}
```

Update rigid-to-flex conversion (the `fresh_flex` path) to convert `Rigid.Name` to `?Ident.Idx` for `Flex.name`:

```zig
const flex_name: ?Ident.Idx = switch (rigid.name) {
    .name => |ident_idx| ident_idx,
    .polarity_open => null,
    .polarity_deferred => unreachable, // handled by polarity branch above
};
const fresh_content = Content{ .flex = Flex{ .name = flex_name, .constraints = fresh_constraints } };
```

### Check.zig Helper

Add a new instantiation helper that passes polarity:

```zig
fn instantiateVarWithPolarity(
    self: *Self,
    var_to_instantiate: Var,
    env: *Env,
    polarity: Polarity,
    region_behavior: InstantiateRegionBehavior,
) std.mem.Allocator.Error!Var {
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

At alias use sites in `generateAnnoTypeInPlace` (`.lookup` / `.local`), call `instantiateVarWithPolarity` instead of `instantiateVar`. The polarity is determined from the current `ctx`:

```zig
const polarity: Polarity = switch (ctx) {
    .annotation => |pol| pol,
    .type_decl => |decl| switch (decl.type_) {
        .alias => .in_alias,
        .nominal => .in_opaque,
    },
};
const instantiated_var = try self.instantiateVarWithPolarity(local_decl_var, env, polarity, .{ .explicit = anno_region });
```

This applies to ALL type lookups (aliases, nominals, plain types). For non-alias types that contain no `polarity_deferred` rigids, the polarity field has no effect — the Instantiator only checks polarity when it encounters a `polarity_deferred` rigid.

## Part 6: Error Reporting and Display

**File**: `src/types/TypeWriter.zig`

### Display Modes

```zig
pub const DisplayMode = enum {
    /// User-facing: elide polarity_open extensions in Pos position
    user_facing,
    /// Test/debug: always show openness explicitly
    /// Open types show as [A, B, ..], closed types show as [A, B]
    explicit,
};
```

Behavior when writing a tag union extension:
- `user_facing`: elide `polarity_open` rigid exts (show `[A, B]`), show flex exts as `..`
- `explicit`: show `..` for any non-empty extension (rigid or flex), omit only for `EmptyTagUnion`

`polarity_deferred` in display: debug assert (should never reach display). In release, render as `<unresolved>`.

### Test Infrastructure

- Existing `checkTypesModule` / `checkTypesModuleDefs`: use `explicit` mode (minimizes churn on existing tests)
- New `checkTypesModuleUserFacing`: uses `user_facing` mode for testing TypeWriter display

## Part 7: Warning for Redundant Explicit Extension

**File**: `src/check/Check.zig`

In the `.tag_union` case of `generateAnnoTypeInPlace`, when `tag_union.ext` is present (user wrote `..` explicitly), check polarity:

```zig
if (tag_union.ext) |ext_anno_idx| {
    const polarity: Polarity = switch (ctx) {
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
    // ... process ext as before ...
}
```

## Part 8: Test Plan

### Passing Tests (explicit display mode)

1. **Basic Pos - open return**: `foo : Str -> [Ok U64, Err Str]` body produces Ok/Err. Expected: `Str -> [Err Str, Ok U64, ..]`
2. **Basic Neg - closed arg**: `foo : [A, B] -> Str` with exhaustive match. Expected: `[A, B] -> Str`
3. **Higher-order flip - callback return closed, arg open**: `foo : ([A, B] -> [X, Y]) -> Str` body matches callback result on X, Y. Expected: `([A, B, ..] -> [X, Y]) -> Str`
4. **Alias in Pos - deferred resolves to open**: `MyResult a : [Ok a, Err Str]` used as `foo : Str -> MyResult U64`. Expected return is open.
5. **Alias in Neg - deferred resolves to closed**: `bar : MyResult U64 -> Bool`. Expected arg is closed.
6. **Opaque - always closed**: Extension is EmptyTagUnion regardless of position.

### Failing Tests (explicit display mode)

7. **Body returns closed value to open return**: `closedBody : [Ok U64, Err Str] -> [Ok U64, Err Str]` where body is `\input -> input` - TYPE MISMATCH (EmptyTagUnion can't unify with polarity_open rigid).
8. **Body adds undeclared tag**: `sneaky : Str -> [Ok U64, Err Str]` where body returns `Bad s` - TYPE MISMATCH (rigid ext can't absorb Bad).
9. **Exhaustive match on open return ext**: matching on a polarity_open rigid ext - TYPE MISMATCH (rigid can't close).

### User-Facing Display Tests (user_facing mode)

10. **Pos return - polarity_open elided**: `Str -> [Err Str, Ok U64]` (no `..`)
11. **Neg arg with flex ext - shown**: `[A, B, ..] -> Str`
12. **Both positions in same type**: consistent, non-confusing output.

### Diagnostic Tests

13. **Redundant `..` in Pos position** - warning: `foo : Str -> [Ok U64, Err Str, ..]`
14. **Redundant `..` in Pos from flip** - warning: `foo : ([A, B, ..] -> Str) -> Str` (callback arg is Pos after flip)
15. **Non-redundant `..` in Neg position** - no warning: `foo : [A, B, ..] -> Str`
16. **Non-redundant `..` in Neg from preserve** - no warning: `foo : (Str -> [X, Y, ..]) -> Str` (callback return is Neg)

## Existing Tests Affected

Two existing tests change behavior:

1. **"check type - tag union - ext hints 1" (line ~6127)**: `bar : [A, B] -> [X, Y]` where `foo` calls `bar` and expects `[X, Y, ..]`. Currently fails because `bar`'s return is closed. With polarity, `bar`'s return becomes open (`polarity_open` rigid) → behavior changes. This test needs to be updated to reflect the new semantics.

2. **"check type - tag union - ext hints 2" (line ~6158)**: `foo : [A, B, ..] -> [A, B]` where body is `|a| a`. Currently fails with "open vs closed" hint. With polarity, both sides are open (different rigids) → still fails, but error message changes from open/closed mismatch to rigid mismatch.

All other existing tests (inferred-type polarity tests, explicit `..` tests, alias tests) are unaffected.

## Files Modified

| File | Changes |
|---|---|
| `src/types/types.zig` | Add `Polarity` enum, change `Rigid.name` to `Rigid.Name` union |
| `src/check/Check.zig` | Thread polarity through `GenTypeAnnoCtx`, polarity-based ext creation in `.tag_union`, `instantiateVarWithPolarity` helper, redundant `..` warning |
| `src/types/instantiate.zig` | Add `polarity` field to `Instantiator`, handle `polarity_deferred` in rigid branch, update `substitute_rigids` and rigid-to-flex for new `Name` |
| `src/types/TypeWriter.zig` | Add `DisplayMode`, elide `polarity_open` in user_facing mode, debug assert on `polarity_deferred` |
| `src/check/test/type_checking_integration.zig` | New tests (16 cases), `checkTypesModuleUserFacing` helper |

## What Does NOT Change

- **Unification** (`src/check/unify.zig`): no changes. Standard rigid var semantics handle everything. `polarity_open` and `polarity_deferred` are purely metadata on `Rigid.Name` and do not affect unification behavior.
- **Generalization** (`src/types/generalize.zig`): no changes. Rigid vars generalize normally.
- **Exhaustiveness checking** (`src/check/exhaustive.zig`): no changes. Exhaustive matches close flex extensions via unification with EmptyTagUnion; rigid extensions fail to close (type error), which is the correct behavior.
