# Uninhabited Constructor Filtering Optimization

## Status: IMPLEMENTED ✓

This optimization has been implemented. Uninhabited constructor paths are now skipped during exhaustiveness checking.

## Overview

This document describes the optimization for skipping uninhabited constructor paths during exhaustiveness checking. **This is an OPTIMIZATION, not a bug fix** - the implementation already handles uninhabited types correctly via output filtering.

## Implementation

The optimization was implemented in `checkExhaustiveSketched` instead of `buildUnionFromTagUnion` (as originally proposed in this doc). This approach was chosen because filtering in `buildUnionFromTagUnion` breaks pattern resolution for patterns that reference uninhabited constructors.

**File:** `src/check/exhaustive.zig` (line ~2691)
```zig
// Optimization: Skip uninhabited constructors entirely.
// A constructor is uninhabited if any of its argument types are uninhabited.
const arg_types = getCtorArgTypes(type_store, first_col_type, alt.tag_id);
if (!try areAllTypesInhabited(type_store, builtin_idents, arg_types)) {
    continue; // Skip this uninhabited constructor
}
```

This optimization is applied in two places:
1. When checking missing constructors (line ~2691)
2. When recursively checking covered constructors (line ~2732)

## How It Works

The exhaustiveness checker handles uninhabited types through two mechanisms:

### Current Mechanism (Output Filtering)

When checking for missing patterns, the code in `checkExhaustive` (around line 1803 and 1850 in `exhaustive.zig`) filters out uninhabited patterns:

```zig
// Only report as missing if the pattern is inhabited
// (e.g., Err on a Try with empty error type is uninhabited)
if (try missing_pattern.isInhabited(column_types.type_store, column_types.builtin_idents)) {
    const result = try allocator.alloc(Pattern, 1);
    result[0] = missing_pattern;
    return result;
}
// Pattern is uninhabited, continue checking others
```

This means:
1. All constructors (including uninhabited ones) are created in `buildUnionFromTagUnion`
2. The algorithm explores all paths, including uninhabited ones
3. When a "missing pattern" is found, it's checked for inhabitedness before reporting
4. If the pattern is uninhabited (e.g., `Err` with empty error type), it's NOT reported

### Why This Works

For `Try(I64, [])` matching only `Ok(n)`:
1. The checker sees `Err` is "missing"
2. It creates pattern `Err(_)` where `_` has type `[]`
3. `isInhabited` on `Err(_)` returns `false` because `[]` is uninhabited
4. The pattern is not reported as missing
5. Result: match is exhaustive

**All existing tests pass with this implementation.**

## The Optimization Opportunity

Adding filtering in `buildUnionFromTagUnion` would:

1. **Reduce work**: Skip exploring uninhabited constructor paths entirely
2. **Improve error messages**: When partial inhabitedness exists, error messages would only show reachable patterns
3. **Match Rust implementation**: Rust's `convert_tag` does this filtering (see `crates/compiler/exhaustive/src/lib.rs`)

### Performance Impact

For a type like `Try(A, [])`:
- Current: Explores both `Ok` and `Err` paths, then filters `Err` at output
- Optimized: Only explores `Ok` path

For most code this is negligible, but for deeply nested types with many uninhabited branches, it could matter.

## Why the Previous Attempt Failed

The previous implementer made a critical error: **they changed working code while debugging**.

### The Mistake

1. Original code used fully-qualified identifiers: `i64_type` = "Builtin.Num.I64"
2. Implementer changed to unqualified: `i64` = "I64"
3. This broke the comparison because `NominalType.ident.ident_idx` stores **fully-qualified** names

### Key Insight

Looking at `src/check/Check.zig` line 695-699:
```zig
// Use fully-qualified type name "Builtin.Num.U8" etc.
// This allows method lookup to work correctly
const qualified_type_name = try std.fmt.allocPrint(self.gpa, "Builtin.Num.{s}", .{type_name});
const type_name_ident = try @constCast(self.cir).insertIdent(base.Ident.for_text(qualified_type_name));
```

Number types have their `ident.ident_idx` set to the **fully-qualified** name like "Builtin.Num.I64".

The `origin_module` is set to just "Builtin" (from `self.cir.idents.builtin_module`).

So for I64:
- `nominal.origin_module` = "Builtin"
- `nominal.ident.ident_idx` = "Builtin.Num.I64"

The original `BuiltinIdents` correctly uses:
- `builtin_module` = "Builtin"
- `i64_type` = "Builtin.Num.I64"

**DO NOT CHANGE THESE TO UNQUALIFIED NAMES.**

## Correct Implementation Steps

### Step 1: Add `builtin_idents` Parameter to `buildUnionFromTagUnion`

Update the function signature:

```zig
fn buildUnionFromTagUnion(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,  // ADD THIS
    tag_union: types.TagUnion,
) error{OutOfMemory}!UnionResult {
```

### Step 2: Update All Callers

Find all calls to `buildUnionFromTagUnion` and add the `builtin_idents` parameter:

```bash
grep -n "buildUnionFromTagUnion" src/check/exhaustive.zig
```

Update `getUnionFromType` to pass `builtin_idents` through.

### Step 3: Add Filtering Logic

In `buildUnionFromTagUnion`, add the filtering:

```zig
fn buildUnionFromTagUnion(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    tag_union: types.TagUnion,
) error{OutOfMemory}!UnionResult {
    const tags_slice = type_store.getTagsSlice(tag_union.tags);
    const tag_names = tags_slice.items(.name);
    const tag_args = tags_slice.items(.args);

    const is_open = isOpenExtension(type_store, tag_union.ext);

    // First pass: count inhabited constructors
    var num_inhabited: usize = 0;
    for (tag_args) |args_range| {
        const arg_vars = type_store.sliceVars(args_range);
        if (try areAllTypesInhabited(type_store, builtin_idents, arg_vars)) {
            num_inhabited += 1;
        }
    }

    // Allocate only for inhabited constructors
    const num_alts = num_inhabited + @as(usize, if (is_open) 1 else 0);
    const alternatives = try allocator.alloc(CtorInfo, num_alts);

    // Second pass: populate inhabited constructors, preserving original tag_id
    var alt_idx: usize = 0;
    for (tag_names, tag_args, 0..) |name, args_range, orig_idx| {
        const arg_vars = type_store.sliceVars(args_range);
        if (try areAllTypesInhabited(type_store, builtin_idents, arg_vars)) {
            alternatives[alt_idx] = .{
                .name = .{ .tag = name },
                .tag_id = @enumFromInt(orig_idx),  // KEEP ORIGINAL INDEX
                .arity = arg_vars.len,
            };
            alt_idx += 1;
        }
    }

    if (is_open) {
        alternatives[alt_idx] = .{
            .name = .{ .tag = Ident.Idx.NONE },
            .tag_id = @enumFromInt(tag_names.len),
            .arity = 0,
        };
    }

    const has_flex = hasFlexExtension(type_store, tag_union.ext);
    return .{ .success = .{
        .alternatives = alternatives,
        .render_as = .tag,
        .has_flex_extension = has_flex,
    } };
}
```

**CRITICAL**: The `tag_id` MUST use `orig_idx` (the original index), not `alt_idx`. This is because pattern matching uses the tag_id to look up constructors, and other parts of the codebase expect the original indices.

### Step 4: Add Helper Function

Add `areAllTypesInhabited` if it doesn't exist:

```zig
fn areAllTypesInhabited(
    type_store: *TypeStore,
    builtin_idents: BuiltinIdents,
    type_vars: []const Var,
) error{OutOfMemory}!bool {
    for (type_vars) |type_var| {
        if (!try isTypeInhabited(type_store, builtin_idents, type_var)) {
            return false;
        }
    }
    return true;
}
```

### Step 5: Add `Str` to BuiltinIdents (If Not Present)

Check if `Str` needs special handling. Looking at Builtin.roc:
```roc
Str :: [ProvidedByCompiler].{}
```

`Str` has backing `[ProvidedByCompiler]` which is inhabited (a tag union with one variant). So it should NOT need special-casing - the `isTypeInhabited` check will correctly see it as inhabited.

However, if you find Str being incorrectly marked uninhabited:
1. Add `str_type: Ident.Idx` to `BuiltinIdents`
2. Initialize it in `Check.zig` with `self.cir.idents.builtin_str`
3. Add to the check in `isBuiltinNumericType` (or rename to `isBuiltinPrimitiveType`)

## Testing Strategy

### Existing Tests (Must All Pass)

Run the full test suite:
```bash
zig build test-check -- --test-filter "exhaustive"
```

Pay special attention to:
- "exhaustive - empty error type means only Ok needed"
- "exhaustive - nested Try with empty inner error"
- "exhaustive - doubly nested empty errors"
- "non-exhaustive - non-empty error type requires Err case"
- "redundant - Err pattern first on empty error type is unreachable"

### New Tests to Add

Add tests in `exhaustiveness_test.zig` for edge cases:

```zig
test "exhaustive - List of empty type is still inhabited" {
    // List([]) is inhabited (the empty list exists)
    const source =
        \\x : List([])
        \\x = []
        \\
        \\result = match x {
        \\    [] => 0i64
        \\    [_, ..] => 1i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefType("I64");
}

test "exhaustive - record with empty field is uninhabited" {
    // A record { field: [] } cannot be constructed
    const source =
        \\MyType := { value: [] }
        \\
        \\f = |x: MyType| match x {
        \\    _ => 0i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    // This should report the wildcard as redundant since no values exist
    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

test "exhaustive - tuple with empty element is uninhabited" {
    const source =
        \\x : (I64, [])
        \\x = (42i64, ???)  // Can't actually construct this
        ;
    // This should be a type error since the tuple can't be constructed
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError(""); // Some type error
}
```

## Debugging Tips

### Verify Identifier Values

If comparisons fail, add debug output:

```zig
pub fn isBuiltinNumericType(self: BuiltinIdents, nominal: types.NominalType) bool {
    std.debug.print("Checking nominal: origin={d}, ident={d}\n", .{
        @intFromEnum(nominal.origin_module),
        @intFromEnum(nominal.ident.ident_idx),
    });
    std.debug.print("Expected: builtin_module={d}, i64_type={d}\n", .{
        @intFromEnum(self.builtin_module),
        @intFromEnum(self.i64_type),
    });
    // ... rest of function
}
```

### Trace Inhabitedness Checks

Add debug output to `isTypeInhabitedImpl`:

```zig
fn isTypeInhabitedImpl(...) error{OutOfMemory}!bool {
    std.debug.print("Checking inhabitedness of var {d}\n", .{@intFromEnum(type_var)});
    // ... rest of function
}
```

### Check NominalType Creation

Verify what values are being stored in NominalType at creation time in `Check.zig`:

```zig
// In mkNumberTypeContent:
std.debug.print("Creating numeric type: origin={d}, ident={d} ('{s}')\n", .{
    @intFromEnum(origin_module_id),
    @intFromEnum(type_name_ident),
    qualified_type_name,
});
```

## Files to Modify

1. `src/check/exhaustive.zig`:
   - `buildUnionFromTagUnion` - add filtering
   - `getUnionFromType` - pass through `builtin_idents`
   - Add `areAllTypesInhabited` helper if needed

2. `src/check/Check.zig`:
   - Update `builtin_idents` construction if new types needed

3. `src/check/test/exhaustiveness_test.zig`:
   - Add new edge case tests

## Verification Checklist (All Passing)

- [x] All existing exhaustiveness tests pass
- [x] `Try(I64, [])` with only `Ok` pattern is exhaustive
- [x] `Try(I64, Str)` with only `Ok` pattern is NOT exhaustive
- [x] Nested empty types work: `Try(Try(I64, []), Str)`
- [x] Lists work: `List(I64)` patterns work correctly
- [x] Records work: patterns on record types work
- [x] No regressions in other tests: `zig build test-check` passes
- [x] Code compiles without warnings

## Summary

The optimization skips uninhabited constructor paths at the exploration stage rather than filtering at output. This is more efficient and was implemented in `checkExhaustiveSketched` rather than `buildUnionFromTagUnion` to preserve pattern resolution for patterns that reference uninhabited constructors (needed for redundancy checking).

Key changes:
1. Added `areAllTypesInhabited` helper function
2. Fixed `findTagId` to return `alt.tag_id` instead of array position
3. Added optimization in `checkExhaustiveSketched` to skip uninhabited constructors
