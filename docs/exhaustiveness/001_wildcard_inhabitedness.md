# Issue: Wildcard Inhabitedness Checking

## Status: IMPLEMENTED ✓

This issue has been resolved. Wildcards on empty types are correctly handled by the exhaustiveness checker.

## Problem Statement

When a wildcard pattern (`_` or a variable binding) matches a type, the exhaustiveness checker should verify that the type is **inhabited** (has at least one possible value). Wildcards on uninhabited types should not contribute to exhaustiveness.

## Current Implementation (Correct)

The exhaustiveness checker correctly handles wildcards on uninhabited types through two mechanisms:

### 1. Type-Tracked Wildcards

Missing patterns are always constructed with type information from `ColumnTypes`:

**File:** `src/check/exhaustive.zig` (line ~1842)
```zig
// Empty matrix but columns remain - return typed wildcards as missing pattern
const missing = try allocator.alloc(Pattern, n);
for (column_types.types, 0..) |col_type, i| {
    missing[i] = .{ .anything = col_type };
}
return missing;
```

### 2. Inhabitedness Checking

`Pattern.isInhabited()` correctly checks if a wildcard's type is inhabited:

**File:** `src/check/exhaustive.zig` (line ~175)
```zig
.anything => |maybe_type| {
    if (maybe_type) |type_var| {
        // Check if the type is inhabited using our comprehensive check
        return isTypeInhabited(type_store, builtin_idents, type_var);
    }
    // Wildcards without type info only occur in error recovery paths
    // (when reifyPattern returns TypeError and we create a placeholder).
    // In those cases, we're already skipping exhaustiveness checking
    // (has_unresolved_ctor is set), so the return value doesn't matter.
    return true;
},
```

### 3. Uninhabited Constructor Filtering

During exhaustiveness checking, uninhabited constructor paths are skipped. This happens
in `checkExhaustiveSketched` when iterating over union alternatives:

**File:** `src/check/exhaustive.zig` (line ~2782)
```zig
// Skip uninhabited constructors - they don't need to be matched
// because no values of that constructor can exist.
const arg_types = getCtorArgTypes(type_store, first_col_type, alt.tag_id);
if (!try areAllTypesInhabited(type_store, builtin_idents, arg_types)) {
    continue;
}
```

Note: Uninhabited constructors are NOT filtered at union construction time. This allows
the redundancy checker to detect patterns like `Err(_)` on `Try(I64, [])` as unreachable.
Filtering only occurs during exhaustiveness checking to avoid requiring matches for
constructors that can never have values.

## How It Works

1. **Pattern Specialization:** During algorithm execution, intermediate wildcards may lack type info, but these are never directly checked for inhabitedness.

2. **Missing Pattern Construction:** When the algorithm constructs patterns to report as "missing", it always uses type info from `ColumnTypes`.

3. **Inhabitedness Filtering:** Missing patterns are filtered through `isInhabited()` before being reported, so uninhabited patterns (like `Err(_)` on `Try(I64, [])`) are never shown to users.

## Test Coverage

The following tests verify correct behavior:

- `"exhaustive - empty error type means only Ok needed"` - `Try(I64, [])` with only `Ok` is exhaustive
- `"exhaustive - nested Try with empty inner error"` - Nested empty types work correctly
- `"exhaustive - doubly nested empty errors"` - Deep nesting works
- `"redundant - wildcard after complete coverage on type with empty variant"` - Wildcard after `Ok` on `Try(I64, [])` is redundant
- `"redundant - Err pattern first on empty error type is unreachable"` - `Err(_)` pattern is redundant
- `"redundant - pattern on tag with direct empty arg"` - `HasEmpty(_)` on `[HasEmpty([]), Normal(I64)]` is redundant
- `"non-exhaustive - not all inhabited tags covered with empty arg"` - Match missing `C` on `[A(I64), B([]), C(Str)]` is non-exhaustive

### 4. Open Union Semantics

Open unions (with flex or rigid extension variables) are correctly handled:

**File:** `src/check/exhaustive.zig` (line ~1277)
```zig
/// An open union is one where additional constructors may exist beyond those
/// explicitly listed. This occurs when the extension is:
/// - A flex var: The type is not yet fully constrained, more tags could be added
/// - A rigid var: The user explicitly said "and potentially more tags"
fn isOpenExtension(type_store: *TypeStore, ext: Var) bool {
    return switch (content) {
        .flex, .rigid => true,  // Both are open
        .structure => |flat_type| switch (flat_type) {
            .empty_tag_union => false,  // Closed
            .tag_union => true,         // Nested tags = open
            else => false,
        },
        // ...
    };
}
```

## Design Decision: Where to Filter Uninhabited Constructors

**Important:** Uninhabited constructors must NOT be filtered at union construction time
(in `buildUnionFromTagUnion`). They must be filtered during exhaustiveness checking
(in `checkExhaustiveSketched`).

### Why This Matters

If we filter uninhabited constructors when building the `Union` structure:

1. **Exhaustiveness works correctly** - we don't require patterns for constructors
   that can't have values.

2. **BUT redundancy checking breaks** - when a user writes an explicit pattern like
   `Err(_)` for an uninhabited type, we can't find the `Err` constructor in the
   filtered union, so we can't detect the pattern as unreachable.

### Example

```roc
x : Try(I64, [])  // Error type is empty, so Err is uninhabited

match x {
    Err(_) => 0   // This should be flagged as REDUNDANT/UNREACHABLE
    Ok(n) => n
}
```

- If we filter `Err` at union construction: `findTagId("Err")` returns `null`,
  causing a type error instead of a redundancy warning.
- If we keep `Err` in the union but skip it during exhaustiveness checking:
  the pattern is correctly identified as unreachable.

### The `isTypeInhabited` Implementation

The inhabitedness check uses a mostly stack-based approach for efficiency:

- **Records/Tuples:** All fields are pushed onto a work stack (AND semantics - if
  any field is uninhabited, the whole thing is uninhabited).
- **Tag Unions:** Checked with a local stack per tag (OR semantics - if any tag
  is fully inhabited, the union is inhabited). For deeply nested tag unions,
  recursive calls are used.
- **Aliases/Nominals:** Backing types are followed via the stack.

The implementation correctly handles builtin numeric types (I64, U8, etc.) which
have `[]` as their backing type but are actually inhabited primitives.

### 5. Extension Chain Following

Tag unions from unification may have their tags split across extension chains. For example,
when unifying `[HasEmpty([]), Normal(I64)]` with `[Normal(I64)]`, the result might be:

```
[Normal(I64), ..ext] where ext = [HasEmpty([]), ..]
```

The exhaustiveness checker follows these extension chains in two key functions:

**File:** `src/check/exhaustive.zig` - `buildUnionFromTagUnion`
```zig
// Follow extension chain to collect all tags
while (iteration_guard < max_iterations) : (iteration_guard += 1) {
    // Add tags from current level
    const tags_slice = type_store.getTagsSlice(current_tags);
    // ... add to all_tags ...

    // Check what the extension is
    switch (ext_content) {
        .structure => |flat_type| switch (flat_type) {
            .tag_union => |ext_tu| {
                // Extension is another tag union - continue following
                current_tags = ext_tu.tags;
                current_ext = ext_tu.ext;
            },
            // ...
        },
        // ...
    }
}
```

**File:** `src/check/exhaustive.zig` - `getCtorArgTypes`

Similarly follows extension chains to find the argument types for a tag at any position
in the gathered tags list.

## Acceptance Criteria (All Met)

- [x] All wildcards have their type information tracked (via `ColumnTypes`)
- [x] `isInhabited()` correctly returns `false` for wildcards on empty types
- [x] No hardcoded `return true` for wildcards without type info in normal paths
- [x] Comprehensive tests for empty type wildcards
- [x] Open union semantics correctly handle both flex and rigid extensions
- [x] Extension chains are followed to gather all tags from unified types
- [x] No TODOs or workarounds in the implementation
