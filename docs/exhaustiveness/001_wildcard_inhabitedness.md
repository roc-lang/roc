# Issue: Wildcard Inhabitedness Checking

## Status: IMPLEMENTED ✓

This issue has been resolved. Wildcards on empty types are correctly handled by the exhaustiveness checker.

## Problem Statement

When a wildcard pattern (`_` or a variable binding) matches a type, the exhaustiveness checker should verify that the type is **inhabited** (has at least one possible value). Wildcards on uninhabited types should not contribute to exhaustiveness.

## Current Implementation (Correct)

The exhaustiveness checker correctly handles wildcards on uninhabited types through two mechanisms:

### 1. Type-Tracked Wildcards

Missing patterns are always constructed with type information from `ColumnTypes`:

**File:** `src/check/exhaustive.zig` (line ~1757)
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

### 3. Uninhabited Constructor Filtering Optimization

As an optimization, uninhabited constructor paths are skipped entirely during exhaustiveness checking:

**File:** `src/check/exhaustive.zig` (line ~2691)
```zig
// Optimization: Skip uninhabited constructors entirely.
// A constructor is uninhabited if any of its argument types are uninhabited.
const arg_types = getCtorArgTypes(type_store, first_col_type, alt.tag_id);
if (!try areAllTypesInhabited(type_store, builtin_idents, arg_types)) {
    continue; // Skip this uninhabited constructor
}
```

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

## Acceptance Criteria (All Met)

- [x] All wildcards have their type information tracked (via `ColumnTypes`)
- [x] `isInhabited()` correctly returns `false` for wildcards on empty types
- [x] No hardcoded `return true` for wildcards without type info in normal paths
- [x] Comprehensive tests for empty type wildcards
- [x] No TODOs, hacks, or workarounds in the solution
