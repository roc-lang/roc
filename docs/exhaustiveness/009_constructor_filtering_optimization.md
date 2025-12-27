# Uninhabited Constructor Filtering Optimization

## Status: IMPLEMENTED

This optimization has been implemented. Uninhabited constructor paths are now skipped during exhaustiveness checking.

## Overview

This document describes the optimization for skipping uninhabited constructor paths during exhaustiveness checking. **This is an OPTIMIZATION, not a bug fix** - the implementation already handles uninhabited types correctly via output filtering.

## Implementation

The optimization was implemented in `checkExhaustiveSketched` instead of `buildUnionFromTagUnion`. This approach was chosen because filtering in `buildUnionFromTagUnion` breaks pattern resolution for patterns that reference uninhabited constructors (needed for redundancy checking).

**File:** `src/check/exhaustive.zig`

```zig
// Optimization: Skip uninhabited constructors entirely.
// A constructor is uninhabited if any of its argument types are uninhabited.
const arg_types = getCtorArgTypes(type_store, first_col_type, alt.tag_id);
if (!try areAllTypesInhabited(type_store, builtin_idents, arg_types)) {
    continue; // Skip this uninhabited constructor
}
```

This optimization is applied in two places:
1. When checking missing constructors
2. When recursively checking covered constructors

## How It Works

The exhaustiveness checker handles uninhabited types through two mechanisms:

### Output Filtering

When checking for missing patterns, the code in `checkExhaustive` filters out uninhabited patterns:

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

### Example

For `Try(I64, [])` matching only `Ok(n)`:
1. The checker sees `Err` is "missing"
2. It creates pattern `Err(_)` where `_` has type `[]`
3. `isInhabited` on `Err(_)` returns `false` because `[]` is uninhabited
4. The pattern is not reported as missing
5. Result: match is exhaustive

## Why Filter During Checking, Not Construction

If we filtered uninhabited constructors when building the `Union` structure:

1. **Exhaustiveness would work correctly** - we don't require patterns for constructors that can't have values.

2. **BUT redundancy checking would break** - when a user writes an explicit pattern like `Err(_)` for an uninhabited type, we can't find the `Err` constructor in the filtered union, so we can't detect the pattern as unreachable.

### Example

```roc
x : Try(I64, [])  // Error type is empty, so Err is uninhabited

match x {
    Err(_) => 0   // This should be flagged as REDUNDANT/UNREACHABLE
    Ok(n) => n
}
```

- If we filter `Err` at union construction: `findTagId("Err")` returns `null`, causing a type error instead of a redundancy warning.
- If we keep `Err` in the union but skip it during exhaustiveness checking: the pattern is correctly identified as unreachable.

## Performance Impact

For a type like `Try(A, [])`:
- Without optimization: Explores both `Ok` and `Err` paths, then filters `Err` at output
- With optimization: Only explores `Ok` path

For most code this is negligible, but for deeply nested types with many uninhabited branches, it matters.

## Comparison to Rust Implementation

The Rust implementation in `crates/compiler/can/src/exhaustive.rs` filters uninhabited constructors at union construction time in `convert_tag`. The Zig implementation intentionally differs by filtering during checking to preserve redundancy detection for patterns on uninhabited constructors.
