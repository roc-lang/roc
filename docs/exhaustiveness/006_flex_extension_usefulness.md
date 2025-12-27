# Issue: Flex Extension Handling in Usefulness Check

## Status: NEEDS REVIEW

The reified path (which contained `isWildcardUseful` and `PatternMatrix`) has been removed as dead code. The sketched path (`isUsefulSketched`) uses a different approach. This issue should be reviewed to verify whether the sketched path handles flex extensions correctly.

---

## Original Problem Statement (Historical)

When checking if a wildcard pattern is "useful" (can match values not covered by other patterns), the code assumes wildcards are always useful when the union has a flex extension. This is an overly conservative approximation that may miss redundancy detection opportunities.

## Current Behavior (Incorrect)

```zig
.ctors => |ctor_info| {
    // TODO: Properly handle flex extensions instead of assuming wildcards useful.
    if (ctor_info.union_info.has_flex_extension) {
        return true;
    }
    // ...
}
```

This means for ANY union with a flex extension, wildcards are assumed useful even when:
1. The flex extension was resolved to empty
2. All possible variants are already covered
3. The wildcard is genuinely redundant

## Expected Behavior (Correct)

The usefulness check should:
1. Check if the flex extension is actually open (unresolved)
2. If resolved to empty/closed, treat as closed union
3. Only assume wildcards useful if the extension genuinely allows more variants
4. When extension is open, still check if known variants are fully covered

## Location in Codebase

**File:** `src/check/exhaustive.zig`

**Line ~1930-1945:**
```zig
fn isWildcardUseful(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    ident_store: *const Ident.Store,
    matrix: PatternMatrix,
    column_types: ColumnTypes,
) !bool {
    // ... collect constructors from first column ...

    switch (collected) {
        // ...
        .ctors => |ctor_info| {
            // TODO: Properly handle flex extensions instead of assuming wildcards useful.
            if (ctor_info.union_info.has_flex_extension) {
                return true;
            }

            // Check if all alternatives are covered
            const covered = isCovered(ctor_info.found, ctor_info.union_info.alternatives);
            if (!covered) {
                return true; // Not all covered, wildcard is useful
            }
            // ...
        },
    }
}
```

## Root Cause

The `has_flex_extension` flag is set during union construction but:
1. It's never updated when the extension gets resolved
2. It's used as a simple boolean without checking the actual extension state
3. It causes overly conservative behavior that misses redundant patterns

## What "Useful" Means

A pattern is **useful** if there exists at least one value that:
1. Matches this pattern
2. Does NOT match any previous pattern

For a wildcard after explicit patterns:
- If all constructors are covered → wildcard is NOT useful (redundant)
- If some constructors are uncovered → wildcard IS useful
- If union is open (more constructors possible) → wildcard IS useful

## Solution Requirements

1. **Check extension resolution:** Instead of just checking `has_flex_extension`, examine the actual extension variable
2. **Use `isOpenExtension()`:** This function (once fixed per issue 004) determines if extension is truly open
3. **Combine checks:** Wildcard is useful if extension is open OR not all constructors covered
4. **Update flag semantics:** Consider whether `has_flex_extension` should be dynamic or removed

## Implementation Approach

```zig
.ctors => |ctor_info| {
    // Check if all known alternatives are covered
    const all_known_covered = isCovered(ctor_info.found, ctor_info.union_info.alternatives);

    // Check if the union is truly open (has unresolved extension)
    const is_open = if (column_types.types.len > 0)
        isOpenExtension(column_types.type_store, getUnionExtension(column_types.types[0]))
    else
        ctor_info.union_info.has_flex_extension;

    // Wildcard is useful if:
    // 1. Not all known constructors are covered, OR
    // 2. The union is open (more constructors possible)
    if (!all_known_covered or is_open) {
        return true;
    }

    // All known constructors covered and union is closed
    // Check recursively if any sub-patterns need wildcards
    // ...
}
```

## Related Issues

This issue is closely related to:
- **Issue 004 (Flex/Rigid Extensions):** The `isOpenExtension()` function needs correct semantics first
- **Issue 002 (Polymorphic Types):** Polymorphic types may have unknown extensions

## Functions to Modify

1. `isWildcardUseful()` (line ~1900): Implement proper flex extension check
2. May need helper: `getUnionExtension()` to extract extension from type
3. May need to update `collectConstructorsFromColumn()` to return extension info

## Testing

Create test cases for:
1. Closed union, all covered, wildcard → wildcard is redundant
2. Open union (flex), all known covered, wildcard → wildcard is useful
3. Resolved flex (now closed), all covered, wildcard → wildcard is redundant
4. Open union, not all covered, no wildcard → non-exhaustive
5. Nested patterns with mixed extension states

## Acceptance Criteria

- [ ] Flex extensions that resolved to empty are treated as closed
- [ ] Truly open unions correctly require wildcards
- [ ] Redundant wildcards are detected when union is actually closed
- [ ] No blanket `return true` for `has_flex_extension`
- [ ] Integration with fixed `isOpenExtension()` from issue 004
- [ ] No TODOs or assumptions in the solution
