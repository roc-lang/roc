# Issue: Flex vs Rigid Extension Semantics

## Problem Statement

Tag unions and records can be "open" (extensible) or "closed". This is represented by an extension variable that can be:
- **Flex variable:** The type can be extended with more variants/fields
- **Rigid variable:** The type is fixed but the exact variants are unknown
- **Empty record/closed:** The type is fully known and closed

The exhaustiveness checker does not properly distinguish between these cases, leading to incorrect exhaustiveness analysis.

## Current Behavior (Incorrect)

```zig
/// TODO: Properly handle flex vs rigid extension semantics for exhaustiveness.
/// Currently rigid vars are treated as open, flex vars as closed.
fn isOpenExtension(type_store: *TypeStore, ext: Var) bool {
```

The current implementation has the semantics **backwards** or at least imprecise:
- Rigid variables should mean "this is the complete type as declared"
- Flex variables should mean "this type may have more variants"

## Expected Behavior (Correct)

### Flex Extensions (Open Union)
```roc
# Type: [Ok(a), Err(e), ...]  (flex extension means more variants possible)
match result {
    Ok(val) => val
    Err(e) => 0
    # Should require a wildcard because more variants might exist
}
```

### Rigid Extensions (Declared but Unknown)
```roc
# In a generic function with rigid type parameter
f : [Ok(a), Err(e)]b -> I64
f = |result|
    match result {
        Ok(val) => val
        Err(e) => 0
        # The `b` is rigid - we know the full shape, but b is a type parameter
    }
```

### Closed Union
```roc
# Type: [Ok(a), Err(e)]  (no extension, fully closed)
match result {
    Ok(val) => val
    Err(e) => 0
    # This IS exhaustive - no more variants possible
}
```

## Location in Codebase

**File:** `src/check/exhaustive.zig`

**Line ~997-1012:**
```zig
/// Check if an extension variable represents an open union.
/// TODO: Properly handle flex vs rigid extension semantics for exhaustiveness.
/// Currently rigid vars are treated as open, flex vars as closed.
fn isOpenExtension(type_store: *TypeStore, ext: Var) bool {
    const resolved = type_store.resolveVar(ext);
    const content = resolved.desc.content;

    return switch (content) {
        // Flex var means the union could have more variants
        .flex => true,
        // Rigid var with constraints - check if it's a union constraint
        .rigid => false,
        // Empty record means closed
        .structure => |s| switch (s) {
            .empty_record => false,
            else => true,
        },
        else => true,
    };
}
```

## Root Cause

The distinction between flex and rigid in type theory:

1. **Flex variables** are unification variables that can be instantiated to any type
2. **Rigid variables** are bound type parameters that cannot be instantiated

For exhaustiveness:
- A **flex** extension means we don't know if there are more variants → requires wildcard
- A **rigid** extension in a type signature means the variants are fixed by the caller

The current code treats rigid as "closed" (`false`) but this is semantically incorrect for some use cases.

## Semantic Analysis Required

The correct handling depends on context:

### At a Call Site
If we're checking exhaustiveness at a call site where types are fully instantiated:
- Flex extensions that unified with empty → closed
- Rigid extensions → the caller provides the full type, so effectively closed

### In a Generic Function Body
If we're checking inside a generic function with type parameters:
- Flex extensions → still open (not yet instantiated)
- Rigid extensions → the shape is known but variants come from caller

## Solution Requirements

1. **Understand the context:** Is this a monomorphic or polymorphic context?
2. **Check if extension was resolved:** A flex variable that unified with `empty_record` is closed
3. **Handle rigid correctly:** Rigid variables in function signatures have fixed structure
4. **Propagate openness:** Track whether a union is open through the algorithm

## Key Functions to Modify

1. `isOpenExtension()` (line ~997): Implement correct semantics
2. `buildUnionFromTagUnion()` (line ~903): Set `has_flex_extension` correctly
3. `checkExhaustive()` / `checkExhaustiveSketched()`: Use openness information correctly
4. May need to track additional context about whether we're in a generic function

## Data Structures

**Union struct (line ~188):**
```zig
pub const Union = struct {
    alternatives: []const CtorInfo,
    render_as: RenderAs,
    has_flex_extension: bool,  // Currently used, but semantics unclear
};
```

The `has_flex_extension` field needs clear semantics:
- Should it mean "requires wildcard for exhaustiveness"?
- Should it track both flex and rigid separately?

## Testing

Create test cases for:
1. Closed union `[A, B]` - should be exhaustive with just A and B
2. Open union with flex `[A, B, ...]` - should require wildcard
3. Generic function with rigid type param - verify correct handling
4. Flex that got resolved to closed - should work as closed
5. Nested unions with mixed extensions

## Acceptance Criteria

- [ ] `isOpenExtension()` has correct semantics documented and implemented
- [ ] Flex extensions correctly require wildcards when unresolved
- [ ] Rigid extensions are handled according to their semantic meaning
- [ ] Resolved flex extensions (unified with empty) are treated as closed
- [ ] `has_flex_extension` has clear, documented semantics
- [ ] No backwards or confused semantics between flex/rigid
- [ ] No TODO or "currently X is treated as Y" comments
