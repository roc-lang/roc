# Flex vs Rigid Extension Semantics

## Status: IMPLEMENTED ✓

The exhaustiveness checker correctly handles flex and rigid extension variables.

## Background

Tag unions can be "open" (extensible) or "closed". This is represented by an extension variable:
- **Closed union:** `[A, B]` - exactly these tags, nothing more
- **Open union (flex):** `[A, B]*` - at least these tags, possibly more during inference
- **Open union (rigid):** `[A, B]a` - at least these tags, `a` is a type parameter

## Implementation

Both flex and rigid extension variables make a union **open** for exhaustiveness checking purposes.

### isOpenExtension()

Located in `src/check/exhaustive.zig`:

```zig
fn isOpenExtension(type_store: *TypeStore, ext: Var) bool {
    const resolved = type_store.resolveVar(ext);
    const content = resolved.desc.content;

    return switch (content) {
        // Both flex and rigid extensions mean the union is open:
        // - Flex: type not fully constrained, could unify with more tags
        // - Rigid: user explicitly marked it as open (e.g., [A, B]a)
        .flex, .rigid => true,
        // Empty tag union means it's closed - no additional tags possible
        .structure => |flat_type| switch (flat_type) {
            .empty_tag_union => false,
            .tag_union => true,  // Extension is another tag union
            else => false,
        },
        // Follow aliases and recursion vars
        .alias => |alias| isOpenExtension(type_store, type_store.getAliasBackingVar(alias)),
        .recursion_var => |rec| isOpenExtension(type_store, rec.structure),
        else => false,
    };
}
```

### has_flex_extension Field

The `Union.has_flex_extension` field tracks specifically whether the extension is a **flex** variable (as opposed to rigid). This is used for **redundancy checking optimization**, not exhaustiveness:

- **Flex extension:** Wildcards are always useful since more tags might be added during type inference
- **Rigid extension:** The union is open, but the type is fixed from the caller's perspective

In `buildUnionFromTagUnion()`:
```zig
.flex => {
    is_open = true;
    has_flex = true;   // Wildcard always useful
},
.rigid => {
    is_open = true;
    has_flex = false;  // Wildcard may be redundant if all tags matched
},
```

## Why Both Are Open

Consider a function with a rigid extension:

```roc
handleResult : [Ok a, Err e]ext -> I64
handleResult = |result|
    when result is
        Ok val -> val
        Err _ -> 0
```

The `ext` is a rigid type parameter. At the function definition, we don't know what `ext` will be instantiated to - the caller might pass `[Ok I64, Err Str, Timeout]`. Therefore:

1. The match must include a wildcard to handle unknown tags
2. This is correct behavior - the function must handle any extension the caller provides

A flex extension works similarly, but during type inference rather than at call sites.

## Exhaustiveness vs Redundancy

The key insight is that "open" affects two things differently:

| Extension Type | Exhaustiveness | Wildcard Redundancy |
|---------------|----------------|---------------------|
| Closed        | All tags required | Wildcard redundant if all tags matched |
| Rigid         | Wildcard required | Wildcard may be redundant |
| Flex          | Wildcard required | Wildcard never redundant |

This is why we track both `is_open` (for exhaustiveness) and `has_flex_extension` (for redundancy).

## Test Cases

The implementation correctly handles:

1. **Closed union** `[A, B]` - exhaustive with just A and B patterns
2. **Flex extension** `[A, B]*` - requires wildcard, wildcard never redundant
3. **Rigid extension** `[A, B]a` - requires wildcard, but may detect redundancy
4. **Resolved flex** - if flex unified with `empty_tag_union`, treated as closed
5. **Nested unions** - extension chains followed correctly with cycle detection
