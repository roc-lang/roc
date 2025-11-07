# Fix Exposing Semantics

## Problem

The current codebase has a fundamental misunderstanding of what gets "exposed" from modules:

1. For **Type Modules** (e.g., `Bool.roc`, `Str.roc`), ONLY the type itself is exposed - nothing else
2. Associated items are NOT exposed - they're available when their parent type is in scope, but they're not "exposed items"
3. The entire `exposed_items` data structure is being misused to store associated items

## Current Broken Architecture

- `CommonEnv.exposed_items: ExposedItems` - maps identifier -> node index for "exposed" things
- `setExposedNodeIndexById()` - adds to exposed_items
- `getExposedNodeIndexById()` - looks up from exposed_items
- `containsExposedById()` - checks if in exposed_items

These are being used for:
1. Top-level defs that are actually exposed (CORRECT)
2. Associated items (WRONG - these should use a different mechanism)

## Correct Semantics

### Type Modules
- Exposes: ONE type with the module's name
- Does NOT expose: associated items, helper functions, etc.
- Associated items become available when the type is imported/in scope

### Lookup Behavior
When canonicalizing `Bool.not(True)`:
1. Check if `Bool` type is in scope ✓
2. Look up `not` as an associated item of `Bool` (NOT as an "exposed item")
3. No concept of "exposed" for associated items

## Required Changes

### 1. Create separate index for associated items
Instead of using `exposed_items` for associated items, create:
- New data structure: `associated_items: AssociatedItems` or similar
- Maps `(parent_type_ident, item_name) -> def_idx` or similar
- Used ONLY for associated items, not for module-level exposed items

### 2. Update canonicalization
In `Can.zig` when processing associated items:
- Remove ALL calls to `setExposedNodeIndexById()` for associated items
- Instead, add to the new `associated_items` index
- Remove checks like `if (self.env.containsExposedById(qualified_idx))`

### 3. Update lookup
In `Can.zig` when looking up `Type.method`:
- Remove call to `getExposedNodeIndexById()`
- Instead, look up in `associated_items` using the parent type + method name
- No "exposed" check needed

### 4. Cleanup exposed_items usage
- `exposed_items` should ONLY contain module-level exposed defs
- For type modules: ONLY the main type declaration
- NOT associated items, NOT helper functions

### 5. Delete obsolete APIs
- `setExposedNodeIndexById()` - replace with type-specific method
- `getExposedNodeIndexById()` - replace with associated items lookup
- `containsExposedById()` - no longer needed for associated items

## Implementation Strategy

This is a large refactor. Suggested approach:
1. Add new `associated_items` data structure to `ModuleEnv`
2. Update canonicalization to populate it
3. Update lookup to use it
4. Remove obsolete code
5. Update serialization/deserialization
6. Update all tests

## Notes

- The `ExposedItems` SortedArrayBuilder is efficient - we can use a similar structure for associated items
- This affects serialization - `ModuleEnv.Serialized` needs to include associated items
- May need to audit all uses of "exposed" throughout the codebase
