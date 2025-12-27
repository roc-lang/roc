# Issue: Type Parameter Substitution for Nominal Types

## Problem Statement

When looking up constructor argument types for nominal types (like `Try(I64, Str)`), the code uses a **heuristic** instead of properly substituting type parameters. This can lead to incorrect type information being used during exhaustiveness checking.

## Current Behavior (Incorrect)

```zig
// TODO: Properly substitute type parameters instead of this heuristic.
if (backing_args.len == 1 and nom_args.len > 0) {
    const first_arg_resolved = type_store.resolveVar(backing_args[0]);
    if (first_arg_resolved.desc.content == .flex or first_arg_resolved.desc.content == .rigid) {
        return nom_args;
    }
}
```

This heuristic assumes that if the backing type has one argument and it's a type variable, we can just use the nominal type's arguments directly. This breaks for:
- Multiple type parameters
- Nested type parameters
- Type parameters used in complex positions

## Expected Behavior (Correct)

Given:
```roc
Try a e : [Ok(a), Err(e)]

myTry : Try(I64, Str)
```

When looking up the argument type for `Ok`, we should:
1. See that `Ok` has argument type `a` in the backing type
2. Find that `a` is the first type parameter of `Try`
3. Look up the first type argument of `myTry`, which is `I64`
4. Return `I64` as the argument type

## Location in Codebase

**File:** `src/check/exhaustive.zig`

**Line ~1068-1090:**
```zig
fn getCtorArgTypes(type_store: *TypeStore, type_var: Var, tag_id: TagId) []const Var {
    // ... earlier code ...

    // Unwrap nominal types to find the backing tag union
    if (content.unwrapNominalType()) |nominal| {
        const backing_var = type_store.getNominalBackingVar(nominal);
        const nom_args = type_store.sliceNominalArgs(nominal);

        // Get the backing type's tag arguments
        const backing_args = getCtorArgTypes(type_store, backing_var, tag_id);

        // TODO: Properly substitute type parameters instead of this heuristic.
        if (backing_args.len == 1 and nom_args.len > 0) {
            const first_arg_resolved = type_store.resolveVar(backing_args[0]);
            if (first_arg_resolved.desc.content == .flex or first_arg_resolved.desc.content == .rigid) {
                return nom_args;
            }
        }

        return backing_args;
    }
    // ...
}
```

## Root Cause

Nominal types have:
1. **Type parameters:** The abstract parameters in the definition (e.g., `a` and `e` in `Try a e`)
2. **Type arguments:** The concrete types provided at use (e.g., `I64` and `Str` in `Try(I64, Str)`)
3. **Backing type:** The actual type structure with parameters (e.g., `[Ok(a), Err(e)]`)

Proper substitution requires:
1. Building a mapping from parameters to arguments
2. Walking the backing type and replacing each parameter with its argument
3. Handling nested/complex type structures

The heuristic only works for simple single-parameter cases.

## How Type Parameters Are Stored

Looking at the type system:

```zig
// From types/store.zig
pub const NominalType = struct {
    ident: Ident.Idx,           // Name like "Try"
    vars: NominalArgs.Range,     // Type arguments [I64, Str]
    origin_module: Module.Idx,
    is_opaque: bool,
};
```

The backing type is accessed via `getNominalBackingVar()`, which returns a `Var` that resolves to the type structure with parameters.

## Solution Requirements

1. **Build parameter-to-argument mapping:**
   ```zig
   // Map each type parameter index to its concrete argument
   // param[0] (a) -> I64
   // param[1] (e) -> Str
   ```

2. **Substitute in backing type:**
   - When encountering a flex/rigid var that's a parameter, replace with argument
   - Handle nested types (e.g., `List a` where `a` is a parameter)
   - Handle multiple occurrences of the same parameter

3. **Return substituted types:**
   - The result should have all parameters replaced with concrete types

## Implementation Approach

```zig
fn getCtorArgTypesWithSubstitution(
    allocator: std.mem.Allocator,
    type_store: *TypeStore,
    nominal: NominalType,
    tag_id: TagId,
) ![]const Var {
    const backing_var = type_store.getNominalBackingVar(nominal);
    const nom_args = type_store.sliceNominalArgs(nominal);

    // Get the backing type's tag arguments (these contain type parameters)
    const backing_args = getCtorArgTypes(type_store, backing_var, tag_id);

    // Allocate result array
    const result = try allocator.alloc(Var, backing_args.len);

    for (backing_args, 0..) |arg_var, i| {
        result[i] = substituteTypeParams(type_store, arg_var, nom_args);
    }

    return result;
}

fn substituteTypeParams(type_store: *TypeStore, var_: Var, substitutions: []const Var) Var {
    const resolved = type_store.resolveVar(var_);

    return switch (resolved.desc.content) {
        .flex, .rigid => |param| {
            // Check if this is a type parameter that should be substituted
            // Need to determine the parameter index and look up in substitutions
            // This requires knowing the parameter's position in the original definition
        },
        // Handle other cases: recurse into structures, etc.
    };
}
```

## Challenges

1. **Identifying parameters:** Need to know which flex/rigid vars are type parameters vs other variables
2. **Parameter ordering:** Need to map parameter position to argument position
3. **Complex types:** Handle `List a`, `Result a e`, nested nominals, etc.

## Functions to Modify

1. `getCtorArgTypes()` (line ~1050): Implement proper substitution
2. May need new helper: `substituteTypeParams()`
3. May need to access nominal type's parameter list (not just arguments)

## Testing

Create test cases for:
1. Single parameter: `Try(I64, Str)` → `Ok` has `I64`, `Err` has `Str`
2. Same parameter used twice: `Pair a : [Pair(a, a)]`
3. Nested parameters: `Result(List a, e)`
4. Complex nesting: `Try(Result(a, b), c)`
5. Parameter in non-first position

## Acceptance Criteria

- [ ] Type parameters are correctly substituted in all positions
- [ ] Works for single and multiple type parameters
- [ ] Handles nested nominal types
- [ ] No heuristics or special cases for "single parameter"
- [ ] No TODOs in the solution
- [ ] Comprehensive tests for various nominal type structures
