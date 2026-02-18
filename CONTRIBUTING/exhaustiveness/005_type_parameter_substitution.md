# Type Parameter Substitution for Nominal Types

## Status: IMPLEMENTED ✓

Proper type parameter substitution is now implemented for nominal types in exhaustiveness checking.

## Problem Statement

When looking up constructor argument types for nominal types like `Try(I64, Str)`, we need to substitute type parameters from the backing type with the concrete type arguments.

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

## Implementation

Located in `src/check/exhaustive.zig`:

### collectTypeParamsFromBackingType()

This helper function traverses the backing type structure and collects all unique flex/rigid variables (type parameters) in order of first encounter:

```zig
fn collectTypeParamsFromBackingType(
    type_store: *TypeStore,
    backing_var: Var,
) error{OutOfMemory}![]const Var
```

The function:
- Uses a depth-first traversal with explicit stack (no recursion)
- Tracks seen variables to avoid duplicates
- Returns parameters in declaration order (first encountered = first parameter)
- Handles all type structures: tag unions, tuples, records, functions, nested nominals

### getCtorArgTypes() - Nominal Type Handling

When encountering a nominal type:

```zig
.nominal_type => |nominal| {
    const backing_var = type_store.getNominalBackingVar(nominal);
    const nom_args = type_store.sliceNominalArgs(nominal);
    const backing_args = getCtorArgTypes(type_store, backing_var, tag_id);

    // If no substitution needed, return as-is
    if (nom_args.len == 0 or backing_args.len == 0) {
        return backing_args;
    }

    // Check if any backing args are still type parameters (flex/rigid)
    var needs_substitution = false;
    for (backing_args) |arg| {
        const arg_resolved = type_store.resolveVar(arg);
        if (arg_resolved.desc.content == .flex or arg_resolved.desc.content == .rigid) {
            needs_substitution = true;
            break;
        }
    }

    if (!needs_substitution) {
        return backing_args;  // Already substituted by unification
    }

    // Collect type parameters and build substitution map
    const type_params = collectTypeParamsFromBackingType(type_store, backing_var);

    // Substitute: param[i] -> nom_args[i]
    for (backing_args, 0..) |arg, i| {
        const arg_resolved = type_store.resolveVar(arg);
        if (arg_resolved.desc.content == .flex or arg_resolved.desc.content == .rigid) {
            // Find which parameter index this is
            for (type_params, 0..) |param, param_idx| {
                if (type_store.resolveVar(param).var_ == arg_resolved.var_) {
                    result[i] = nom_args[param_idx];
                    break;
                }
            }
        } else {
            result[i] = arg;
        }
    }

    return result;
}
```

## How It Works

1. **Get backing type args**: Recursively get the constructor's argument types from the backing type
2. **Early exit**: If no nominal args or backing args, return as-is
3. **Check for type parameters**: If backing args don't contain flex/rigid vars, unification already handled substitution
4. **Collect type params**: Traverse backing type to find all type parameter vars in order
5. **Build substitution**: Map each type parameter to its corresponding nominal type argument by position
6. **Apply substitution**: For each backing arg that is a type parameter, replace with the substituted value

## Edge Cases Handled

- **Already unified types**: If type inference already substituted parameters, we return the backing args directly
- **Multiple parameters**: Works for any number of type parameters (e.g., `Result(A, B, C)`)
- **Same parameter multiple times**: Parameter lookup correctly maps duplicates (e.g., `Pair(a, a)`)
- **OOM during collection**: Falls back to returning unsubstituted args (conservative)
- **Mismatched counts**: If param count != arg count, returns unsubstituted args

## Test Cases

The implementation correctly handles:

1. **Single parameter**: `Box(I64)` where `Box a : [Box(a)]` → constructor arg is `I64`
2. **Two parameters**: `Try(I64, Str)` where `Try a e : [Ok(a), Err(e)]`
   - `Ok` arg → `I64`
   - `Err` arg → `Str`
3. **Same parameter twice**: `Pair(I64)` where `Pair a : [Pair(a, a)]` → both args are `I64`
4. **Nested nominals**: Follows through to inner backing types
5. **Complex structures**: Records, tuples, functions with type parameters

## Memory Management

The substituted result array is allocated from `type_store.gpa`. This memory is not explicitly freed but is expected to be cleaned up when the arena allocator used for exhaustiveness checking is deallocated.
