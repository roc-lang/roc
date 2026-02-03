# Issue: Nominal Type Emptiness Checking

## Status: IMPLEMENTED ✓

This issue has been resolved. Nominal types with empty variants are correctly handled by the exhaustiveness checker.

## Problem Statement

Nominal types (like `Try`, `Result`, or user-defined types) can have uninhabited variants depending on their type parameters. For example, `Try(I64, [])` wraps a union where the error case is uninhabited. The exhaustiveness checker needs to properly determine this.

## Current Implementation (Correct)

```roc
# Try(I64, []) is effectively just Ok(I64) since the error union is empty
myTry : Try(I64, [])

match myTry {
    Ok(val) => val
    Err(e) => 0  # This branch IS correctly flagged as redundant!
}
```

The `Err` branch is correctly flagged as redundant/unreachable.

## How It Works

The exhaustiveness checker correctly handles nominal types through two mechanisms:

### 1. Backing Type Unwrapping

For nominal types, `isTypeInhabitedImpl` unwraps to the backing type:

**File:** `src/check/exhaustive.zig` (line ~1114)
```zig
.nominal_type => |nominal| {
    // Check if this is a builtin number type (Builtin.Num.*)
    // These have [] as backing type but are inhabited primitives.
    if (builtin_idents.isBuiltinNumericType(nominal)) {
        // Builtin number types (I64, U8, F32, Dec, etc.) are always inhabited
        continue;
    }
    // For other nominal types, check the backing var
    const backing_var = type_store.getNominalBackingVar(nominal);
    try stack.append(type_store.gpa, backing_var);
},
```

### 2. Tag Union Inhabitedness

For tag unions like `[Ok I64, Err []]`, the checker verifies if at least one variant is inhabited:

**File:** `src/check/exhaustive.zig` (line ~1090)
```zig
.tag_union => |tag_union| {
    // Check if at least one tag variant is inhabited
    const tag_args = tags_slice.items(.args);
    var any_tag_inhabited = false;

    for (tag_args) |args_range| {
        const arg_vars = type_store.sliceVars(args_range);
        var all_args_inhabited = true;

        for (arg_vars) |arg_var| {
            if (!try isTypeInhabited(type_store, builtin_idents, arg_var)) {
                all_args_inhabited = false;
                break;
            }
        }

        if (all_args_inhabited) {
            any_tag_inhabited = true;
            break;
        }
    }
    // ...
},
```

### 3. Uninhabited Constructor Filtering

As an optimization, uninhabited constructor paths are skipped during exhaustiveness checking:

**File:** `src/check/exhaustive.zig` (line ~2691)
```zig
// Optimization: Skip uninhabited constructors entirely.
const arg_types = getCtorArgTypes(type_store, first_col_type, alt.tag_id);
if (!try areAllTypesInhabited(type_store, builtin_idents, arg_types)) {
    continue; // Skip this uninhabited constructor
}
```

## How Nominal Types Work

### Structure
```
Nominal Type: Try(I64, [])
  ├── Name: Try
  ├── Type Arguments: [I64, []]
  └── Backing Type: [Ok(a), Err(e)]  (where a=I64, e=[])
```

### After Substitution
```
[Ok(I64), Err([])]
  ├── Ok(I64) - inhabited (I64 has values)
  └── Err([]) - NOT inhabited ([] is empty)
```

## Key Data Structures

**From `src/types/store.zig`:**
```zig
pub const NominalType = struct {
    ident: Ident.Idx,
    vars: NominalArgs.Range,  // Type arguments
    origin_module: Module.Idx,
    is_opaque: bool,
};
```

**Accessing backing type:**
```zig
const backing_var = type_store.getNominalBackingVar(nominal);
```

**Accessing type arguments:**
```zig
const args = type_store.sliceNominalArgs(nominal);
```

## Test Coverage

The following tests verify correct behavior:

- `"exhaustive - empty error type means only Ok needed"` - `Try(I64, [])` with only `Ok` is exhaustive
- `"exhaustive - nested Try with empty inner error"` - Nested nominal types work correctly
- `"exhaustive - doubly nested empty errors"` - Deep nesting works
- `"redundant - Err pattern first on empty error type is unreachable"` - `Err(_)` on `Try(I64, [])` is redundant
- `"non-exhaustive - non-empty error type requires Err case"` - `Try(I64, Str)` requires both `Ok` and `Err`

## Acceptance Criteria (All Met)

- [x] `isTypeInhabited()` correctly handles nominal types by unwrapping to backing type
- [x] Redundant patterns on empty variants are detected
- [x] Type information flows correctly through `ColumnTypes` and `getCtorArgTypes()`
- [x] Works for builtin nominal types (Try, Result) and user-defined ones
- [x] Builtin numeric types are special-cased (they have `[]` backing but are inhabited)
- [x] Uninhabited constructor paths are skipped as an optimization
