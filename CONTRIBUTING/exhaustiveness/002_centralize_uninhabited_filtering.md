# Task: Centralize Uninhabited Constructor Filtering

## Overview

The exhaustiveness checker needs to filter out "uninhabited" constructors - tag variants that can never have values because their argument types are empty. Currently, this filtering happens in two places, which makes the code harder to reason about. This task centralizes it to one location.

## Background

### What is an uninhabited type?

A type is **uninhabited** if no values of that type can exist. The simplest example is an empty tag union `[]` - there are no tags, so no values can be constructed.

More complex examples include:
- `Err([])` in `Try(I64, [])` - the `Err` variant takes an empty type, so no `Err` values exist
- `HasEmpty({ value: I64, empty: [] })` - the record contains an uninhabited field
- `Mixed(I64, [], Str)` - one of the tag's arguments is uninhabited

### Why does this matter for exhaustiveness?

When checking if a `match` expression covers all cases, we should NOT require patterns for uninhabited constructors. For example:

```roc
x : Try(I64, [])  # Error type is empty - Err can never exist

match x {
    Ok(n) => n    # This should be exhaustive! No Err case needed.
}
```

### The current problem

The code currently filters uninhabited constructors in **two places**:

1. **During exhaustiveness checking** in `checkExhaustiveSketched()` at lines ~2866-2871:
   ```zig
   // Skip uninhabited constructors - they don't need to be matched
   const arg_types = getCtorArgTypes(type_store, first_col_type, alt.tag_id);
   if (!try areAllTypesInhabited(type_store, builtin_idents, arg_types)) {
       continue;
   }
   ```

2. **During pattern inhabitedness checking** in `Pattern.isInhabited()` at lines ~196-207:
   ```zig
   // Check if this constructor exists in the union.
   var found = false;
   for (c.union_info.alternatives) |alt| {
       if (alt.tag_id == c.tag_id) {
           found = true;
           break;
       }
   }
   if (!found and !c.union_info.has_flex_extension) {
       return false;
   }
   ```

This dual-path approach is confusing because:
- It's unclear which check is the "real" one
- The second check implies constructors might be missing from unions, but why would they be?
- Changes to filtering logic must be made in two places
- Developers reading the code can't easily understand the invariants

## Goal

Centralize uninhabited constructor filtering to **one location**: during exhaustiveness checking, when iterating over union alternatives.

This means:
- Remove the "constructor exists in union" check from `Pattern.isInhabited()`
- Keep the filtering in `checkExhaustiveSketched()` where it currently skips uninhabited alternatives
- Add corresponding filtering in `isUsefulSketched()` (the redundancy checker) for consistency
- The `Union` structure will always contain ALL constructors, including uninhabited ones

## Files to Modify

All changes are in `src/check/exhaustive.zig`.

## Step-by-Step Implementation

### Step 1: Understand the current code

Read these sections in `src/check/exhaustive.zig`:
- `Pattern.isInhabited()` (around line 169) - the inhabitedness check for patterns
- `checkExhaustiveSketched()` (around line 2805) - the main exhaustiveness algorithm
- `isUsefulSketched()` (if it exists) or `isUseful()` - the redundancy/usefulness algorithm

### Step 2: Remove the constructor-lookup check from `Pattern.isInhabited()`

Find the `.ctor` case in `Pattern.isInhabited()`. It currently:
1. Checks if the union is empty (keep this)
2. Checks if the constructor exists in the union's alternatives (REMOVE this)
3. Checks if all arguments are inhabited (keep this)

Change it to only:
1. Check if the union is empty (unless `has_flex_extension` is true)
2. Check if all arguments are inhabited

The simplified code should look like:
```zig
.ctor => |c| {
    // An empty closed union is uninhabited
    if (c.union_info.alternatives.len == 0 and !c.union_info.has_flex_extension) {
        return false;
    }

    // All arguments must be inhabited for the pattern to be inhabited
    for (c.args) |arg| {
        if (!try arg.isInhabited(type_store, builtin_idents)) return false;
    }
    return true;
},
```

### Step 3: Verify filtering in `checkExhaustiveSketched()`

The filtering in `checkExhaustiveSketched()` should already be correct. Verify that:
- When iterating over `ctor_info.union_info.alternatives`, uninhabited constructors are skipped
- This happens in TWO places in the function:
  1. When checking missing constructors (the `if (num_found < num_alts)` branch)
  2. When all constructors are covered (the else branch)

Both should have code like:
```zig
const arg_types = getCtorArgTypes(type_store, first_col_type, alt.tag_id);
if (!try areAllTypesInhabited(type_store, builtin_idents, arg_types)) {
    continue;
}
```

### Step 4: Add filtering to usefulness checking (if needed)

Find the usefulness/redundancy checking code (look for `isUseful` or `isUsefulSketched`). When it iterates over union alternatives to check if a wildcard pattern is useful, it should also skip uninhabited constructors.

Look for code that does something like:
```zig
for (ctor_info.union_info.alternatives) |alt| {
    // ... specialize and check usefulness ...
}
```

Add the same uninhabited filtering:
```zig
for (ctor_info.union_info.alternatives) |alt| {
    const arg_types = getCtorArgTypes(type_store, first_col_type, alt.tag_id);
    if (!try areAllTypesInhabited(type_store, builtin_idents, arg_types)) {
        continue;
    }
    // ... specialize and check usefulness ...
}
```

### Step 5: Update comments/documentation

Remove or update any comments that mention filtering constructors from unions or checking if constructors exist in unions. The new invariant is:
- Unions contain ALL constructors
- Filtering happens at usage time during exhaustiveness/redundancy checking

## Testing

### Existing tests to verify still pass

Run the exhaustiveness tests:
```bash
zig build test-check
```

Key tests in `src/check/test/exhaustiveness_test.zig`:
- `"exhaustive - empty error type means only Ok needed"` - Try(I64, []) with only Ok is exhaustive
- `"redundant - Err pattern first on empty error type is unreachable"` - Err(_) is redundant
- `"redundant - pattern on tag with direct empty arg"` - HasEmpty(_) on [HasEmpty([]), Normal(I64)] is redundant
- `"non-exhaustive - not all inhabited tags covered with empty arg"` - Missing C on [A(I64), B([]), C(Str)]

### New tests to add

Add tests that specifically verify the invariant that unions contain all constructors:

1. **Test: Pattern on uninhabited constructor is correctly identified as uninhabited**
   ```zig
   // A pattern like Err(_) on Try(I64, []) should be detected as uninhabited
   // through argument checking, not through "constructor missing from union"
   ```

2. **Test: Nested uninhabited types work correctly**
   ```zig
   // Try(Try(I64, []), Str) - Ok(Err(_)) should be uninhabited
   ```

## How to Know You're Done

1. All existing tests pass
2. The `Pattern.isInhabited()` function no longer checks if a constructor exists in the union
3. Uninhabited constructor filtering happens in `checkExhaustiveSketched()` (and usefulness checking if applicable)
4. The code is easier to understand - there's one clear place where filtering happens
5. New tests verify the behavior works correctly after the change

## Additional Context

### Why not filter when building the Union?

You might wonder: why not just exclude uninhabited constructors when building the `Union` structure in `buildUnionFromTagUnion()`?

The reason is **redundancy checking**. When a user writes:
```roc
match x {
    Err(_) => 0   # This should be flagged as REDUNDANT
    Ok(n) => n
}
```

If `Err` was filtered out of the union entirely, then `findTagId("Err")` would return `null`, causing a type error instead of the correct redundancy warning. By keeping all constructors in the union but filtering during exhaustiveness checking, we can:
1. Correctly identify `Err(_)` as matching the `Err` constructor
2. Detect that this pattern is unreachable because `Err` is uninhabited

This design decision is documented in `docs/exhaustiveness/001_wildcard_inhabitedness.md` under "Design Decision: Where to Filter Uninhabited Constructors".
