# Work-list Algorithm for Type Inhabitedness Checking

## Status: IMPLEMENTED ✓

The work-list algorithm for type inhabitedness checking has been implemented in `src/check/exhaustive.zig`. This document describes the algorithm and its implementation.

## Overview

The type inhabitedness checking algorithm uses an iterative work-list approach instead of recursion, preventing potential stack overflow on deeply nested types.

## Background

### What is Inhabitedness?

A type is **inhabited** if at least one value of that type can exist. A type is **uninhabited** if no values of that type can ever be constructed.

Examples:
- `I64` is inhabited (contains integers like 0, 1, -42, etc.)
- `Bool` is inhabited (contains `Bool.true` and `Bool.false`)
- `{}` (empty record/unit) is inhabited (contains exactly one value: `{}`)
- `[]` (empty tag union) is uninhabited (no constructors exist)

### Why Does Exhaustiveness Checking Care?

When checking if pattern matching is exhaustive, we need to know which constructors must be matched. Uninhabited constructors don't need matching because no value of that type can ever reach the match expression.

Consider:
```roc
Result ok err : [Ok ok, Err err]

# If err = [] (empty tag union), then Err can never be constructed
x : Result I64 []

when x is
    Ok n -> n
```

This match IS exhaustive even without an `Err` branch, because `Err` takes an argument of type `[]`, which has no values.

### The Logical Structure

Inhabitedness checking has two kinds of logical combination:

1. **AND semantics**: Records and tuples are inhabited only if ALL their fields are inhabited
   - `{ name: Str, age: I64 }` is inhabited if BOTH `Str` AND `I64` are inhabited

2. **OR semantics**: Tag unions are inhabited if ANY variant is inhabited
   - `[Ok I64, Err []]` is inhabited because `Ok I64` is inhabited (even though `Err []` is not)

## Implementation

Located in `src/check/exhaustive.zig`, the `isTypeInhabited` function uses a work-list approach with explicit AND/OR combination.

### Data Structures

```zig
const WorkItem = union(enum) {
    /// Check if this type is inhabited, push result onto results stack
    check_type: Var,

    /// Pop N results, AND them together, push combined result
    /// (all must be true for result to be true)
    and_combine: u32,

    /// Pop N results, OR them together, push combined result
    /// (any must be true for result to be true)
    or_combine: u32,

    /// Pop result; if false and extension is open, push true; else push original
    check_open_extension: Var,
};
```

Two stacks are maintained:
1. `work_list`: Work items to process (LIFO order)
2. `results`: Boolean results from completed checks

### Algorithm Flow

1. Push initial `check_type` work item
2. Loop while work list is not empty:
   - Pop work item
   - For `check_type`: analyze the type and push appropriate work items
   - For `and_combine(n)`: pop n results, AND them, push result
   - For `or_combine(n)`: pop n results, OR them, push result
   - For `check_open_extension`: adjust result if extension is open
3. Return final result from results stack

### Type Handling

| Type | Action |
|------|--------|
| Flex/Rigid/RecursionVar | Push `true` (assume inhabited) |
| EmptyTagUnion `[]` | Push `false` (definitely uninhabited) |
| EmptyRecord `{}` | Push `true` (unit type, inhabited) |
| Record | Push `and_combine(n)`, then `check_type` for each field |
| Tuple | Push `and_combine(n)`, then `check_type` for each element |
| TagUnion | Push `check_open_extension`, `or_combine(tags)`, then for each tag: `and_combine(args)` and `check_type` for each arg |
| Function | Push `true` (functions are values) |
| Alias | Push `check_type` for backing var |
| NominalType | Check if builtin numeric, else check backing var |

### Example Trace

Checking: `[Ok { value: I64 }, Err []]`

```
Initial:
  work_list: [CheckType([Ok {value: I64}, Err []])]
  results: []

Step 1 - Process TagUnion:
  work_list: [CheckOpenExtension(ext), OrCombine(2),
              AndCombine(1), CheckType(I64),      # for Ok's args
              AndCombine(1), CheckType([])]       # for Err's args
  results: []

Step 2 - CheckType([]):
  results: [false]  # empty tag union

Step 3 - AndCombine(1):
  results: [false]  # AND of [false] = false

Step 4 - CheckType(I64):
  results: [false, true]  # I64 is inhabited

Step 5 - AndCombine(1):
  results: [false, true]  # AND of [true] = true

Step 6 - OrCombine(2):
  results: [true]  # OR of [false, true] = true

Step 7 - CheckOpenExtension (extension is closed):
  results: [true]  # stays true

Final result: true (the type is inhabited)
```

## Benefits

1. **No stack overflow**: Arbitrary nesting depth is handled with heap allocation
2. **Explicit control flow**: The AND/OR combination logic is visible in the work items
3. **Debuggable**: Can inspect work-list state at any point during execution
4. **Testable**: Easy to write unit tests for specific work-list scenarios

## Implementation Notes

### Work Item Order

Work items are pushed in reverse order because we use LIFO (stack) semantics. When processing a tag union, we push:
1. `CheckOpenExtension` first (processed last)
2. `OrCombine` second (processed after all tag checks)
3. Tag argument checks last (processed first)

### Zero-Argument Constructors

A tag with zero arguments (like `None` in `[Some a, None]`) is handled correctly:
- `AndCombine(0)` pushes `true` (empty AND = true)
- This represents "all zero arguments are inhabited" which is vacuously true

### Cycle Detection

Recursive types contain recursion variables that point back to the type being defined. The algorithm treats recursion variables as inhabited, which is correct:
- If the type was inferred from a value, that value exists
- If from an annotation, illegal recursive types are caught earlier

## References

- Implementation: `src/check/exhaustive.zig`, function `isTypeInhabited`
- Exhaustiveness checking paper: "Warnings for pattern matching" (Maranget, 2007)
