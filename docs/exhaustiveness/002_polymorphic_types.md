# Issue: Polymorphic Type Handling

## Status: DEFERRED / BY DESIGN

**Current behavior:** When a match expression's condition has a polymorphic type, exhaustiveness checking is skipped via `error.TypeError`.

**Design clarification:** Skipping is correct when:
1. The type is erroneous (type mismatch already reported)
2. The type is genuinely polymorphic in valid generic code

The reified path (which used `ReifiedRows.has_unresolved_ctor`) has been removed as dead code. The sketched path handles polymorphic types via `error.TypeError` which triggers silent skip in `Check.zig`.

Future work could add constraint-based checking for polymorphic types, but this is not a high priority since the common case (type error) is already handled correctly.

---

## Original Problem Statement (Historical)

When a match expression's condition has a polymorphic type (a type variable that hasn't been resolved to a concrete type), the exhaustiveness checker currently **skips checking entirely**. This means users get no exhaustiveness errors for these matches, even when they should.

## Current Behavior (Incorrect)

```roc
# x has type `a` (polymorphic)
match x {
    Ok(val) => val
    # Missing: Err case
}
```

Currently, because `x` is polymorphic, exhaustiveness checking is silently skipped. No error is reported for the missing `Err` case.

## Expected Behavior (Correct)

The exhaustiveness checker should handle polymorphic types in one of these ways:
1. **Defer checking:** Wait until the type is resolved, then check
2. **Conservative checking:** Report that the match may be non-exhaustive because the type is unknown
3. **Constraint-based checking:** Use type constraints to determine what patterns are needed

## Locations in Codebase

**File:** `src/check/exhaustive.zig`

### Location 1 - ReifiedRows flag (Line ~686-690)
```zig
/// True if any constructor patterns couldn't be fully resolved
/// (e.g., polymorphic types where the union structure isn't known).
/// TODO: We should handle polymorphic types properly instead of skipping checks.
has_unresolved_ctor: bool,
```

### Location 2 - getUnionFromType (Line ~878-882)
```zig
.recursion_var => |rec| {
    return getUnionFromType(allocator, type_store, ident_store, rec.structure);
},
// TODO: Handle polymorphic types properly instead of treating as not a union.
.flex, .rigid => return .not_a_union,
```

### Location 3 - reifyPattern polymorphic arg check (Line ~728-731)
```zig
if (is_unresolved and arg != .anything) {
    // Arg type is polymorphic but pattern expects something specific.
    // We can't reliably do exhaustiveness checking.
    return error.TypeError;
}
```

## Root Cause

The type system uses `flex` (flexible) and `rigid` type variables to represent polymorphic types:
- **Flex variables:** Can be unified with any type (like `a` in `List a`)
- **Rigid variables:** Must remain polymorphic (like `a` in `forall a. a -> a`)

When the exhaustiveness checker encounters these, it has no information about what constructors exist for the type, so it gives up.

## How Type Resolution Works

1. **Type inference** runs first, resolving many type variables
2. **Exhaustiveness checking** runs after inference is complete
3. By the time exhaustiveness runs, most types should be concrete
4. However, some types remain polymorphic (e.g., in generic functions)

## Solution Approaches

### Approach A: Constraint-Based Checking

For a pattern like `Ok(val)`, we know the type must be *at least* a union containing `Ok`. Even without knowing the full type, we can:
1. Record that `Ok` is covered
2. Require either a wildcard or explicit handling of "other cases"

### Approach B: Conservative Reporting

When the type is polymorphic:
1. Check what patterns are present
2. If there's no wildcard/catch-all, warn that the match may be non-exhaustive
3. The warning should explain that the type is polymorphic

### Approach C: Deferred Checking

This would require architectural changes:
1. Record match expressions that couldn't be checked
2. Re-check them after more type information is available
3. This may not be feasible given the current compiler architecture

## Key Functions to Modify

1. `getUnionFromType()` (line ~856): Instead of returning `.not_a_union` for flex/rigid, return a new variant indicating polymorphic type
2. `reifyPattern()` (line ~700): Handle polymorphic types by creating a special pattern representation
3. `checkMatch()` (line ~3057): Add logic for polymorphic type handling
4. `ReifiedRows` struct: Change `has_unresolved_ctor` to something more actionable

## Testing

Create test cases for:
1. Match on a value with polymorphic type and no wildcard → should warn
2. Match on a value with polymorphic type with wildcard → should be OK
3. Generic function with match expression → verify proper handling
4. Polymorphic type that gets resolved → verify checking happens

## Acceptance Criteria

- [ ] Matches on polymorphic types are NOT silently skipped
- [ ] Users receive appropriate warnings/errors for potentially non-exhaustive matches
- [ ] Matches with wildcards on polymorphic types are handled correctly
- [ ] Clear error messages explain why the type is polymorphic
- [ ] No TODOs, `has_unresolved_ctor` workarounds, or silent skipping
- [ ] The solution handles both flex and rigid variables appropriately
