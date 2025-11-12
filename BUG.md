# Type Checker Bug: Missing Static Dispatch Constraints on Binary Operation Results

## Summary

When the type checker processes binary operations (like `x + x`), it creates a flex type variable for the result but **fails to add the corresponding static dispatch constraint**. This causes runtime failures when the interpreter tries to compute layouts, as it has no information to infer a default numeric type.

## Failing Test Case

**Test**: `test "simple lambdas"` in `/Users/rtfeldman/code/roc4/src/eval/test/eval_test.zig:394`

**Specific failing expression**: `(|x| x + x)(7)` (expected result: 14)

## Detailed Analysis

### What's Happening

**1. Compile-Time Type:**
- The result of the `x + x` expression has type: **`flex` with ZERO constraints**
- Specifically: `.{ .flex = .{ .name = null, .constraints = .{ .start = @enumFromInt(0), .count = 0 } } }`

**2. Runtime Type:**
- After `translateTypeVar`, it remains: **`flex` with ZERO constraints**
- The `translateTypeVar` function correctly copies the (empty) constraint list from compile-time to runtime

**3. Layout Computation:**
- When trying to compute the layout for this flex var, we hit `/Users/rtfeldman/code/roc4/src/layout/store.zig:1359`
- Error: `LayoutError.BugUnboxedFlexVar` because the flex var has no constraints to infer a default type from

### What Constraints Should It Have

For the expression `(|x| x + x)(7)`:

**Expected constraints on the result**:
- The result of `x + x` should have a **`plus` constraint** from the `+` operator
- When `x` is unified with `7` (an integer literal), `x` should get a **`from_int_digits` constraint**
- Therefore, the result should also inherit numeric constraints

**What we're getting**:
- **ZERO constraints** on the result flex var

### Why Is This Wrong?

When the type checker processes `|x| x + x`:
1. `x` is a parameter with initially unconstrained flex type
2. The `+` operator should add a `plus` static dispatch constraint to the result type
3. When called with `(7)`, the literal `7` should add `from_int_digits` constraint
4. These constraints should flow through to the result

**The type checker is failing to add the `plus` constraint to the result of binary operations.**

## Consequences

**Without constraints**:
- The layout computation has NO information about what type this is
- It can't default to I128 or Dec because there's no hint about numeric operations
- We return `BugUnboxedFlexVar` error
- The test panics

**With constraints (expected behavior)**:
- If the flex var had a `plus` constraint (or `from_int_digits` from the literal), we would:
  - Detect it's a numeric operation
  - Default to I128 for integer operations
  - Compute the layout successfully
  - Execute the code and return 14

## Root Cause Location

**The bug is in the type checker** (likely in constraint generation during binary operation type-checking).

When processing binary operations like `x + x`, the type checker should:
1. Create a fresh flex var for the result
2. Add a static dispatch constraint (e.g., `plus`) to that flex var
3. This would signal "this is the result of an addition, so it should be a numeric type"

**Expected**: Result type = `flex` with `plus` constraint
**Actual**: Result type = `flex` with NO constraints

This is a **type checker bug**, not an interpreter or layout bug. The fix belongs in the constraint generation phase of type checking.

## Test Coverage

A reproduction test has been added at:
- `/Users/rtfeldman/code/roc4/src/check/test/binop_constraint_test.zig`

This test verifies that binary operations add the appropriate static dispatch constraints to their result type variables.

## Workaround

The layout computation code in `/Users/rtfeldman/code/roc4/src/layout/store.zig` has been enhanced to:
1. Check flex vars for numeric constraints
2. Default to Dec if `from_dec_digits` constraint is present
3. Default to I128 for any other numeric constraint (like `plus`, `from_int_digits`, etc.)
4. Return `LayoutError.BugUnboxedFlexVar` only for truly unconstrained flex vars

This workaround allows constrained flex vars to work, but the root cause (missing constraints) still needs to be fixed in the type checker.
