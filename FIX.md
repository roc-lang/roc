# Fix Plan: Add Static Dispatch Constraints to Binary Operation Results

## Problem Summary

When the type checker processes binary operations (e.g., `x + y`), it creates static dispatch constraints for the operands but **fails to add the constraint to the result type**. This means the result gets an unconstrained flex variable, which causes runtime failures when the layout computation has no information to infer a default type.

## Root Cause

**Location**: `/Users/rtfeldman/code/roc4/src/check/Check.zig:3719-3750`

In the `checkBinopExpr` function's static dispatch path:

```zig
// Line 3724: Create return type - just a fresh unconstrained variable
const ret_var = try self.fresh(env, expr_region);

// Lines 3727-3731: Create constraint function type
const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
    .args = args_range,  // [lhs_var, rhs_var]
    .ret = ret_var,      // ← PROBLEM: unconstrained!
    .needs_instantiation = false,
} } }, env, expr_region);

// Lines 3734-3739: Create and add constraint
const constraint = StaticDispatchConstraint{
    .fn_name = method_name,  // e.g., "plus"
    .fn_var = constraint_fn_var,
    .origin = .desugared_binop,
};
const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});

// Lines 3742-3747: Add constraint to LHS only
const constrained_var = try self.freshFromContent(
    .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
    env,
    expr_region,
);
_ = try self.unify(constrained_var, lhs_var, env);

// Line 3750: Unify result with unconstrained return var
_ = try self.unify(expr_var, ret_var, env);  // ← BUG: ret_var has no constraints!
```

**The Issue**: For a binary operation like `a + b`, the proper static dispatch constraint should be:
```
a.plus : a, a -> a
```

This means ALL THREE type variables (left operand, right operand, and result) should reference the same constrained type `a`. However, currently:
- ✅ LHS (`a`) gets the constraint
- ❌ RHS (`b`) doesn't explicitly get the constraint (but might get it via unification with LHS)
- ❌ **Result has NO constraint** - it's just a fresh flex variable

## The Fix

### Step 1: Add Constraint to Return Type Variable

**File**: `/Users/rtfeldman/code/roc4/src/check/Check.zig`
**Function**: `checkBinopExpr`
**Lines**: ~3724-3750

Instead of creating an unconstrained `ret_var`, create a constrained return variable with the same constraint as the LHS.

**Before**:
```zig
// Line 3724: Create return type - just a fresh unconstrained variable
const ret_var = try self.fresh(env, expr_region);

// ... create constraint ...

// Add constraint only to LHS
const constrained_var = try self.freshFromContent(
    .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
    env,
    expr_region,
);
_ = try self.unify(constrained_var, lhs_var, env);

// Unify result with unconstrained return var
_ = try self.unify(expr_var, ret_var, env);
```

**After**:
```zig
// Create the static dispatch constraint FIRST (before creating ret_var)
const constraint = StaticDispatchConstraint{
    .fn_name = method_name,
    .fn_var = undefined,  // Will be filled in after creating constraint_fn_var
    .origin = .desugared_binop,
};

// Create constrained return variable with the SAME constraint
const ret_var = try self.freshFromContent(
    .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
    env,
    expr_region,
);

// Now create the constraint function type with constrained return
const args_range = try self.types.appendVars(&.{ lhs_var, rhs_var });
const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
    .args = args_range,
    .ret = ret_var,  // Now constrained!
    .needs_instantiation = false,
} } }, env, expr_region);

// Update the constraint with the function var
// (This requires making constraint mutable or reconstructing it)
const final_constraint = StaticDispatchConstraint{
    .fn_name = method_name,
    .fn_var = constraint_fn_var,
    .origin = .desugared_binop,
};
const constraint_range = try self.types.appendStaticDispatchConstraints(&.{final_constraint});

// Add constraint to LHS
const constrained_lhs_var = try self.freshFromContent(
    .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
    env,
    expr_region,
);
_ = try self.unify(constrained_lhs_var, lhs_var, env);

// Unify result with constrained return var
_ = try self.unify(expr_var, ret_var, env);  // Now ret_var has the constraint!
```

**WAIT** - there's a circular dependency issue here. The constraint needs the `constraint_fn_var`, but the `constraint_fn_var` needs the `ret_var`, but the `ret_var` needs the constraint. Let me think about this differently...

### Alternative Approach: Add Constraint After Creating Function Type

Actually, looking at the code more carefully, I think the issue is that we need to create THREE constrained variables (lhs, rhs, ret) all with the SAME constraint range. Here's the correct approach:

```zig
// Step 1: Create unconstrained return var (temporarily)
const ret_var = try self.fresh(env, expr_region);

// Step 2: Create the constraint function type
const args_range = try self.types.appendVars(&.{ lhs_var, rhs_var });
const constraint_fn_var = try self.freshFromContent(.{ .structure = .{ .fn_unbound = Func{
    .args = args_range,
    .ret = ret_var,
    .needs_instantiation = false,
} } }, env, expr_region);

// Step 3: Create the constraint
const constraint = StaticDispatchConstraint{
    .fn_name = method_name,
    .fn_var = constraint_fn_var,
    .origin = .desugared_binop,
};
const constraint_range = try self.types.appendStaticDispatchConstraints(&.{constraint});

// Step 4: Add constraint to LHS (existing code)
const constrained_lhs_var = try self.freshFromContent(
    .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
    env,
    expr_region,
);
_ = try self.unify(constrained_lhs_var, lhs_var, env);

// Step 5: **NEW** - Add the SAME constraint to the return variable
const constrained_ret_var = try self.freshFromContent(
    .{ .flex = Flex{ .name = null, .constraints = constraint_range } },
    env,
    expr_region,
);
_ = try self.unify(constrained_ret_var, ret_var, env);

// Step 6: Unify result with the constrained return var
_ = try self.unify(expr_var, constrained_ret_var, env);
```

This approach:
1. Creates the constraint with an unconstrained return type
2. Creates constrained flex variables for both LHS and return type
3. Unifies them with their respective type variables
4. This propagates the constraint to both the LHS and the result

### Step 2: Test the Fix

Run the tests in `/Users/rtfeldman/code/roc4/src/check/test/binop_constraint_test.zig`:

```bash
zig build test
```

Expected results:
- ✅ `addition result should have 'plus' constraint` - PASS
- ✅ `subtraction result should have 'minus' constraint` - PASS
- ✅ `multiplication result should have 'mul' constraint` - PASS
- ✅ `division result should have 'div' or 'div_trunc' constraint` - PASS
- ✅ `chained operations should accumulate constraints` - PASS
- ✅ `integer literal should have 'from_int_digits' constraint` - PASS (or fail if literals also need fixing)
- ✅ `decimal literal should have 'from_dec_digits' constraint` - PASS (or fail if literals also need fixing)

### Step 3: Verify Interpreter Tests Pass

The failing interpreter tests should now pass:

```bash
zig build test
```

Expected:
- `test "simple lambdas"` in `/Users/rtfeldman/code/roc4/src/eval/test/eval_test.zig:394` should PASS
- Specifically `(|x| x + x)(7)` should evaluate to `14` without panicking

### Step 4: Handle Numeric Literals (If Needed)

The last two tests check that numeric literals (`42` and `3.14`) have the appropriate constraints. If these tests fail, we need to also fix the numeric literal handling.

**Location**: `/Users/rtfeldman/code/roc4/src/check/Check.zig` around line 2189 (`.e_num` case)

The numeric literal handling should add `from_int_digits` or `from_dec_digits` constraints to the type variable. Currently it creates `Num.num_unbound` types but may not be adding the constraint properly.

If the tests fail, investigate how numeric literals are supposed to get their constraints and ensure they're being added.

## Testing Strategy

1. **Unit Tests**: The `binop_constraint_test.zig` tests will verify that constraints are present after type checking
2. **Integration Tests**: The existing `eval_test.zig` tests will verify that the interpreter can execute code that previously panicked
3. **Manual Testing**: Try expressions like:
   - `|x, y| x + y` should type as `a, a -> a where [a.plus : a, a -> a]`
   - `(|x| x + x)(7)` should evaluate to `14`
   - `add = |x, y| x + y; add(10, 5)` should evaluate to `15`

## Potential Issues and Solutions

### Issue 1: RHS Constraint

**Question**: Should the RHS also get an explicit constraint?

**Answer**: In the current implementation, the RHS doesn't get an explicit constraint added, but it gets constrained through unification with the LHS (via the function signature). This should be sufficient, but if tests show otherwise, we can add an explicit constraint to RHS as well using the same pattern.

### Issue 2: Constraint Ordering

**Question**: Does the order of unifications matter?

**Answer**: Yes, potentially. We should unify the constrained vars with the actual vars BEFORE unifying expr_var with ret_var, to ensure constraints propagate properly.

### Issue 3: Multiple Constraints

**Question**: What if a type variable already has constraints and we're adding more?

**Answer**: The unification process should merge constraints. If there are issues, we may need to check if the variable already has constraints and merge them appropriately.

## Success Criteria

✅ All 7 tests in `binop_constraint_test.zig` pass
✅ All interpreter tests pass (no panics, correct results)
✅ Build passes with no errors
✅ The test count returns to ~1411/1426 or better

## Code Locations Reference

- **Main fix location**: `/Users/rtfeldman/code/roc4/src/check/Check.zig:3719-3750` (function `checkBinopExpr`)
- **Test file**: `/Users/rtfeldman/code/roc4/src/check/test/binop_constraint_test.zig`
- **Bug documentation**: `/Users/rtfeldman/code/roc4/BUG.md`
- **Integration tests**: `/Users/rtfeldman/code/roc4/src/eval/test/eval_test.zig`
