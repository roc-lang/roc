# Polymorphic Numeric Type Inference Bug

## Problem Statement

When a polymorphic function containing numeric literals is called in a context that requires a specific numeric type, the literals inside the function body get the wrong runtime type.

### Reproduction Case

```roc
» sum = |num| 0 + num
» U64.to_str(sum(2400))
```

**Expected output:** `"2400"`
**Actual output:** `"1923270417758289920"` (garbage from type mismatch)

The literal `0` in `sum` should be inferred as `U64` when `sum(2400)` is called in a `U64.to_str` context, but instead it defaults to `Dec` at runtime.

## Root Cause Analysis

### The Type-Carrying Architecture

Roc's interpreter uses a "type-carrying" architecture where:
1. **Compile time:** The type checker infers and generalizes types. For `sum = |num| 0 + num`, the type is generalized to `Num a -> Num a`.
2. **Runtime:** Types are translated from compile-time representations to runtime representations via `translateTypeVar`.

### Where the Bug Occurs

In `src/eval/interpreter.zig`, when evaluating binary operations (lines ~7455-7530):

```zig
if (lhs_is_flex and rhs_is_flex) {
    // Both unresolved - default both to Dec
    const dec_content = try self.mkNumberTypeContentRuntime("Dec");
    const dec_var = try self.runtime_types.freshFromContent(dec_content);
    // ... unify both operands with Dec
}
```

When both operands of a binary operation have flex (unresolved) types at runtime, the interpreter defaults both to `Dec`. This is the proximate cause of the bug.

### Why Both Operands Are Flex

When `sum(2400)` is called:
1. The type checker instantiates `sum`'s type parameter `a` to `U64`
2. BUT: The CIR (Canonical Intermediate Representation) stores the **generalized** type for expressions, not the instantiated type
3. When `translateTypeVar` is called on `0` inside `sum`, it gets the generalized flex type, not `U64`
4. Similarly, `num` (the parameter) translates to flex because its compile-time type is the polymorphic parameter type

### The Fundamental Issue

The instantiation that happens at compile time (when `sum` is called with U64 context) is not propagated to the runtime evaluation of the function body. The runtime sees only the generalized types stored in the CIR.

## What Has Been Tried

### Approach 1: Lazy Binding Evaluation (Invasive - Rejected)

Changed `Binding.value` from `StackValue` to `?StackValue` to support lazy evaluation:
- Numeric literals in `s_decl_gen` declarations would be evaluated lazily
- When a polymorphic value is first used, evaluate it with the expected type from the use site

**Problems:**
- Very invasive - affects every interaction with bindings
- Required changes throughout the codebase
- Broke capture resolution for closures
- Recursion handling became complex

### Approach 2: Use `expected_rt_var` in Binop (Partially Works)

When both operands are flex, use the `expected_rt_var` (expected return type from call context) instead of defaulting to Dec.

**Problems:**
- Works for arithmetic ops like `+` where the return type matches operand types
- Breaks for comparison ops like `>` where return type is `Bool` but operands are numeric
- The `List.any` test (`|x| x > 0`) failed because `expected_rt_var` was `Bool`, not the numeric type

### Approach 3: Defer Type Resolution to Continuation (Complex)

Evaluate one operand first, then use its actual layout to determine the type for the other.

**Problems:**
- By the time the first operand is evaluated, `getRuntimeLayout` has already defaulted flex to Dec (line ~5899)
- The layout is baked in before we can use the other operand's type

### Approach 4: Reverse Evaluation Order (Incomplete)

For `0 + num`, evaluate `num` first since it's more likely to have a concrete type from a binding.

**Problems:**
- Requires adding new continuation types (`binop_eval_lhs`)
- The parameter `num` still translates to flex because its compile-time type is polymorphic
- Doesn't solve the fundamental issue that instantiation info isn't propagated

## Key Code Locations

### Binary Operation Handling
- `src/eval/interpreter.zig` lines ~7415-7570: `e_binop` case in `scheduleExprEval`
- `src/eval/interpreter.zig` lines ~10600-10720: `binop_eval_rhs` and `binop_apply` continuations

### Type Translation
- `src/eval/interpreter.zig` lines ~5970-6200: `translateTypeVar` function
- `src/eval/interpreter.zig` lines ~5880-5920: `getRuntimeLayout` - note the flex->Dec defaulting at line ~5899

### Numeric Literal Evaluation
- `src/eval/interpreter.zig` lines ~8184-8230: `evalNum` function
- Uses `expected_rt_var` if provided, otherwise calls `translateTypeVar`

### Type Instantiation
- `src/eval/interpreter.zig` lines ~7980-8050: Polymorphic call handling with `instantiateType` and `subst_map`
- `rigid_subst` map is used for rigid variable substitution, but doesn't help with flex variables from generalization

## Potential Solution Directions

### Direction A: Propagate Instantiation Context

The cleanest fix would ensure that when a polymorphic function is called:
1. Build a substitution map from the function's generalized type variables to the instantiated concrete types
2. Use this map when translating types inside the function body

This is similar to what `rigid_subst` does for rigid variables, but would need to work for generalized (flex) variables.

**Key insight:** The type checker DOES know the instantiated types at the call site. The challenge is threading that information to the runtime evaluation of the function body.

### Direction B: Fix getRuntimeLayout Defaulting

Instead of eagerly defaulting flex to Dec in `getRuntimeLayout`, defer the defaulting to a point where we have more context.

**Challenge:** Many parts of the codebase expect `getRuntimeLayout` to return a concrete layout. Would require careful analysis of all call sites.

### Direction C: Use Value Layout for Type Inference

When evaluating binops, if one operand has a concrete layout (from being a bound variable), use that layout to derive the type for the other operand.

**Challenge:** Need to evaluate operands before determining their types, which inverts the current flow.

### Direction D: Store Instantiated Types in CIR

Enhance the CIR to store instantiated types at call sites, not just generalized types.

**Challenge:** Significant change to the compilation model. Would affect canonicalization and type checking phases.

## Test Cases

### Must Pass
1. `sum = |num| 0 + num` then `U64.to_str(sum(2400))` → `"2400"`
2. `x = 42` then `I64.to_str(x)` → `"42"`
3. Recursive factorial function (tests closures/recursion)
4. `List.any([1, 0, 1, 0, -1], |x| x > 0)` → `True` (comparison in lambda)
5. `List.all` tests with various types

### Test Files
- `test/snapshots/repl/polymorphic_sum_with_typed_wrapper.md`
- `test/snapshots/repl/type_backprop_test.md`
- `test/snapshots/repl/polymorphic_numeric_literals_in_block.md`
- `src/eval/test/interpreter_style_test.zig` - List.any/List.all tests

## Guidelines for Implementation

1. **Do not use lazy binding** - It's too invasive and creates subtle bugs
2. **Test both arithmetic AND comparison operators** - They have different return types
3. **Ensure recursive functions still work** - Closures need to capture themselves
4. **The fix should be in the interpreter** - Avoid changes to canonicalization/type checking if possible
5. **Use the existing `rigid_subst` pattern as inspiration** - It solves a similar problem for rigid variables

## Commands

```bash
# Run all tests
zig build test

# Run specific test
zig build test -- --test-filter "List.any True on integers"

# Generate/update snapshot
zig build snapshot -- test/snapshots/repl/polymorphic_sum_with_typed_wrapper.md

# Run with timeout (tests can hang on infinite loops)
timeout 60 zig build test
```
