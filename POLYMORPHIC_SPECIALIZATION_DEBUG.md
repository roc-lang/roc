# Polymorphic Specialization Implementation - Debug Report

## Current Status: Three Polymorphic Tests Failing

The three failing tests are testing polymorphic function behavior:
1. `polymorphic identity function` - Tests `identity = |val| val` called with different types
2. `direct polymorphic function usage`
3. `multiple polymorphic instantiations`

All three expect different specializations of the same polymorphic function to handle different types correctly.

## The Issue in Detail

### Problem 1: Wrong Type Passed to Specialization (PARTIALLY FIXED)

**Original Bug**: Lower.zig was passing `ModuleEnv.varFrom(expr_idx)` - the **result type** of the call - to specialization instead of the function type or argument type.

**What we changed**: We modified Lower.zig to pass `first_arg_type_var` - the type of the first call argument:

```zig
const arg_indices = module_env.store.sliceExpr(call.args);
const first_arg_type_var = if (arg_indices.len > 0)
    ModuleEnv.varFrom(arg_indices[0])
else
    ModuleEnv.varFrom(call.func);
```

### Problem 2: Type Variable Transformation (THE REAL ISSUE)

**Discovery**: The type variable passed to specialization undergoes a transformation before reaching dev_evaluator.zig.

For `identity(5)` and `identity("Hello")`:
- We pass argument type vars **5** (decimal) and **10** (string)
- By the time they reach specialization, they become **7** and **13** (fresh instantiated vars)
- Vars 7 and 13 resolve to **nominal types** that wrap other structures
- These nominal types don't directly map to the concrete types we need

**Debug Output Evidence**:

```
For identity(5):
  first_arg_type_var = 5 (decimal)
  → specialization receives spec.type_var = 7
  → var 7 resolves to nominal type wrapping something else

For identity("Hello"):
  first_arg_type_var = 10 (string)
  → specialization receives spec.type_var = 13
  → var 13 resolves to nominal type wrapping something else
```

### Problem 3: Layout Resolution Failure

**What we tried**: Setting up type scope to map generic parameters to concrete types:

```zig
// Map generic param to the argument type
try spec_scope.put(generic_param, types.ModuleVar{
    .module_idx = spec.module_idx,
    .var_ = spec.type_var,  // vars 7 or 13
});
```

**What went wrong**: When getPatternLayout resolves these mappings:
- First specialization: generic param 1 → var 7 → nominal type → layout `.dec` ✓ (correct by accident)
- Second specialization: generic param 1 → var 13 → nominal type → layout `.dec` ✗ (WRONG! should be `.str`)

**The Result**: Both specializations get the same layout even though they should be different.

### Problem 4: Execution Failure

**Current Behavior**:
- Interpreter correctly returns `"Hello"`
- Dev evaluator returns `""`  (empty string)
- This happens for BOTH `identity(5)` and `identity("Hello")`

**Why**:
1. The cache key IS working - we get different layouts (26 vs 29)
2. But both specializations fail to execute correctly
3. The parameters aren't getting the right concrete types
4. The function bodies execute with wrong type information

## Key Findings from Exploration

### 1. Nominal Types in Roc

- Nominal types are **wrappers** around concrete types, not replacements
- Structure: `vars[0]` = backing type variable, `vars[1+]` = type arguments
- They provide type identity while wrapping the actual structure
- This is why unwrapping them gives tag unions instead of builtin types

**Source**: `/home/lbw/Documents/Github/roc/src/types/store.zig` lines 664-699

**Helper Functions**:
- `getNominalBackingVar(nominal)` - Returns vars[0], the backing type
- `sliceNominalArgs(nominal)` - Returns vars[1+], the type arguments
- `canLiftInner(nominal, cur_module_idx)` - Checks if backing type can be accessed

**Unwrapping Pattern** (from Lower.zig lines 506-510):
```zig
if (resolved.desc.content.unwrapNominalType()) |nominal| {
    const backing_var = types_store.getNominalBackingVar(nominal);
    resolved = types_store.resolveVar(backing_var);
}
```

### 2. Type Transformation During Specialization

The type system transforms argument type variables into fresh variables:

```
Original arg type var (5 or 10)
    ↓
Fresh instantiation (7 or 13)
    ↓
Resolves to nominal type
    ↓
Problem: can't recover the original concrete types
```

This transformation happens during instantiation/specialization in the type system, and by the time we receive `spec.type_var` in dev_evaluator.zig, we've lost the connection to the original argument types.

### 3. What COR Does Differently

COR stores the **complete function type** and decomposes it:

```ocaml
let in', _lset, out' = extract_fn t_new in  (* Extract from FUNCTION TYPE *)
let arg = lower_type mono_cache fresh_tvar in'
let ret = lower_type mono_cache fresh_tvar out'
```

COR extracts concrete types at specialization time using the function type structure. We're trying to do something similar but the nominal types are getting in the way.

## The Fundamental Challenge

### The Core Problem

We need to map generic parameters to their concrete types, but:

1. ✅ We CAN get different specializations (cache key works - layouts 26 vs 29)
2. ❌ We CAN'T get the right concrete types for type scope mapping
3. ❌ Type variables undergo transformation we can't easily reverse
4. ❌ Nominal type wrappers don't give us what we need

### Why It Matters

- Generic parameter `a` in `|a| a` needs to map to the ACTUAL concrete type (Int or String)
- Without this, parameters get default/unresolved layouts
- This causes calling convention mismatches and wrong execution
- The function `identity` is created once as a generic lambda, then specialized for each type
- Each specialization needs parameters with the correct type and layout information

## What Would Fix It

We need ONE of the following:

### Option 1: Access to Actual Call Arguments in Specialization Code

**Requirements**:
- Currently we only have `spec.type_var` (transformed)
- We'd need `spec.call_arg_types` or similar
- Would require modifying `NeededSpecialization` structure
- **Status**: Plan explicitly says "no data structure changes needed"

**Approach**:
```zig
pub const NeededSpecialization = struct {
    // ... existing fields ...
    call_arg_types: []types.Var,  // ← NEW: actual argument types from call
};
```

### Option 2: Reverse the Type Transformation

**Requirements**:
- Find where vars 5→7, 10→13 transformation happens
- Trace back to get the original concrete types
- Then use those for type scope mapping

**Approach**:
- Investigate `Instantiate.zig` where specializations are created
- Look for where fresh variables are generated
- See if we can preserve or recover the mapping

### Option 3: Different Extraction from Function Type

**Requirements**:
- Current approach extracts function type args (gives nominal types)
- Need to find a path that gives actual concrete types
- Similar to how Lower.zig at line 506-510 unwraps nominal types

**Approach**:
```zig
// Current (wrong):
const concrete_arg_vars = spec_module_env.types.sliceVars(concrete_func_type.args);
// Returns nominal-wrapped types

// Needed:
// Some way to get unwrapped concrete types directly
```

### Option 4: Modify COR's Approach

**Requirements**:
- Instead of storing function type, store the actual arg/ret types
- Would require changes to Instantiate.zig to capture concrete types
- Different from how we currently build NeededSpecialization

**Approach**:
```zig
pub const NeededSpecialization = struct {
    type_var: types_mod.Var,      // Current: generic or transformed
    concrete_arg_type: types_mod.Var,  // ← Alternative: actual arg type
};
```

## Implementation Status

### Completed ✅

- Changed Lower.zig to pass argument type instead of result type
- Implemented fresh symbol creation for each specialization
- Added cache key differentiation (layouts 26 vs 29)
- Implemented re-lowering for each specialization
- Set up type scope mapping infrastructure
- Found nominal type unwrapping pattern in codebase

### In Progress / Blocked ❌

- Type scope mapping with correct concrete types
  - Issue: spec.type_var has been transformed
  - Mapping to nominal-wrapped types doesn't work
  - Both specializations get same layout (.dec)

- Getting actual concrete types to dev_evaluator.zig
  - Type variable transformation is opaque
  - Can't recover original arg types (5, 10) from transformed vars (7, 13)

- Making specializations execute correctly
  - Even with different layouts, both return empty strings
  - Parameters not getting correct type information

## Files Modified

1. **src/mono/Lower.zig**
   - Line 2357-2369: Changed from passing result type to passing first argument type
   - Added tracking of lowered symbol CIR expressions
   - Some debug print statements (should be removed)

2. **src/eval/dev_evaluator.zig**
   - Lines 744-788: Specialization solving loop
   - Attempted to set up type scope mapping
   - Currently simplified to just re-lower without type scope

3. **src/backend/dev/MonoExprCodeGen.zig** (already implemented)
   - Parameter layout hashing in cache key
   - Ensures different specializations get different cache keys

## Test Results

### Current Test Output

```
getPatternLayout: computing from type_var=@enumFromInt(1), use_type_scope=true, scopes.len=1
  Found type_scope mapping: type_var=@enumFromInt(1) -> module_idx=0, mapped_var=@enumFromInt(7)
    Resolves to: var=@enumFromInt(77), content=.{ .structure = .{ .nominal_type = ... } }
  getPatternLayout: result layout_idx=.dec

getPatternLayout: computing from type_var=@enumFromInt(1), use_type_scope=true, scopes.len=1
  Found type_scope mapping: type_var=@enumFromInt(1) -> module_idx=0, mapped_var=@enumFromInt(13)
    Resolves to: var=@enumFromInt(72), content=.{ .structure = .{ .nominal_type = ... } }
  getPatternLayout: result layout_idx=.dec

Evaluator mismatch! Interpreter: "Hello", DevEvaluator: ""
```

**Analysis**:
- Type scope IS being used ✓
- Mappings ARE being found ✓
- BUT both map to nominal types ✗
- AND both get .dec layout ✗ (second should be .str)
- RESULT: Wrong execution ✗

## Next Steps for Investigation

1. **Trace type variable transformation**
   - Add debug output in Instantiate.zig to see where 5→7 happens
   - Check if we can preserve the original type mapping

2. **Investigate function type structure**
   - When we have the function type (String -> String), what does it contain?
   - Can we extract concrete arg types from function type before it's transformed?

3. **Check type scope implementation**
   - Why does getPatternLayout return .dec for both nominal types?
   - Should nominal types be unwrapped automatically in layout resolution?

4. **Review setupLocalCallLayoutHints**
   - This function correctly handles polymorphic calls
   - Compare its approach with our specialization approach
   - See if we can use similar logic

## Related Code References

- **Type system unwrapping**: `/home/lbw/Documents/Github/roc/src/types/store.zig` lines 664-699
- **Nominal type unwrapping pattern**: `/home/lbw/Documents/Github/roc/src/mono/Lower.zig` lines 506-510
- **Reference implementation**: COR's `cor/experiments/lss/lambdamono/specializations.ml`
- **Type scope mapping**: `/home/lbw/Documents/Github/roc/src/mono/Lower.zig` lines 1219-1385
- **Layout resolution**: `/home/lbw/Documents/Github/roc/src/layout/store.zig`

---

**Last Updated**: During implementation of polymorphic specialization fix
**Status**: Blocked by type variable transformation and nominal type handling
**Priority**: Medium - affects 3 core polymorphism tests
