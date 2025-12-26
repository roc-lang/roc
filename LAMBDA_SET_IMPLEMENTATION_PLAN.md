# Lambda Set Implementation Plan: Zig Compiler

## Summary

After investigating the actual code, the Zig implementation has most lambda set functionality working correctly. The Cor architecture (resolving lambda sets after type checking) simplifies things significantly compared to the Rust compiler.

**What works:**
- Lambda set creation and tracking
- Closure transformation and defunctionalization
- Recursion detection for self-referential closures
- Single-module static dispatch resolution
- `is_eq` for structural types (records, tuples, tag unions)

**What's actually broken:**
1. Cross-module static dispatch resolution (plumbing exists but isn't connected in production - but is now fully tested)
2. Only `is_eq` is derived for structural types (no `hash`, etc.)

---

## Issue 1: Cross-Module Static Dispatch Resolution

### The Problem

The infrastructure exists but is never connected:

- `ClosureTransformer.requestExternalLambdaSet()` creates requests (line 987)
- `ClosureTransformer.getExternalLambdaSetRequests()` returns pending requests (line 1021)
- `ClosureTransformer.processExternalLambdaSetRequests()` can process them (line 1135)

**But**: `processExternalLambdaSetRequests()` is never called in the compilation pipeline. Requests accumulate and are silently ignored.

### Testing Status

`processExternalLambdaSetRequests()` is now fully tested with three test cases:
- `"ClosureTransformer: processExternalLambdaSetRequests resolves matching requests"` - tests partial resolution (some succeed, some fail)
- `"ClosureTransformer: processExternalLambdaSetRequests with no requests"` - tests empty case
- `"ClosureTransformer: processExternalLambdaSetRequests resolves all"` - tests full resolution

These tests are in `src/canonicalize/ClosureTransformer.zig:3408-3584`.

### Files Involved

- `src/canonicalize/ClosureTransformer.zig:987-1171` - Request creation and processing
- `src/compile/compile_build.zig` - Build pipeline (needs integration point)

### Fix

Add a call to process external requests after closure transformation completes. The `processExternalLambdaSetRequests` function takes a callback that looks up implementations from other modules:

```zig
// In compile_build.zig, after closure transformation for a module:
const result = closure_transformer.processExternalLambdaSetRequests(
    lookupExternalImpl,  // Callback to query other modules
    build_context,       // Context passed to callback
);

if (result.failed_count > 0) {
    // Report errors for unresolved external implementations
}
```

The callback needs to:
1. Given (source_module, ability_member, concrete_type_info)
2. Look up the implementation in the source module's exports
3. Return the resolved closure info

### Steps

1. **Find the right integration point** in `compile_build.zig` where all modules are available
2. **Implement the lookup callback** that queries other modules' exported closures
3. **Wire up the call** to `processExternalLambdaSetRequests()`
4. **Add error reporting** for failed resolutions
5. **Test** with a multi-module program using static dispatch across modules

```bash
git add -A && git commit -m "Wire up cross-module static dispatch resolution"
zig build minici
```

---

## Issue 2: Derived Methods for Structural Types

### The Problem

Only `is_eq` is implemented. There's no infrastructure for `hash` or other methods on structural types.

Current state in `Check.zig:5606`:
```zig
// Other methods are not supported on anonymous types
```

### What's Needed (If You Want More Derived Methods)

For each new method (e.g., `hash`):

1. **Add the ident** to `ModuleEnv.zig:101-115`:
   ```zig
   hash: Ident.Idx,
   ```

2. **Add constraint checking** in `Check.zig:5581-5614`:
   ```zig
   } else if (constraint.fn_name == self.cir.idents.hash) {
       if (try self.typeSupportsHash(dispatcher_var, env)) {
           // Unify return type with U64
       } else {
           try self.reportDoesNotSupportHash(dispatcher_var, env);
       }
   }
   ```

3. **Implement the type check** (similar to `typeSupportsIsEq`):
   ```zig
   fn typeSupportsHash(self: *Self, var_: types.Var, env: *Env) !bool {
       // Recursively check all fields/elements support hash
   }
   ```

4. **Add runtime implementation** in `interpreter.zig:15838-15855`:
   ```zig
   } else if (method_name == self.idents.hash) {
       return self.structuralHash(value, layout);
   }
   ```

### Note

This might not be necessary depending on how Roc handles `hash`. If `hash` is only defined on nominal types (via explicit implementations), then structural types wouldn't need derived `hash` - you'd just wrap them in a nominal type first.

**Check with the language design** before implementing this.

---

## What You DON'T Need

The following were in my original plan but are **not necessary** in the Cor architecture:

### Ambient Function Unification Algorithm
**Not needed.** This solved a problem in the Rust compiler where type variables weren't unified properly during type inference. In Cor, type checking completes before monomorphization, so all type variables are already resolved.

### Region-Based Ordering
**Not needed.** This was for resolving nested unspecialized lambda sets in the right order during type inference. In Cor, we're not doing this during type inference.

### uls_of_var Tracking
**Not needed in the same way.** We can just iterate through unspecialized closures during monomorphization since types are already concrete.

### Let-Generalization Fresh Variables
**Already handled by the type checker.** The Zig type checker handles instantiation properly.

### Recursive Lambda Set Handling
**Already fixed.** Recent commits fixed the issues with recursive lambda sets (issues #3444, #5026, #4725).

---

## Testing

After making changes, verify with:

```bash
zig build minici
```

For cross-module testing, create a test case with:
- Module A that defines a type and method
- Module B that imports A and calls the method on A's type
- Verify the correct implementation is resolved

---

## File Reference

| File | Purpose |
|------|---------|
| `src/canonicalize/ClosureTransformer.zig` | Closure transformation, external request tracking |
| `src/canonicalize/Monomorphizer.zig` | Type specialization |
| `src/check/Check.zig` | Type checking, constraint resolution |
| `src/canonicalize/ModuleEnv.zig` | Method ident definitions |
| `src/eval/interpreter.zig` | Runtime implementation |
| `src/compile/compile_build.zig` | Build pipeline |
