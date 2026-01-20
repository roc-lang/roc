# Proper Lambda/Closure Implementation Plan

## !!!! CRITICAL: NO SIMPLIFIED VERSIONS ALLOWED !!!!

**DO NOT DO SIMPLER APPROACHES THAN ANY OF THIS.**
**ALL OF THIS IS A STRICT REQUIREMENT.**
**NO SIMPLIFIED VERSIONS ALLOWED, PERIOD.**

If you are stuck, look at how it's done in `crates/compiler/mono/src/ir.rs` and `crates/compiler/mono/src/layout.rs`.

---

## Goal

Implement full closure support with:
- Proper closure environment representation (runtime closure values)
- Lambda set tracking and specialization
- First-class functions (closures as values, stored in data structures, passed as arguments)
- Cross-module support with globally unique identifiers
- Layout resolution for closures and captures

---

## Status: Phase 1 COMPLETED

**Test Results: 349 -> 350 passing** (fixed curried function test)

### What Was Implemented

1. **MonoIR.zig** - Added closure representation types:
   - `ClosureRepresentation` union with 5 variants (unwrapped_capture, struct_captures, enum_dispatch, union_repr, direct_call)
   - `LambdaSet`, `LambdaSetMember`, `LambdaSetMemberSpan` types
   - Updated `closure` expression to include `representation` field
   - `Recursive` enum (not_recursive, recursive, tail_recursive)
   - `SelfRecursive` union with JoinPointId

2. **MonoExprStore.zig** - Added lambda set member storage:
   - `lambda_set_members` array
   - `addLambdaSetMembers()` and `getLambdaSetMembers()` methods

3. **Lower.zig** - Added representation selection:
   - `selectClosureRepresentation()` - selects optimal representation based on capture count
   - Updated closure lowering to populate representation field
   - Recursion detection via `exprContainsPatternRef()`

4. **MonoExprCodeGen.zig** - Updated code generation:
   - `generateClosure()` - creates runtime values based on representation
   - `generateClosureCall()` - handles representation-based dispatch
   - `bindClosureCaptures()` helper
   - Fixed `generateChainedCall()` to handle closures at all nesting levels
   - Join point infrastructure for recursive closures
   - Higher-order function support (closures as arguments)

---

## Phase 2: Remaining Work

### Priority 1: Recursive Closures (Roc-style) - COMPLETED

### Priority 2: Closures Passed as Arguments - COMPLETED

### Priority 3: Lambda Set Dispatch (enum_dispatch, union_repr)

**Problem:** Multiple closures with same signature need runtime dispatch

```roc
choose = |cond| if cond |x| x + 1 else |x| x * 2
f = choose(True)
g = choose(False)
f(5)  // Should be 6
g(5)  // Should be 10
```

**REQUIRED Implementation (from Roc crates/):**

1. **LambdaSet must track ALL closures** - Not just the current one, but all possible closures at each call site

2. **Tag value computation** (from `construct_closure_data` in ir.rs):
   - EnumDispatch::Bool (2 closures): `tag = name != first_in_set` (true/false)
   - EnumDispatch::U8 (3-256 closures): `tag = position_in_set` (0-255)

3. **Switch dispatch at call sites** (from `enum_lambda_set_to_switch`):
   ```
   switch(closure_tag) {
       0 => call first_lambda(args)
       1 => call second_lambda(args)
       ...
   }
   ```

4. **ClosureCallOptions enum** for dispatch:
   - Void: Empty lambda set (error)
   - Union(UnionLayout): Multiple capturing functions
   - Struct(field_layouts): Single function with struct captures
   - UnwrappedCapture(layout): Single function capturing one value
   - EnumDispatch(Bool/U8): Multiple non-capturing functions

**Files to Reference:**
- `crates/compiler/mono/src/layout.rs:1434-1774` - LambdaSet, ClosureCallOptions
- `crates/compiler/mono/src/ir.rs` - construct_closure_data, enum_lambda_set_to_switch

### Priority 4: Boxed/Erased Closures

**Existing Infrastructure:**
- `Box(item)` already exists in `src/build/roc/Builtin.roc`

**Implementation:**
- Wire up `Box.box` for closures: allocate on heap, store function pointer + environment
- Wire up `Box.unbox` for closures: extract function pointer + environment
- Calling boxed closure: indirect call through function pointer after unboxing

### Priority 5: Stack vs Heap (Roc-style)

**Roc's Approach:**
- Depends on size/representation
- Small closures (unwrapped, small structs) stay on stack
- Large closures or those that escape may need heap allocation

### Priority 6: Multimorphic Lambdas (Roc-style deduplication)

**Roc's Approach (from layout.rs:1903-1950):**
- Detect when same function appears multiple times in lambda set
- Deduplicate specializations to avoid code bloat

### Priority 7: Empty Lambda Sets (Roc-style)

**Roc's Approach (from layout.rs:1974-1985):**
- Give empty representation to unbound lambda sets
- Handle void/unit cases gracefully

---

## Key Files Reference

- `crates/compiler/mono/src/layout.rs` - Roc's LambdaSet, ClosureCallOptions
- `crates/compiler/mono/src/ir.rs` - Roc's construct_closure_data, dispatch generation
- `src/canonicalize/ClosureTransformer.zig` - Has `recursion_closure` detection
- `src/canonicalize/LambdaSetInference.zig` - Global closure naming
- `src/mono/MonoIR.zig` - `ClosureRepresentation`, `LambdaSet` types
- `src/mono/Lower.zig` - `selectClosureRepresentation()`
- `src/backend/dev/MonoExprCodeGen.zig` - Code generation for closures
