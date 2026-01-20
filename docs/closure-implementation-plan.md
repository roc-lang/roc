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

### Priority 3: Lambda Set Dispatch (enum_dispatch, union_repr) - COMPLETED

**Problem:** Multiple closures with same signature need runtime dispatch

```roc
choose = |cond| if cond |x| x + 1 else |x| x * 2
f = choose(True)
g = choose(False)
f(5)  // Should be 6
g(5)  // Should be 10
```

**Implementation:**
- Added `collectIfClosureLambdaSet` to detect closure branches in if-then-else
- Added `lowerIfBranchesWithLambdaSet` to collect lambda set members
- Added `generateEnumDispatchCall` with Bool dispatch (2 fns) and U8 dispatch (3+ fns)
- Added `generateBoolDispatchCall` using conditional branch on tag
- Added `generateU8DispatchCall` using cascading comparisons

### Priority 4: Stack vs Heap - NOT NEEDED

**Finding:** The dev backend inlines closure calls at call sites (evaluates lambda body
directly with captures bound from symbol_locations). Closures never need to be passed
around as standalone values, so stack vs heap allocation decisions don't apply.

**Roc's approach:** 4 machine words threshold (32 bytes on 64-bit), but this is for
passing closures by reference vs by value - not heap allocation.

### Priority 5: Multimorphic Lambdas - ALREADY HANDLED

**Finding:** Our approach of creating separate LambdaSetMember entries per branch
handles multimorphic lambdas correctly. Each entry has its own `lambda_body` and
`captures`, so dispatch works even when the same function appears with different
capture layouts.

**Roc's deduplication** is for catching edge cases where unification creates subtle
duplicates - not needed for our current lowering approach.

### Priority 6: Empty Lambda Sets - ALREADY HANDLED

**Finding:** Already handled in `generateEnumDispatchCall`:
```zig
if (members.len == 0) {
    return Error.UnsupportedExpression;
}
```

This returns an error when dispatching on empty lambda sets, matching Roc's approach
of returning a runtime error for void lambda sets.

### Priority 7: Boxed/Erased Closures

**Existing Infrastructure:**
- `Box(item)` already exists in `src/build/roc/Builtin.roc`

**Implementation:**
- Wire up `Box.box` for closures: allocate on heap, store function pointer + environment
- Wire up `Box.unbox` for closures: extract function pointer + environment
- Calling boxed closure: indirect call through function pointer after unboxing

**Note:** This requires significant infrastructure work:
- Lower.zig needs to detect Box method calls and emit `low_level` expressions
- MonoExprCodeGen.zig needs to handle `low_level` expressions with `box_box`/`box_unbox`
- Heap allocation support in the dev backend

---

## Key Files Reference

- `crates/compiler/mono/src/layout.rs` - Roc's LambdaSet, ClosureCallOptions
- `crates/compiler/mono/src/ir.rs` - Roc's construct_closure_data, dispatch generation
- `src/canonicalize/ClosureTransformer.zig` - Has `recursion_closure` detection
- `src/canonicalize/LambdaSetInference.zig` - Global closure naming
- `src/mono/MonoIR.zig` - `ClosureRepresentation`, `LambdaSet` types
- `src/mono/Lower.zig` - `selectClosureRepresentation()`
- `src/backend/dev/MonoExprCodeGen.zig` - Code generation for closures
