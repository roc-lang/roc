# Monomorphization Implementation Plan

This document outlines the steps needed to complete the monomorphization and lambda set inference system in the Roc Zig compiler (`src/`). The goal is to achieve a fully feature-complete implementation using the Cor architecture (lambda sets resolved AFTER type checking, not during).

## Background and Architecture

### What is Monomorphization?

Monomorphization is the process of converting polymorphic (generic) code into specialized versions for each concrete type used at runtime. For example, a function `identity : a -> a` called with both `I64` and `Str` becomes two separate functions: `identity_I64` and `identity_Str`.

### What are Lambda Sets?

Lambda sets track which closures could possibly reach a given function variable. For example:

```roc
choose = \b ->
    if b then \x -> x + 1 else \x -> x * 2

result = (choose Bool.true) 5
```

Here, the lambda set for `choose`'s return value is `{\x -> x + 1, \x -> x * 2}` - both closures could potentially be called.

### The Three Implementations

1. **Rust (crates/)** - The old implementation. Lambda sets were tracked IN the type system during canonicalization/type-checking. This turned out to be architecturally incorrect.

2. **Cor (OCaml, ~/code/cor/)** - The correct architecture. Lambda sets are resolved AFTER type checking. Simpler and cleaner, but less feature-complete.

3. **Zig (src/)** - Our target. Uses the Cor architecture (commit `8e1ea38ee0` removed `lambda_set` from `Func` struct). Currently incomplete.

### Key Architectural Insight

The Cor approach separates concerns:
1. **Type Checking** - Standard Hindley-Milner with rank-based generalization
2. **Lambda Lifting** - Lifts nested functions to top level, tracking captures
3. **Closure Transformation** - Builds lambda sets and determines dispatch strategy
4. **Monomorphization** - Specializes polymorphic functions to concrete types

The Rust approach tried to do lambda sets during type checking, which created complexity and circular dependencies.

## Current State of the Codebase

### Files to Understand

- `src/canonicalize/Monomorphizer.zig` (182 lines) - **Very incomplete**. Only generates specialization names, no body duplication.
- `src/canonicalize/ClosureTransformer.zig` (1387+ lines) - Implements lambda set tracking. Has a known bug with type-driven dispatch.
- `src/canonicalize/LambdaLifter.zig` (750+ lines) - Lifts nested functions to top level.
- `src/types/generalize.zig` (532 lines) - Rank-based type generalization.
- `src/types/instantiate.zig` - Type instantiation (deep copy with fresh variables).
- `src/types/types.zig` - Core type definitions. `Func` struct has NO lambda_set field.

### What Works

- Lambda lifting (extracting nested functions)
- Basic closure transformation (building lambda sets)
- Type inference and generalization
- Basic specialization name generation

### What's Missing/Broken

- Full function body duplication during specialization
- Type store caching for specializations
- Recursive type specialization
- Type-driven dispatch for lambda sets (ClosureTransformer.zig:47-49)
- Polymorphic recursion support
- Real tests with proper Store setup

---

## Implementation Steps

### Pre-requisite: Ensure Build Passes

Before starting any work:

```bash
zig build minici
```

If this fails, fix any existing issues first.

---

## Phase 1: Fix Critical Bug in ClosureTransformer

### Step 1.1: Understand the Bug

**File:** `src/canonicalize/ClosureTransformer.zig`
**Lines:** 47-49

The bug comment states: "BUG: Type-driven dispatch using lambda sets from the type system is incomplete"

This means when we have a lambda set like `{closure1, closure2}`, the code for dispatching to the correct closure based on runtime tag is not fully working.

**What to do:**

1. Read through `ClosureTransformer.zig` completely
2. Find all usages of `LambdaSet` and `ClosureInfo`
3. Trace the code path from lambda set creation to dispatch generation
4. Identify where type information is not being properly propagated

**Reference:** The Rust implementation in `crates/compiler/mono/src/layout.rs` (lines 1434-1444) shows how lambda sets should be represented:

```rust
pub struct LambdaSet<'a> {
    pub(crate) args: &'a &'a [InLayout<'a>],
    pub(crate) ret: InLayout<'a>,
    pub(crate) set: &'a &'a [(Symbol, &'a [InLayout<'a>])],
    pub(crate) representation: InLayout<'a>,
    pub(crate) full_layout: InLayout<'a>,
}
```

The Zig `LambdaSet` needs to track:
- The argument types
- The return type
- The set of (closure_symbol, capture_types) pairs
- A representation (how to store the closure at runtime)
- The full layout for memory allocation

### Step 1.2: Implement Closure Representation Strategies

**Reference:** `crates/compiler/mono/src/layout.rs` (ClosureRepresentation enum)

The Rust implementation has four strategies:

1. **Union** - Multiple closures with different captures, stored as tagged union
2. **AlphabeticOrderStruct** - Single closure with captures, stored as struct
3. **UnwrappedCapture** - Single closure with single capture, stored directly
4. **EnumDispatch** - Multiple closures with NO captures, stored as enum tag

Add these to `ClosureTransformer.zig`:

```zig
pub const ClosureRepresentation = union(enum) {
    /// Multiple closures with different captures - tagged union
    tagged_union: TaggedUnionInfo,
    /// Single closure with captures - struct
    capture_struct: CaptureStructInfo,
    /// Single closure, single capture - store directly
    unwrapped_capture: UnwrappedCaptureInfo,
    /// Multiple closures, no captures - enum tag only
    enum_dispatch: EnumDispatchInfo,
};
```

### Step 1.3: Fix Dispatch Generation

The dispatch code needs to:

1. Check the representation type
2. For `tagged_union`: Generate switch on tag, call appropriate closure
3. For `capture_struct`: Call the single closure with captures
4. For `unwrapped_capture`: Call with the single unwrapped capture
5. For `enum_dispatch`: Generate switch on enum tag

After completing this step:

```bash
zig build minici
git add -A && git commit -m "Fix type-driven lambda set dispatch in ClosureTransformer"
```

---

## Phase 2: Complete Monomorphizer - Function Body Duplication

### Step 2.1: Understand Current State

**File:** `src/canonicalize/Monomorphizer.zig`

Currently the monomorphizer only:
- Tracks specialization keys (original_ident + type_hash)
- Generates specialized names

It does NOT:
- Duplicate function bodies
- Substitute types in duplicated bodies
- Handle recursive functions
- Cache specializations properly

### Step 2.2: Add PartialProc and Specialized Tracking

**Reference:** `crates/compiler/mono/src/ir.rs` (lines 913-927)

The Rust implementation tracks:
- `partial_procs` - Functions waiting to be specialized
- `pending_specializations` - Specializations discovered but not yet made
- `specialized` - Completed specializations

Add similar structures to `Monomorphizer.zig`:

```zig
/// A function that hasn't been specialized yet
pub const PartialProc = struct {
    /// The original function identifier
    original_ident: base.Ident.Idx,
    /// The function's CIR expression index
    body_expr: CIR.Expr.Idx,
    /// The function's type (polymorphic)
    type_var: types.Var,
    /// Captured variables (if closure)
    captures: []const CapturedVar,
};

/// Map from original ident to partial proc
partial_procs: std.AutoHashMap(base.Ident.Idx, PartialProc),

/// Specializations that need to be made
pending: std.ArrayList(PendingSpecialization),

/// Completed specializations: (original, type_hash) -> specialized proc
specialized: std.AutoHashMap(SpecializationKey, SpecializedProc),
```

### Step 2.3: Implement Two-Phase Specialization

**Reference:** `crates/compiler/mono/src/ir.rs` (lines 627-648)

The Rust implementation uses two phases:

1. **Finding Phase** - Walk the code, discover all needed specializations
2. **Making Phase** - Actually duplicate and specialize function bodies

This separation prevents infinite loops with recursive types.

Add a phase enum:

```zig
pub const Phase = enum {
    /// Discovering what specializations are needed
    finding,
    /// Actually creating specialized functions
    making,
};
```

### Step 2.4: Implement Body Duplication

Create a function that duplicates a function body with type substitution:

```zig
/// Duplicate a function body, substituting polymorphic types with concrete ones
pub fn duplicateBody(
    self: *Self,
    original_body: CIR.Expr.Idx,
    type_substitutions: *const std.AutoHashMap(types.Var, types.Var),
) !CIR.Expr.Idx {
    // Walk the CIR expression tree
    // For each expression:
    //   1. Clone the expression
    //   2. If it has a type annotation, substitute using type_substitutions
    //   3. If it's a call to a polymorphic function, queue that for specialization
    // Return the new expression index
}
```

**Key insight:** When you encounter a call to another polymorphic function during duplication, don't recurse immediately. Instead, add it to `pending` and continue. This is how the two-phase approach prevents stack overflow.

### Step 2.5: Handle Recursive Types

**Reference:** `crates/compiler/mono/src/ir.rs` (Suspended specialization stack)

For recursive types like:

```roc
LinkedList a : [Nil, Cons a (LinkedList a)]
```

The specialization of `LinkedList I64` references itself. Handle this by:

1. Before making a specialization, check if it's already in progress
2. If so, use a forward reference (placeholder)
3. After completing, patch the forward references

Add a "suspended" stack:

```zig
/// Specializations currently being made (to detect recursion)
in_progress: std.AutoHashMap(SpecializationKey, void),
```

After completing this phase:

```bash
zig build minici
git add -A && git commit -m "Implement function body duplication in Monomorphizer"
```

---

## Phase 3: Type Store Integration

### Step 3.1: Proper Type Caching

**File:** `src/types/store.zig`

Currently the monomorphizer uses `typeHash()` which is a simple hash. This needs to be more robust:

1. Two structurally equivalent types should have the same hash
2. Different types should (almost always) have different hashes
3. The hash should be stable across compilations

Implement a proper structural hash:

```zig
pub fn structuralTypeHash(self: *Self, type_var: types.Var) u64 {
    var hasher = std.hash.Wyhash.init(0);
    self.hashTypeRecursive(&hasher, type_var, &std.AutoHashMap(types.Var, void).init(self.allocator));
    return hasher.final();
}

fn hashTypeRecursive(
    self: *Self,
    hasher: *std.hash.Wyhash,
    type_var: types.Var,
    seen: *std.AutoHashMap(types.Var, void),
) void {
    // Resolve the variable
    const resolved = self.types_store.resolveVar(type_var);

    // Check for cycles
    if (seen.contains(type_var)) {
        hasher.update("CYCLE");
        return;
    }
    seen.put(type_var, {}) catch return;

    // Hash based on content
    switch (resolved.desc.content) {
        .structure => |flat_type| {
            // Hash the structure tag
            hasher.update(std.mem.asBytes(&@as(u8, @intFromEnum(flat_type))));
            // Recursively hash children
            // ...
        },
        // ... handle other cases
    }
}
```

### Step 3.2: Instantiation for Specialization

**Reference:** `crates/compiler/types/src/subs.rs` (deep_copy functions)

When specializing, we need to create a fresh copy of the type with concrete substitutions. The `src/types/instantiate.zig` file has some of this, but verify it handles:

1. All type constructors (functions, records, tags, etc.)
2. Recursive types (with proper cycle detection)
3. Type aliases (expand or preserve based on context)

After completing this phase:

```bash
zig build minici
git add -A && git commit -m "Add proper type caching and instantiation for specialization"
```

---

## Phase 4: Polymorphic Recursion Support

### Step 4.1: Understand Polymorphic Recursion

Polymorphic recursion is when a function calls itself with different type arguments:

```roc
nested : I64, a -> I64
nested = \n, x ->
    if n == 0 then
        0
    else
        nested (n - 1) [x]  # Called with List a, not a!
```

This is problematic because it can require infinite specializations:
- `nested` specialized for `a`
- `nested` specialized for `List a`
- `nested` specialized for `List (List a)`
- ...

### Step 4.2: Detect Polymorphic Recursion

Add detection during the finding phase:

```zig
fn detectPolymorphicRecursion(
    self: *Self,
    original: base.Ident.Idx,
    call_site_type: types.Var,
    definition_type: types.Var,
) bool {
    // If the call site type is "bigger" than the definition type
    // in a way that involves the recursive call, it's polymorphic recursion
    // ...
}
```

### Step 4.3: Handle with Bounded Specialization

Two approaches:

1. **Error on detection** - The simplest approach. Emit a compile error when polymorphic recursion is detected.

2. **Bounded specialization** - Allow up to N levels of nesting, then use a boxed/dynamic representation.

For now, implement approach 1 (error on detection) with a clear error message:

```zig
if (self.detectPolymorphicRecursion(original, call_type, def_type)) {
    return error.PolymorphicRecursionNotSupported;
}
```

After completing this phase:

```bash
zig build minici
git add -A && git commit -m "Add polymorphic recursion detection"
```

---

## Phase 5: Closure Capture Layout

### Step 5.1: Fix Refcounted Capture Verification

**File:** `src/eval/interpreter.zig`
**Line:** 136

There's a TODO about closure capture layout verification for refcounted data. The issue is ensuring captures that need reference counting are properly tracked.

Captures can be:
- Primitives (no refcounting needed)
- Heap-allocated (need refcounting)
- Closures themselves (may need refcounting)

### Step 5.2: Add Capture Layout Tracking

In `ClosureTransformer.zig`, ensure each capture tracks:

```zig
pub const CapturedVar = struct {
    /// The captured variable
    ident: base.Ident.Idx,
    /// The type of the capture
    type_var: types.Var,
    /// Whether this capture needs reference counting
    needs_refcount: bool,
    /// The offset in the closure struct
    offset: u32,
};
```

After completing this phase:

```bash
zig build minici
git add -A && git commit -m "Fix closure capture layout for refcounted data"
```

---

## Phase 6: Testing Infrastructure

### Step 6.1: Write Real Monomorphizer Tests

**File:** `src/canonicalize/Monomorphizer.zig` (lines 154-181)

Current tests are stubs. Create proper tests:

```zig
test "monomorphizer: specialize identity function" {
    const allocator = testing.allocator;

    // Set up a real ModuleEnv
    var module_env = try ModuleEnv.init(allocator, "test");
    defer module_env.deinit();

    // Set up a real types.Store
    var types_store = types.Store.init(allocator);
    defer types_store.deinit();

    // Create a polymorphic identity function: a -> a
    const a = types_store.freshFlexVar();
    const identity_type = types_store.createFunc(&[_]types.Var{a}, a);

    // Create monomorphizer
    var mono = Monomorphizer.init(allocator, &module_env, &types_store);
    defer mono.deinit();

    // Specialize for I64
    const i64_type = types_store.getBuiltinType(.i64);
    const specialized = try mono.specialize(identity_ident, i64_type);

    // Verify specialization was created
    try testing.expect(specialized != identity_ident);
}
```

### Step 6.2: Add Integration Tests

Create tests that exercise the full pipeline:

1. Parse Roc code with polymorphic functions
2. Type check
3. Lambda lift
4. Closure transform
5. Monomorphize
6. Verify correct specialized functions exist

After completing this phase:

```bash
zig build minici
git add -A && git commit -m "Add real tests for Monomorphizer"
```

---

## Phase 7: Cross-Module Specialization

### Step 7.1: Understand the Problem

When module A imports a polymorphic function from module B, and A uses it with a concrete type, the specialization needs to happen in B (where the body is), but is triggered by A.

### Step 7.2: Track External Specializations

**Reference:** `crates/compiler/mono/src/ir.rs` (ExternalSpecializations)

Add tracking for specializations needed from other modules:

```zig
/// Specializations this module needs from other modules
external_specializations: std.AutoHashMap(
    ModuleId,
    std.ArrayList(ExternalSpecialization),
),

pub const ExternalSpecialization = struct {
    /// The function to specialize
    symbol: Symbol,
    /// The concrete type to specialize for
    type_var: types.Var,
    /// Where this specialization is needed (for error reporting)
    call_site: SourceLocation,
};
```

### Step 7.3: Implement Cross-Module Protocol

1. During monomorphization, when encountering a call to an external polymorphic function, record it in `external_specializations`
2. After all modules are processed, collect all external specializations
3. For each module, add the specializations it needs to provide
4. Re-run monomorphization for those modules

After completing this phase:

```bash
zig build minici
git add -A && git commit -m "Implement cross-module specialization"
```

---

## Phase 8: Tag and Union Handling

### Step 8.1: Resolve Tag Deduplication

**File:** `src/layout/store.zig`
**Lines:** 601-623

There's uncertainty about tag deduplication. When two tags have the same payload types, should they share layout?

Example:
```roc
Result ok err : [Ok ok, Err err]
Maybe a : [Just a, Nothing]
```

`Ok I64` and `Just I64` have the same payload. Should they share layout?

**Decision:** Generally NO, keep them separate for clarity. Only deduplicate within the same type.

### Step 8.2: Optimize Tag Sorting

**File:** `src/layout/store.zig`
**Lines:** 1344-1345

Tag sorting is currently done in two places. Consolidate to one location for consistency.

After completing this phase:

```bash
zig build minici
git add -A && git commit -m "Clean up tag deduplication and sorting"
```

---

## Phase 9: Debug Infrastructure

### Step 9.1: Implement testRocDbg

**Files:**
- `src/eval/test_runner.zig:75`
- `src/repl/repl_test_env.zig:136`
- `src/eval/test/TestEnv.zig:148`

The `testRocDbg` function is missing. This is needed for testing debug output in the REPL and test runner.

### Step 9.2: Implement snapshotRocDbg

**File:** `src/snapshot_tool/main.zig:4080`

Similar to `testRocDbg`, implement snapshot functionality for debug output.

After completing this phase:

```bash
zig build minici
git add -A && git commit -m "Implement testRocDbg and snapshotRocDbg"
```

---

## Phase 10: Optimization and Polish

### Step 10.1: Hardcode Built-in Idents

**File:** `src/types/store.zig`
**Lines:** 238, 255

Built-in identifiers like `I64`, `Str`, `List`, etc. are currently allocated. Hardcode them to reduce allocations.

### Step 10.2: Add Store Capacity Heuristics

**File:** `src/types/store.zig`
**Line:** 97

Add heuristics for initial store capacity based on source file size or other metrics.

### Step 10.3: Remove Dead Parameters

**File:** `src/canonicalize/Can.zig`
**Line:** 1381

Remove the dead function parameter in `registerIdentifierHierarchically`.

### Step 10.4: Verify Boolean Layout Optimization

**File:** `src/layout/store.zig`
**Line:** 1287

Verify that the Boolean layout optimization is actually necessary and working correctly.

After completing this phase:

```bash
zig build minici
git add -A && git commit -m "Optimization and polish pass"
```

---

## Phase 11: Documentation

### Step 11.1: Document Architecture

Create documentation explaining:

1. Why Cor-style defunctionalization was chosen
2. The separation between type checking and lambda set resolution
3. The two-phase specialization approach
4. Trade-offs compared to the Rust implementation

### Step 11.2: Add Code Comments

Add comments to key functions explaining:

1. What they do
2. Why they exist
3. How they fit into the overall pipeline

After completing this phase:

```bash
zig build minici
git add -A && git commit -m "Add architecture documentation"
```

---

## Final Checklist

Before considering the implementation complete:

- [ ] All tests pass (`zig build minici`)
- [ ] No compiler warnings
- [ ] Cross-module specialization works
- [ ] Polymorphic recursion is detected and reported
- [ ] Lambda set dispatch is correct for all representation types
- [ ] Closure captures handle refcounting correctly
- [ ] Debug infrastructure works (testRocDbg, snapshotRocDbg)
- [ ] Documentation is complete

---

## Appendix: Key Data Structures Reference

### From Rust (for reference)

```rust
// LambdaSet - crates/compiler/mono/src/layout.rs:1434
pub struct LambdaSet<'a> {
    pub(crate) args: &'a &'a [InLayout<'a>],
    pub(crate) ret: InLayout<'a>,
    pub(crate) set: &'a &'a [(Symbol, &'a [InLayout<'a>])],
    pub(crate) representation: InLayout<'a>,
    pub(crate) full_layout: InLayout<'a>,
}

// Procs - crates/compiler/mono/src/ir.rs:913
pub struct Procs<'a> {
    pub partial_procs: PartialProcs<'a>,
    ability_member_aliases: AbilityAliases,
    pending_specializations: PendingSpecializations<'a>,
    specialized: Specialized<'a>,
    host_exposed_lambda_sets: HostExposedLambdaSets<'a>,
}

// ClosureRepresentation - crates/compiler/mono/src/layout.rs
pub enum ClosureRepresentation<'a> {
    Union { ... },
    AlphabeticOrderStruct(&'a [(Symbol, InLayout<'a>)]),
    UnwrappedCapture(InLayout<'a>),
    EnumDispatch(TagIdIntType),
}
```

### From Cor (for reference)

```ocaml
(* type.ml - Lambda sets in type system *)
and ty_content =
  | TFn of loc_tvar * tvar * loc_tvar  (* arg, lambda_set, ret *)
  | TLambdaSet of ty_lset

and ty_lambda = { lambda : symbol; captures : tvar list }
and ty_lset = { lambdas : ty_lambda list; ambient_fn : tvar }
```

### Current Zig (to be extended)

```zig
// Monomorphizer.zig - Current state
pub const SpecializationKey = struct {
    original_ident: base.Ident.Idx,
    type_hash: u64,
};

// ClosureTransformer.zig - Current state
pub const LambdaSet = struct {
    closures: std.ArrayList(ClosureInfo),
};
```
