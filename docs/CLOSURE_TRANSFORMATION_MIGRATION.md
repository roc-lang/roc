# Closure Transformation Migration Plan

> **Note:** This document is for internal planning purposes only and should NOT be committed to source control. Please keep it local or delete it once the migration is complete.

## Overview

This document describes how to migrate Roc5's closure transformation system from its current single-pass imperative approach to Cor's multi-stage type-driven approach. The goal is to have lambda sets embedded in the type system, with explicit lambda lifting and type-driven dispatch generation.

**Important:** As you complete each phase, please:
1. Make commits with clear messages describing what was changed
2. Update this document's "Progress" section at the bottom with what's been completed
3. Run `zig build snapshot` to verify tests pass before moving to the next phase

---

## Current State (Roc5)

### Files Involved

- **`src/canonicalize/ClosureTransformer.zig`** - The main transformation logic
- **`src/canonicalize/CIR.zig`** - Canonical IR definitions (expressions, patterns, etc.)
- **`src/check/Check.zig`** - Type checking/inference
- **`src/types/mod.zig`** - Type system definitions
- **`src/snapshot_tool/main.zig`** - Runs transformations for snapshot tests
- **`test/snapshots/mono_*.md`** - Snapshot tests for mono (monomorphization) output

### How It Currently Works

The current approach uses a **single-pass transformation** with parallel tracking maps:

```zig
// In ClosureTransformer.zig
pub const ClosureInfo = struct {
    tag_name: base.Ident.Idx,           // e.g., "Closure_addX_1"
    lambda_body: Expr.Idx,              // The lambda's body expression
    lambda_args: CIR.Pattern.Span,      // The lambda's parameters
    capture_names: std.ArrayList(base.Ident.Idx),  // Captured variable names
};

pub const LambdaSet = struct {
    closures: std.ArrayList(ClosureInfo),  // All closures that can reach this point
};

// Multiple tracking maps during transformation:
closures: std.AutoHashMap(Expr.Idx, ClosureInfo),
pattern_lambda_sets: std.AutoHashMap(CIR.Pattern.Idx, LambdaSet),
lambda_return_sets: std.AutoHashMap(Expr.Idx, LambdaSet),
pattern_lambda_return_sets: std.AutoHashMap(CIR.Pattern.Idx, LambdaSet),
```

**Key characteristics:**
1. Lambda sets are tracked **separately** from the type system
2. Transformation happens in a **single pass** with recursive descent
3. Dispatch matches are generated **inline** at call sites
4. Pure lambdas mixed with closures require **explicit conversion** to closure tags

---

## Target State (Cor-style)

### Architecture Overview

The Cor approach uses a **multi-stage pipeline**:

1. **Type Inference with Lambda Sets** - Lambda sets become part of function types
2. **Lambda Lifting** - Closures are extracted to top-level function definitions
3. **Specialization & Dispatch** - Generate monomorphic versions and dispatch code

### Key Concepts

#### 1. Lambda Sets in Types

In Cor, every function type has an embedded lambda set:

```
FunctionType = { input: Type, lambda_set: LambdaSet, output: Type }
```

For example, the type of `|x| x + captured_y` would be:
```
Int -[ Closure_1({ y: Int }) ]-> Int
```

The `[ Closure_1({ y: Int }) ]` is the lambda set, indicating this function could be the closure `Closure_1` with capture `y: Int`.

#### 2. Lambda Lifting

Instead of inlining lambda bodies at dispatch sites, closures become top-level functions:

```roc
# Before lifting:
f = |x| x + y   # y is captured

# After lifting:
fn closure_f_1(x: Int, captures: { y: Int }) -> Int {
    x + captures.y
}
f = Closure_f_1({ y: y })
```

#### 3. Type-Driven Dispatch

When calling a function with a lambda set type, dispatch is generated from the type:

```roc
# f has type: Int -[ Closure_1({y: Int}), Closure_2({}) ]-> Int
result = f(10)

# Becomes:
result = match f {
    Closure_1(captures) => closure_1_impl(10, captures)
    Closure_2(captures) => closure_2_impl(10, captures)
}
```

---

## Migration Phases

### Phase 1: Add Lambda Set to Type System

**Goal:** Extend the type system to include lambda sets in function types, without changing transformation logic yet.

**Files to modify:**
- `src/types/mod.zig` - Add lambda set representation to types

#### Step 1.1: Define Lambda Set Types

In `src/types/mod.zig`, add a representation for lambda sets:

```zig
// A lambda set is a collection of possible closures
pub const LambdaSetEntry = struct {
    /// The closure tag name (e.g., "Closure_f_1")
    tag_name: Ident.Idx,
    /// The capture record type (or null for empty captures)
    captures_type: ?Type.Idx,
};

pub const LambdaSet = struct {
    /// All closures in this lambda set
    entries: std.ArrayList(LambdaSetEntry),

    pub fn isEmpty(self: *const LambdaSet) bool {
        return self.entries.items.len == 0;
    }

    pub fn merge(self: *LambdaSet, allocator: Allocator, other: *const LambdaSet) !void {
        for (other.entries.items) |entry| {
            try self.entries.append(allocator, entry);
        }
    }
};
```

#### Step 1.2: Extend Function Type

Currently, function types in the type system don't include lambda sets. Find where function types are defined and extend them.

Look in `src/types/mod.zig` for the `Type` union and find the function type variant. You'll need to add a lambda set field:

```zig
// Before (approximate - find actual structure):
.function => |f| {
    .arg_type = ...,
    .return_type = ...,
}

// After:
.function => |f| {
    .arg_type = ...,
    .lambda_set = ...,  // New field
    .return_type = ...,
}
```

#### Step 1.3: Update Type Unification

In `src/check/Check.zig`, find where function types are unified and update to handle lambda sets:

```zig
// When unifying two function types:
// 1. Unify arg types
// 2. Unify lambda sets (merge them)
// 3. Unify return types
```

For lambda set unification, use **set union** - combine all closures from both sides.

#### Step 1.4: Verify Tests Pass

Run `zig build snapshot` - all tests should still pass since we haven't changed transformation logic yet.

**Commit point:** "Add lambda set representation to type system"

---

### Phase 2: Populate Lambda Sets During Canonicalization

**Goal:** When creating closure expressions during canonicalization, also populate the lambda set in the closure's type.

**Files to modify:**
- `src/canonicalize/Can.zig` - Update closure creation to set lambda set types

#### Step 2.1: Find Closure Creation

In `src/canonicalize/Can.zig`, find where `e_closure` expressions are created (around line 4943):

```zig
const closure_expr = Expr{
    .e_closure = .{
        .lambda_idx = lambda_idx,
        .captures = capture_info,
    },
};
```

#### Step 2.2: Create Lambda Set Entry

When creating a closure, also create a lambda set entry for the type:

```zig
// Generate a unique tag name for this closure
const tag_name = try generateClosureTagName(self, hint);

// Build the captures record type from the captured variables
const captures_type = try buildCapturesRecordType(self, captures_slice);

// Create lambda set with this single closure
const lambda_set = try createSingletonLambdaSet(self, tag_name, captures_type);

// Set this lambda set in the closure's function type
try setLambdaSetInType(self, closure_type, lambda_set);
```

#### Step 2.3: Handle Pure Lambdas

Pure lambdas (no captures) should have an empty lambda set, or a lambda set with a single entry that has no captures:

```zig
.e_lambda => {
    // Pure lambda - create lambda set entry with empty captures
    const lambda_set = try createSingletonLambdaSet(self, tag_name, null);
    // ...
}
```

#### Step 2.4: Merge Lambda Sets for If Expressions

When an if expression has function-typed branches, the result type's lambda set should be the union of all branch lambda sets:

```zig
// In canonicalization of if expressions:
if (branch_returns_function) {
    result_lambda_set = mergeLambdaSets(branch_lambda_sets);
}
```

#### Step 2.5: Verify Tests Pass

Run `zig build snapshot` - tests should pass. The MONO output won't change yet, but the type system now tracks lambda sets.

**Commit point:** "Populate lambda sets during canonicalization"

---

### Phase 3: Create Lambda Lifting Infrastructure

**Goal:** Set up infrastructure to lift closures to top-level function definitions.

**Files to create/modify:**
- `src/canonicalize/LambdaLifter.zig` (new file) - Lambda lifting logic
- `src/canonicalize/mod.zig` - Export the new module

#### Step 3.1: Create LambdaLifter Module

Create `src/canonicalize/LambdaLifter.zig`:

```zig
//! Lambda Lifter
//!
//! Converts inline closures to top-level function definitions.
//! Each closure becomes a function that takes its captures as an extra parameter.
//!
//! ## Example
//!
//! Before:
//! ```roc
//! make_adder = |y| |z| x + y + z
//! ```
//!
//! After:
//! ```roc
//! fn closure_1(z: Dec, captures: { y: Dec }) -> Dec {
//!     x + captures.y + z
//! }
//!
//! make_adder = |y| Closure_1({ y: y })
//! ```

const std = @import("std");
const base = @import("base");
const CIR = @import("CIR.zig");
const ModuleEnv = @import("ModuleEnv.zig");

const Self = @This();

/// A lifted function definition
pub const LiftedFunction = struct {
    /// The function name (matches the closure tag name)
    name: base.Ident.Idx,
    /// The original lambda arguments
    args: CIR.Pattern.Span,
    /// The captures parameter pattern (record destructure)
    captures_pattern: ?CIR.Pattern.Idx,
    /// The transformed function body
    body: CIR.Expr.Idx,
    /// The function type
    fn_type: Type.Idx,
};

allocator: std.mem.Allocator,
module_env: *ModuleEnv,

/// Lifted functions to add to the module
lifted_functions: std.ArrayList(LiftedFunction),

/// Map from original closure expression to its lifted function name
closure_to_function: std.AutoHashMap(CIR.Expr.Idx, base.Ident.Idx),

pub fn init(allocator: std.mem.Allocator, module_env: *ModuleEnv) Self {
    return .{
        .allocator = allocator,
        .module_env = module_env,
        .lifted_functions = std.ArrayList(LiftedFunction).empty,
        .closure_to_function = std.AutoHashMap(CIR.Expr.Idx, base.Ident.Idx).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.lifted_functions.deinit(self.allocator);
    self.closure_to_function.deinit();
}

/// Lift a closure to a top-level function
pub fn liftClosure(
    self: *Self,
    closure_idx: CIR.Expr.Idx,
    tag_name: base.Ident.Idx,
) !void {
    // TODO: Implement in Phase 4
}

/// Get all lifted functions
pub fn getLiftedFunctions(self: *const Self) []const LiftedFunction {
    return self.lifted_functions.items;
}
```

#### Step 3.2: Export from mod.zig

In `src/canonicalize/mod.zig`, add:

```zig
pub const LambdaLifter = @import("LambdaLifter.zig");
```

#### Step 3.3: Verify Tests Pass

Run `zig build snapshot` - tests should pass (we haven't used the lifter yet).

**Commit point:** "Add LambdaLifter infrastructure"

---

### Phase 4: Implement Lambda Lifting

**Goal:** Actually lift closures to top-level functions during transformation.

**Files to modify:**
- `src/canonicalize/LambdaLifter.zig` - Implement lifting logic
- `src/snapshot_tool/main.zig` - Integrate lifting into the transformation pipeline

#### Step 4.1: Implement liftClosure

```zig
pub fn liftClosure(
    self: *Self,
    closure_idx: CIR.Expr.Idx,
    tag_name: base.Ident.Idx,
) !void {
    const closure_expr = self.module_env.store.getExpr(closure_idx);
    const closure = closure_expr.e_closure;

    const lambda_expr = self.module_env.store.getExpr(closure.lambda_idx);
    const lambda = lambda_expr.e_lambda;

    // Get captures
    const captures = self.module_env.store.sliceCaptures(closure.captures);

    // Build captures parameter pattern (record destructure)
    const captures_pattern = if (captures.len > 0) blk: {
        // Create { name1, name2, ... } pattern
        const pattern = try self.buildCapturesPattern(captures);
        break :blk pattern;
    } else null;

    // Transform the lambda body
    // Replace captures with lookups into the captures parameter
    const transformed_body = try self.transformBodyWithCaptures(
        lambda.body,
        captures,
        captures_pattern,
    );

    // Create the lifted function
    const lifted = LiftedFunction{
        .name = tag_name,
        .args = lambda.args,
        .captures_pattern = captures_pattern,
        .body = transformed_body,
        .fn_type = try self.buildFunctionType(lambda, captures),
    };

    try self.lifted_functions.append(self.allocator, lifted);
    try self.closure_to_function.put(closure_idx, tag_name);
}

fn buildCapturesPattern(self: *Self, captures: []const CIR.Expr.Capture.Idx) !CIR.Pattern.Idx {
    // Build a record destructure pattern for the captures
    const destruct_start = self.module_env.store.scratchRecordDestructTop();

    for (captures) |capture_idx| {
        const capture = self.module_env.store.getCapture(capture_idx);

        const assign_pattern = try self.module_env.store.addPattern(
            Pattern{ .assign = .{ .ident = capture.name } },
            base.Region.zero(),
        );

        const destruct = Pattern.RecordDestruct{
            .label = capture.name,
            .ident = capture.name,
            .kind = .{ .Required = assign_pattern },
        };
        const destruct_idx = try self.module_env.store.addRecordDestruct(destruct, base.Region.zero());
        try self.module_env.store.addScratchRecordDestruct(destruct_idx);
    }

    const destructs_span = try self.module_env.store.recordDestructSpanFrom(destruct_start);

    return try self.module_env.store.addPattern(
        Pattern{ .record_destructure = .{ .destructs = destructs_span } },
        base.Region.zero(),
    );
}

fn transformBodyWithCaptures(
    self: *Self,
    body: CIR.Expr.Idx,
    captures: []const CIR.Expr.Capture.Idx,
    captures_pattern: ?CIR.Pattern.Idx,
) !CIR.Expr.Idx {
    // Walk the body and replace lookups to captured variables
    // with lookups into the captures record
    // This is a recursive transformation
    // TODO: Implement
    return body; // Placeholder
}
```

#### Step 4.2: Integrate into Snapshot Tool

In `src/snapshot_tool/main.zig`, after canonicalization but before the current closure transformation:

```zig
// Lambda lifting pass
var lifter = LambdaLifter.init(allocator, can_ir);
defer lifter.deinit();

// First pass: identify and lift all closures
// (You'll need to walk the CIR and call liftClosure for each e_closure)

// Add lifted functions to the module's definitions
for (lifter.getLiftedFunctions()) |lifted_fn| {
    // Add as a new top-level definition
}
```

#### Step 4.3: Update MONO Output Generation

The lifted functions should appear in the MONO output. Update `generateMonoSection` to include them.

#### Step 4.4: Verify Tests Pass

Run `zig build snapshot` - the MONO output will now show lifted functions. You may need to update expected snapshots.

**Commit point:** "Implement lambda lifting"

---

### Phase 5: Type-Driven Dispatch Generation

**Goal:** Generate dispatch matches from the lambda set in the function's type, not from runtime tracking maps.

**Files to modify:**
- `src/canonicalize/ClosureTransformer.zig` - Rewrite dispatch generation
- Remove or simplify the tracking maps

#### Step 5.1: Change Dispatch to Use Types

Currently, dispatch is generated by looking up `pattern_lambda_sets`:

```zig
// Current approach:
if (self.pattern_lambda_sets.getPtr(lookup.pattern_idx)) |lambda_set| {
    return try self.generateLambdaSetDispatchMatch(...);
}
```

Change to look up the lambda set from the expression's type:

```zig
// New approach:
const func_type = try self.getExpressionType(call.func);
if (getLambdaSetFromType(func_type)) |lambda_set| {
    return try self.generateTypeBasedDispatch(
        call.func,
        lambda_set,
        transformed_args,
    );
}
```

#### Step 5.2: Update Dispatch to Call Lifted Functions

Instead of inlining lambda bodies, dispatch should call the lifted functions:

```zig
fn generateTypeBasedDispatch(
    self: *Self,
    func_expr: Expr.Idx,
    lambda_set: *const LambdaSet,
    args: []const Expr.Idx,
) !Expr.Idx {
    const branch_start = self.module_env.store.scratchMatchBranchTop();

    for (lambda_set.entries.items) |entry| {
        // Pattern: ClosureName(captures)
        const tag_pattern = try self.createTagPattern(entry.tag_name);

        // Body: call the lifted function with args and captures
        const lifted_fn_lookup = try self.module_env.store.addExpr(Expr{
            .e_lookup_local = .{ .pattern_idx = entry.lifted_pattern },
        }, base.Region.zero());

        // Build call: lifted_fn(arg1, arg2, ..., captures)
        const call_args = try self.buildCallArgs(args, captures_expr);
        const call_expr = try self.module_env.store.addExpr(Expr{
            .e_call = .{
                .func = lifted_fn_lookup,
                .args = call_args,
                .called_via = .direct,
            },
        }, base.Region.zero());

        // Add branch
        try self.addMatchBranch(tag_pattern, call_expr);
    }

    return try self.createMatchExpr(func_expr, branch_start);
}
```

#### Step 5.3: Remove Old Tracking Maps

Once type-driven dispatch works, you can remove or simplify:
- `pattern_lambda_sets`
- `lambda_return_sets`
- `pattern_lambda_return_sets`

The lambda set information now lives in the type system.

#### Step 5.4: Verify Tests Pass

Run `zig build snapshot` - all tests should pass with the new type-driven approach.

**Commit point:** "Switch to type-driven dispatch generation"

---

### Phase 6: Clean Up and Optimize

**Goal:** Remove unused code, optimize the implementation, and ensure robustness.

#### Step 6.1: Remove Unused Code

- Delete tracking maps that are no longer used
- Simplify `ClosureTransformer` since much logic moved to `LambdaLifter`
- Consider merging or reorganizing modules

#### Step 6.2: Add Additional Tests

Create new snapshot tests that exercise edge cases:
- Deeply nested closures
- Closures in match expressions
- Recursive functions that return closures
- Closures that capture other closures

#### Step 6.3: Performance Review

- Profile the transformation on large files
- Optimize hot paths if needed

#### Step 6.4: Documentation

- Update code comments to reflect the new architecture
- Document the multi-stage pipeline

**Commit point:** "Clean up and optimize closure transformation"

---

## Testing Strategy

### Existing Tests

The snapshot tests in `test/snapshots/mono_*.md` exercise the closure transformation:
- `mono_nested_closures.md` - Nested closures with captures
- `mono_closure_dispatch.md` - Mixed closures and pure lambdas in if expressions
- `mono_pure_lambda.md` - Pure lambdas (no captures)

### Running Tests

```bash
# Run all snapshot tests
zig build snapshot

# Run specific test
zig build snapshot -- test/snapshots/mono_nested_closures.md

# Run with verbose output
zig build snapshot -- --verbose test/snapshots/mono_nested_closures.md
```

### Expected Changes to Snapshots

As you implement each phase, the MONO output will change. The key differences will be:

**Before (current):**
```roc
make_adder = |y| Closure_make_adder_1({ y: y })

result = match add_five {
    Closure_make_adder_1({ y }) => {
        z = 3
        x + y + z
    }
}
```

**After (with lambda lifting):**
```roc
fn Closure_make_adder_1(z: Dec, captures: { y: Dec }) -> Dec {
    x + captures.y + z
}

make_adder = |y| Closure_make_adder_1({ y: y })

result = match add_five {
    Closure_make_adder_1(captures) => Closure_make_adder_1(3, captures)
}
```

---

## Reference: Cor Implementation

The Cor implementation is in `~/code/cor/experiments/lss/`. Key files:

- `lambdasolved/type.ml` - Type definitions with lambda sets
- `lambdasolved/solve.ml` - Type unification including lambda set merging
- `monotype_lifted/lower.ml` - Lambda lifting implementation
- `lambdamono/lower.ml` - Dispatch generation from types

You can study these for reference, but note they're in OCaml so the idioms differ.

---

## Glossary

- **Lambda Set**: The set of all closures that could inhabit a function value at runtime
- **Closure**: A lambda with captured variables from its environment
- **Lambda Lifting**: Converting inline closures to top-level function definitions
- **Captures**: Variables from the enclosing scope that a closure references
- **Dispatch**: Pattern matching on a function value to call the appropriate implementation
- **Defunctionalization**: The overall process of converting higher-order functions to first-order code

---

## Progress

_Update this section as you complete phases._

- [x] Phase 1: Add Lambda Set to Type System
- [ ] Phase 2: Populate Lambda Sets During Canonicalization
- [ ] Phase 3: Create Lambda Lifting Infrastructure
- [ ] Phase 4: Implement Lambda Lifting
- [ ] Phase 5: Type-Driven Dispatch Generation
- [ ] Phase 6: Clean Up and Optimize

### Notes

#### Phase 1 Completed (2024-12-24)

Added lambda set infrastructure to the type system:

1. **New types in `src/types/types.zig`:**
   - `LambdaSetEntry` struct with `tag_name: Ident.Idx` and `captures_var: Var`
   - `LambdaSetEntry.SafeList` for storing lists of lambda set entries
   - Added `lambda_set: LambdaSetEntry.SafeList.Range` field to `Func` struct

2. **Store updates in `src/types/store.zig`:**
   - Added `lambda_set_entries: LambdaSetEntrySafeList` storage
   - Updated `initCapacity`, `deinit`, and all serialization methods
   - Added `appendLambdaSetEntries`, `appendLambdaSetEntry`, `sliceLambdaSetEntries` helpers
   - Updated `mkFuncUnbound`, `mkFuncPure`, `mkFuncEffectful` to include empty lambda sets

3. **Updated all Func construction sites:**
   - `src/check/Check.zig` - multiple static dispatch constraint sites
   - `src/check/copy_import.zig` - cross-module type copying
   - `src/types/instantiate.zig` - type instantiation
   - `src/snapshot_tool/main.zig` - test infrastructure
   - `src/check/test/unify_test.zig` - test file

4. **Size changes:**
   - `Func`: 16 -> 24 bytes (+8 for lambda_set range)
   - `FlatType`: 24 -> 28 bytes
   - `Content`: 28 -> 32 bytes
   - `Descriptor`: 36 -> 40 bytes

All tests pass after running `zig build rebuild-builtins` to regenerate compiled modules with new format.

---

## Questions?

If you have questions about this migration plan, you can:
1. Look at the Cor reference implementation in `~/code/cor/experiments/lss/`
2. Read the current Roc5 implementation in `src/canonicalize/ClosureTransformer.zig`
3. Run the tests frequently to verify your understanding
