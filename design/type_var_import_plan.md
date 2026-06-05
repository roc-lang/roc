# Type Variable Import Implementation Plan

## Overview

This document describes the implementation plan for a new syntax feature: importing a module alias based on a type variable's resolved type. This enables static dispatch within function bodies using the type that a type variable resolves to at call-site.

### What Problem Does This Solve?

In Roc, you can define polymorphic functions that work on any type that implements certain methods (via `where` clauses). However, inside the function body, you couldn't easily call methods on that type because the concrete type isn't known until call-site.

**Example Use Case - `List.join_with`:**
```roc
# We want to join a list of items using a joiner of the same type
# e.g., ["hello", "world"].join_with(", ") should produce "hello, world"
# This works by calling Str.join_with on the elements

join_with : List(item_type_var), item_type_var -> item_type_var
    where [item_type_var.join_with : List(item_type_var), item_type_var -> item_type_var]
join_with = |items, joiner| {
    import item_type_var as Item   # <-- This is the new syntax
    Item.join_with(items, joiner)   # Calls Str.join_with when item_type_var = Str
}
```

### Syntax

The `import type_var as Alias` statement:
1. References a type variable (`item_type_var`) from the function's type annotation
2. Creates a module alias (`Alias`) that resolves to whatever module defines the type that the type variable is instantiated with
3. Allows `Alias.method(...)` to perform static dispatch based on the resolved type

## Background: How Roc's Compiler Works

For contributors new to the codebase, here's how code flows through the compiler:

1. **Parsing** (`src/parse/`): Source code → AST (Abstract Syntax Tree)
2. **Canonicalization** (`src/canonicalize/`): AST → CIR (Canonical Intermediate Representation)
   - Resolves names, scopes, imports
   - Produces a canonical form for type checking
3. **Type Checking** (`src/check/`): Adds type information to CIR
   - Hindley-Milner type inference with constraint solving
   - Handles `where` clauses and static dispatch constraints
4. **Interpretation** (`src/eval/`): Executes CIR directly (for REPL, comptime, etc.)
   - Evaluates expressions
   - Handles method dispatch at runtime

### Key Concepts

- **Rigid Type Variable**: A type variable from an annotation that must be preserved (not unified with concrete types until call-site)
- **Flex Type Variable**: An inferred type variable that can unify with anything
- **Static Dispatch**: Calling a method on a type where the method is looked up in the type's defining module
- **`rigid_subst`**: A map in the interpreter that tracks substitutions from rigid type variables to their concrete instantiations during function calls
- **Where Clause**: Specifies constraints on type variables (e.g., `where [a.method : a -> a]` means type `a` must have a `method`)

## Implementation Status

### ✅ Phase 1: Parsing (COMPLETE)

**Goal**: Parse `import type_var as Alias` statements inside function bodies.

**Files Modified**:
- `src/parse/AST.zig`: Added `type_var_import` statement variant
- `src/parse/Parser.zig`: Modified import parsing to detect the pattern
- `src/parse/Node.zig`: Added `type_var_import` node tag
- `src/parse/NodeStore.zig`: Added statement storage/retrieval

**How It Works**:
- When the parser sees `import` followed by a lowercase identifier (type variable) instead of an uppercase one (module), it creates a `type_var_import` node
- The node stores: `type_var_tok` (the type variable name), `alias_tok` (the alias), `region` (for error reporting)

### ✅ Phase 2: Canonicalization (COMPLETE)

**Goal**: Transform parsed type var imports into canonical form and handle `Alias.method` lookups.

**Files Modified**:
- `src/canonicalize/CIR.zig`: Added `TypeVarImport` struct
- `src/canonicalize/Expression.zig`: Added `e_lookup_type_var_member` expression
- `src/canonicalize/Scope.zig`: Added `type_var_imports` map to track aliases
- `src/canonicalize/Node.zig`: Added node tags
- `src/canonicalize/NodeStore.zig`: Added storage functions
- `src/canonicalize/ModuleEnv.zig`: Added wrapper functions
- `src/canonicalize/Can.zig`: Main canonicalization logic
- `src/canonicalize/Diagnostic.zig`: Error types
- `src/canonicalize/DependencyGraph.zig`: Dependency handling

**How It Works**:
1. When canonicalizing `import item as Item`:
   - Look up `item` in scope to find the type annotation (must be from a `where` clause)
   - Create a `TypeVarImport` record linking the annotation to the alias
   - Register `Item` in `scope.type_var_imports`

2. When canonicalizing `Item.join_with(...)`:
   - Parser produces a single identifier `Item.join_with` (qualified name)
   - Canonicalizer checks `scope.type_var_imports` for `Item`
   - If found, creates `e_lookup_type_var_member { type_var_import_idx, member_name }`

**Key Code Location**: `src/canonicalize/Can.zig` in the `.ident` case (around line 3864)

### ✅ Phase 3: Type Checking (COMPLETE)

**Goal**: Infer the type of `e_lookup_type_var_member` expressions.

**File Modified**: `src/check/Check.zig` (around line 3057)

**How It Works**:
1. Get the `TypeVarImport` from the CIR store
2. Get the type variable from `type_var_anno` and resolve it
3. If it's a rigid var with constraints (from `where` clause):
   - Search constraints for one matching the member name
   - Instantiate the function type from that constraint
4. Unify the expression type with the instantiated function type

**Current Behavior**: Works correctly for type inference. The function type is properly inferred from the where clause constraint.

### ✅ Phase 4: Diagnostics (COMPLETE)

**Goal**: Provide helpful error messages.

**Errors Implemented**:
- `type_var_import_not_type_var`: When the imported name isn't a type variable in scope
- `type_var_import_duplicate_alias`: When the same alias is used twice in the same scope

### ⚠️ Phase 5: Runtime / Interpreter (PARTIALLY COMPLETE)

**Goal**: Execute `e_lookup_type_var_member` at runtime by resolving to the concrete type's method.

**File Modified**: `src/eval/interpreter.zig`

**What's Implemented**:
- `evalLookupTypeVarMember` function exists
- Retrieves `TypeVarImport` from CIR store
- Translates type variable to runtime type
- Checks `rigid_subst` for substitutions
- For nominal types, calls `resolveMethodFunction`

**What's Broken**:
The type variable doesn't resolve to the concrete type at runtime. See "Current Bug" section below.

### ✅ Phase 6: Testing (COMPLETE)

**Snapshot Tests** (`test/snapshots/type_var_import/`):
- `basic_type_var_import.md`: Tests parsing, canonicalization, type inference
- `type_var_import_not_in_scope.md`: Tests error when type var not found
- `type_var_import_method_not_in_where.md`: Tests calling undeclared method

**Running Tests**:
```bash
# Run all tests
zig build test

# Generate/update snapshots
zig build snapshot

# Run specific snapshot
zig build snapshot -- test/snapshots/type_var_import/basic_type_var_import.md
```

## Current Bug: Runtime Type Variable Resolution

### The Problem

When executing `List.join_with(["hello", "world"], ", ")`:

1. **Expected**: `item_type_var` should resolve to `Str`, then `Item.join_with` becomes `Str.join_with`
2. **Actual**: `item_type_var` resolves to a rigid type variable, causing a `TypeMismatch` error

### Detailed Root Cause Analysis (Updated 2024)

After extensive debugging, the root cause has been identified as a **cross-module type variable translation issue** combined with **unification side effects**.

#### The Translation Cache Problem

The interpreter uses `translateTypeVar` to convert compile-time type variables to runtime type variables. This function uses a cache keyed by `(module_ptr, compile_time_var)`:

```zig
const key: u64 = (@as(u64, @intFromPtr(module)) << 32) | @as(u64, @intFromEnum(resolved.var_));
if (self.translate_cache.get(key)) |found| {
    return found;
}
```

When `List.join_with` is called from the REPL:
1. The REPL module translates the function type, creating runtime vars (e.g., `9`, `10`, `11`)
2. These are structures (concrete types), not rigid vars, because the REPL already knows the argument types
3. Inside `List.join_with`'s body (Builtin module), `translateTypeVar` uses a DIFFERENT cache key
4. This creates NEW runtime vars (e.g., `19` for `item_type_var`) that are rigid

#### The Instantiation and Unification Problem

When `instantiateType` is called:
1. It creates `rigid -> fresh_flex` mappings (e.g., `19 -> 22`)
2. Variable `22` is created as a `flex` type variable

However, later unification changes `22` from `flex` to `rigid`:
- In `src/check/unify.zig`, when a flex unifies with a rigid, the result is rigid:
  ```zig
  .rigid => |b_rigid| {
      // ...
      self.merge(vars, .{ .rigid = b_rigid });
  },
  ```
- This happens because the inner `Item.join_with` call also instantiates its type
- The unification between the outer flex var and the inner rigid var converts the flex to rigid

#### Debug Output Trace

```
# First e_call (REPL calling List.join_with):
subst_map entries:
    9 (structure) -> 9 (structure)   # Already concrete, no change
    10 (structure) -> 10 (structure)
    11 (structure) -> 12 (structure)

# instantiateType for inner call:
rigid 19 -> fresh 22 (fresh content: flex)  # 22 is flex!

# Second e_call (List.join_with body calling Item.join_with):
subst_map entries:
    19 (rigid) -> 22 (flex)   # 22 is still flex here

# In evalLookupTypeVarMember:
resolved var: 22, content type: rigid  # 22 is now rigid!
```

The key insight: var `22` changes from `flex` to `rigid` due to unification with another rigid variable during the inner function call setup.

### Why rigid_subst Doesn't Help

The `rigid_subst` map correctly contains `19 -> 22`, but:
1. Variable `22` was supposed to be unified with `Str` (concrete type)
2. Instead, `22` got unified with another rigid var from the inner call
3. When we resolve `22`, we get `rigid` content instead of `Str`

### Recommended Fix: Infer from Parameter Bindings

The most robust fix is to **infer the concrete type from the actual runtime values** bound to function parameters:

1. When `evalLookupTypeVarMember` runs, look at the current bindings
2. Find a parameter whose type involves the type variable (e.g., `items : List(item)`)
3. Get the runtime type of the bound value (e.g., `List(Str)`)
4. Extract the type argument (`Str`) as the concrete type for `item`

Implementation sketch:
```zig
fn evalLookupTypeVarMember(...) Error!StackValue {
    // ... existing code to get type_var_import ...
    
    // NEW: Try to infer concrete type from parameter bindings
    for (self.bindings.items) |binding| {
        const binding_value = binding.value;
        if (binding_value.rt_var) |rv| {
            const resolved_binding = self.runtime_types.resolveVar(rv);
            // Check if this is a nominal type (like List) with type args
            if (resolved_binding.desc.content == .structure) {
                if (resolved_binding.desc.content.structure == .nominal_type) |nom| {
                    const type_args = self.runtime_types.sliceNominalArgs(nom);
                    // Match type args against the type variable we're looking for
                    // Return the concrete type if found
                }
            }
        }
    }
    
    // Fall back to rigid_subst lookup...
}
```

### Alternative Fix: Store Substitutions in Closure

Add a `type_subst` field to the `Closure` struct in `src/layout/layout.zig`:
```zig
pub const Closure = struct {
    body_idx: CIR.Expr.Idx,
    params: CIR.Pattern.Span,
    // ... existing fields ...
    // NEW: Snapshot of rigid_subst when closure was invoked
    type_subst: ?*std.AutoHashMap(types.Var, types.Var),
};
```

When invoking a closure, capture the current `rigid_subst` snapshot. When looking up type variables inside the closure body, use this captured snapshot.

### Files to Investigate

- `src/eval/interpreter.zig`:
  - `evalLookupTypeVarMember` (line ~8220): Where we need the correct type
  - `e_call` handling (line ~7318): Where instantiation and unification happen
  - `instantiateType` (line ~5737): Creates rigid -> flex mappings
  - `translateTypeVar` (line ~5464): Module-specific type translation with caching

- `src/check/unify.zig`:
  - `unifyFlex` (line ~489): Where flex + rigid = rigid
  - `unifyRigid` (line ~605): Rigid unification rules

- `src/layout/layout.zig`:
  - `Closure` struct (line ~115): Would need modification for closure-based fix

### Test Files

- `test/snapshots/type_var_import/type_var_import_repl.md`: REPL test calling `List.join_with`
- `test/snapshots/type_var_import/type_var_import_runtime.md`: File-type test for compilation
- `src/build/roc/Builtin.roc`: Contains `List.join_with` implementation (currently using `...`)

### How to Debug

Run the REPL test:
```bash
zig build snapshot -- test/snapshots/type_var_import/type_var_import_repl.md
```

Add debug output in `evalLookupTypeVarMember` to trace variable resolution:
```zig
std.debug.print("rt_type_var: {d}, resolved content: {s}\n", 
    .{@intFromEnum(rt_type_var), @tagName(resolved.desc.content)});
```

## Known TODOs

### `src/build/roc/Builtin.roc` - List.join_with

```roc
join_with : List(item_type_var), item_type_var -> item_type_var
    where [item_type_var.join_with : List(item_type_var), item_type_var -> item_type_var]
join_with = |_items, _joiner| {
    # TODO: Re-enable when type variable import runtime resolution is fixed
    # import item_type_var as Item
    # Item.join_with(items, joiner)
    ...
}
```

Re-enable this once the runtime bug is fixed.

## Lessons Learned

### Qualified Identifiers vs Field Access

The original plan assumed `Item.join_with` would be parsed as a field access expression (like `record.field`). Actually, Roc parses this as a single qualified identifier with the `.` included.

**Implication**: The type variable import alias check needed to be in the `.ident` canonicalization path, not in field access handling.

**Key code**: `src/canonicalize/Can.zig`, `.ident` case

### Scope Hierarchy for Type Variables

Type variables from a function's annotation are available in the function body via `scopeLookupTypeVar`. The type variable import uses this existing infrastructure - it just looks up the type var and stores the association.

### Node Count Constants

When adding new expression types or diagnostics, update the compile-time constants:
- `src/canonicalize/NodeStore.zig`: `MODULEENV_DIAGNOSTIC_NODE_COUNT`, `MODULEENV_EXPR_NODE_COUNT`

Forgetting this causes test failures with cryptic messages about incomplete coverage.

### Statement vs Expression

Type variable imports are statements that modify scope but don't produce CIR output. `canonicalizeBlockStatement` returns `null` for the statement (similar to regular imports at top level).

## Architecture Reference

### Data Flow Diagram

```
Source Code
    │
    ▼
┌─────────────────────────────────────────────────────────────────┐
│ PARSING                                                          │
│                                                                  │
│ "import item as Item"                                            │
│     ↓                                                            │
│ AST.Statement.type_var_import {                                  │
│     type_var_tok: "item",                                        │
│     alias_tok: "Item",                                           │
│     region: ...                                                  │
│ }                                                                │
└─────────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────┐
│ CANONICALIZATION                                                 │
│                                                                  │
│ 1. Look up "item" in scope → TypeAnno.Idx (rigid var from where)│
│ 2. Create TypeVarImport { type_var_anno, alias_name, region }   │
│ 3. Register "Item" → TypeVarImport.Idx in scope.type_var_imports│
│                                                                  │
│ Later, for "Item.join_with":                                     │
│ 1. Check scope.type_var_imports["Item"] → Found!                │
│ 2. Create e_lookup_type_var_member {                            │
│        type_var_import_idx,                                      │
│        member_name: "join_with"                                  │
│    }                                                             │
└─────────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────┐
│ TYPE CHECKING                                                    │
│                                                                  │
│ For e_lookup_type_var_member:                                    │
│ 1. Get TypeVarImport → get type_var_anno                        │
│ 2. Resolve type_var_anno → rigid var with constraints           │
│ 3. Find constraint with fn_name == "join_with"                  │
│ 4. Instantiate constraint.fn_var for this use site              │
│ 5. Unify expression type with instantiated function type        │
└─────────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────────┐
│ RUNTIME (INTERPRETER)                                            │
│                                                                  │
│ For e_lookup_type_var_member:                                    │
│ 1. Get TypeVarImport from CIR                                   │
│ 2. Translate type_var_anno to runtime type                      │
│ 3. Check rigid_subst for substitution → PROBLEM HERE            │
│ 4. If nominal type, call resolveMethodFunction                  │
│ 5. Return method as callable value                              │
└─────────────────────────────────────────────────────────────────┘
```

### Key Files Quick Reference

| File | Purpose |
|------|---------|
| `src/parse/AST.zig` | AST node definitions |
| `src/parse/Parser.zig` | Parsing logic |
| `src/canonicalize/CIR.zig` | CIR type definitions |
| `src/canonicalize/Can.zig` | Main canonicalization |
| `src/canonicalize/Scope.zig` | Scope management |
| `src/canonicalize/Expression.zig` | Expression types |
| `src/check/Check.zig` | Type checking |
| `src/eval/interpreter.zig` | Runtime execution |
| `test/snapshots/type_var_import/` | Test files |

### Common Commands

```bash
# Run all tests
zig build test

# Run specific test filter
zig build test -- --test-filter "type_var_import"

# Generate snapshots
zig build snapshot

# Generate specific snapshot
zig build snapshot -- test/snapshots/type_var_import/basic_type_var_import.md

# Build and test snapshots
zig build snapshot && zig build test
```

## Next Steps for Contributors

1. **Understand the bug**: Read the "Current Bug" section and add debug output to trace `rigid_subst` through a simple test case

2. **Identify where substitutions are lost**: Find the exact point where `rigid_subst` changes from having the correct substitution to having the wrong one

3. **Implement a fix**: Choose one of the potential solutions or devise a new one

4. **Test the fix**: 
   - Create a REPL-type snapshot test that actually executes `List.join_with`
   - Verify it produces the expected output

5. **Re-enable `List.join_with`**: Update `src/build/roc/Builtin.roc` to use the type variable import

6. **Update this document**: Mark Phase 5 as complete and document any new lessons learned