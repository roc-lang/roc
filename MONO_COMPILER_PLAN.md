# Monomorphizing Roc Compiler: Implementation Plan

## Overview

This document describes the plan to implement a full monomorphizing compiler for Roc that follows the proven approach from the Cor proof-of-concept (`~/code/cor/`). The key insight is that the compilation pipeline must follow this **exact order**:

1. **Monomorphization** - Specialize polymorphic functions to concrete types
2. **Lambda Set Inference** - Determine which closures can reach which call sites
3. **Lambda Set Specialization** - Create concrete versions for each lambda set
4. **Code Generation** - Output monomorphic Roc code (as a backend)

This order is critical. Deviating from it leads to subtle bugs, as learned from experience with the old Rust-based compiler.

---

## CRITICAL WARNING: NO RUST CODE

**THIS IS EXTREMELY IMPORTANT:**

- **NEVER** modify, read, reference, or interact with ANY Rust code
- **NEVER** touch ANYTHING in the `crates/` directory
- **NEVER** look at the old Rust compiler for "inspiration" or "reference"
- **ALL** implementation must be in Zig, in the `src/` directory

The Rust codebase is legacy code that is being replaced. Any interaction with it will cause confusion and wasted effort. The Cor proof-of-concept (`~/code/cor/`) in OCaml is the authoritative reference for the algorithms - NOT the old Rust compiler.

If you find yourself tempted to look at Rust code for any reason, **STOP** and refer to:
1. The Cor implementation in `~/code/cor/`
2. This document
3. The existing Zig code in `src/`

---

## Development Strategy: Small End-to-End Slices

**The core principle**: Get small slices of functionality working correctly end-to-end before moving on. Each slice should have:

1. **Input**: Roc source code (possibly using closures, captures, polymorphism)
2. **Output**: Emitted Roc code (with lambda sets resolved, captures handled)
3. **Verification**: Both input and output produce the same result when interpreted

This approach ensures we're always building on working code and can catch regressions immediately.

---

## Process & Workflow

### Development Process

For each slice:

1. **Write the test first** - Define input Roc code and expected output Roc code
2. **Implement minimally** - Build just enough to make the test pass
3. **Verify roundtrip** - Run both input and output through interpreter, compare results
4. **Commit** - Make a checkpoint commit
5. **Push PR** - Push a draft PR to GitHub for review
6. **Update This Document** - Mark the slice as complete

**Important**: This document itself should **NEVER** be committed. It serves as a living roadmap during development.

### Snapshot Tests (type=mono)

We use snapshot tests to verify monomorphization output. These are markdown files in `test/snapshots/` with `type=mono` in the META section.

**Format:**
```markdown
# META
~~~ini
description=Mono test: description here
type=mono
~~~
# SOURCE
~~~roc
x = 42
result = x + 1
~~~
# MONO
~~~roc
x : Dec
x = 42
result : Dec
result = 43
~~~
```

**Key requirements for MONO section:**
- SOURCE is a headerless type module (Roc type modules have no `module []` header)
- Each definition shows type annotation on separate line, then the definition
- Polymorphic number literals default to `Dec` (like the interpreter does)
- Constant folding is applied (e.g., `x + 1` where `x = 42` becomes `43`)
- Pure lambdas (no captures) keep their polymorphic type signature (e.g., `_a -> _a`)
- Closures with captures show the lifted parameter types (but NOT top-level captures, which are always in scope)

**Closure example:**
```roc
# SOURCE
x = 10
make_adder = |y| |z| x + y + z
add_five = make_adder(5)
result = add_five(3)

# MONO
x : Dec
x = 10
make_adder : Dec -> (Dec, Dec -> Dec)
make_adder = |y| |y0, z| x + y0 + z
add_five : Dec -> Dec
add_five = make_adder(5)
result : Dec
result = 18
```

Note: `x` is top-level, so it's not lifted. `y` is captured by the inner closure so it becomes parameter `y0`.

Run snapshot tests with: `zig build snapshot -- test/snapshots/mono_*.md`

---

## Current Status

### Completed

**Slice 1.1: Roc Emitter Infrastructure** ✅
- `src/canonicalize/RocEmitter.zig` - Converts CIR to Roc source code
- Handles: integers, strings, lists, records, tuples, lambdas, tags, if/else, match, blocks, binops
- Location: `src/canonicalize/RocEmitter.zig`

**Slice 1.2: Basic Roundtrip Tests** ✅
- Tests in `src/eval/test/mono_emit_test.zig`
- Verifies: parse → canonicalize → emit → re-parse → eval produces same result
- Covers: integers, arithmetic, booleans, if expressions, simple lambdas

**Slice 1.3: Monomorphizer Skeleton** ✅
- `src/canonicalize/Monomorphizer.zig` - Basic structure in place
- Has: `SpecializationKey`, `createSpecializedName`, `typeHash` helpers
- **Note**: The actual `monomorphize()` function is a placeholder

**Slice 2: Snapshot Tests with Type Annotations** ✅
- Snapshot tool generates MONO section with type annotations
- Format: type on separate line, then definition
- Closure captures are lifted as extra parameters (except top-level captures)
- Binop precedence preserved correctly (only add parens when needed)
- All mono snapshot tests pass

**Slice 2.5: Closure Capture Lifting** ✅
- `src/canonicalize/ClosureTransformer.zig` - Transforms closures to tagged values
- Closures emit with captured variables as additional lambda parameters
- Captured variables are renamed to avoid shadowing (e.g., `y` → `y0`)
- Top-level captures are NOT lifted (they're always in scope)
- Type signatures correctly reflect lifted parameters

**Slice 3: Dispatch Match Generation** ✅
- ClosureTransformer extended with `LambdaSet` type to track multiple closures per variable
- `transformExprWithLambdaSet()` collects closures from if/else branches
- `generateLambdaSetDispatchMatch()` generates match expressions for closure calls
- Integrated into snapshot tool pipeline (runs before constant folding)
- Closure tags now include counter for uniqueness (e.g., `#f_1`, `#f_2`)
- Snapshot test: `test/snapshots/mono_closure_dispatch.md`

### In Progress

**Slice 4: Polymorphic Function Specialization** ← CURRENT FOCUS

---

## Slice 2 Details (Completed)

**Goal**: Create snapshot tests that show monomorphized Roc code with explicit type annotations.

### 2.1 The Output Format

The MONO section shows each definition with its type on a separate line:

```roc
x : Dec
x = 42
identity : _a -> _a
identity = |x| x
result : Dec
result = 42
```

### 2.2 Implementation Steps (All Complete)

1. [x] Add `type=mono` to snapshot tool NodeType enum
2. [x] Create `generateMonoSection()` function using RocEmitter
3. [x] Create sample snapshot test files
4. [x] Reorder sections: SOURCE → MONO → FORMATTED → rest
5. [x] Type annotations on separate line from definition
6. [x] Implement type defaulting for numerals → Dec
7. [x] Closure captures lifted as extra parameters (except top-level)
8. [x] Binop precedence preserved correctly
9. [x] Captured variables renamed to avoid shadowing
10. [x] Verify all mono snapshot tests pass
11. [x] Commit and push

### 2.3 Current Snapshot Test Files

Located in `test/snapshots/`:
- `mono_integer_literal.md` - simple integer
- `mono_arithmetic.md` - arithmetic expression
- `mono_boolean_true.md` - boolean tag
- `mono_empty_list.md` - empty list
- `mono_list_with_elements.md` - list with elements
- `mono_if_expression.md` - if/else expression
- `mono_tuple.md` - tuple
- `mono_block_with_let.md` - block with let binding
- `mono_identity_applied.md` - identity function application
- `mono_closure_single_capture.md` - closure with one capture
- `mono_closure_multiple_captures.md` - closure with multiple captures
- `mono_multiple_closures.md` - multiple different closures
- `mono_nested_closures.md` - nested closures with captures

### 2.4 Type Rendering

Types are rendered in Roc syntax:
- `Dec` for decimal numbers (default for numeric literals)
- `[True, .._others]` for open tag unions
- `Str` for strings
- `List(a)` for lists
- `(a, b)` for tuples
- `{ field : Type }` for records
- `a -> b` for functions
- `_a` for unconstrained type variables

---

## Slice 3 Details (Completed)

**Goal**: Generate dispatch `match` expressions for closure calls.

When calling a closure variable that could hold multiple different closures, we need to generate a `match` that dispatches to the correct closure body based on which closure tag is present.

### 3.1 Example

```roc
# Input
condition = True
f = if condition |x| x + 1 else |x| x * 2
f(10)

# Output - f is a tagged union, call becomes match
condition : Bool
condition = True
f : _a
f = if condition #f_1({}) else #f_2({})
result = match f {
    #f_1({}) => {
        x = 10
        x + 1
    },
    #f_2({}) => {
        x = 10
        x * 2
    },
}
```

### 3.2 Implementation Steps (All Complete)

1. [x] Identify calls where the function is a closure variable (not a direct lambda)
2. [x] Track lambda sets (all possible closures) for each pattern/variable
3. [x] Generate a `match` expression with one arm per lambda in the set
4. [x] Inline the lambda body with arguments substituted
5. [x] Handle captures correctly in each match arm
6. [x] Add snapshot tests for dispatch match generation

### 3.3 Key Implementation Details

- Added `LambdaSet` type to ClosureTransformer to track multiple closures
- `transformExprWithLambdaSet()` recursively collects closures from if/else branches
- `pattern_lambda_sets` map tracks which closures can reach each variable
- `generateLambdaSetDispatchMatch()` generates the match expression for dispatch
- Memory management: branch lambda sets are freed after merging, capture_names are shared with closures map
- Closure tags include counter for uniqueness: `#f_1`, `#f_2`, etc.
- Constant folding is skipped when closure transformations are applied (to preserve dispatch structure)

---

## Future Slices

### Slice 4: Polymorphic Function Specialization

Specialize polymorphic functions to concrete types:

```roc
# Input
identity = |x| x
identity(42)
identity("hello")

# Output - two specialized versions
identity_Dec = |x| x
identity_Str = |x| x
identity_Dec(42)
identity_Str("hello")
```

### Slice 5: Lambda Sets Through Function Calls

Lambda sets must propagate through higher-order functions:

```roc
# Input
apply = |f, x| f(x)
addOne = |x| x + 1
apply(addOne, 41)

# Output - apply is specialized for the addOne lambda set
```

### Slice 6: Ability/Prototype Dispatch

Handle Roc abilities (maps to Cor's prototypes):

```roc
# Input
Default implements
    default : {} -> a where a implements Default

Foo := {} implements [Default { default: fooDefault }]
fooDefault = |{}| @Foo {}

default({})  # Should resolve to fooDefault when type is Foo
```

---

## Architecture Reference

### Current State (src/)

The Zig-based compiler has:
- **Parser** (`parse/`) - Tokenizes and parses to AST
- **Canonicalizer** (`canonicalize/`) - Name resolution, produces CIR
- **Type Checker** (`check/`) - Hindley-Milner type inference
- **RocEmitter** (`canonicalize/RocEmitter.zig`) - Emits CIR as Roc source ✅
  - Handles closure capture lifting (adds captured vars as extra lambda params)
  - Skips top-level captures (always in scope)
  - Renames captured variables to avoid shadowing
  - Preserves binop precedence correctly
- **ClosureTransformer** (`canonicalize/ClosureTransformer.zig`) - Transforms closures to tagged values
- **Monomorphizer** (`canonicalize/Monomorphizer.zig`) - Skeleton in place
- **Eval** (`eval/`) - Stack-based interpreter
- **Snapshot Tool** (`snapshot_tool/`) - Generates snapshot tests including `type=mono` ✅
  - Generates MONO section with type annotations on separate lines
  - Uses `getMonoTypeString` to build correct type signatures for closures

### Cor's Pipeline (Reference)

From `~/code/cor/experiments/uls/`:

```
parse -> solve (type inference + lambda set inference) -> ir (monomorphization) -> eval
```

Key Cor files:
- `experiments/uls/syntax.ml` - Types including lambda sets (lines 22-41)
- `experiments/uls/solve.ml` - Lambda set inference during unification (lines 85-99)
- `experiments/uls/ir.ml` - Monomorphization and closure lifting (lines 126-199, 204-245)

### Lambda Set Type (from Cor)

```ocaml
type lset = {
  mutable solved : lambda list;           (* Known lambdas *)
  mutable unspec : unspec ref list;       (* Pending lambda sets *)
}

and unspec =
  | Solved of lset
  | Pending of ty uls                     (* Waiting to specialize *)
```

Key insight: **Lambda sets ARE types and must be unified**. When two function types unify, their lambda sets merge.

---

## Quality Checklist

Before marking any slice complete:

- [ ] `zig build minici` passes (this is the CI check - run it before pushing!)
- [ ] All tests pass
- [ ] No `undefined` values passed to functions
- [ ] No empty test bodies with "would need proper setup" comments
- [ ] No placeholder strings like `<external>` in emitted output
- [ ] Roundtrip verification passes (input and output produce same interpreter result)
- [ ] Snapshot tests show correct monomorphized output with types
- [ ] Code has been committed
- [ ] PR has been pushed

---

## FINAL REMINDER: NO RUST CODE

**DO NOT:**
- Look at `crates/`
- Read any `.rs` files
- Reference the old Rust compiler
- Use Cargo or any Rust tooling

**DO:**
- Work exclusively in `src/` with Zig
- Reference `~/code/cor/` (OCaml) for algorithms
- Use `zig build` for building
- Follow this document

The Rust code is legacy. Cor is the reference. Zig is the implementation language.
