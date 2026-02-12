# LLVM Code Generation Comparison: Zig vs Legacy Rust

This document compares the new Zig LLVM evaluator (`src/eval/llvm_evaluator.zig`) with the legacy Rust LLVM code generation (`crates/compiler/gen_llvm/`).

## Scale Comparison

| Metric | New Zig | Legacy Rust |
|--------|---------|-------------|
| Total lines | ~1,700 | ~18,000 |
| Main file | 1,659 lines | 6,907 lines (build.rs) |
| Number of files | 1 | 18 |
| Low-level ops | ~20 | ~100+ (2,897 lines) |
| Refcounting | 0 lines | 1,808 lines |
| Comparison ops | ~50 lines | 1,509 lines |
| List operations | ~20 lines | 738 lines |

## Architectural Differences

### Input IR

**Legacy Rust:**
- Works on **Mono IR** (`roc_mono::ir`) - already monomorphized
- All types are concrete, all functions are specialized
- Lambda sets resolved to concrete function pointers
- Input is `Stmt` and `Expr` enums from monomorphization phase

**New Zig:**
- Works on **CIR** (Canonical IR) - pre-monomorphization
- Types still have polymorphism
- Direct from canonicalization phase
- Input is `CIR.Expr` from `can.CIR`

### LLVM API

**Legacy Rust:**
```rust
// Uses inkwell (Rust LLVM bindings)
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, BasicValueEnum};

// Creates full LLVM infrastructure
let context = Context::create();
let module = context.create_module("roc_app");
let builder = context.create_builder();
```

**New Zig:**
```zig
// Uses Zig's standard library LLVM bindings
const LlvmBuilder = std.zig.llvm.Builder;

// Lightweight builder that serializes to bitcode
var builder = LlvmBuilder.init(.{
    .allocator = allocator,
    .name = "roc_repl_eval",
    .target = &builtin.target,
    .triple = getLlvmTriple(),
}) catch return error.OutOfMemory;
```

### Purpose

**Legacy Rust:**
- Full AOT (Ahead-of-Time) compiler backend
- Produces object files for linking
- Used by `roc build`, `roc run`
- Targets all supported platforms

**New Zig:**
- JIT evaluation for REPL
- Produces bitcode for immediate execution
- Used by `roc repl` with `--opt` flags
- Single-expression evaluation only

## Feature Comparison

### Expression Support

| Expression Type | Legacy Rust | New Zig |
|-----------------|-------------|---------|
| Literals (int, float, str) | Yes | Yes |
| Binary operations | Yes | Yes |
| Unary operations | Yes | Yes |
| If expressions | Yes (full CFG) | Yes (select-based) |
| Match/Switch | Yes (full) | No |
| Struct creation | Yes | Yes |
| Struct field access | Yes | Yes |
| Tag creation | Yes (all union layouts) | Limited |
| Tag matching | Yes (full) | No |
| List literals | Yes | Limited |
| List operations | Yes (all) | Empty list only |
| Function calls | Yes (all) | Limited builtins |
| Lambdas | Yes | No |
| Closures | Yes | No |
| Foreign calls | Yes | No |

### Statement Support

| Statement Type | Legacy Rust | New Zig |
|----------------|-------------|---------|
| Let bindings | Yes | Yes (via blocks) |
| Return | Yes | N/A (expression-based) |
| Switch/Jump | Yes | No |
| Join points | Yes | No |
| Refcount ops | Yes | No |
| Expect | Yes | No |
| Crash | Yes | No |

### Type Support

| Type | Legacy Rust | New Zig |
|------|-------------|---------|
| i8-i128 | Yes | Yes |
| u8-u128 | Yes | Yes |
| f32, f64 | Yes | Yes |
| Dec | Yes | Yes |
| Bool | Yes | Yes |
| Str (small) | Yes | Yes |
| Str (large) | Yes | No |
| List | Yes | Limited |
| Record/Struct | Yes | Yes |
| Recursive unions | Yes | No |
| Non-recursive unions | Yes | Limited |
| Function pointers | Yes | No |
| Opaque types | Yes | No |
| Erased types | Yes | No |

### Low-Level Operations

**Legacy Rust** (`lowlevel.rs` - 2,897 lines) supports all operations:

```rust
match op {
    StrConcat => { /* full impl */ }
    StrJoinWith => { /* full impl */ }
    StrStartsWith | StrEndsWith => { /* full impl */ }
    StrToNum => { /* handles all numeric types */ }
    ListLen | ListGetUnsafe | ListSet => { /* full impl */ }
    ListConcat | ListAppend | ListPrepend => { /* full impl */ }
    ListMap | ListMap2 | ListMap3 | ListMap4 => { /* higher-order */ }
    NumAdd | NumSub | NumMul | NumDiv => { /* all widths */ }
    NumAbs | NumNeg | NumSin | NumCos => { /* math ops */ }
    NumToStr => { /* for all numeric types */ }
    // ... 100+ more operations
}
```

**New Zig** (`llvm_evaluator.zig`) supports basic operations:

```zig
// Only handles simple builtin calls by ident matching
if (func_ident == idents.not and args.len == 1) { /* bool not */ }
if (func_ident == idents.abs and args.len == 1) { /* numeric abs */ }
if (func_ident == idents.negate and args.len == 1) { /* negate */ }
if (func_ident == idents.abs_diff and args.len == 2) { /* abs_diff */ }
```

### Memory Management

**Legacy Rust** (`refcounting.rs` - 1,808 lines):
- Full reference counting implementation
- Increment/decrement for all refcounted types
- Deallocation with alignment handling
- Reset/reuse optimization for unique references
- Special handling for lists, strings, recursive structures

```rust
pub fn increment_refcount_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: InLayout<'a>,
) { /* recursive increment */ }

pub fn decrement_refcount_layout<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    value: BasicValueEnum<'ctx>,
    layout: InLayout<'a>,
) { /* recursive decrement with deallocation */ }
```

**New Zig:**
- No reference counting (single expression evaluation)
- No memory management needed for REPL use case

### Comparison Operations

**Legacy Rust** (`compare.rs` - 1,509 lines):
- Generic equality for all types
- Recursive comparison for structs, unions, lists
- Special handling for floats (NaN)
- String comparison via bitcode

```rust
pub fn generic_eq<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: InLayout<'a>,
    rhs_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> { /* handles all types */ }
```

**New Zig:**
- Integer/float comparison via LLVM icmp/fcmp
- String comparison returns UnsupportedType

### Bitcode Integration

**Legacy Rust** (`bitcode.rs` - 1,351 lines):
- Calls into pre-compiled Zig bitcode for builtins
- Platform-specific ABI handling (Windows i128, macOS arm64)
- Wraps list/string operations
- Handles Dec arithmetic

```rust
pub fn call_bitcode_fn<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    args: &[BasicValueEnum<'ctx>],
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    // Call into compiled Zig code
    // Handle platform ABI differences
}
```

**New Zig:**
- Generates standalone functions
- No external bitcode dependencies for evaluated expressions

### Debug Information

**Legacy Rust:**
- Full DWARF debug info generation
- Source locations, line numbers
- Variable names and types
- Integration with `DebugInfoBuilder`

**New Zig:**
- No debug info (REPL evaluation doesn't need it)

### Optimization

**Legacy Rust:**
- Uses LLVM `PassManager`
- Configurable optimization levels
- Inlining, constant folding, etc.

**New Zig:**
- No optimization passes
- Relies on LLVM JIT's basic optimizations

## Control Flow Comparison

### If Expressions

**Legacy Rust** - Full CFG with basic blocks:
```rust
// Creates then_block, else_block, merge_block
// Uses conditional branch and phi nodes
env.builder.new_build_conditional_branch(cond, then_block, else_block);
// ... build then block ...
// ... build else block ...
// Merge with phi
```

**New Zig** - Full CFG with basic blocks and phi nodes:
```zig
// Creates merge block, then evaluates each branch
const merge_block = ctx.wip.block(num_incoming, "if_merge");

// For each branch: create then/else blocks, emit conditional branch
_ = ctx.wip.brCond(cond_i1, then_block, else_block, .none);

// Emit then block, branch to merge
ctx.wip.cursor = .{ .block = then_block };
const then_val = try ctx.emitExpr(branch.body);
_ = ctx.wip.br(merge_block);

// Merge with phi node
ctx.wip.cursor = .{ .block = merge_block };
const wip_phi = ctx.wip.phi(result_type, "if_result");
wip_phi.finish(phi_values[0..phi_count], phi_blocks[0..phi_count], ctx.wip);
```

### Switch/Match

**Legacy Rust** - Full switch implementation:
```rust
fn build_switch_ir<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    // ...
    switch_args: SwitchArgsIr<'a, 'ctx>,
) -> BasicValueEnum<'ctx> {
    // Build switch instruction with cases
    // Handle join points for complex control flow
}
```

**New Zig:**
- Returns `UnsupportedType` for match expressions

## Union/Tag Handling

### Legacy Rust

Handles all union layouts (`UnionLayout` enum):
- `NonRecursive` - Tags stored by value
- `Recursive` - Heap-allocated with pointer tagging
- `NonNullableUnwrapped` - Single recursive variant
- `NullableWrapped` - Optional-like with null pointer
- `NullableUnwrapped` - Single non-null variant

```rust
fn build_tag<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    scope: &Scope<'a, 'ctx>,
    union_layout: &UnionLayout<'a>,
    tag_id: u8,
    arguments: &[Symbol],
    // ...
) -> BasicValueEnum<'ctx> {
    match union_layout {
        UnionLayout::NonRecursive(_) => { /* stack allocation */ }
        UnionLayout::Recursive(_) => { /* heap allocation with refcount */ }
        // ... all variants
    }
}
```

### New Zig

Only handles simple tags:
```zig
fn emitZeroArgTag(ctx: *ExprContext, tag: anytype) ExprError!LlvmBuilder.Value {
    // For Bool tags, True is 1 and False is 0
    const value: i128 = if (tag.name == ctx.module_env.idents.true_tag) 1 else 0;
    return (ctx.builder.intConst(.i8, value) catch return error.CompilationFailed).toValue();
}
```

## Higher-Order Functions

**Legacy Rust** - Full support:
```rust
pub(crate) fn run_higher_order_low_level<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    layout_ids: &mut LayoutIds<'a>,
    scope: &Scope<'a, 'ctx>,
    layout: InLayout<'a>,
    op: &HigherOrderLowLevel<'a>,
    // ...
) -> BasicValueEnum<'ctx> {
    // Handles List.map, List.map2, List.walk, etc.
    // Generates loops with function pointer calls
}
```

**New Zig:**
- No higher-order function support
- Returns `UnsupportedType` for lambdas

## Summary

| Aspect | Legacy Rust | New Zig (WIP) |
|--------|-------------|---------------|
| Purpose | Full AOT compiler | Full compiler (work in progress) |
| Input | Mono IR (monomorphized) | CIR (pre-mono) |
| LLVM API | inkwell (full LLVM) | Zig std.zig.llvm.Builder |
| Lines of code | ~18,000 | ~1,700 |
| Type coverage | Complete | Basic scalars + simple records |
| Control flow | Full CFG | Full CFG (if), WIP (switch) |
| Memory mgmt | Full refcounting | Not yet implemented |
| Unions | All layouts | Bool tags only |
| Functions | Full (closures, HOF) | Limited builtins |
| Low-level ops | 100+ | ~20 |
| Debug info | Full DWARF | Not yet implemented |
| Optimization | PassManager | Not yet implemented |

The new Zig evaluator is a **work in progress** that aims to achieve full feature parity with the legacy Rust implementation. Current status:

**Implemented:**
- Basic scalar types (integers, floats, Dec, Bool)
- Binary and unary operations
- If expressions with proper CFG (basic blocks, conditional branches, phi nodes)
- Simple records and field access
- Multi-branch if-else chains

**Not yet implemented:**
- Switch/match expressions
- Reference counting and memory management
- All union layouts (only Bool tags work)
- Closures and lambda sets
- Higher-order functions
- Full low-level operation set
- Debug information
- Optimization passes

The legacy Rust code is a full production compiler backend that serves as the reference implementation for feature parity.
