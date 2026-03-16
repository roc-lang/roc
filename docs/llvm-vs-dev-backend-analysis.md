# LLVM Backend vs Dev Backend Analysis

This document compares the two evaluator approaches in the Roc codebase:
- **LLVM Backend** (`src/eval/llvm_evaluator.zig`) - current branch
- **Dev Backend** (`origin/dev-backends` branch) - alternative approach

Both are trying to achieve parity with the interpreter for REPL evaluation.

## Architecture Overview

### LLVM Backend

The LLVM evaluator uses Zig's built-in LLVM bindings to generate LLVM IR:

```
CIR Expression → LLVM IR (via LlvmBuilder) → Bitcode → JIT Execution
```

Key components:
- `LlvmBuilder` from Zig's standard library
- `WipFunction` for building function bodies
- Returns `BitcodeResult` containing serialized LLVM bitcode
- JIT compilation handled by separate `llvm_compile` module

### Dev Backend

The dev backend generates native machine code directly without LLVM:

```
CIR Expression → Native Machine Code (x86_64/aarch64) → JIT Execution
```

Key components:
- Custom `ComptimeHeap` and `ComptimeValue` for compile-time evaluation
- Direct machine code emission (`generateReturnI64Code`, etc.)
- Object file writers (ELF, Mach-O, COFF) for linking
- Explicit per-architecture code generation

## Similarities

### 1. Structure and Organization

Both evaluators:
- Are in the `src/eval/` directory
- Export their evaluator as a struct (`LlvmEvaluator`, `DevEvaluator`)
- Load builtin modules at startup
- Have `init()`, `deinit()`, and main `generate*()` methods
- Use `Error` enum for error handling

### 2. Identifier Comparison

Both use interned ident indices instead of string comparison:

```zig
// LLVM Backend (llvm_evaluator.zig:669)
const value: i128 = if (tag.name == ctx.module_env.idents.true_tag) 1 else 0;

// Dev Backend (dev_evaluator.zig:807-808)
if (tag.name == module_env.idents.true_tag) return 1;
if (tag.name == module_env.idents.false_tag) return 0;
```

### 3. Type Handling

Both map CIR types to result types similarly and support:
- Numeric types (i8-i128, u8-u128, f32, f64)
- Dec (fixed-point decimal)
- Bool (True/False tags)

### 4. Expression Support

Core expressions supported by both:
- Numeric literals (`e_num`, `e_typed_int`, `e_typed_frac`)
- Binary operations (`e_binop`)
- Unary operations (`e_unary_minus`, `e_unary_not`)
- If expressions (`e_if`)
- Tags (`e_zero_argument_tag`, `e_tag`)
- Local lookups (`e_lookup_local`)
- Blocks (`e_block`)

### 5. Builtin Loading

Both load compiled builtins at startup:

```zig
// Identical pattern in both
const builtin_indices = builtin_loading.deserializeBuiltinIndices(
    allocator,
    compiled_builtins.builtin_indices_bin,
) catch return error.OutOfMemory;
```

## Key Differences

### 1. Code Generation Approach

**LLVM Backend:**
- Generates LLVM IR using `LlvmBuilder`
- Relies on LLVM for optimization and code generation
- Platform-agnostic IR generation
- JIT via LLVM's execution engine

**Dev Backend:**
- Generates native machine code directly
- Architecture-specific code (`x86_64`, `aarch64`)
- Manual instruction encoding
- Custom JIT execution

Example - returning an integer:

```zig
// LLVM Backend: builds LLVM constant
const zero = (ctx.builder.intConst(llvm_type, 0) catch return error.CompilationFailed).toValue();

// Dev Backend: emits raw machine code
fn generateReturnI64Code(self: *DevEvaluator, value: i64, _: ResultType) Error![]const u8 {
    var code = self.allocator.alloc(u8, 11) catch return error.OutOfMemory;
    code[0] = 0x48; // REX.W
    code[1] = 0xB8; // MOV RAX, imm64
    @memcpy(code[2..10], std.mem.asBytes(&value));
    code[10] = 0xC3; // RET
    return code;
}
```

### 2. Compile-time Value System

**LLVM Backend:**
- No explicit compile-time value system
- Relies on LLVM's constant folding

**Dev Backend:**
- Has explicit `ComptimeHeap` for memory allocation
- `ComptimeValue` wraps bytes with layout info
- `ComptimeEnv` tracks bindings and closure references
- Supports child environments for scoping

```zig
// Dev Backend's compile-time value system
pub const ComptimeValue = struct {
    bytes: [*]u8,
    size: usize,
    layout_idx: LayoutIdx,

    pub fn as(self: ComptimeValue, comptime T: type) T { ... }
    pub fn set(self: ComptimeValue, comptime T: type, value: T) void { ... }
};
```

### 3. Lambda and Closure Support

**LLVM Backend:**
- Returns `UnsupportedType` for lambdas and closures
- Limited function call support

**Dev Backend:**
- Full lambda application support
- Closure support with captured values
- Higher-order function support via `bindArgumentToParam`
- Stores closures by expression reference

```zig
// Dev Backend supports closures
fn applyClosure(self: *DevEvaluator, module_env: *ModuleEnv, closure: CIR.Expr.Closure, ...) Error![]const u8 {
    // Restore captured values to environment
    const capture_indices = module_env.store.sliceCaptures(closure.captures);
    for (capture_indices) |capture_idx| {
        const capture = module_env.store.getCapture(capture_idx);
        // ... bind captured value to new environment
    }
    // Evaluate lambda body with new environment
    return self.generateCodeForExprWithEnv(module_env, body_expr, result_type, &new_env);
}
```

### 4. Low-Level Operations

**LLVM Backend:**
- Handles builtin calls via ident matching (`abs`, `negate`, `not`, `abs_diff`)
- Uses LLVM intrinsics for operations

**Dev Backend:**
- Has explicit `generateLowLevelCallCode` function
- Handles `LowLevel.Op` enum directly
- Explicit support for boolean ops, list operations
- Returns `UnsupportedExpression` for complex string operations

### 5. Object File Support

**LLVM Backend:**
- No explicit object file handling (LLVM manages this)

**Dev Backend:**
- Full object file writer infrastructure:
  - `ElfWriter` for Linux
  - `MachOWriter` for macOS
  - `CoffWriter` for Windows
- `Backend.zig` coordinates code gen + object writing

### 6. Error Handling

**LLVM Backend:**
```zig
pub const Error = error{
    OutOfMemory,
    CompilationFailed,
    UnsupportedType,
    UnsupportedLayout,
    ParseError,
    CanonicalizeError,
    TypeError,
};
```

**Dev Backend:**
```zig
pub const Error = error{
    OutOfMemory,
    CompilationFailed,
    UnsupportedType,
    UnsupportedExpression,  // More granular
    ParseError,
    CanonicalizeError,
    TypeError,
    JitError,
    NotImplemented,
    Crash,                   // Can store crash message
    RuntimeError,
};
```

Dev backend also stores crash messages:
```zig
crash_message: ?[]const u8 = null,

fn setCrashMessage(self: *DevEvaluator, message: []const u8) !void {
    self.crash_message = try self.allocator.dupe(u8, message);
}
```

### 7. Match Expression Support

**LLVM Backend:**
- Returns `UnsupportedType` for match expressions

**Dev Backend:**
- Full match expression support with pattern matching
- `patternMatches` function for pattern evaluation
- Supports underscore, num_literal, assign, applied_tag patterns

### 8. Code Size

- LLVM Backend: ~1700 lines
- Dev Backend: ~2700 lines (60% larger)

The dev backend is larger due to:
- Explicit machine code generation
- ComptimeValue system
- More expression support
- Pattern matching implementation

## Expression Support Comparison

| Expression | LLVM Backend | Dev Backend |
|------------|-------------|-------------|
| e_num | Yes | Yes |
| e_typed_int | Yes | Yes |
| e_typed_frac | Yes | Yes |
| e_frac_f32/f64 | Yes | Yes |
| e_dec | Yes | Yes |
| e_dec_small | Yes | Yes |
| e_binop | Yes | Yes |
| e_unary_minus | Yes | Yes |
| e_unary_not | Yes | Yes |
| e_if | Yes (single branch) | Yes (all branches) |
| e_match | No | Yes |
| e_block | Yes | Yes |
| e_lookup_local | Yes | Yes |
| e_lambda | No | Yes |
| e_closure | No | Yes |
| e_call | Limited | Full (including HOF) |
| e_low_level_lambda | No | Yes |
| e_tag | Limited | Yes |
| e_zero_argument_tag | Yes | Yes |
| e_list | Limited | Limited |
| e_empty_list | Yes | Yes |
| e_record | Yes | Yes |
| e_empty_record | Yes | Yes |
| e_dot_access | Yes | Yes |
| e_str | Limited | Limited |
| e_str_segment | Yes | Limited |
| e_nominal | Yes | Yes |
| e_nominal_external | Yes | Yes |
| e_dbg | No | Yes |
| e_crash | No | Yes |
| e_expect | No | Yes |
| e_for | No | Yes (returns unit) |

## Summary

### When to use LLVM Backend

- Simpler implementation, fewer lines of code
- Leverages LLVM's optimization passes
- Better for optimized code (`--opt=size`, `--opt=speed`)
- Platform-independent IR generation

### When to use Dev Backend

- Need closure and lambda support
- Need match expression support
- Need crash message handling
- Building full native compiler without LLVM dependency
- Need explicit control over code generation

### Convergence Opportunities

1. **Lambda/Closure support**: LLVM backend could adopt similar environment-based approach
2. **Match support**: Pattern matching logic could be shared
3. **Error handling**: LLVM backend could add crash message support
4. **Low-level ops**: Both could share the Op enum handling logic
