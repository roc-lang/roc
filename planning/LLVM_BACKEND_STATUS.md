# LLVM Backend Implementation Status

This document describes the current state of the LLVM backend implementation, how it works, and what remains to be done.

---

## Overview

The LLVM backend is partially implemented. It currently supports **numeric literals only** (integers, floats, decimals) and uses **JIT compilation** for REPL evaluation. The dual-mode snapshot testing infrastructure is in place and working, comparing interpreter output against LLVM output for all `type=repl` snapshot tests.

**Important distinction:**
- **REPL (`roc repl`)**: Uses JIT compilation via LLVM ORC - executes code directly in memory
- **Build (`roc build`)**: Will need traditional compilation (bitcode → object file → linker → executable) - NOT YET IMPLEMENTED

---

## Architecture

### File Structure

```
src/
├── llvm_compile/                    # Shared LLVM compilation module
│   ├── mod.zig                      # Module root, re-exports public API
│   ├── bindings.zig                 # LLVM C API bindings (extensive)
│   └── compile.zig                  # JIT compilation and execution
│
├── eval/
│   ├── llvm_evaluator.zig           # LLVM-based expression evaluator
│   └── interpreter.zig              # Existing interpreter (reference impl)
│
├── snapshot_tool/
│   └── main.zig                     # Dual-mode testing (runs both evaluators)
│
└── build.zig                        # Build configuration with LLVM support
```

### Data Flow (REPL/Snapshot Tests)

```
Roc Source Code
       ↓
   Parser (src/parse/)
       ↓
   Canonicalizer → CIR (Canonical IR)
       ↓
   Type Checker → assigns types
       ↓
   ┌─────────────────────────────────────────┐
   │         DUAL EVALUATION                 │
   │                                         │
   │  Interpreter          LLVM Evaluator    │
   │      ↓                      ↓           │
   │  Direct eval         Generate Bitcode   │
   │      ↓                      ↓           │
   │  String output       JIT Compile (ORC)  │
   │      ↓                      ↓           │
   │      ↓               Call roc_eval()    │
   │      ↓                      ↓           │
   │      └──────┬───────────────┘           │
   │             ↓                           │
   │      Compare outputs                    │
   │      (must match exactly)               │
   └─────────────────────────────────────────┘
```

---

## Component Details

### 1. `src/llvm_compile/bindings.zig`

This file contains comprehensive LLVM C API bindings, including:

**Core LLVM Types:**
- `Context`, `Module`, `Builder`, `Value`, `Type`
- `MemoryBuffer` for bitcode handling
- Target machine and target triple utilities

**ORC JIT Types (for REPL):**
- `OrcLLJIT` - The JIT compiler instance
- `OrcThreadSafeContext` / `OrcThreadSafeModule` - Thread-safe wrappers for JIT
- `OrcDefinitionGenerator` - For resolving symbols from the host process
- `OrcJITDylib` - Dynamic library abstraction within JIT

**Key Functions:**
- `createLLJIT()` - Create a JIT compiler instance
- `OrcLLJIT.lookup()` - Look up a symbol address after JIT compilation
- `OrcLLJIT.addLLVMIRModule()` - Add a module to the JIT
- `createDynamicLibrarySearchGeneratorForProcess()` - Allow JIT to find host symbols
- `Context.parseBitcodeInContext2()` - Parse bitcode into a module

**LLD Linker Functions (for future `roc build`):**
- `ZigLLDLinkMachO`, `ZigLLDLinkELF`, `ZigLLDLinkCOFF`, `ZigLLDLinkWasm`
- These are available but NOT used for REPL (due to Mach-O multi-invocation issues)

**Bitcode Compilation:**
- `compileBitcodeToObject()` - Compile bitcode to native object file

### 2. `src/llvm_compile/compile.zig`

Implements JIT-based execution for the REPL. Key function:

```zig
pub fn compileAndExecute(
    allocator: Allocator,
    bitcode: []const u32,
    is_float: bool,
) Error![]const u8
```

**How it works:**

1. **Initialize LLVM targets** - Required for JIT compilation
2. **Create ThreadSafeContext** - ORC requires thread-safe wrappers
3. **Parse bitcode** - Convert bitcode bytes into an LLVM Module
4. **Create LLJIT instance** - The ORC JIT compiler
5. **Add process symbol generator** - Allows JIT code to call libc functions if needed
6. **Add module to JIT** - Triggers compilation
7. **Look up `roc_eval` symbol** - Get the function address (uses `_roc_eval` on macOS)
8. **Call the function** - Cast address to function pointer and invoke
9. **Format result** - Convert return value (i64 or f64) to string

**Why JIT instead of file-based linking:**

LLD's Mach-O port has global state that corrupts when invoked multiple times in the same process. Since snapshot tests run thousands of evaluations, we use JIT which:
- Avoids the linker entirely
- Is faster (no filesystem I/O)
- Works reliably with repeated invocations

### 3. `src/eval/llvm_evaluator.zig`

Translates CIR expressions to LLVM bitcode. Key types and functions:

```zig
pub const LlvmEvaluator = struct {
    allocator: Allocator,
    counter: u64,  // For unique temp file names (legacy, not used with JIT)

    pub fn generateBitcodeFromSource(source: []const u8) Error!BitcodeResult
    pub fn generateBitcode(module_env: *ModuleEnv, expr: CIR.Expr) Error!BitcodeResult
};

pub const BitcodeResult = struct {
    bitcode: []const u32,
    is_float: bool,
    allocator: Allocator,

    pub fn deinit(self: *BitcodeResult) void
};
```

**How bitcode generation works:**

1. **Parse source** into CIR (if using `generateBitcodeFromSource`)
2. **Canonicalize and type-check** the expression
3. **Create Zig LLVM Builder** - Uses `std.zig.llvm.Builder` from Zig's standard library
4. **Determine expression type** - Map CIR type to LLVM type
5. **Emit value constant** - Create LLVM constant for the expression value
6. **Emit `roc_eval` function** - Simple function that returns the value directly:
   ```llvm
   define i64 @roc_eval() {
     ret i64 42
   }
   ```
7. **Serialize to bitcode** - Use `builder.toBitcode()`

**Currently supported CIR expressions:**

| CIR Expression | LLVM Handling |
|----------------|---------------|
| `e_num` | `builder.intConst()` with appropriate type (i8-i128) |
| `e_frac_f32` | `builder.floatConst()` |
| `e_frac_f64` | `builder.doubleConst()` |
| `e_dec` | Convert to f64, use `builder.doubleConst()` |
| `e_dec_small` | Convert to f64, use `builder.doubleConst()` |

**Symbol naming:**

On macOS, C symbols require underscore prefix. The code uses `\x01_roc_eval` in LLVM IR, where `\x01` tells LLVM to use the name verbatim (producing `_roc_eval` in the symbol table).

### 4. `src/snapshot_tool/main.zig`

The snapshot tool runs dual-mode testing for all `type=repl` snapshots. Relevant code is in `generateReplOutputSection()`:

```zig
// Run interpreter (existing behavior)
const interp_output = runInterpreter(input);

// Also run LLVM evaluator
var llvm_evaluator = LlvmEvaluator.init(allocator);
var bitcode_result = llvm_evaluator.generateBitcodeFromSource(input);
const llvm_output = llvm_compile.compileAndExecute(
    allocator,
    bitcode_result.bitcode,
    bitcode_result.is_float,
);

// Compare outputs
if (!std.mem.eql(u8, interp_output, llvm_output)) {
    // Report mismatch - this is a test failure
    std.debug.print("LLVM/Interpreter MISMATCH...");
}
```

### 5. `build.zig` Integration

LLVM support is conditionally enabled via the `enable_llvm` flag:

```zig
if (enable_llvm) {
    // Add LLVM library paths and includes
    snapshot_exe.addLibraryPath(.{ .cwd_relative = llvm_paths.lib });
    snapshot_exe.addIncludePath(.{ .cwd_relative = llvm_paths.include });

    // Link required LLVM libraries (LLVMCore, LLVMOrcJIT, etc.)
    try addStaticLlvmOptionsToModule(snapshot_exe.root_module);

    // Add the llvm_compile module
    snapshot_exe.root_module.addAnonymousImport("llvm_compile", .{
        .root_source_file = b.path("src/llvm_compile/mod.zig"),
    });
}
```

---

## What's Working Now

### Numeric Literals (All Types)

| Roc Type | Example | Status |
|----------|---------|--------|
| Unbound Int | `42` | ✅ Working |
| I8, I16, I32, I64 | `42i32` | ✅ Working |
| U8, U16, U32, U64 | `255u8` | ✅ Working |
| I128, U128 | Large integers | ✅ Working |
| F32 | `3.14f32` | ✅ Working |
| F64 | `3.14f64`, `3.14` | ✅ Working |
| Dec | `1.5dec` | ✅ Working |
| Hex literals | `0xFF` | ✅ Working |
| Binary literals | `0b1010` | ✅ Working |
| Underscore separators | `1_000_000` | ✅ Working |
| Scientific notation | `1.5e10` | ✅ Working |

### Testing Infrastructure

- ✅ Dual-mode testing runs automatically for all `type=repl` snapshots
- ✅ All 2146 tests pass with identical interpreter/LLVM output
- ✅ Mismatches are clearly reported with both outputs shown

---

## What's NOT Implemented

### Types

| Type | Status | Notes |
|------|--------|-------|
| Bool | ❌ | Need to handle `Bool.true`, `Bool.false` |
| Str | ❌ | Need string representation, SSO |
| List | ❌ | Need heap allocation, refcounting |
| Record | ❌ | Need struct layout |
| Tag Union | ❌ | Need discriminant + payload |
| Function | ❌ | Need function pointers or defunctionalization |
| Opaque | ❌ | Need to unwrap to underlying type |

### Expressions

| Expression | Status | Notes |
|------------|--------|-------|
| Arithmetic ops | ❌ | `+`, `-`, `*`, `/`, etc. |
| Comparison ops | ❌ | `==`, `<`, `>`, etc. |
| Boolean ops | ❌ | `&&`, `||`, `!` |
| If/then/else | ❌ | Need branching |
| Pattern matching | ❌ | Need switch/branch emission |
| Let bindings | ❌ | Need local variable handling |
| Function calls | ❌ | Need call emission |
| Lambda/closure | ❌ | Need closure transformation integration |
| Record access | ❌ | Need GEP instructions |
| Tag construction | ❌ | Need union construction |

### Builtins

None of the builtin functions are implemented:

- `List.map`, `List.get`, `List.len`, `List.append`, etc.
- `Str.concat`, `Str.len`, `Str.toUtf8`, etc.
- `Num.add`, `Num.sub`, `Num.mul`, `Num.div`, etc.
- `Bool.and`, `Bool.or`, `Bool.not`
- `dbg`, `crash`

### Features

| Feature | Status | Notes |
|---------|--------|-------|
| `--optimize` REPL flag | ❌ | Need CLI integration |
| `roc build` compilation | ❌ | Need non-JIT pipeline (see below) |
| Monomorphization integration | ❌ | Currently only handles simple expressions |
| Closure transformation | ❌ | Not integrated with LLVM emission |

---

## `roc build` vs REPL: Different Approaches Needed

### REPL (Current Implementation)

The REPL uses **JIT compilation**:

```
Source → CIR → Bitcode → JIT (ORC) → Execute in-process → Return value
```

This is ideal for REPL because:
- Fast iteration (no file I/O)
- Direct value return
- Works with repeated invocations

### `roc build` (NOT YET IMPLEMENTED)

For `roc build`, we need **traditional compilation**:

```
Source → CIR → Bitcode → Object File → Linker → Executable
```

This requires:

1. **Compile bitcode to object file:**
   ```zig
   bindings.compileBitcodeToObject(
       bitcode_bytes,
       target_triple,
       cpu,
       features,
       output_path,
       is_debug,
   )
   ```

2. **Link with LLD:**
   - For single-shot builds, LLD works fine (the multi-invocation issue only affects REPL)
   - Use `ZigLLDLinkMachO`, `ZigLLDLinkELF`, etc.
   - Or use Zig's self-hosted linker for better Mach-O support

3. **Generate a proper `main` function:**
   - Current code generates `roc_eval` which just returns a value
   - Need to generate `main` that initializes runtime, runs user code, handles I/O

4. **Runtime library:**
   - Need to link against Roc runtime for refcounting, allocations
   - Builtins implementation
   - Platform glue

### Key Differences

| Aspect | REPL (JIT) | Build (AOT) |
|--------|------------|-------------|
| Execution | In-process | Separate executable |
| Linking | None (JIT handles it) | LLD or system linker |
| Entry point | `roc_eval()` returns value | `main()` with full program |
| Runtime | Minimal (just compute value) | Full runtime needed |
| I/O | Host process stdout | Executable's own I/O |
| Speed | Fast iteration | One-time compilation |

---

## Implementation Recommendations

### Priority Order for Remaining Work

1. **Arithmetic operations** - Most impactful for testing coverage
2. **Bool type and comparisons** - Enables conditionals
3. **If/then/else** - Basic control flow
4. **Let bindings** - Local variables
5. **Function definitions/calls** - Required for non-trivial programs
6. **Strings** - Common in real programs
7. **Lists** - Common data structure
8. **Records and Tag Unions** - Full type system support
9. **Builtins** - Standard library functionality
10. **`roc build` pipeline** - Full compilation

### Testing Strategy

For each new feature:

1. Add REPL snapshot tests exercising the feature
2. Run `zig build snapshot` to verify dual-mode passes
3. Check for any edge cases in interpreter vs LLVM output

### Code Locations for Extensions

- **New types**: Add cases to `getExprLlvmType()` and `emitExprValue()` in `llvm_evaluator.zig`
- **New expressions**: Add cases to `emitExprValue()` switch statement
- **Complex code gen**: May need to use `WipFunction` for control flow (branches, loops)
- **Builtins**: May need to link against runtime library or emit inline LLVM

---

## Technical Notes

### Why ORC JIT Instead of MCJIT?

MCJIT is deprecated. ORC (On-Request Compilation) is the modern LLVM JIT API:
- Better support for lazy compilation
- Cleaner API for multiple modules
- Active development

### Symbol Naming Conventions

| Platform | C Symbol | LLVM IR Name |
|----------|----------|--------------|
| macOS | `_roc_eval` | `\x01_roc_eval` |
| Linux | `roc_eval` | `roc_eval` |
| Windows | `roc_eval` | `roc_eval` |

The `\x01` prefix tells LLVM to use the name verbatim without adding platform-specific decorations.

### Memory Management

Currently simple because we only handle constants:
- No heap allocations in generated code
- No refcounting needed
- JIT context is disposed after each evaluation

For full implementation, will need:
- Refcounting for heap-allocated types (Str, List)
- Proper cleanup on errors
- Integration with Roc's memory management runtime

---

## References

- `src/llvm_compile/bindings.zig` - All LLVM C API bindings
- `src/llvm_compile/compile.zig` - JIT execution implementation
- `src/eval/llvm_evaluator.zig` - CIR to bitcode translation
- [LLVM ORC Documentation](https://llvm.org/docs/ORCv2.html)
- [LLVM-C/LLJIT.h](https://llvm.org/doxygen/LLJIT_8h.html)
