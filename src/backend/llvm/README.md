# Roc LLVM Backend

This directory contains the LLVM code generation backend for the Roc compiler. It takes monomorphized IR and compiles it to native object code using LLVM.

## Overview

The LLVM backend follows this pipeline:

```
Monomorphized IR  →  LLVM IR Builder  →  LLVM Bitcode  →  Object Code (.o)  →  Executable
```

1. **Monomorphized IR**: The input from the Roc compiler's earlier stages (type checking, monomorphization)
2. **LLVM IR Builder** (`Builder.zig`): Constructs LLVM IR in memory
3. **Bitcode Writer** (`bitcode_writer.zig`): Serializes the IR to LLVM bitcode format
4. **LLVM Backend** (`bindings.zig`): Parses bitcode and compiles to native object code
5. **Linker**: Links object code with platform host to produce final executable

## Files

| File | Description |
|------|-------------|
| `Builder.zig` | LLVM IR builder - constructs LLVM modules, functions, basic blocks, and instructions in memory |
| `ir.zig` | LLVM IR data structures, opcodes, and type definitions |
| `bitcode_writer.zig` | Serializes Builder's in-memory IR to LLVM bitcode binary format |
| `BitcodeReader.zig` | Parses LLVM bitcode (for debugging/verification) |
| `bindings.zig` | Zig bindings to LLVM C API for compiling bitcode to object code |

## Building with LLVM

LLVM is always enabled. The build system will automatically download pre-built LLVM libraries from roc-bootstrap for supported platforms.

```bash
# Standard build (downloads LLVM automatically)
zig build

# Use a custom LLVM installation
zig build -Dllvm-path=/path/to/llvm
```

### Supported Platforms

Pre-built LLVM is available for:
- `x86_64-linux-musl` / `x86_64-linux-gnu`
- `aarch64-linux-musl` / `aarch64-linux-gnu`
- `x86_64-macos`
- `aarch64-macos`
- `x86_64-windows`
- `aarch64-windows`

For other platforms, use `-Dllvm-path` to specify a custom LLVM installation.

## Usage: Compiling Monomorphized IR to Executable

### Step 1: Generate LLVM IR from Monomorphized IR

The `Builder.zig` module constructs LLVM IR. Here's the general pattern:

```zig
const llvm = @import("backend/llvm/Builder.zig");
const ir = @import("backend/llvm/ir.zig");

// Create a new LLVM module builder
var builder = llvm.Builder.init(allocator);
defer builder.deinit();

// Set target triple (e.g., "x86_64-linux-gnu", "aarch64-macos-none")
builder.setTargetTriple(target_triple);

// Define types
const i64_type = builder.intType(64);
const void_type = builder.voidType();

// Create a function
const main_fn = builder.addFunction(
    "main",                           // function name
    builder.functionType(i64_type, &.{}),  // return i64, no params
);

// Create entry basic block
const entry_block = builder.appendBasicBlock(main_fn, "entry");
builder.positionAtEnd(entry_block);

// Add instructions
const result = builder.buildAdd(lhs, rhs, "sum");
builder.buildRet(result);
```

### Step 2: Serialize to Bitcode

Once the LLVM module is built, serialize it to bitcode:

```zig
const bitcode_writer = @import("backend/llvm/bitcode_writer.zig");

// Get bitcode as bytes
const bitcode = builder.toBitcode(allocator);
defer allocator.free(bitcode);

// Or write directly to a file
try builder.writeBitcodeToFile("output.bc");
```

### Step 3: Compile Bitcode to Object Code

Use the `bindings.zig` module to compile bitcode to a native object file:

```zig
const bindings = @import("backend/llvm/bindings.zig");

// High-level convenience function
const error_msg = bindings.compileBitcodeToObject(
    bitcode,                    // []const u8 - LLVM bitcode bytes
    "x86_64-linux-gnu",         // target triple
    "generic",                  // CPU name (or null for default)
    null,                       // CPU features (or null)
    "output.o",                 // output object file path
    false,                      // is_debug (false = optimized)
);

if (error_msg) |msg| {
    std.debug.print("LLVM error: {s}\n", .{std.mem.span(msg)});
    return error.LlvmCompilationFailed;
}
```

### Step 4: Link to Executable

Link the object file with the platform host library to create the final executable:

```zig
const bindings = @import("backend/llvm/bindings.zig");

// Using LLD (LLVM's linker) for ELF targets
const args = [_:null]?[*:0]const u8{
    "ld.lld",
    "output.o",
    "libhost.a",           // platform host library
    "-o", "my_program",
    null,
};
const success = bindings.LinkELF(
    args.len - 1,
    &args,
    false,  // can_exit_early
    false,  // disable_output
);

if (!success) {
    return error.LinkFailed;
}
```

For different platforms, use the appropriate linker:
- **Linux**: `bindings.LinkELF`
- **macOS**: `bindings.LinkMachO`
- **Windows**: `bindings.LinkCOFF`
- **WebAssembly**: `bindings.LinkWasm`

## Low-Level API

For more control, use the lower-level bindings directly:

```zig
const bindings = @import("backend/llvm/bindings.zig");

// Initialize all LLVM targets
bindings.initializeAllTargets();

// Create memory buffer from bitcode
const mem_buf = bindings.MemoryBuffer.createMemoryBufferWithMemoryRange(
    bitcode.ptr,
    bitcode.len,
    "module_name",
    bindings.Bool.False,
);
defer mem_buf.dispose();

// Create LLVM context
const context = bindings.Context.create();
defer context.dispose();

// Parse bitcode into module
var module: *bindings.Module = undefined;
if (context.parseBitcodeInContext2(mem_buf, &module).toBool()) {
    return error.BitcodeParseError;
}
defer module.dispose();

// Get target from triple
var target: *bindings.Target = undefined;
var error_msg: [*:0]const u8 = undefined;
if (bindings.Target.getFromTriple("x86_64-linux-gnu", &target, &error_msg).toBool()) {
    std.debug.print("Target error: {s}\n", .{std.mem.span(error_msg)});
    return error.InvalidTarget;
}

// Create target machine with full control over options
const target_machine = bindings.TargetMachine.create(
    target,
    "x86_64-linux-gnu",    // triple
    "generic",             // CPU
    null,                  // features
    .Default,              // optimization level: None, Less, Default, Aggressive
    .PIC,                  // relocation mode: Default, Static, PIC, DynamicNoPIC
    .Default,              // code model: Default, Small, Kernel, Medium, Large
    true,                  // function_sections (for linker GC)
    true,                  // data_sections (for linker GC)
    .Default,              // float ABI: Default, Soft, Hard
    null,                  // ABI name
    false,                 // emulated TLS
);
defer target_machine.dispose();

// Configure emit options
const emit_options = bindings.TargetMachine.EmitOptions{
    .is_debug = false,
    .is_small = false,           // true for -Oz optimization
    .time_report_out = null,
    .tsan = false,               // thread sanitizer
    .sancov = false,             // sanitizer coverage
    .lto = .None,                // LTO phase
    .allow_fast_isel = false,
    .allow_machine_outliner = true,
    .asm_filename = null,        // set to emit assembly
    .bin_filename = "output.o",  // object file output
    .llvm_ir_filename = null,    // set to emit LLVM IR text
    .bitcode_filename = null,    // set to emit bitcode
    .coverage = .{...},          // coverage options
};

// Emit object code
var emit_error: [*:0]const u8 = undefined;
if (target_machine.emitToFile(module, &emit_error, &emit_options)) {
    std.debug.print("Emit error: {s}\n", .{std.mem.span(emit_error)});
    return error.EmitFailed;
}
```

## Target Triples

Common target triples for Roc:

| Platform | Triple |
|----------|--------|
| Linux x86_64 | `x86_64-linux-gnu` or `x86_64-linux-musl` |
| Linux ARM64 | `aarch64-linux-gnu` or `aarch64-linux-musl` |
| macOS x86_64 | `x86_64-macos-none` |
| macOS ARM64 | `aarch64-macos-none` |
| Windows x86_64 | `x86_64-windows-msvc` |
| WebAssembly | `wasm32-freestanding-none` |

## Optimization Levels

The `CodeGenOptLevel` enum controls LLVM's optimization:

| Level | Description |
|-------|-------------|
| `.None` | No optimization (fastest compile, -O0) |
| `.Less` | Basic optimizations (-O1) |
| `.Default` | Standard optimizations (-O2) |
| `.Aggressive` | Maximum optimizations (-O3) |

For size optimization, set `emit_options.is_small = true` to get `-Oz` behavior.

## Debugging

To debug LLVM IR generation:

1. **Emit LLVM IR text**: Set `emit_options.llvm_ir_filename = "output.ll"` to get human-readable IR
2. **Emit bitcode**: Set `emit_options.bitcode_filename = "output.bc"` to inspect with `llvm-dis`
3. **Use debug builds**: Set `is_debug = true` to preserve debug info and disable optimizations

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     Roc Compiler Frontend                        │
│  (Parsing → Canonicalization → Type Checking → Monomorphization) │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
                    ┌───────────────────────┐
                    │   Monomorphized IR    │
                    └───────────────────────┘
                                │
                                ▼
┌─────────────────────────────────────────────────────────────────┐
│                      LLVM Backend (this directory)               │
│  ┌─────────────┐    ┌────────────────┐    ┌─────────────────┐  │
│  │ Builder.zig │ →  │bitcode_writer  │ →  │  bindings.zig   │  │
│  │ (Build IR)  │    │(Serialize)     │    │(Compile to .o)  │  │
│  └─────────────┘    └────────────────┘    └─────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
                                │
                                ▼
                    ┌───────────────────────┐
                    │    Object File (.o)   │
                    └───────────────────────┘
                                │
                                ▼
                    ┌───────────────────────┐
                    │   LLD Linker          │
                    │ + Platform Host (.a)  │
                    └───────────────────────┘
                                │
                                ▼
                    ┌───────────────────────┐
                    │  Final Executable     │
                    └───────────────────────┘
```

## Attribution

The LLVM bitcode generation code has been adapted from the Zig compiler at https://codeberg.org/ziglang/zig and licensed under the MIT license. Thanks, Zig team!
