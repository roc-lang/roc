# Target Architecture Documentation

This document defines the different machines and targets involved in building and running the Roc compiler, and compiling Roc programs. Understanding these distinctions is critical for correct cross-compilation support.

## Machines and Targets

There are six distinct machine contexts to consider:

### 1. Build Host

The machine that compiles the Roc CLI itself (runs `zig build`).

- **When it matters**: Build scripts, compile-time constants in build.zig
- **In code**: Available via `@import("builtin")` during Roc CLI compilation
- **Example**: Your development MacBook running `zig build`

### 2. CLI Target

The architecture the Roc CLI binary is compiled FOR. This determines what machine can run the resulting `roc` binary.

- **When it matters**: Cross-compiling the Roc CLI itself (rare)
- **Usually**: Same as Build Host (native compilation)
- **In code**: The `-Dtarget=` flag to `zig build`, or native by default
- **Example**: Building the CLI on x64 Linux to run on arm64 Linux

### 3. Shim Target

The architecture each embedded interpreter shim is compiled FOR. The Roc CLI embeds multiple pre-compiled interpreter shims for different targets.

- **When it matters**: The interpreter shim's own memory layout and operations
- **In code**: `@import("builtin").cpu.arch` inside interpreter shim code
- **Key insight**: When a shim runs, `builtin.cpu.arch` reflects what it was compiled FOR
- **Examples**:
  - Native shim: compiled for CLI Target (runs directly)
  - wasm32-freestanding shim: compiled for wasm32 (runs in wasm runtime)

### 4. CLI Runtime

The machine actually executing the Roc CLI.

- **When it matters**: Runtime behavior, file I/O, process spawning
- **Usually**: Same as CLI Target
- **In code**: Runtime detection via `std.Target.current` or OS APIs
- **Example**: The CI server running `roc build`

### 5. Compilation Target

The target architecture for the Roc APPLICATION being compiled. This is what the user specifies when they want to cross-compile their Roc program.

- **When it matters**: Generated code layout, struct sizes, pointer widths
- **In code**: `--target=` flag to `roc build`, represented as `RocTarget`
- **Examples**:
  - `roc build --target=wasm32 app.roc` → Compilation Target is wasm32
  - `roc build app.roc` → Compilation Target defaults to first compatible as specified by the platform header, usually a native executable (CLI Runtime) but could build a static or shared library e.g. `app.wasm`.

### 6. App Runtime

The machine/environment where the compiled Roc application will eventually execute.

- **When it matters**: The generated binary's behavior
- **Relationship**: Must be compatible with Compilation Target
- **Examples**:
  - wasm32 target → runs in browser or wasm runtime
  - x64linux target → runs on x64 Linux machines

## Key Relationships

```
Build Host ──compiles──▶ CLI Target (Roc CLI binary)
                              │
                              │ embeds multiple
                              ▼
                    ┌─────────────────────┐
                    │  Interpreter Shims  │
                    │  - native shim      │◀── Shim Target (each has its own)
                    │  - wasm32 shim      │
                    │  - ...              │
                    └─────────────────────┘
                              │
                              │ runs on
                              ▼
                    CLI Runtime (user's machine)
                              │
                              │ compiles Roc app for
                              ▼
                    Compilation Target (--target flag)
                              │
                              │ produces binary for
                              ▼
                    App Runtime (where app runs)
```

## Common Scenarios

### Native Development (Most Common)

All targets are the same architecture:
- Build Host = CLI Target = CLI Runtime = Compilation Target = App Runtime
- Example: Developing on arm64 Mac, building apps to run on arm64 Mac

### Cross-Compiling Roc Apps

CLI runs natively, but compiles for a different target:
- Build Host = CLI Target = CLI Runtime (e.g., x64 Linux)
- Compilation Target = App Runtime (e.g., wasm32)
- Example: `roc build --target=wasm32 app.roc` on a Linux server

### Cross-Compiling the CLI Itself

Building the Roc CLI for a different machine:
- Build Host (e.g., x64 Linux CI) ≠ CLI Target (e.g., arm64 Mac)
- Example: CI building release binaries for multiple platforms
