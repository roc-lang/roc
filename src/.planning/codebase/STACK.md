# Technology Stack

**Analysis Date:** 2026-01-20

## Languages

**Primary:**
- Zig (0.15.x) - All compiler implementation code in `src/`

**Secondary:**
- C++ - LLVM wrappers and Tracy profiler shutdown helpers (`src/build/zig_llvm.cpp`, `src/build/tracy-shutdown.cpp`)
- C - LLVM C API bindings (`src/build/zig_llvm.h`)
- Nix - Development environment configuration (`flake.nix`)
- Markdown - Documentation and snapshot tests

**Legacy (do not modify):**
- Rust - Legacy compiler prototype in `crates/` - read-only reference

## Runtime

**Environment:**
- Zig 0.15.x (development version visible in `.direnv` profile)
- LLVM and LLD required for code generation backend
- No external runtime dependencies for compiled output

**Build System:**
- Zig build system via `build.zig` (at repository root)
- `build.zig.zon` for dependency management
- Nix flake for reproducible dev environments (`flake.nix`)

## Frameworks

**Core:**
- None - vanilla Zig with standard library

**Testing:**
- Zig built-in test framework (`std.testing`)
- Custom snapshot testing tool (`src/snapshot_tool/main.zig`)
- Markdown-based snapshot files in `test/snapshots/`

**Build/Dev:**
- Zig build system - compilation, testing, formatting
- Tracy profiler integration (optional, via `-Dtracy` flag)

## Key Dependencies

**Critical (Native/External):**
- LLVM - Code generation backend (`src/llvm_compile/bindings.zig`, `src/backend/llvm/`)
- LLD - Linker integration (`src/build/zig_llvm.cpp`)
- Tracy - Performance profiling (optional, `src/build/tracy-shutdown.cpp`)

**Infrastructure:**
- Zig standard library (`std.*`) - file I/O, memory, collections
- OS platform APIs - Windows kernel32, POSIX mmap/munmap (`src/ipc/platform.zig`)

**Dev Environment (via Nix):**
- zls - Zig Language Server
- git
- elfutils, pkg-config, curl, zlib (for kcov/coverage on Linux)

## Configuration

**Build Flags:**
- `-Dno-bin` - Skip binary generation (fast feedback)
- `-fincremental` - Incremental compilation
- `-Dtrace-eval=true` - Evaluation tracing
- `-Dtrace-refcount=true` - Reference count tracing
- `-Dtrace-modules=true` - Module tracing
- `-Doptimize=ReleaseFast` - Release optimization
- `-Dtracy=$HOME/vendor/tracy` - Tracy profiler path

**Environment:**
- No required environment variables
- Configuration via CLI flags and build options
- Nix flake for reproducible dev shells (`flake.nix`)

**Build Config Files:**
- `build.zig` (repository root) - Main build configuration
- `build.zig.zon` (repository root) - Dependencies
- `flake.nix` - Nix development environment

## Platform Requirements

**Development:**
- Linux, macOS, or Windows with Zig 0.15.x
- LLVM/LLD for full code generation
- Optional: Nix for reproducible environment

**Production:**
- Distributed as compiled binary (`roc`)
- Self-contained after build (links LLVM/LLD statically or dynamically)
- Targets: x86_64, aarch64 (native backends in `src/backend/dev/`)

---

*Stack analysis: 2026-01-20*
*Update after major dependency changes*
