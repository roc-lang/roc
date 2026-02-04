# External Integrations

**Analysis Date:** 2026-01-20

## APIs & External Services

**External APIs:**
- None detected - This is a compiler, not a networked application

**Note:** The CLI supports URL-based platform specs (`http`/`https` prefixes in `src/cli/main.zig`) for fetching remote packages, but this is user-initiated package retrieval, not a service integration.

## Data Storage

**Databases:**
- None - No database dependencies

**File Storage:**
- Local filesystem only
- Filesystem abstraction: `src/fs/Filesystem.zig`
- Cache management: `src/compile/cache_manager.zig`

**Caching:**
- File-based compilation cache (`src/compile/cache_manager.zig`)
- No external caching services

## Native Tool Integrations

**LLVM (Code Generation):**
- Purpose: Backend code generation and optimization
- Zig bindings: `src/llvm_compile/bindings.zig`
- C++ wrapper: `src/build/zig_llvm.cpp`, `src/build/zig_llvm.h`
- Functions: bitcode emission, JIT compilation, linking
- Required: Yes, for producing executables

**LLD (Linker):**
- Purpose: Link object files into executables
- Integration: Via LLVM C++ wrapper (`src/build/zig_llvm.cpp`)
- Functions: `ZigLLDLink*` in zig_llvm.cpp

**Tracy Profiler (Optional):**
- Purpose: Performance profiling during development
- Zig imports: `@import("tracy")` in many modules
- Shutdown helper: `src/build/tracy-shutdown.cpp`
- Build flag: `-Dtracy=$HOME/vendor/tracy`
- Required: No, optional for profiling

**Clang (Fallback):**
- Purpose: Fallback compiler if embedded LLVM unavailable
- Detection: `builder.isLLVMAvailable()` check in `src/cli/main.zig`
- Required: No, only as fallback

## Authentication & Identity

- Not applicable - This is a compiler tool

## Monitoring & Observability

**Error Tracking:**
- None - Errors reported to stdout/stderr

**Profiling:**
- Tracy profiler (optional, build-time flag)
- Trace flags: `-Dtrace-eval`, `-Dtrace-refcount`, `-Dtrace-modules`

**Logs:**
- stdout/stderr only
- No external logging service

## CI/CD & Deployment

**CI Pipeline:**
- Scripts referenced: `./ci/zig_lints.sh`
- CI checks: `zig build fmt && ./ci/zig_lints.sh && zig build && zig build snapshot && zig build test`

**Development Environment:**
- Nix flake: `flake.nix`
- Dev shell provides: zig, zls, git, elfutils, pkg-config, curl, zlib

## Environment Configuration

**Development:**
- No required environment variables
- Nix flake for reproducible environment
- Build flags for debugging/tracing

**Production:**
- Self-contained binary after build
- No runtime configuration needed

## Platform APIs (OS Integration)

**Windows:**
- kernel32 externs: `src/ipc/platform.zig`

**POSIX:**
- mmap/munmap: `src/ipc/platform.zig`

**Purpose:** Inter-process communication and memory mapping for compiler internals

---

*Integration audit: 2026-01-20*
*Update when adding/removing external services*
