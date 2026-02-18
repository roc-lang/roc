# CLI Directory Reorganization Plan

This document outlines future work to reorganize the CLI module for better maintainability and idiomatic Zig conventions.

## Design Principles

1. **Flat structure** - No subdirectories, use descriptive filenames to group related code
2. **TitleCase.zig** for files defining a single primary type (e.g., `CliContext.zig`)
3. **snake_case.zig** for namespace/function modules (e.g., `cli_args.zig`)
4. **cli_roc_* prefix** for command implementation files to avoid conflicts
5. Direct imports rather than re-export hubs

## Current State

The CLI module is functional but `main.zig` is ~5,500 lines containing all command implementations.

## Future Structure

```
src/cli/
├── main.zig                      # Slim entrypoint (~300 lines): dispatch only
│
├── CliContext.zig                # Main CLI context type (DONE)
├── CliProblem.zig                # Runtime error types (DONE)
├── cli_args.zig                  # Argument parsing (ArgProblem renamed, DONE)
│
├── cli_roc_run.zig               # rocRun command implementation
├── cli_roc_build.zig             # rocBuild command implementation
├── cli_roc_check.zig             # rocCheck command implementation
├── cli_roc_test.zig              # rocTest command implementation
├── cli_roc_format.zig            # rocFormat command implementation
├── cli_roc_docs.zig              # rocDocs command implementation
├── cli_roc_bundle.zig            # rocBundle command implementation
├── cli_roc_unbundle.zig          # rocUnbundle command implementation
│
├── platform_resolution.zig       # Platform spec parsing, path resolution
├── platform_cache.zig            # getRocCacheDir, URL platform resolution
├── platform_validation.zig       # Platform header validation (existing)
│
├── compile_serialization.zig     # compileAndSerializeModulesForEmbedding
├── compile_shared_memory.zig     # POSIX/Windows shared memory
│
├── builder.zig                   # LLVM bitcode compilation (existing)
├── linker.zig                    # LLD wrapper (existing)
├── platform_host_shim.zig        # LLVM shim generation (existing)
│
├── target.zig                    # Target definitions (existing)
├── targets_validator.zig         # Targets section validation (existing)
├── cross_compilation.zig         # Cross-compilation validation (existing)
├── libc_finder.zig               # Linux libc detection (existing)
│
├── util_windows.zig              # Windows console functions
├── util_posix.zig                # POSIX shm wrappers
├── util_timing.zig               # formatElapsedTime
│
├── bench.zig                     # Benchmarking utility (existing)
├── targets/                      # Pre-compiled shim libraries (keep)
└── test/                         # Test files (keep)
```

## Migration Phases

### Phase 1: Extract Utilities from main.zig
1. Create `util_windows.zig` - Windows console functions
2. Create `util_posix.zig` - POSIX shm wrappers
3. Create `util_timing.zig` - formatElapsedTime, printTimingBreakdown

### Phase 2: Extract Compilation Infrastructure
1. Create `compile_shared_memory.zig` - SharedMemoryHandle, write functions
2. Create `compile_serialization.zig` - compileAndSerializeModulesForEmbedding

### Phase 3: Extract Platform Resolution
1. Create `platform_resolution.zig` - extractPlatformSpecFromApp, resolvePlatformPaths
2. Create `platform_cache.zig` - getRocCacheDir, resolveUrlPlatform

### Phase 4: Extract Commands (Ordered by Complexity)
1. `cli_roc_format.zig` (simplest)
2. `cli_roc_unbundle.zig`
3. `cli_roc_bundle.zig`
4. `cli_roc_test.zig`
5. `cli_roc_check.zig`
6. `cli_roc_docs.zig`
7. `cli_roc_build.zig`
8. `cli_roc_run.zig` (most complex)

### Phase 5: Slim main.zig
Reduce main.zig to ~300 lines containing only:
- Entry point
- Command dispatch
- Top-level error handling
