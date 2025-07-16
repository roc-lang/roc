# Roc Run Design Document

## Overview

The `roc run` command implements a system for executing Roc programs by linking a host static library with a Roc runtime object file. This design enables fast execution by caching linked executables and using inter-process communication for data exchange.

## Architecture

### Build-Time Components

1. **Host Static Library (`host.a`)**
   - Created at build time using `zig build-lib`
   - Contains the main executable logic that calls into Roc
   - Expects an external symbol `_roc_entrypoint` to be linked in
   - Source: `src/host.c`
   - Installed to `zig-out/lib/libhost.a`

2. **Roc Runtime Object File (`shared_memory.o`)**
   - Created at runtime using `zig build-obj`
   - Contains the `_roc_entrypoint` symbol implementation
   - Reads string data from POSIX shared memory
   - Source: `src/shared_memory.zig` (embedded as source in binary)
   - Compiled to object file on first run

### Runtime Components

1. **Cache Management**
   - Uses compiler cache system to store linked executables
   - Cache key based on Roc file path hash
   - Cache location: `~/.cache/roc/[version]/entries/executables/`

2. **Inter-Process Communication**
   - Uses POSIX shared memory (`shm_open`, `mmap`)
   - Shared memory name: `/ROC_FILE_TO_INTERPRET`
   - Format: `[usize length][u8... data]`
   - Parent writes data, child reads data
   - Important: Must unlink existing shared memory before creating new

3. **Dynamic Linking**
   - Uses LLD (LLVM Linker) to combine `host.a` + `shared_memory.o`
   - Creates platform-specific executable
   - Links at runtime only if not cached

## Process Flow

### 1. Cache Check
```
roc run example.roc
├── Generate cache key from file path hash (CRC32)
├── Check if executable exists at cache path
│   └── Path: ~/.cache/roc/[version]/entries/executables/roc_run_[hash]
└── If found: Skip to step 3 (no linking needed)
```

### 2. Runtime Linking (Cache Miss)
```
├── Copy pre-built host.a from zig-out/lib/libhost.a to cache
├── Compile shared_memory.zig to object file in cache
│   └── Uses: zig build-obj with embedded source
├── Use LLD to link: host.a + shared_memory.o → executable
└── Executable stored at cache path for future runs
```

### 3. Inter-Process Communication Setup
```
├── Unlink any existing shared memory object
├── Create new shared memory: shm_open("/ROC_FILE_TO_INTERPRET", ...)
├── Set size with ftruncate
├── Map memory with mmap
├── Write data: [length: usize][data: u8...]
└── Keep shared memory open during child execution
```

### 4. Child Process Execution
```
├── Execute cached/linked binary as child process
├── Child calls _roc_entrypoint()
├── _roc_entrypoint reads from shared memory
├── Child prints result to stdout
└── Parent captures and displays output
```

### 5. Cleanup
```
├── Unmap shared memory (munmap)
├── Close shared memory file descriptor
├── Unlink shared memory object (shm_unlink)
└── Cached executable remains for future runs
```

## Key Benefits

1. **Performance**: Cached executables avoid re-linking on subsequent runs
2. **Portability**: Uses Zig's cross-compilation for consistent builds
3. **Separation**: Clear boundary between host (C) and runtime (Zig) code
4. **Extensibility**: Host library can be replaced by third parties

## File Structure

```
roc/
├── src/
│   ├── host.c                 # Host static library source
│   ├── shared_memory.zig      # Roc runtime object source
│   └── main.zig              # CLI implementation
├── build.zig                 # Build configuration
└── ROC_RUN_DESIGN.md         # This document
```

## Symbol Interface

### Host Library (`host.a`)
- **Expects**: `extern char* _roc_entrypoint(void)`
- **Provides**: `main()` function that calls `_roc_entrypoint` and prints result

### Roc Runtime (`shared_memory.o`)
- **Exports**: `_roc_entrypoint() -> ?[*:0]u8`
- **Functionality**: Reads string from shared memory, returns null-terminated C string

## Future Enhancements

1. **Host Library Replacement**: Third parties can provide their own `host.a`
2. **Multiple Communication Channels**: Support for complex data types beyond strings
3. **Cache Invalidation**: Automatic cache cleanup based on age/size
4. **Performance Monitoring**: Metrics for cache hits/misses and execution times

## Error Handling

- **Link Failures**: Graceful error reporting if LLD fails
- **Memory Allocation**: Proper cleanup on allocation failures
- **Shared Memory**: Handle permission errors (errno 13 on macOS)
- **Child Process**: Proper handling of child process failures
- **macOS Specifics**: Must unlink shared memory before creation

## Platform Support

- **macOS**: Uses Mach-O format and system linker flags
- **Linux**: Uses ELF format and appropriate linker configuration
- **Windows**: Uses COFF format with Windows-specific linking

This design provides a solid foundation for the `roc run` command while maintaining flexibility for future enhancements.