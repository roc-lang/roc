# Roc Run Design Document

## Overview

The `roc` command (without sub-command) implements a system for executing Roc programs by linking the platform host with a Roc interpreter shim.

This design enables fast execution by caching linked executables and using inter-process communication to pass the path to the application for the interpreter.

The current implementation is mostly a stub, but it's design validates the overall architecture and provides a foundation for implementing the full functionality.

## Architecture

### Build-Time Components

1. **Example Host Static Library (`libplatform_host_str_simple.a`)**
   - Created at build time using `zig build-lib`
   - Contains the main executable logic that calls into Roc
   - Expects an external symbol `roc_entrypoint` to be linked in
   - Source: `src/platform_host_str_simple.zig`
   - Installed to `zig-out/lib/libplatform_host_str_simple.a`
   - Placeholder, will be replaced in future with platform specific implementations, used for testing purposes.

2. **Roc Runtime Static Library (`libread_roc_file_path_shim.a`)**
   - Created at build time using `zig build-lib`
   - Contains the `roc_entrypoint` symbol implementation
   - Reads string data from POSIX shared memory (which is provided by the parent process)
   - Source: `src/read_roc_file_path_shim.zig`
   - Compiled to static library at build time and embedded as resource in binary
   - Extracted to cache directory when needed for linking

### Runtime Components

1. **Cache Management**
   - Uses compiler cache system to store linked executables
   - Cache key based on Roc file path hash
   - Cache location: `~/.cache/roc/[version]/entries/executables/`
   - Can be bypassed with `--no-cache` flag

2. **Inter-Process Communication**
   - Uses POSIX shared memory (`shm_open`, `mmap`)
   - Shared memory name: `/ROC_FILE_TO_INTERPRET`
   - Format: `[usize length][u8... data]`
   - Parent writes data, child reads data
   - Important: Must unlink existing shared memory before creating new

3. **Dynamic Linking**
   - Uses LLD (LLVM Linker) to combine `libplatform_host_str_simple.a` + `libread_roc_file_path_shim.a`
   - Creates platform-specific executable
   - Links at runtime only if not cached

### Roc run new flag

- `--no-cache`: Bypass the executable cache and force recompilation
  - Useful during development when the shim or host library changes
  - Forces extraction and recompilation of embedded sources
  - Executable is still saved to cache for future use

### Examples
```bash
# Run with default settings (uses cache if available)
roc app.roc

# Force recompilation, bypassing cache
roc --no-cache app.roc

# Run with arguments passed to the Roc application
roc app.roc -- arg1 arg2 arg3
```

## File Structure

```
roc/
├── src/
│   ├── platform_host_str_simple.zig  # Host static library source
│   ├── read_roc_file_path_shim.zig   # Roc runtime object source
│   └── main.zig                      # CLI implementation
└── build.zig                         # Build configuration
```

## Symbol Interface

### Host Library (`libplatform_host_str_simple.a`)
- **Expects**: `extern fn roc_entrypoint(roc_alloc: *const fn (size: usize, alignment: u32) callconv(.C) ?*anyopaque) RocStr`
- **Provides**: `main()` function that calls `roc_entrypoint` and prints result using RocStr

### Roc Runtime (`libread_roc_file_path_shim.a`)
- **Exports**: `roc_entrypoint(roc_alloc: *const fn (size: usize, alignment: u32) callconv(.C) ?*anyopaque) RocStr`
- **Functionality**: Reads string from shared memory, returns a RocStr (supports both small and large strings)

## RocStr Implementation Details

The `RocStr` type is a key data structure used for string handling between the host and runtime:

```zig
const RocStr = extern struct {
    bytes: ?[*]u8,
    length: usize,
    capacity_or_alloc_ptr: usize,
};
```

## Platform Support

- **macOS**: Uses Mach-O format and system linker flags
- **Linux**: Uses ELF format and appropriate linker configuration
- **Windows**: Uses COFF format with Windows-specific linking
