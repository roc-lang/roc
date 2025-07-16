# Roc Run Design Document

## Overview

The `roc` command (without sub-command) implements a system for executing Roc programs by linking a host static library with a Roc runtime object file. This design enables fast execution by caching linked executables and using inter-process communication for data exchange.

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
   - Uses LLD (LLVM Linker) to combine `libplatform_host_str_simple.a` + `read_roc_file_path_shim.o`
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
├── Copy pre-built libplatform_host_str_simple.a from zig-out/lib/libplatform_host_str_simple.a to cache
├── Extract embedded libread_roc_file_path_shim.a to cache directory
│   └── Uses: embedded static library resource
├── Use LLD to link: libplatform_host_str_simple.a + libread_roc_file_path_shim.a → executable
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

## Command Line Options

### Basic Usage
```
roc [OPTIONS] <file.roc> [-- <args>...]
```

### Options

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
├── build.zig                         # Build configuration
└── ROC_RUN_DESIGN.md                 # This document
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

### Small String Optimization

- **Small strings** (≤ 23 bytes): Data stored directly in the struct
  - String bytes stored from the beginning of the struct
  - Length stored in the last byte (byte 23) XORed with 0x80
  - Identified by high bit set in `capacity_or_alloc_ptr`
  - No heap allocation required

- **Large strings** (> 23 bytes): Data stored on heap
  - `bytes` points to heap-allocated memory
  - `length` contains the actual string length
  - `capacity_or_alloc_ptr` contains the allocation capacity
  - Memory allocated using provided `roc_alloc` function

### Key Methods

- `isSmallStr()`: Checks if high bit of `capacity_or_alloc_ptr` is set
- `asU8ptr()`: Returns pointer to string data (struct itself for small, heap for large)
- `setLen()`: Sets length appropriately based on string type
- `asSlice()`: Returns a slice view of the string data

## Error Handling

- **Link Failures**: Graceful error reporting if LLD fails
- **Memory Allocation**: Proper cleanup on allocation failures
- **Shared Memory**: Handle permission errors (errno 13 on macOS)
- **Child Process**: Proper handling of child process failures
- **Library Extraction**: Handle file system errors when extracting embedded libraries
- **macOS Specifics**: Must unlink shared memory before creation

## Platform Support

- **macOS**: Uses Mach-O format and system linker flags
- **Linux**: Uses ELF format and appropriate linker configuration
- **Windows**: Uses COFF format with Windows-specific linking

This design provides a solid foundation for the `roc run` command while maintaining flexibility for future enhancements.
