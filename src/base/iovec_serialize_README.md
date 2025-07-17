# IoVec-Based Serialization

This document describes the pwritev-based serialization approach implemented as an alternative to the existing mmap-based serialization.

## Overview

The iovec serialization system provides a scatter-gather I/O approach to writing serialized data, using `pwritev()` on POSIX systems and a fallback implementation on Windows.

## Architecture

### Key Components

1. **IovecWriter** (`iovec_serialize.zig`)
   - Accumulates iovec entries instead of writing to a single buffer
   - Supports aligned writes with automatic padding
   - Handles deferred writes for header structures
   - Provides cross-platform file writing (pwritev on POSIX, buffer+write on Windows)

2. **appendToIovecs Methods**
   - Each serializable structure implements `appendToIovecs(writer: *IovecWriter)`
   - Returns the offset where the main structure was written
   - Maintains compatibility with existing serialization format

3. **Zero Padding** (`write_aligned.zig`)
   - Provides a constant array of zeros for alignment padding
   - `appendAlignedToIovecs` adds padding iovecs as needed

## Implementation

### Basic Usage

```zig
// Create an iovec writer
var writer = IovecWriter.init(allocator);
defer writer.deinit();

// Append data structures
const offset = try myStruct.appendToIovecs(&writer);

// Finalize deferred writes (headers, etc.)
try writer.finalize();

// Write to file
try writer.writevToFile(file, 0);
```

### Adding IoVec Support to a Structure

```zig
pub fn appendToIovecs(self: *const Self, writer: *IovecWriter) !usize {
    const struct_offset = writer.getOffset();
    
    // Reserve space for header if needed
    const header_offset = try writer.reserveStruct(Self);
    
    // Append child data
    const child_offset = try writer.appendAligned(self.child_data, @alignOf(ChildType));
    
    // Create header with offsets
    const header = Self{
        .child_ptr = @ptrFromInt(child_offset),
        // ... other fields
    };
    
    // Write header at reserved position
    try writer.writeDeferredStruct(header_offset, header);
    
    return struct_offset;
}
```

## Platform Differences

### POSIX Systems (Linux, macOS, etc.)
- Uses native `pwritev()` system call
- Efficient scatter-gather I/O without intermediate buffers
- Multiple memory regions written in a single system call

### Windows
- No native `pwritev()` support
- Falls back to:
  1. Allocating a single buffer
  2. Copying all iovec data into the buffer
  3. Single `pwrite()` call

## Benefits

1. **No Large Buffer Allocation**: On POSIX systems, avoids allocating a single large buffer for serialization
2. **Reduced Memory Copies**: Data structures can be written directly from their original locations
3. **Maintains Compatibility**: Produces identical on-disk format to mmap approach
4. **Incremental Construction**: Can build the serialized format incrementally

## Testing

The implementation includes:

1. **Unit Tests** (`iovec_serialize.zig`)
   - Basic iovec operations
   - Alignment handling
   - Buffer reconstruction

2. **Integration Tests** (`ModuleEnv.zig`)
   - Verifies iovec serialization matches buffer serialization byte-for-byte
   - Tests with real data structures

3. **Example Program** (`test_iovec_example.zig`)
   - Demonstrates both serialization methods
   - Compares performance
   - Verifies output equivalence

## Migration Strategy

The iovec serialization is implemented alongside the existing mmap approach:

1. Both methods produce identical on-disk formats
2. Deserialization remains unchanged (uses relocations)
3. Can switch between methods with a compile flag or runtime option
4. Allows gradual migration and testing

## Performance Considerations

- **POSIX**: Potentially faster due to fewer memory allocations and copies
- **Windows**: Similar performance to mmap approach due to fallback
- **Memory Usage**: Lower peak memory usage on POSIX systems
- **Syscall Count**: Single pwritev vs multiple writes

## Future Improvements

1. **Optimize Windows Path**: Could use WriteFileGather on Windows for true scatter-gather I/O
2. **Reduce Temporary Buffers**: Some structures (hash maps, types) still serialize to temporary buffers
3. **Streaming Serialization**: Could implement true streaming for complex structures
4. **Async I/O**: Could use io_uring on Linux for async writes