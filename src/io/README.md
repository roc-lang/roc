# Roc I/O Abstractions

This directory contains cross-platform abstractions for I/O operations in Roc.

## Gather I/O

The `write_gather.zig` module provides a cross-platform abstraction over:

- `WriteFileGather` on Windows
- `writev` on POSIX systems

### Key Features

- **Cross-platform API**: Unified API that works on both Windows and POSIX systems
- **Minimal API surface**: Provides only the "lowest common denominator" functionality needed across platforms
- **Documented requirements**: Each function details the requirements from both Windows and POSIX perspectives

### Alignment and Size Requirements

When working with gather I/O, be aware of the following cross-platform requirements:

1. **Buffer Alignment**: On Windows, buffers must be aligned to the volume's sector size (typically 512 bytes). The POSIX `writev` function doesn't have this requirement, but our cross-platform API enforces it for consistency.

2. **Buffer Size**: On Windows, buffer sizes must be multiples of the volume's sector size. Again, this isn't required by POSIX, but our API enforces it.

3. **File Offset**: On Windows, the file offset must be aligned to the volume's sector size. Our implementation handles this requirement internally.

### Helper Functions

To simplify working with these requirements, the API provides helper functions:

- `getSectorSize`: Returns the appropriate sector size for a given file handle
- `allocateAlignedBuffer`: Allocates properly aligned memory suitable for gather operations
- `freeAlignedBuffer`: Properly frees memory allocated with `allocateAlignedBuffer`
- `alignOffset`: Ensures file offsets are properly aligned for Windows requirements

### Performance Considerations

- In debug builds, buffer alignment and size validation are performed to ensure compliance with platform requirements
- In release builds, these validations are skipped for better performance
- On Linux and FreeBSD, the implementation uses `pwritev` directly to avoid the overhead of seeking
- On other POSIX platforms, the implementation uses temporary position adjustments with `lseek` followed by `writev`
- Applications should still ensure they're using properly aligned buffers even in release builds, as failing to do so can cause undefined behavior on Windows