# Roc I/O Abstractions

This directory contains cross-platform abstractions for I/O operations in Roc.

## Gather I/O

The `write_gather.zig` module provides a cross-platform abstraction over:

- `WriteFileGather` on Windows
- `writev` on POSIX systems

### Alignment and Size Requirements

When working with gather I/O, be aware of the following cross-platform requirements:

1. **Buffer Alignment**: On Windows, buffers must be aligned to the volume's sector size (typically 512 bytes) and also sized to a multiple of the volume's sector size. The POSIX `writev` function doesn't have this requirement, but our cross-platform API enforces it for consistency.

2. **File Offset**: On Windows, the file offset must be aligned to the volume's sector size. Our implementation handles this requirement internally.

### Helper Functions

To simplify working with these requirements, the API provides helper functions:

- `allocateAlignedBuffer`: Allocates properly aligned memory suitable for gather operations
- `freeAlignedBuffer`: Properly frees memory allocated with `allocateAlignedBuffer`
- `alignOffset`: Ensures file offsets are properly aligned for Windows requirements

### Performance Considerations

- In debug builds only, buffer alignment and size validation are performed to ensure compliance with platform requirements
- Applications should still ensure they're using properly aligned buffers even in release builds, as failing to do so can cause undefined behavior on Windows
