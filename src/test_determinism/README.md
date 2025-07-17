# Deterministic Serialization Fixes

This document describes the changes made to ensure deterministic serialization in the Roc compiler's cache system.

## Problem

The original code used `undefined` values in serialization, which causes non-deterministic behavior because:
1. `undefined` values can contain arbitrary memory contents
2. Different runs may have different values in those memory locations
3. This makes cache files non-reproducible and breaks distributed builds

## Solution

We replaced all `undefined` values with deterministic zero or dummy values:

### 1. Allocator Fields

Instead of:
```zig
.gpa = undefined
```

We use:
```zig
.gpa = std.mem.Allocator{
    .ptr = @ptrFromInt(1),
    .vtable = @ptrFromInt(1),
}
```

This creates a deterministic dummy allocator that will be replaced by the deserializer.

### 2. Pointer Fields

Instead of:
```zig
.ptr = if (condition) @ptrFromInt(offset) else undefined
```

We use:
```zig
.ptr = if (condition) @ptrFromInt(offset) else @ptrFromInt(0)
```

For non-nullable pointers, we use small non-zero values like `@ptrFromInt(1)`.

### 3. Complex Structures

For hash maps and other complex structures, instead of:
```zig
.exposed_by_str = undefined
```

We explicitly initialize with zero values:
```zig
.exposed_by_str = .{
    .metadata = null,
    .size = 0,
    .available = 0,
    .header = .{ .values = .{ .metadata = .{ .count = 0, .capacity = 0, .fingerprint = 0 } } },
}
```

## Testing

Added deterministic serialization tests that:
1. Serialize the same data twice to different buffers
2. Verify both buffers are byte-for-byte identical
3. This ensures no random or uninitialized data leaks into the serialized output

## Benefits

1. **Reproducible Builds**: Same source always produces identical cache files
2. **Distributed Caching**: Cache files can be shared between machines
3. **Debugging**: Binary diffs show only real changes, not random pointer values
4. **Security**: No information leakage through uninitialized memory

## Implementation Notes

- Use `@ptrFromInt(1)` for non-nullable pointers that need dummy values
- Use explicit zero initialization for all fields
- The deserializer will overwrite these dummy values with correct ones
- Tests verify determinism by comparing multiple serialization runs