# FixupCache - Zero-Copy Deserialization Strategy

FixupCache is an alternative caching strategy that provides extremely fast deserialization by eliminating the need to parse or reconstruct data structures. Instead, it uses pointer relocation to transform serialized data directly into valid in-memory structures.

## Core Concept

The key insight is that we can serialize the exact memory layout of our data structures to disk, but with pointers stored as **byte offsets from the beginning of the file** rather than absolute memory addresses. During deserialization:

1. We read the entire file into memory (using mmap for efficiency)
2. We cast the buffer to our root data structure
3. We relocate all pointers by adding the base address of the buffer to each stored offset

This transforms the offsets back into valid memory addresses, giving us a fully functional data structure with zero parsing overhead.

## How It Works

### Serialization

1. **Layout Calculation**: Calculate the total size needed for all data structures and their contents
2. **Sequential Writing**: Write all data sequentially to memory-mapped file:
   - First, the root structure with placeholder pointers
   - Then, all referenced data (arrays, strings, hash tables, etc.)
3. **Pointer Conversion**: Store pointers as byte offsets from the file start instead of memory addresses

### Deserialization

1. **Memory Mapping**: Map the file into memory at an aligned address
2. **Type Casting**: Cast the mapped memory to the root structure type
3. **Pointer Relocation**: Use `relocate.zig` to traverse all pointers and add the base address

## Example

```zig
// During serialization, if a string is at byte offset 0x1000 in the file,
// we store 0x1000 as the "pointer" value.

// During deserialization, if the file is mapped at address 0x7FFF0000,
// we relocate the pointer: 0x7FFF0000 + 0x1000 = 0x7FFF1000

// The string data at file offset 0x1000 is now at memory address 0x7FFF1000
```

## Advantages

- **Extremely Fast**: Deserialization is O(n) where n is the number of pointers, not data size
- **Zero Allocation**: No memory allocation needed during deserialization
- **Cache Friendly**: Data layout is preserved exactly as it was in memory
- **Simple**: No complex parsing logic or data reconstruction

## Limitations

- **Architecture Specific**: Serialized data is tied to pointer size and endianness
- **Version Sensitivity**: Any change to data structure layout breaks compatibility
- **Memory Overhead**: Entire file must be mapped into memory at once

## Implementation Requirements

1. **Alignment**: All pointers must be properly aligned in the serialized data
2. **Relocation Function**: Must handle every pointer type in the data structure
3. **Offset Calculation**: Must correctly calculate byte offsets during serialization
4. **Memory Mapping**: Platform-specific code for mmap (Unix) or equivalent (Windows)

## Production Considerations

- Use checksums to verify data integrity
- Include version numbers for compatibility checking
- Consider compression for disk storage (decompress to aligned buffer)
- Handle platform differences (32-bit vs 64-bit, endianness)