# FixupCache Implementation Notes

## Overview

FixupCache is a zero-copy deserialization strategy for the Roc compiler's module cache. It achieves extremely fast deserialization by serializing data structures with pointers stored as file offsets, then relocating them during deserialization.

## Key Components

### 1. Proof of Concept (`proof_of_concept.zig`)

The proof of concept demonstrates the core technique with the actual Roc compiler data structures:
- `SafeList(u32)`: The real type-safe dynamic array from collections
- `SmallStringInterner`: The actual string interner with deduplication and regions
- `SafeStringHashMap(u16)`: The real string hash map with owned keys

The test successfully:
- Serializes these production data structures to a buffer with pointers as offsets
- Handles the complex internal structure of SmallStringInterner (including its internal hash table)
- Properly serializes StringHashMapUnmanaged with its metadata and key/value arrays
- Writes the buffer to simulate disk storage
- Reads it back and casts to the original structure type
- Relocates all pointers by adding the base address
- Verifies all data is correctly accessible, including:
  - String deduplication in the interner
  - Region information preservation
  - Hash map iteration and lookups

### 2. Production Framework (`FixupCache.zig`)

The production implementation provides:
- **Header Structure**: Contains magic number, version, platform compatibility info, and checksums
- **Cross-Platform Memory Mapping**: Abstractions for mmap (Unix) and equivalent Windows APIs
- **Alignment Handling**: Ensures all data is properly aligned for direct casting
- **Error Handling**: Comprehensive error checking for corrupted or incompatible cache files

### 3. Relocation Support (`relocate.zig`)

The existing `relocate.zig` already provides the necessary infrastructure to traverse and update all pointers in a ModuleEnv. This is the key component that makes FixupCache practical.

## Implementation Strategy

### Phase 1: Complete ModuleEnv Serialization
1. Implement `calculateModuleEnvDataSize` to compute exact buffer size
2. Implement `serializeModuleEnv` to write all ModuleEnv data with offset-based pointers
3. Test with real ModuleEnv instances

### Phase 2: CIR Integration
1. Add relocation support for CIR data structures
2. Implement CIR serialization with offset-based pointers
3. Ensure all CIR node types are handled correctly

### Phase 3: Performance Optimization
1. Use memory-mapped files for writing (not just reading)
2. Implement incremental updates for changed modules
3. Add compression support (decompress to aligned buffer)

### Phase 4: Production Hardening
1. Add recovery from corrupted cache files
2. Implement cache versioning and migration
3. Add metrics for cache hit/miss rates

## Technical Challenges

### 1. Alignment Requirements
- All pointers must be properly aligned in the serialized data
- Different data types have different alignment requirements
- Solution: Use `writeAlignedData` helper that adds padding as needed

### 2. Platform Compatibility
- Pointer sizes differ between 32-bit and 64-bit platforms
- Endianness varies between architectures
- Solution: Platform ID in header prevents loading incompatible caches

### 3. Data Structure Evolution
- Any change to struct layout breaks cache compatibility
- Solution: Version numbers and graceful fallback to regeneration

## Performance Characteristics

### Serialization
- O(n) where n is the size of data to write
- Single pass through all data structures
- Memory-mapped writing can improve performance

### Deserialization
- O(p) where p is the number of pointers to relocate
- No parsing or allocation overhead
- Typically orders of magnitude faster than traditional deserialization

## Memory Usage

- **During Serialization**: Requires buffer equal to total data size
- **During Deserialization**: Only the mapped file (shared with OS page cache)
- **Runtime**: No additional overhead beyond the data structures themselves

## Future Enhancements

1. **Lazy Loading**: Map file but only fault in pages as accessed
2. **Shared Cache**: Multiple processes can map the same cache file
3. **Incremental Updates**: Only regenerate changed portions
4. **Network Cache**: Stream cache files from build servers

## Conclusion

FixupCache provides a robust foundation for extremely fast module caching in the Roc compiler. The proof of concept demonstrates the technique works correctly with the actual production data structures used in the compiler (SafeList, SmallStringInterner, and SafeStringHashMap), handling their complex internal layouts including hash tables and string deduplication. The production framework provides the infrastructure needed for real-world use. The main remaining work is implementing the serialization logic for ModuleEnv and CIR data structures, which will follow the same patterns demonstrated in the proof of concept.