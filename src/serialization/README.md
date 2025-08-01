# Serialization

Serialization and deserialization utilities for compiler data structures and intermediate representations.

- [src/serialization/mod.zig](./mod.zig): Entry point and common utilities for serialization operations including encoding and decoding of compiler data structures.
- [src/serialization/CompactWriter.zig](./CompactWriter.zig): A writer that collects multiple memory allocations and writes them efficiently using vectored I/O (pwritev on UNIX). Automatically aligns all writes to 8-byte boundaries for consistent deserialization.
- [src/serialization/safety.zig](./safety.zig): Safety utilities and validation for serialization operations ensuring data integrity and preventing corruption during encoding/decoding.
- [src/serialization/testing.zig](./testing.zig): Testing utilities and helpers for validating serialization correctness including round-trip testing and data integrity verification.