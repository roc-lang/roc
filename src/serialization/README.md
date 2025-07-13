# Serialization

Serialization and deserialization utilities for compiler data structures and intermediate representations.

- [src/serialization/mod.zig](./mod.zig): Entry point and common utilities for serialization operations including encoding and decoding of compiler data structures.
- [src/serialization/safety.zig](./safety.zig): Safety utilities and validation for serialization operations ensuring data integrity and preventing corruption during encoding/decoding.
- [src/serialization/testing.zig](./testing.zig): Testing utilities and helpers for validating serialization correctness including round-trip testing and data integrity verification.