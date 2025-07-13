# Types

Core type system definitions and utilities for representing, manipulating, and managing types throughout the Roc compiler.

- [src/types/types.zig](./types.zig): Fundamental type definitions and core type system data structures including type variables, constraints, and type representations.
- [src/types/store.zig](./store.zig): Type storage and management system for efficiently storing, retrieving, and deduplicating type information across compilation phases.
- [src/types/writers.zig](./writers.zig): Type formatting and serialization utilities for converting type representations into human-readable strings and diagnostic output.
- [src/types/test_rigid_instantiation.zig](./test_rigid_instantiation.zig): Unit tests for rigid variable instantiation ensuring correct handling of generic type parameters and polymorphic type instantiation.