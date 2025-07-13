# Base Layer

Provides core data structures and utilities that are shared across the Roc compiler.

- [src/base/ModuleEnv.zig](./ModuleEnv.zig): Centralized store for a module's state that lives for the full lifetime of a compilation (i.e. lives beyond any individual intermediate representation).
- [src/base/Ident.zig](./Ident.zig): Efficient interning and deduplication of identifiers.
- [src/base/StringLiteral.zig](./StringLiteral.zig): Efficient interning and deduplication of string literals.
- [src/base/Region.zig](./Region.zig) and [src/base/RegionInfo.zig](./RegionInfo.zig): Efficiently represent source code regions for diagnostic reporting and debug information.
- [src/base/SExprTree.zig](./SExprTree.zig): Provides efficient and pretty formatting of S-expressions for debugging intermediate representations.
- [src/base/PackedDataSpan.zig](./PackedDataSpan.zig): Memory-efficient data structure for storing spans of packed data.
- [src/base/Scratch.zig](./Scratch.zig): Temporary scratch space allocation and management.
- [src/base/parallel.zig](./parallel.zig): Utilities for concurrent compilation operations.
- [src/base/target.zig](./target.zig): Target platform definitions and cross-compilation support.