# Base Layer

Provides core data structures and utilities that are shared across the Roc compiler.

- **Module Environment** (`src/base/ModuleEnv.zig`): Centralized store for a module's state that lives for the full lifetime of a compilation (i.e. lives beyond any individual intermediate representation).
- **Identifier Management** (`src/base/Ident.zig`): Efficient interning and deduplication of identifiers.
- **String Management** (`src/base/StringLiteral.zig`): Efficient interning and deduplication of string literals.
- **Source Representation** (`src/base/Region.zig`, `src/base/RegionInfo.zig`): Efficiently represent source code regions for diagnostic reporting and debug information.
- **S-Expression Formatting** (`src/base/SExprTree.zig`): Provides efficient and pretty formatting of S-expressions for debugging intermediate representations.
