# Roc Compiler (Zig Implementation)

## Navigation

Core compiler directories:

- [base/](./base/) - Core data structures and utilities shared across the compiler
- [check/](./check/) - Main compilation pipeline (parsing, canonicalization, type checking)
- [types/](./types/) - Type system definitions and utilities
- [builtins/](./builtins/) - Built-in types, functions, and operations
- [reporting/](./reporting/) - Error reporting and diagnostic formatting
- [collections/](./collections/) - Efficient data structures used throughout the compiler
- [cache/](./cache/) - Compilation caching system for faster incremental builds
- [coordinate/](./coordinate/) - Cross-module compilation coordination and dependency management
- [eval/](./eval/) - Runtime evaluation and interpretation system
- [fs/](./fs/) - Filesystem operations and file I/O utilities
- [layout/](./layout/) - Memory layout computation and management for code generation
- [serialization/](./serialization/) - Serialization utilities for compiler data structures
- [snapshots/](./snapshots/) - Snapshot tests validating compiler behavior across all stages

## Status

This table provides a summary of progress for the zig compiler re-write and should be updated with the PR that includes new features.

|                          | Str & Num | Functions  | Modules | Collections | Records &  Tuples | Recursive  Types | Static  Dispatch |
|--------------------------|:-----------:|:----------:|:-------:|:-----------:|:-----------------:|:----------------:|:----------------:|
| **Parse**                | 🔋          | 🔋         | 🪫      | 🪫          |  🔋               |  🪫              |  🚧              |
| **Canonicalize**         | 🪫          | 🪫         | 🪫      | 🪫          |  🪫               |  🪫              |  🚧              |
| **Resolve Imports**      | 🚧          | 🚧         | 🪫      | 🚧          |  🚧               |  🚧              |  🚧              |
| **Check Types**          | 🪫          | 🚧         | 🚧      | 🚧          |  🚧               |  🚧              |                  |
| **Interpreter**          |             |            |         |             |                   |                  |                  |
| **Specialize Types**     |             |            |         |             |                   |                  |                  |
| **Lift Functions**       |             |            |         |             |                   |                  |                  |
| **Solve Functions**      |             |            |         |             |                   |                  |                  |
| **Specialize Functions** |             |            |         |             |                   |                  |                  |
| **Reference Counting**   |             |            |         |             |                   |                  |                  |
| **Lower IR**             |             |            |         |             |                   |                  |                  |
| **Gen LLVM**             |             |            |         |             |                   |                  |                  |

- N/A   Not applicable
- 🚧    Work Started
- 🪫    Tests Passing
- 🔋    Polished

## Fast Feedback Loop

The roc zig compiler can have a very fast feedback loop. We support zigs incremental compilation and watch mode.
By avoiding generating final executables, we can build and typecheck much much faster.

Try it with `zig build -Dno-bin -fincremental --watch`

### Running Language Server (LSP) Tests

The LSP tests are split into fast unit coverage and compiler-backed integration
coverage.

```sh
zig build run-test-zig-module-lsp_unit
zig build run-test-zig-module-lsp_integration
```

For faster iteration with incremental compilation and watch mode:

```sh
zig build run-test-zig-module-lsp_unit -fincremental --watch
```

Use `lsp_unit` for protocol, transport, document store, token-only coloring,
and handler routing work that does not need compiler checking. Use
`lsp_integration` when changing syntax checking, completions, hover,
definition, document symbols, diagnostics, or behavior that depends on real Roc
source, compiled builtins, and platform/app checking. The integration command
is a parallel harness; pass `-- --threads N` to cap workers or
`-- --test-filter "case name"` to run matching integration cases.

### Expanding to ZLS

This fast config can also be used with `zls`. Simply follow these steps:
1. run `zls --version` and make sure it supports Zig `0.16.0`.
2. run `zls env` and grab the `config_file` path.
3. Edit the config file to include
```json
{
  "enable_build_on_save": true,
  "build_on_save_args": ["-Dno-bin", "-fincremental"]
}
```
4. Advised, also changing the cache dir, I use `--cache-dir .zig-cache/zls`.
Otherwise, zig commands run manually can lead to the lsp breaking and requiring a restart.
5. Optionally, add `-Dfuzz` above as well to get type checking of fuzz scripts as well.
6. Note, I had to fully delete my `.zig-cache` to get `zls` to start.
Make sure to check the logs if you aren't getting type failures.
7. Enjoy better lsp results.

### Simply testing feedback loop

Sadly, this is not nearly as fast due to building binaries.
One day, we will get dev zig backends, and it should be fast.

Try it with `zig build run-test-zig -fincremental --watch`

## Overview

![Zig Dependency Graph](https://anton-4.github.io/roc-compiler-vis/zig_dependency_graph.webp)
