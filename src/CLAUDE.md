# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Structure

This repository contains the Roc compiler, with two implementations:
- **Zig compiler** (`src/`): The actively developed new compiler - work here
- **Rust compiler** (`crates/`): Legacy prototype - do not modify

Always exclude `crates/` from searches and modifications.

## Build Commands

```bash
# Build the compiler
zig build roc

# Fast feedback loop (no binaries, incremental, watch mode)
zig build -Dno-bin -fincremental --watch

# Format code
zig build fmt

# Run all tests
zig build snapshot && zig build test

# Run specific test
zig build test -- --test-filter "name of test"

# Run tests for a specific module
zig build test-parse      # parse tests only
zig build test-can        # canonicalize tests only
zig build test-check      # type check tests only
zig build test-eval       # interpreter tests only
zig build test-repl       # repl tests only

# CI checks
zig build fmt && ./ci/zig_lints.sh && zig build && zig build snapshot && zig build test
```

## Snapshot Testing

Snapshots are the primary testing strategy. They are markdown files that test compiler behavior across all stages.

```bash
# Generate/update all snapshots
zig build snapshot

# Generate/update specific snapshot
zig build snapshot -- <file_path>

# Update EXPECTED from PROBLEMS
zig build update-expected -- <file_path>
```

Snapshot files have these sections:
- **META**: `description` and `type` (file, expr, statement, header, repl)
- **SOURCE**: Roc code being tested
- **EXPECTED**: Expected outcome (NIL, error name, or specific output)
- **Generated**: TOKENS, PARSE, CANONICALIZE, TYPES, PROBLEMS

For REPL snapshots (`type=repl`): SOURCE uses `»` prefix for commands, EXPECTED uses `---` to separate outputs.

## Compiler Pipeline

The Zig compiler follows this pipeline:
1. **parse** - Source code → Abstract Syntax Tree (AST)
2. **canonicalize** - AST → Canonical Intermediate Representation (CIR)
3. **check** - Hindley-Milner type inference on CIR
4. **eval** - Runtime evaluation (powers REPL, snapshot tool)

Key modules:
- `base/` - Shared data structures, memory management, parallel processing
- `types/` - Type system implementation
- `collections/` - Specialized compiler data structures
- `reporting/` - Error diagnostics and formatting
- `builtins/` - Built-in types and functions

## Debugging

```bash
# Build with evaluation tracing
zig build -Dtrace-eval=true

# Build with refcount tracing (memory debugging)
zig build -Dtrace-refcount=true

# Build with module tracing
zig build -Dtrace-modules=true

# Snapshot with eval tracing (REPL snapshots only)
./zig-out/bin/snapshot --trace-eval path/to/snapshot.md

# Build with Tracy profiler
zig build -Doptimize=ReleaseFast -Dtracy=$HOME/vendor/tracy roc
```

## Documentation Standards

- Every `.zig` file should start with module documentation (`//!`)
- Every `src/` directory should have a `README.md`
- Documentation should be concise, focusing on WHY not WHAT/HOW

## Roc Language Syntax

When writing Roc code in tests:
- Use `snake_case` for identifiers
- Boolean operators: `and`, `or` (not `&&`, `||`)
- Lambda syntax: `|arg1, arg2| body`
- If expressions: `if condition then_branch else else_branch`
- Blocks use curly braces, last expression is return value
- Use `...` ellipsis for "not implemented" in examples
