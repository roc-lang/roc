# Codebase Structure

**Analysis Date:** 2026-01-20

## Directory Layout

```
src/
├── parse/              # Tokenization and parsing -> AST
├── canonicalize/       # AST -> Canonical IR (CIR)
├── types/              # Type system representation
├── check/              # Type inference and checking
├── eval/               # Interpreter for REPL/tests
├── backend/            # Code generation
│   ├── llvm/           # LLVM backend
│   └── dev/            # Native backends (x86_64, aarch64)
├── compile/            # Package compilation orchestration
├── cli/                # Command-line interface
├── repl/               # REPL implementation
├── lsp/                # Language Server Protocol
├── base/               # Shared utilities
├── builtins/           # Built-in types and functions
├── collections/        # Specialized data structures
├── reporting/          # Error diagnostics
├── fmt/                # Code formatting
├── fs/                 # Filesystem abstraction
├── ipc/                # Inter-process communication
├── layout/             # Memory layout computation
├── bundle/             # Package bundling
├── unbundle/           # Package extraction
├── build/              # Native build helpers (LLVM, Tracy)
├── llvm_compile/       # LLVM compilation bindings
├── snapshot_tool/      # Test snapshot tool
├── interpreter_shim/   # Interpreter embedding
├── roc_src/            # Source span utilities
├── base58/             # Base58 encoding
├── CLAUDE.md           # AI assistant guidance
└── flake.nix           # Nix dev environment
```

## Directory Purposes

**src/parse/**
- Purpose: Tokenization and parsing of Roc source to AST
- Contains: Tokenizer, parser, AST nodes, node store
- Key files: `Parser.zig`, `AST.zig`, `tokenize.zig`, `NodeStore.zig`
- README: `src/parse/README.md`

**src/canonicalize/**
- Purpose: Transform AST to Canonical IR, resolve names and scopes
- Contains: Canonicalizer, CIR, ModuleEnv, scope resolution
- Key files: `Can.zig`, `CIR.zig`, `ModuleEnv.zig`, `Scope.zig`
- README: `src/canonicalize/README.md`

**src/types/**
- Purpose: Type system representation and storage
- Contains: Type store, type variables, instantiation, generalization
- Key files: `types.zig`, `store.zig`, `instantiate.zig`, `generalize.zig`
- README: `src/types/README.md`

**src/check/**
- Purpose: Hindley-Milner type inference and constraint solving
- Contains: Type checker, unifier, occurs check, exhaustiveness
- Key files: `Check.zig`, `unify.zig`, `occurs.zig`, `exhaustive.zig`
- README: `src/check/README.md`

**src/eval/**
- Purpose: Interpreter for REPL and snapshot testing
- Contains: Interpreter, builtins, stack, comptime evaluation
- Key files: `interpreter.zig`, `builtins.zig`, `stack.zig`
- README: `src/eval/README.md`

**src/backend/**
- Purpose: Code generation backends
- Subdirectories:
  - `llvm/` - LLVM-based code generation
  - `dev/` - Native backends (x86_64, aarch64, object formats)
- Key files: `llvm/Builder.zig`, `dev/CodeGen.zig`

**src/compile/**
- Purpose: Package compilation orchestration and caching
- Contains: Package compiler, cache manager, module discovery
- Key files: `compile_package.zig`, `cache_manager.zig`
- README: `src/compile/README.md`

**src/cli/**
- Purpose: Command-line interface and subcommands
- Contains: Main entry, argument parsing, REPL launch
- Key files: `main.zig`, `cli_args.zig`, `repl.zig`
- README: `src/cli/README.md`

**src/repl/**
- Purpose: REPL evaluation and session management
- Contains: Eval loop, output formatting
- Key files: `eval.zig`, `mod.zig`
- README: `src/repl/README.md`

**src/lsp/**
- Purpose: Language Server Protocol implementation
- Contains: Server, handlers, syntax analysis
- Key files: `server.zig`, `syntax.zig`
- README: `src/lsp/README.md`

**src/base/**
- Purpose: Shared data structures and utilities
- Contains: Identifiers, regions, common environment
- Key files: `Ident.zig`, `Region.zig`, `CommonEnv.zig`

**src/builtins/**
- Purpose: Built-in types and functions
- Contains: Builtin definitions, str/list implementations
- Key files: `mod.zig`, `str.zig`, `list.zig`
- README: `src/builtins/README.md`

**src/reporting/**
- Purpose: Error diagnostics and formatting
- Contains: Report builder, renderer
- Key files: `report.zig`, `renderer.zig`

**src/snapshot_tool/**
- Purpose: Snapshot testing tool for compiler
- Contains: Test runner, snapshot comparison
- Key files: `main.zig`

## Key File Locations

**Entry Points:**
- `src/cli/main.zig` - CLI entry point (`roc` command)
- `src/snapshot_tool/main.zig` - Snapshot test runner
- `src/interpreter_shim/main.zig` - Embedded interpreter

**Configuration:**
- `build.zig` (repo root) - Zig build configuration
- `build.zig.zon` (repo root) - Dependencies
- `flake.nix` - Nix development environment
- `CLAUDE.md` - AI assistant guidance

**Core Pipeline:**
- `src/parse/mod.zig` - Parse stage entry
- `src/canonicalize/mod.zig` - Canonicalize stage entry
- `src/check/mod.zig` - Type check stage entry
- `src/eval/mod.zig` - Eval stage entry
- `src/backend/mod.zig` - Backend stage entry

**Testing:**
- `src/*/test/*.zig` - Unit tests per module
- `test/snapshots/*.md` (repo root) - Snapshot test files

**Documentation:**
- `src/CLAUDE.md` - Development guidance
- `src/*/README.md` - Per-module documentation

## Naming Conventions

**Files:**
- PascalCase.zig for primary types/modules (e.g., `Parser.zig`, `Check.zig`)
- snake_case.zig for utilities and helpers (e.g., `tokenize.zig`, `unify.zig`)
- mod.zig for module re-exports (e.g., `src/parse/mod.zig`)
- *_test.zig for test files in `test/` subdirectories

**Directories:**
- snake_case for all directories
- `test/` subdirectory for unit tests within each module

**Special Patterns:**
- `mod.zig` - Re-exports public API from directory
- `README.md` - Per-directory documentation
- `TestEnv.zig` - Shared test environment helpers

## Where to Add New Code

**New Compiler Feature:**
- Parse changes: `src/parse/`
- AST nodes: `src/parse/Node.zig`, `src/parse/AST.zig`
- Canonicalization: `src/canonicalize/`
- Type inference: `src/check/`
- Runtime: `src/eval/`
- Tests: Corresponding `test/` subdirectory

**New CLI Command:**
- Definition: `src/cli/main.zig`
- Arguments: `src/cli/cli_args.zig`
- Tests: `src/cli/test/`

**New Builtin:**
- Implementation: `src/builtins/`
- Interpreter support: `src/eval/builtins.zig`
- Tests: `src/builtins/` and snapshots

**New Backend Target:**
- Architecture code: `src/backend/dev/{arch}/`
- Object format: `src/backend/dev/object/`
- Tests: Corresponding test files

**Utilities:**
- Shared helpers: `src/base/`
- Collections: `src/collections/`
- Type utilities: `src/types/`

## Special Directories

**src/build/**
- Purpose: Native build helpers (C++ LLVM wrappers, Tracy)
- Source: C/C++ code for LLVM/LLD integration
- Key files: `zig_llvm.cpp`, `zig_llvm.h`, `tracy-shutdown.cpp`

**test/snapshots/ (repo root)**
- Purpose: Snapshot test files (markdown format)
- Source: Manually written test cases
- Generated: TOKENS, PARSE, CANONICALIZE, TYPES, PROBLEMS sections

**crates/ (repo root)**
- Purpose: Legacy Rust compiler (do not modify)
- Status: Read-only reference
- Note: Excluded from all development work

---

*Structure analysis: 2026-01-20*
*Update when directory structure changes*
