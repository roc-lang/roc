# Architecture

**Analysis Date:** 2026-01-20

## Pattern Overview

**Overall:** Staged Compiler Pipeline with Interpreter

**Key Characteristics:**
- Multi-stage compilation pipeline (parse -> canonicalize -> check -> eval/codegen)
- Hindley-Milner type inference with constraint solving
- Interpreter for REPL and snapshot testing
- Native code generation via LLVM backend
- Module-based organization with clear stage boundaries

## Layers

**Parse Layer:**
- Purpose: Convert source text to Abstract Syntax Tree (AST)
- Contains: Tokenizer, parser, AST data structures
- Location: `src/parse/`
- Key files: `src/parse/Parser.zig`, `src/parse/AST.zig`, `src/parse/tokenize.zig`
- Depends on: Base utilities only
- Used by: Canonicalize layer

**Canonicalize Layer:**
- Purpose: Transform AST to Canonical IR (CIR), resolve names and scopes
- Contains: Canonicalizer, ModuleEnv, CIR, scope resolution
- Location: `src/canonicalize/`
- Key files: `src/canonicalize/Can.zig`, `src/canonicalize/CIR.zig`, `src/canonicalize/ModuleEnv.zig`
- Depends on: Parse layer, base utilities
- Used by: Type check layer

**Type System Layer:**
- Purpose: Type representation and storage
- Contains: Type store, type variables, instantiation, generalization
- Location: `src/types/`
- Key files: `src/types/types.zig`, `src/types/store.zig`
- Depends on: Base utilities
- Used by: Type check layer, eval layer

**Type Check Layer:**
- Purpose: Hindley-Milner type inference and constraint solving
- Contains: Checker, unifier, occurs check, exhaustiveness checking
- Location: `src/check/`
- Key files: `src/check/Check.zig`, `src/check/unify.zig`, `src/check/exhaustive.zig`
- Depends on: Canonicalize layer, types layer
- Used by: Eval layer, backend layer

**Eval Layer:**
- Purpose: Runtime evaluation (powers REPL and snapshot tool)
- Contains: Interpreter, builtin implementations, stack management
- Location: `src/eval/`
- Key files: `src/eval/interpreter.zig`, `src/eval/builtins.zig`
- Depends on: Type check layer, canonicalize layer
- Used by: REPL, snapshot tool

**Backend Layer:**
- Purpose: Code generation for native executables
- Contains: LLVM builder, native backends (x86_64, aarch64), object formats
- Location: `src/backend/`
- Key files: `src/backend/llvm/Builder.zig`, `src/backend/dev/CodeGen.zig`
- Depends on: Type check layer, layout layer
- Used by: CLI (build command)

**Compile Layer:**
- Purpose: Orchestrate compilation, caching, package management
- Contains: Package compilation, cache manager, module discovery
- Location: `src/compile/`
- Key files: `src/compile/compile_package.zig`, `src/compile/cache_manager.zig`
- Depends on: All compiler layers
- Used by: CLI

## Data Flow

**Compilation Pipeline (Source -> Executable):**

1. Source text loaded via `CommonEnv` (`src/base/CommonEnv.zig`)
2. **Parse**: Tokenize and parse to AST (`src/parse/mod.zig` -> `Parser.zig`)
3. **Canonicalize**: Convert AST to CIR, resolve scopes (`src/canonicalize/Can.zig`)
4. **Type Check**: HM inference, unification, collect problems (`src/check/Check.zig`)
5. **Codegen**: Generate LLVM IR or native code (`src/backend/`)
6. **Link**: Link to executable via LLD

**REPL/Eval Flow:**

1. Parse expression
2. Canonicalize to CIR
3. Type check
4. Interpret directly (`src/eval/interpreter.zig`)
5. Render result

**State Management:**
- `ModuleEnv` is the central module container throughout pipeline (`src/canonicalize/ModuleEnv.zig`)
- Types are module-local (imported types copied into local `types.Store`)
- Compilation state flows through pipeline stages, not shared globally

## Key Abstractions

**AST / Parser:**
- Purpose: Represent parsed source structure
- Location: `src/parse/AST.zig`, `src/parse/Parser.zig`
- Pattern: Data-oriented node store with indices

**CIR / ModuleEnv:**
- Purpose: Canonical IR and module container for downstream stages
- Location: `src/canonicalize/CIR.zig`, `src/canonicalize/ModuleEnv.zig`
- Pattern: Central data structure passed through pipeline

**Types Store:**
- Purpose: Type variable storage and management
- Location: `src/types/store.zig`, `src/types/types.zig`
- Pattern: Arena-based storage with indices

**Check / Solver:**
- Purpose: Constraint-based type inference
- Location: `src/check/Check.zig`
- Pattern: Hindley-Milner with unification

**Interpreter:**
- Purpose: Direct evaluation of CIR
- Location: `src/eval/interpreter.zig`
- Pattern: Stack-based interpreter with builtin dispatch

## Entry Points

**CLI Entry:**
- Location: `src/cli/main.zig`
- Triggers: `roc run`, `roc build`, `roc check`, `roc repl`, etc.
- Responsibilities: Parse args, orchestrate compilation, dispatch to subcommands

**REPL:**
- Location: `src/cli/repl.zig`, `src/repl/eval.zig`
- Triggers: `roc repl` command
- Responsibilities: Read-eval-print loop, maintain session state

**Snapshot Tool:**
- Location: `src/snapshot_tool/main.zig`
- Triggers: `zig build snapshot`
- Responsibilities: Run compiler pipeline on test files, compare outputs

**Tests:**
- Location: `*/test/*.zig` directories throughout
- Triggers: `zig build test`, `zig build test-{module}`
- Responsibilities: Unit and integration tests per module

## Error Handling

**Strategy:** Problem collection and reporting at stage boundaries

**Patterns:**
- Each stage collects `problems` rather than failing immediately
- Problems rendered via `src/reporting/` at the end
- `@panic` used for internal invariants (should become errors in some cases)
- Error types propagated where possible

**Diagnostics:**
- Reporting system: `src/reporting/report.zig`, `src/reporting/renderer.zig`
- Rich error messages with source locations
- Configurable output format

## Cross-Cutting Concerns

**Logging/Tracing:**
- Build flags: `-Dtrace-eval`, `-Dtrace-refcount`, `-Dtrace-modules`
- Tracy profiler integration (optional)

**Memory Management:**
- Arena allocators throughout
- Reference counting for runtime values (`-Dtrace-refcount`)
- Base utilities: `src/base/`

**Builtin Types:**
- Auto-imported in canonicalization (`Bool`, `Str`, numeric types)
- Builtin module: `src/builtins/`
- Copied into module's type store during check

---

*Architecture analysis: 2026-01-20*
*Update when major patterns change*
