# Coding Conventions

**Analysis Date:** 2026-01-20

## Naming Patterns

**Files:**
- PascalCase.zig for primary types/structs (e.g., `Parser.zig`, `Check.zig`, `ModuleEnv.zig`)
- snake_case.zig for utilities and functions (e.g., `tokenize.zig`, `unify.zig`, `store.zig`)
- mod.zig for directory-level re-exports
- *_test.zig for test files in `test/` subdirectories

**Functions:**
- snake_case for all functions (e.g., `canonicalizeExpr`, `checkFile`)
- Camel case often used for method-like functions on structs
- No special prefix for async functions (Zig doesn't have async in traditional sense)

**Variables:**
- snake_case for variables and parameters
- Single-letter names acceptable in tight loops (`i`, `n`)
- Descriptive names for function parameters

**Types:**
- PascalCase for structs, enums, unions (e.g., `ModuleEnv`, `NodeStore`, `Var`)
- No prefix conventions (no `I` for interfaces, no `T` for types)
- Enum values in snake_case (e.g., `.record_field`, `.tag_union`)

## Code Style

**Formatting:**
- `zig fmt` for all formatting (run via `zig build fmt`)
- 4-space indentation (Zig default)
- No specific line length limit (Zig fmt handles wrapping)
- Trailing commas encouraged in multi-line lists

**Linting:**
- `./ci/zig_lints.sh` for CI linting
- Zig compiler warnings as errors in release builds

**Run Commands:**
```bash
zig build fmt              # Format all code
./ci/zig_lints.sh         # Run lints
```

## Import Organization

**Order:**
1. Standard library imports (`const std = @import("std");`)
2. External/sibling module imports (`const tracy = @import("tracy");`)
3. Local imports from same module

**Pattern:**
```zig
const std = @import("std");
const tracy = @import("tracy");
const base = @import("../base/mod.zig");

const Self = @This();
```

**Re-exports:**
- `mod.zig` files re-export public API from directory
- Use `pub const X = @import("X.zig");` pattern

## Error Handling

**Patterns:**
- Return error unions where appropriate (`!T` or `Error!T`)
- Use `catch` for error handling
- `@panic` for internal invariants (should not reach production)
- Problem collection for user-facing errors (don't fail immediately)

**Error Types:**
- Module-specific error sets
- `@panic("message")` for internal bugs and not-yet-implemented features
- User errors collected as "problems" and reported at stage end

**Anti-patterns to Avoid:**
- `catch @panic("OOM")` - Should propagate allocation failures
- `unreachable` in paths that could be reached by user input

## Logging

**Framework:**
- No logging framework - uses stdout/stderr directly
- Tracy profiler for performance tracing (optional)

**Patterns:**
- Trace flags for debugging: `-Dtrace-eval`, `-Dtrace-refcount`, `-Dtrace-modules`
- `tracy.trace(@src())` for profiling zones
- `std.debug.print` for debug output (remove before committing)

## Comments

**Module Documentation:**
- Every `.zig` file should start with `//!` module documentation
- Explains WHY the module exists, not WHAT it does

**Example:**
```zig
//! The canonicalizer transforms an AST into Canonical IR (CIR).
//! This stage resolves names, validates scopes, and prepares for type checking.
```

**Function Documentation:**
- Use `///` doc comments for public functions
- Focus on non-obvious behavior and edge cases

**TODO Comments:**
- Format: `// TODO: description`
- Many TODOs exist for incomplete features (this is an active development project)

## Function Design

**Size:**
- No strict limit, but prefer smaller focused functions
- Large files exist (interpreter ~20k lines) - candidates for refactoring

**Parameters:**
- Zig conventions: `self` for methods, named parameters via structs
- Allocator passed explicitly where needed
- Use `anytype` sparingly, prefer concrete types

**Return Values:**
- Explicit return types always
- Error unions for fallible operations
- Optional types (`?T`) for "maybe" values

## Module Design

**Exports:**
- `mod.zig` re-exports public API
- Internal helpers not exported
- Tests in separate `test/` subdirectory

**Pattern:**
```zig
// mod.zig
pub const Parser = @import("Parser.zig");
pub const AST = @import("AST.zig");
pub const parse = @import("Parser.zig").parse;
```

**Organization:**
- One primary type per file (e.g., `Parser.zig` contains `Parser` struct)
- Related functions grouped with the type
- Tests in `test/*.zig` or inline with `test "name" { ... }`

## Zig-Specific Conventions

**Memory:**
- Explicit allocators passed to functions
- Arena allocators for temporary data
- `defer allocator.free(...)` for cleanup

**Extern Structs:**
- Used for FFI and consistent memory layout
- Comment: "Uses extern struct to guarantee consistent field layout across optimization levels"

**Comptime:**
- Comptime for generic programming
- Type reflection via `@TypeOf`, `@typeInfo`

## Roc Language Syntax (for tests)

When writing Roc code in snapshot tests:
- Use `snake_case` for identifiers
- Boolean operators: `and`, `or` (not `&&`, `||`)
- Lambda syntax: `|arg1, arg2| body`
- If expressions: `if condition then_branch else else_branch`
- Blocks use curly braces, last expression is return value
- Use `...` ellipsis for "not implemented" in examples

---

*Convention analysis: 2026-01-20*
*Update when patterns change*
