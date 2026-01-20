# Testing Patterns

**Analysis Date:** 2026-01-20

## Test Framework

**Runner:**
- Zig built-in test framework (`std.testing`)
- Custom snapshot testing tool (`src/snapshot_tool/main.zig`)

**Assertion Library:**
- `std.testing.expect*` functions
- `std.testing.expectEqual`, `std.testing.expectError`

**Run Commands:**
```bash
# Run all tests
zig build snapshot && zig build test

# Run specific module tests
zig build test-parse      # parse tests only
zig build test-can        # canonicalize tests only
zig build test-check      # type check tests only
zig build test-eval       # interpreter tests only
zig build test-repl       # repl tests only

# Run specific test by name
zig build test -- --test-filter "name of test"

# Generate/update all snapshots
zig build snapshot

# Generate/update specific snapshot
zig build snapshot -- <file_path>

# Update EXPECTED from PROBLEMS
zig build update-expected -- <file_path>

# Full CI check
zig build fmt && ./ci/zig_lints.sh && zig build && zig build snapshot && zig build test
```

## Test File Organization

**Location:**
- Unit tests in `*/test/*.zig` subdirectories
- Snapshot tests in `test/snapshots/*.md` (repository root)
- Test helpers in `*/test/TestEnv.zig`

**Naming:**
- `*_test.zig` for unit tests (e.g., `unify_test.zig`, `eval_test.zig`)
- `TestEnv.zig` for shared test environments
- `helpers.zig` for test utilities

**Structure:**
```
src/
  check/
    Check.zig
    mod.zig
    test/
      TestEnv.zig
      unify_test.zig
      exhaustiveness_test.zig
      cross_module_test.zig
  eval/
    interpreter.zig
    test/
      TestEnv.zig
      eval_test.zig
      interpreter_style_test.zig

test/snapshots/  (repo root)
  001.md
  annotations.md
  ...
```

## Test Structure

**Unit Test Organization:**
```zig
const std = @import("std");
const testing = std.testing;

test "descriptive test name" {
    // arrange
    const allocator = testing.allocator;
    const input = "test input";

    // act
    const result = functionUnderTest(input);

    // assert
    try testing.expectEqual(expected, result);
}

test "should handle error case" {
    const result = functionThatFails();
    try testing.expectError(error.ExpectedError, result);
}
```

**Patterns:**
- Descriptive test names (string literals)
- Arrange/act/assert structure
- Use `testing.allocator` (detects leaks)
- Group related tests in same file

## Snapshot Testing

**Primary Testing Strategy** - Snapshots test compiler behavior across all stages.

**Snapshot File Format:**
```markdown
~~~META
description=Test description here
type=expr
~~~

~~~SOURCE
1 + 2
~~~

~~~EXPECTED
3
~~~

~~~TOKENS
(generated)
~~~

~~~PARSE
(generated)
~~~

~~~CANONICALIZE
(generated)
~~~

~~~TYPES
(generated)
~~~

~~~PROBLEMS
(generated)
~~~
```

**META Types:**
- `type=file` - Full file compilation
- `type=expr` - Single expression
- `type=statement` - Statement
- `type=header` - Module header
- `type=repl` - REPL session

**REPL Snapshots:**
- SOURCE uses `»` prefix for commands
- EXPECTED uses `---` to separate outputs

**Snapshot Commands:**
```bash
zig build snapshot                    # Generate/update all
zig build snapshot -- path/to/test.md # Specific file
zig build update-expected -- path.md  # Update EXPECTED from PROBLEMS
```

## Mocking

**Framework:**
- No external mocking framework
- Manual test doubles via interfaces

**Patterns:**
```zig
// Test environment provides mock implementations
const TestEnv = struct {
    allocator: std.mem.Allocator,
    // ... test-specific state

    pub fn init(allocator: std.mem.Allocator) TestEnv {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *TestEnv) void {
        // cleanup
    }
};
```

**What to Mock:**
- Filesystem operations (via `src/fs/Filesystem.zig` abstraction)
- External process calls

**What NOT to Mock:**
- Core compiler logic (test through snapshots)
- Type system internals (use real implementation)

## Fixtures and Factories

**Test Data:**
```zig
// TestEnv.zig provides shared setup
const TestEnv = @import("TestEnv.zig");

test "with test environment" {
    var env = TestEnv.init(testing.allocator);
    defer env.deinit();

    // use env for test setup
}
```

**Location:**
- `*/test/TestEnv.zig` - Per-module test environment
- `*/test/helpers.zig` - Shared utilities
- Inline test data for simple cases

## Coverage

**Requirements:**
- No enforced coverage target
- Focus on critical paths (parser, type checker, interpreter)
- Snapshot tests provide broad coverage

**Configuration:**
- kcov available via Nix (on Linux)
- No built-in Zig coverage tooling used

## Test Types

**Unit Tests:**
- Test single function/struct in isolation
- Located in `*/test/*.zig`
- Fast execution
- Examples: `src/check/test/unify_test.zig`, `src/parse/test/ast_node_store_test.zig`

**Integration Tests:**
- Test multiple modules together
- Often use TestEnv for setup
- Examples: `src/check/test/type_checking_integration.zig`, `src/check/test/cross_module_test.zig`

**Snapshot Tests:**
- Test full compiler pipeline
- Located in `test/snapshots/*.md`
- Primary testing strategy
- Run via `zig build snapshot`

**REPL Tests:**
- Test REPL behavior
- Located in `src/repl/repl_test.zig`
- Run via `zig build test-repl`

## Common Patterns

**Async/Error Testing:**
```zig
test "should return error" {
    const result = fallibleFunction();
    try testing.expectError(error.SomeError, result);
}

test "should succeed" {
    const result = try fallibleFunction();
    try testing.expectEqual(expected, result);
}
```

**Memory Leak Detection:**
```zig
test "no memory leaks" {
    // testing.allocator detects leaks automatically
    const allocator = testing.allocator;

    var data = try allocator.alloc(u8, 100);
    defer allocator.free(data);
    // test passes only if all allocations freed
}
```

**Test Organization in mod.zig:**
```zig
// Re-export tests for zig build test
test {
    _ = @import("test/unify_test.zig");
    _ = @import("test/exhaustiveness_test.zig");
}
```

## Test Gaps

**Known Gaps:**
- Backend/codegen lacks fine-grained unit tests
- Some skipped tests due to segfaults (see CONCERNS.md)
- Cross-module function calls need more coverage
- LLVM builder lacks targeted tests

**Adding Tests:**
- New features: Add snapshot test first
- Bug fixes: Add regression snapshot
- Unit tests for complex logic
- Integration tests for module interactions

---

*Testing analysis: 2026-01-20*
*Update when test patterns change*
