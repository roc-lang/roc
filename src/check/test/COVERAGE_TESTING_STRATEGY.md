# Type Checker Coverage Testing Strategy

This document outlines strategies for testing error paths in `problem.zig` that are difficult to trigger through standard integration tests.

## Current Coverage Status

As of this writing:
- **Overall**: 83.27%
- **problem.zig**: 53.9% (848/1839 lines uncovered)
- **Check.zig**: 77.9%
- **exhaustive.zig**: 86.8%
- **unify.zig**: 86.5%

## Error Categories and Testing Strategies

### 1. Well-Covered Error Types (No Additional Work Needed)

These are exercised by existing tests:
- `TYPE MISMATCH` (generic and specific variants)
- `INCOMPATIBLE MATCH PATTERNS`
- `INCOMPATIBLE MATCH BRANCHES`
- `NON-EXHAUSTIVE MATCH`
- `REDUNDANT PATTERN`
- `UNMATCHABLE PATTERN`
- `TOO MANY ARGUMENTS` / `TOO FEW ARGUMENTS`
- `TOO MANY ARGS` / `TOO FEW ARGS` (type application)
- `INVALID BOOL OPERATION`
- `INVALID IF CONDITION`
- `TYPE DOES NOT SUPPORT EQUALITY`
- `MISSING METHOD`
- `RECURSIVE ALIAS`

### 2. Comptime-Triggered Errors

**Error Types:**
- `NUMBER DOES NOT FIT IN TYPE`
- `NEGATIVE UNSIGNED INTEGER`
- `INVALID NUMERIC LITERAL`
- `COMPTIME CRASH`
- `COMPTIME EXPECT FAILED`
- `COMPTIME EVAL ERROR`

**Why They're Hard to Test:**
These errors are detected during compile-time evaluation, which runs AFTER type checking. The type checker creates `DeferredNumericLiteral` entries that are validated during comptime.

**Testing Strategy:**
1. **Unit Test the Report Builders Directly**: Create mock `NumberDoesNotFit`, `NegativeUnsignedInt`, etc. data structures and call the report builder functions directly.

2. **Integration Tests with Comptime Module**: Create tests that go through the full compilation pipeline including comptime evaluation:
   ```zig
   test "comptime - number overflow U8" {
       const source =
           \\x : U8
           \\x = 256
       ;
       // Need a test helper that runs comptime evaluation
       var test_env = try TestEnv.initWithComptime("Test", source);
       defer test_env.deinit();
       try test_env.assertComptimeError("NUMBER DOES NOT FIT IN TYPE");
   }
   ```

3. **Add Comptime Test Infrastructure**: Extend `TestEnv.zig` with methods that trigger comptime evaluation after type checking.

### 3. Platform Requirement Errors

**Error Types:**
- `MISSING PLATFORM REQUIRED TYPE`
- `MISSING PLATFORM REQUIRED DEFINITION`

**Why They're Hard to Test:**
These require a platform module that specifies required types/definitions, and an app module that fails to provide them.

**Testing Strategy:**
1. **Create Platform Test Fixtures**: Add test helper for creating mock platform modules:
   ```zig
   test "platform - missing required type" {
       const platform_source =
           \\module [requires { Model }]
       ;
       const app_source =
           \\# Missing Model type alias
           \\main = {}
       ;
       var test_env = try TestEnv.initPlatformApp(platform_source, app_source);
       defer test_env.deinit();
       try test_env.assertFirstTypeError("MISSING PLATFORM REQUIRED TYPE");
   }
   ```

2. **Unit Test Report Builders**: Call `buildPlatformAliasNotFound` and `buildPlatformDefNotFound` directly with mock data.

### 4. Cross-Module Import Errors

**Error Type:**
- `TYPE MISMATCH` with `cross_module_import` detail

**Why They're Hard to Test:**
Requires specific type mismatches between imported and expected types across module boundaries.

**Testing Strategy:**
Use `TestEnv.initWithImport()` which already exists:
```zig
test "cross module - import type mismatch" {
    const module_a =
        \\getValue : I64
        \\getValue = 42
    ;
    var env_a = try TestEnv.init("A", module_a);
    defer env_a.deinit();

    const module_b =
        \\import A
        \\
        \\x : Str
        \\x = A.getValue
    ;
    var env_b = try TestEnv.initWithImport("B", module_b, "A", &env_a);
    defer env_b.deinit();

    try env_b.assertFirstTypeError("TYPE MISMATCH");
}
```

### 5. Unused Value Errors

**Error Type:**
- `UNUSED VALUE`

**Why They're Hard to Test:**
Requires expressions in statement position that produce non-unit values.

**Testing Strategy:**
```zig
test "unused value - discarded expression" {
    const source =
        \\f = |_| {
        \\    42       # This produces I64, not {}
        \\    "result"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertFirstTypeError("UNUSED VALUE");
}
```

Note: The block syntax `{ expr1; expr2 }` may not trigger this if block semantics return last expression. Need to verify exact syntax that triggers unused value detection.

### 6. Unimplemented Error Types

**Error Types:**
- `infinite_recursion`
- `anonymous_recursion`
- `invalid_number_type`
- `invalid_record_ext`
- `invalid_tag_union_ext`
- `bug`

**Status:**
These currently return `buildUnimplementedReport()` placeholder. Testing them requires:
1. First implementing the actual report builders
2. Then creating test cases that trigger those error conditions

## Recommended Implementation Order

1. **High Value, Low Effort**:
   - Cross-module import type mismatches (infrastructure exists)
   - Unused value detection (if syntax can be determined)

2. **Medium Effort**:
   - Direct unit tests for report builder functions
   - Platform requirement tests (need platform module fixtures)

3. **High Effort**:
   - Comptime evaluation tests (need TestEnv comptime support)
   - Implementing unimplemented error types

## Direct Unit Testing Approach

For maximum coverage with minimal effort, create a dedicated test file that calls report builder functions directly:

```zig
// problem_unit_test.zig
const problem = @import("../problem.zig");

test "buildNumberDoesNotFitReport generates correct report" {
    // Create mock data structures
    var store = try ProblemStore.init(allocator);
    defer store.deinit();

    // Add test data
    const problem_idx = store.add(.{
        .number_does_not_fit = .{
            .literal_var = test_var,
            .expected_type = snapshot_idx,
        },
    });

    // Build and verify report
    const report = try store.build(store.get(problem_idx));
    try testing.expectEqualStrings("NUMBER DOES NOT FIT IN TYPE", report.title);
}
```

This approach bypasses the need to trigger error conditions through the type checker and directly tests the report generation logic.

## Dead Code Removed

The following dead code was identified and removed:
- `buildNumberDoesNotFitReport` (~35 lines)
- `buildNegativeUnsignedIntReport` (~40 lines)
- `buildInvalidNumericLiteralReport` (~65 lines)
- Associated struct definitions (`NumberDoesNotFit`, `NegativeUnsignedInt`, `InvalidNumericLiteral`)
- Error types in unify.zig error set

**Why it was dead code:**
The error types were defined but never returned anywhere. Number validation happens during comptime evaluation (in `validateDeferredNumericLiterals`), which creates `comptime_eval_error` problems instead of the specific number validation problems.

## Summary

The main coverage gaps are in error paths that require:
1. **Platform modules** (platform requirement errors)
2. **Unimplemented features** (extension type errors returning "unimplemented")
