//! Shared test specifications for fx platform tests.
//!
//! This module defines IO specs for all fx platform tests that can be run
//! using the --test mode. These specs are shared between:
//! - Native Zig tests (fx_platform_test.zig)
//! - Unified test platform runner (test_runner.zig)
//!
//! IO Spec Format: "0<stdin|1>stdout|2>stderr" (pipe-separated)
//! - 0<text: stdin input
//! - 1>text: expected stdout output
//! - 2>text: expected stderr output

/// Test specification with a roc file path and expected IO spec
pub const TestSpec = struct {
    /// Path to the roc file (relative to project root)
    roc_file: []const u8,
    /// IO spec for --test mode
    io_spec: []const u8,
    /// Optional description of what the test verifies
    description: []const u8 = "",
};

/// All fx platform tests that can be run with --test mode IO specs.
/// These tests work with cross-compilation because they only test
/// the compiled binary's IO behavior, not build-time features.
pub const io_spec_tests = [_]TestSpec{
    // Basic effectful function tests
    .{
        .roc_file = "test/fx/app.roc",
        .io_spec = "1>Hello from stdout!|1>Line 1 to stdout|2>Line 2 to stderr|1>Line 3 to stdout|2>Error from stderr!",
        .description = "Basic effectful functions: Stdout.line!, Stderr.line!",
    },
    .{
        .roc_file = "test/fx/subdir/app.roc",
        .io_spec = "1>Hello from stdout!|1>Line 1 to stdout|2>Line 2 to stderr|1>Line 3 to stdout|2>Error from stderr!",
        .description = "Relative paths starting with ..",
    },

    // Stdin tests
    .{
        .roc_file = "test/fx/stdin_to_stdout.roc",
        .io_spec = "0<test input|1>test input",
        .description = "Stdin to stdout passthrough",
    },
    .{
        .roc_file = "test/fx/stdin_echo.roc",
        .io_spec = "0<hello world|1>hello world",
        .description = "Stdin echo",
    },
    .{
        .roc_file = "test/fx/stdin_test.roc",
        .io_spec = "1>Before stdin|0<user input|1>After stdin",
        .description = "Stdin with output before and after",
    },
    .{
        .roc_file = "test/fx/stdin_simple.roc",
        .io_spec = "0<simple test|2>simple test",
        .description = "Stdin to stderr",
    },

    // Match expression tests
    .{
        .roc_file = "test/fx/match_str_return.roc",
        .io_spec = "1>0",
        .description = "Match expressions with string returns",
    },
    .{
        .roc_file = "test/fx/match_with_wildcard.roc",
        .io_spec = "1>0",
        .description = "Wildcard patterns in match expressions",
    },

    // Opaque type tests
    .{
        .roc_file = "test/fx/opaque_with_method.roc",
        .io_spec = "1>My favourite color is Red",
        .description = "Opaque type with attached method",
    },

    // Language feature tests
    .{
        .roc_file = "test/fx/question_mark_operator.roc",
        .io_spec = "1>hello",
        .description = "Question mark operator for error propagation",
    },
    .{
        .roc_file = "test/fx/numeric_fold.roc",
        .io_spec = "1>Sum: 15.0",
        .description = "List.fold with numeric accumulators",
    },
    .{
        .roc_file = "test/fx/list_for_each.roc",
        .io_spec = "1>Item: apple|1>Item: banana|1>Item: cherry",
        .description = "List.for_each! with effectful callback",
    },
    .{
        .roc_file = "test/fx/string_pattern_matching.roc",
        .io_spec = "1>Hello Alice!|1>Hey Bob!",
        .description = "Pattern matching on string literals",
    },

    // Basic output tests
    .{
        .roc_file = "test/fx/hello_world.roc",
        .io_spec = "1>Hello, world!",
        .description = "Hello world",
    },
    .{
        .roc_file = "test/fx/function_wrapper_stdout.roc",
        .io_spec = "1>Hello from stdout!",
        .description = "Function wrapper stdout",
    },
    .{
        .roc_file = "test/fx/function_wrapper_multiline.roc",
        .io_spec = "1>Hello from stdout!|1>Line 2",
        .description = "Function wrapper multiline output",
    },
    .{
        .roc_file = "test/fx/multiline_stdout.roc",
        .io_spec = "1>Hello|1>World",
        .description = "Multiple stdout lines",
    },

    // List and string tests
    .{
        .roc_file = "test/fx/empty_list_get.roc",
        .io_spec = "1>is err",
        .description = "Empty list get returns error",
    },
    .{
        .roc_file = "test/fx/str_interp_valid.roc",
        .io_spec = "1>Hello, World!",
        .description = "String interpolation",
    },

    // Lookup tests
    .{
        .roc_file = "test/fx/numeric_lookup_test.roc",
        .io_spec = "1>done",
        .description = "Numeric lookup",
    },
    .{
        .roc_file = "test/fx/string_lookup_test.roc",
        .io_spec = "1>hello",
        .description = "String lookup",
    },
    .{
        .roc_file = "test/fx/test_direct_string.roc",
        .io_spec = "1>Hello",
        .description = "Direct string output",
    },
    .{
        .roc_file = "test/fx/test_one_call.roc",
        .io_spec = "1>Hello",
        .description = "Single function call",
    },
    .{
        .roc_file = "test/fx/test_with_wrapper.roc",
        .io_spec = "1>Hello",
        .description = "Function with wrapper",
    },

    // Inspect tests
    .{
        .roc_file = "test/fx/inspect_compare_test.roc",
        .io_spec = "1>With to_inspect: Custom::Red|1>Without to_inspect: ColorWithoutInspect.Red|1>Primitive: 42",
        .description = "Inspect comparison with and without to_inspect",
    },
    .{
        .roc_file = "test/fx/inspect_custom_test.roc",
        .io_spec = "1>Color::Red|1>Expected: Color::Red",
        .description = "Custom inspect implementation",
    },
    .{
        .roc_file = "test/fx/inspect_nested_test.roc",
        // Note: field order may differ from expected - record fields are rendered in their internal order
        .io_spec = "1>{ color: Color::Red, count: 42, name: \"test\" }|1>Expected: { color: Color::Red, count: 42, name: \"test\" }",
        .description = "Nested struct inspection",
    },
    .{
        .roc_file = "test/fx/inspect_no_method_test.roc",
        .io_spec = "1>Result: Color.Red|1>(Default rendering)",
        .description = "Inspect without to_inspect method",
    },
    .{
        .roc_file = "test/fx/inspect_record_test.roc",
        .io_spec = "1>{ count: 42, name: \"test\" }",
        .description = "Record inspection",
    },
    .{
        .roc_file = "test/fx/inspect_wrong_sig_test.roc",
        .io_spec = "1>Result: 1",
        .description = "Inspect with wrong signature",
    },
    .{
        .roc_file = "test/fx/inspect_open_tag_test.roc",
        .io_spec = "1>Closed: TagB|1>With payload: Value(42)|1>Number: 123",
        .description = "Str.inspect on tag unions",
    },
    // Bug regression tests
    .{
        .roc_file = "test/fx/unify_scratch_fresh_vars_rank_bug.roc",
        .io_spec = "1>ok",
        .description = "Regression test: unify scratch fresh_vars must be cleared between calls",
    },
    .{
        .roc_file = "test/fx/recursive_tuple_list.roc",
        .io_spec = "1>Result count: 4",
        .description = "Regression test: recursive function with List of tuples and append",
    },
    .{
        .roc_file = "test/fx/list_map_fallible.roc",
        .io_spec = "1>done",
        .description = "Regression test: List.map with fallible function (U64.from_str)",
    },
    .{
        .roc_file = "test/fx/list_append_stdin_uaf.roc",
        .io_spec = "0<000000010000000100000001|1>000000010000000100000001",
        .description = "Regression test: List.append with effectful call on big string (24+ chars)",
    },
    .{
        .roc_file = "test/fx/list_first_method.roc",
        .io_spec = "1>ok",
        .description = "Regression test: List.first with method syntax",
    },
    .{
        .roc_file = "test/fx/list_first_function.roc",
        .io_spec = "1>ok",
        .description = "Regression test: List.first with function syntax",
    },
    .{
        .roc_file = "test/fx/stdin_while_uaf.roc",
        .io_spec = "0<123456789012345678901234|1>123456789012345678901234|0<|1>",
        .description = "Regression test: Stdin.line! in while loop with 24 char input (heap-allocated string)",
    },
    .{
        .roc_file = "test/fx/stdin_while_uaf.roc",
        .io_spec = "0<short|1>short|0<|1>",
        .description = "Regression test: Stdin.line! in while loop with short input (small string optimization)",
    },
    .{
        .roc_file = "test/fx/list_method_get.roc",
        .io_spec = "1>is ok",
        .description = "Regression test: List.get with method syntax (issue #8662)",
    },
    .{
        .roc_file = "test/fx/issue8654.roc",
        .io_spec = "1>False",
        .description = "Regression test: Method lookup for nominal types in roc build executables (issue #8654)",
    },
    .{
        .roc_file = "test/fx/dbg_corrupts_recursive_tag_union.roc",
        .io_spec = "1>Child is Text: hello",
        .description = "Regression test: dbg on recursive tag union preserves variant discriminant (issue #8804)",
    },
    .{
        .roc_file = "test/fx/issue8795.roc",
        .io_spec = "1>type: Type.Array((0, Box(<tag_union variant=1>)))",
        .description = "Regression test: recursive opaque type with nested payload (issue #8795)",
    },
};

/// Get the total number of IO spec tests
pub fn getTestCount() usize {
    return io_spec_tests.len;
}

/// Find a test spec by roc file path
pub fn findByPath(roc_file: []const u8) ?TestSpec {
    for (io_spec_tests) |spec| {
        if (std.mem.eql(u8, spec.roc_file, roc_file)) {
            return spec;
        }
    }
    return null;
}

const std = @import("std");

test "all test specs have valid paths" {
    for (io_spec_tests) |spec| {
        // Just verify the paths are non-empty and start with test/fx
        try std.testing.expect(spec.roc_file.len > 0);
        try std.testing.expect(std.mem.startsWith(u8, spec.roc_file, "test/fx"));
        try std.testing.expect(spec.io_spec.len > 0);
    }
}

test "find by path works" {
    const found = findByPath("test/fx/hello_world.roc");
    try std.testing.expect(found != null);
    try std.testing.expectEqualStrings("1>Hello, world!", found.?.io_spec);

    const not_found = findByPath("nonexistent.roc");
    try std.testing.expect(not_found == null);
}
