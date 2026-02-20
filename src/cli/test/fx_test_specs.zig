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
    /// If set, test is skipped with this reason
    skip: []const u8 = "",
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
    .{
        .roc_file = "test/fx/test_issue9034.roc",
        .io_spec = "1>test",
        .description = "Platform-exposed opaque types in type annotations (issue #9034)",
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
        .io_spec = "1>done: 42",
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
        .io_spec = "1>With to_inspect: Custom::Red|1>Without to_inspect: ColorWithoutInspect.Red|1>Primitive: 42.0",
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
        .io_spec = "1>{ color: Color::Red, count: 42.0, name: \"test\" }|1>Expected: { color: Color::Red, count: 42, name: \"test\" }",
        .description = "Nested struct inspection",
    },
    .{
        .roc_file = "test/fx/inspect_no_method_test.roc",
        .io_spec = "1>Result: Color.Red|1>(Default rendering)",
        .description = "Inspect without to_inspect method",
    },
    .{
        .roc_file = "test/fx/inspect_record_test.roc",
        .io_spec = "1>{ count: 42.0, name: \"test\" }",
        .description = "Record inspection",
    },
    .{
        .roc_file = "test/fx/inspect_wrong_sig_test.roc",
        .io_spec = "1>Result: 1",
        .description = "Inspect with wrong signature",
    },
    .{
        .roc_file = "test/fx/inspect_open_tag_test.roc",
        .io_spec = "1>Closed: TagB|1>With payload: Value(42)|1>Number: 123.0",
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
        .skip = "dev backend does not yet support List.map with fallible functions",
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
        .roc_file = "test/fx/primitive_encode.roc",
        .io_spec = "1>Bool.encode(True): true|1>U64.encode(42): 42|1>Done",
        .description = "Primitive types have encode methods for static dispatch (issue #8853)",
    },
    .{
        .roc_file = "test/fx/issue8866.roc",
        .io_spec = "1>Done: 2",
        .description = "Regression test: List.append with opaque type containing Str (issue #8866)",
    },
    .{
        .roc_file = "test/fx/issue8897.roc",
        .io_spec = "1>done",
        .description = "Regression test: Multiple expects with polymorphic function panic (issue #8897)",
    },
    .{
        .roc_file = "test/fx/issue8897_min.roc",
        .io_spec = "1>done",
        .description = "Regression test: Minimal repro for issue #8897 panic",
    },
    .{
        .roc_file = "test/fx/issue8898.roc",
        .io_spec = "1>done",
        .description = "Regression test: Polymorphic function with for loop list literal panic (issue #8898)",
    },
    .{
        .roc_file = "test/fx/static_dispatch_platform_module.roc",
        .io_spec = "1>Result: start-middle-end",
        .description = "Regression test: Static dispatch on platform-exposed opaque types (issue #8928)",
    },
    .{
        .roc_file = "test/fx/static_dispatch_effect_bug.roc",
        .io_spec = "1>SUCCESS: Builder.print_value! called via static dispatch!|1>  value: test|1>  count: 0",
        .description = "Regression test: Static dispatch on effect methods (issue #8928)",
    },
    .{
        .roc_file = "test/fx/record_builder_cli_parser.roc",
        // Multi-line strings (\\) create strings with embedded newlines - use \n in spec
        .io_spec = "1>=== Record Builder ===\n|1>Test 1: Two-field record builder\n  Result: host=localhost, port=8080\n|1>Test 2: Three-field record builder\n  Result: name=world, count=1, verbose=False\n|1>Test 3: Four-field record builder\n  Result: w=10, x=20, y=30, z=40\n|1>Test 4: Combined help text\n  Help:  --input <value>  --output <value>|1>Test 5: Equivalence with direct map2|1>  Builder: a=1, b=2|1>  Direct:  a=1, b=2|1>|1>=== All tests passed! ===",
        .description = "True applicative record builder: { a: Cli.option(...), b: Cli.flag(...) }.Cli with parameterized Cli(a) type",
    },
    .{
        .roc_file = "test/fx/issue9049.roc",
        .io_spec = "1>Direct: False|1>Via pure/run: False",
        .description = "Regression test: Bool.False inspected via opaque type extraction shows correct value (issue #9049)",
    },
    .{
        .roc_file = "test/fx/hosted_effect_opaque_with_data.roc",
        .io_spec = "1>Hello, World!",
        .description = "Regression test: Hosted effects on opaque types with data (not just [])",
    },
    .{
        .roc_file = "test/fx/record_field_access.roc",
        .io_spec = "1>Alice|1>30|1>100",
        .description = "Regression test: Record field access with alignment-reordered fields (layout order != monotype order)",
    },
    .{
        .roc_file = "test/fx/early_return_rc.roc",
        .io_spec = "1>empty",
        .description = "Regression test: Early return properly cleans up live refcounted symbols",
    },
    .{
        .roc_file = "test/fx/float_comparison.roc",
        .io_spec = "1>3.14 > 0.0: True|1>0.0 < 3.14: True|1>3.14 >= 3.14: True",
        .description = "Regression test: F64 comparisons use float instructions, not integer bit-pattern",
    },
    .{
        .roc_file = "test/fx/many_args.roc",
        .io_spec = "1>36",
        .description = "Test: Function with 8 arguments exercises register spilling",
    },
    .{
        .roc_file = "test/fx/or_pattern.roc",
        .io_spec = "1>cool|1>warm|1>cool|1>warm",
        .description = "Test: OR-pattern (pat1 | pat2 => body) with tag union",
    },
    .{
        .roc_file = "test/fx/match_guard_basic.roc",
        .io_spec = "1>positive",
        .description = "Match guard: guard passes on wildcard bind",
    },
    .{
        .roc_file = "test/fx/match_guard_fallthrough.roc",
        .io_spec = "1>small",
        .description = "Match guard: guard fails, falls to next branch",
    },
    .{
        .roc_file = "test/fx/match_guard_tag.roc",
        .io_spec = "1>big some",
        .description = "Match guard: guard on tag payload (Some(n) if n > 5)",
    },
    .{
        .roc_file = "test/fx/match_guard_multiple.roc",
        .io_spec = "1>positive",
        .description = "Match guard: multiple guarded branches, only third matches",
    },
    .{
        .roc_file = "test/fx/record_destructure.roc",
        .io_spec = "1>Bob 25 99",
        .description = "Regression test: Record destructuring with alignment-reordered fields",
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
