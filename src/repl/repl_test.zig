//! Tests for the REPL
const builtin = @import("builtin");
const std = @import("std");
const Repl = @import("eval.zig").Repl;
const TestEnv = @import("repl_test_env.zig").TestEnv;

const testing = std.testing;
const posix = std.posix;

const alloc = std.testing.allocator;
const Backend = enum { interpreter, dev, wasm, llvm };

/// Run expression on interpreter only (for tests with known dev backend bugs).
fn expectInterpreter(expr: []const u8, expected: []const u8) !void {
    var test_env = TestEnv.init(alloc);
    defer test_env.deinit();
    var repl = try Repl.init(alloc, test_env.get_ops(), null);
    defer repl.deinit();
    const result = try repl.step(expr);
    defer alloc.free(result);
    testing.expectEqualStrings(expected, result) catch |err| {
        std.debug.print("INTERPRETER FAILED for: {s}\n", .{expr});
        return err;
    };
}

fn expectBackend(backend: Backend, expr: []const u8, expected: []const u8) !void {
    var test_env = TestEnv.init(alloc);
    defer test_env.deinit();
    var repl = switch (backend) {
        .interpreter => try Repl.init(alloc, test_env.get_ops(), null),
        .dev => try Repl.initWithBackend(alloc, test_env.get_ops(), test_env.crashContextPtr(), .dev),
        .wasm => try Repl.initWithWasmBackend(alloc, test_env.get_ops(), test_env.crashContextPtr()),
        .llvm => try Repl.initWithBackend(alloc, test_env.get_ops(), test_env.crashContextPtr(), .llvm),
    };
    defer repl.deinit();

    const result = try repl.step(expr);
    defer alloc.free(result);
    testing.expectEqualStrings(expected, result) catch |err| {
        const backend_name = switch (backend) {
            .interpreter => "INTERPRETER",
            .dev => "DEV BACKEND",
            .wasm => "WASM BACKEND",
            .llvm => "LLVM BACKEND",
        };
        std.debug.print("{s} FAILED for: {s}\n", .{ backend_name, expr });
        return err;
    };
}

/// Run expression on interpreter, dev, wasm, and llvm backends, assert same expected output.
fn expectAllNative(expr: []const u8, expected: []const u8) !void {
    try expectBackend(.interpreter, expr, expected);
    try expectBackend(.dev, expr, expected);
    try expectBackend(.wasm, expr, expected);
    try expectBackend(.llvm, expr, expected);
}

test "Repl - initialization and cleanup" {
    var test_env = TestEnv.init(alloc);
    defer test_env.deinit();
    var repl = try Repl.init(alloc, test_env.get_ops(), null);
    defer repl.deinit();
    try testing.expect(repl.definitions.count() == 0);
}

test "Repl - special commands" {
    var test_env = TestEnv.init(alloc);
    defer test_env.deinit();
    var repl = try Repl.init(alloc, test_env.get_ops(), null);
    defer repl.deinit();

    const help_result = try repl.step(":help");
    defer alloc.free(help_result);
    try testing.expect(std.mem.indexOf(u8, help_result, "Enter an expression") != null);

    const exit_result = try repl.step(":exit");
    defer alloc.free(exit_result);
    try testing.expectEqualStrings("Goodbye!", exit_result);

    const empty_result = try repl.step("");
    defer alloc.free(empty_result);
    try testing.expectEqualStrings("", empty_result);
}

test "Repl - simple expressions" {
    try expectAllNative("42", "42.0");
}

test "Repl - string expressions" {
    try expectAllNative("\"Hello, World!\"", "\"Hello, World!\"");
}

test "Repl - Bool.True" {
    try expectAllNative("Bool.True", "True");
}

test "Repl - Bool.False" {
    try expectAllNative("Bool.False", "False");
}

test "Repl - Bool.not(False)" {
    try expectAllNative("Bool.not(False)", "True");
}

test "Repl - Bool.not(Bool.True)" {
    try expectAllNative("Bool.not(Bool.True)", "False");
}

test "Repl - Bool.not(Bool.False)" {
    try expectAllNative("Bool.not(Bool.False)", "True");
}

test "Repl - !Bool.True" {
    try expectAllNative("!Bool.True", "False");
}

test "Repl - !Bool.False" {
    try expectAllNative("!Bool.False", "True");
}

test "Repl - I8.mod_by negative positive" {
    try expectAllNative("I8.mod_by(-10, 3)", "2");
}

test "Repl - I8.mod_by positive negative" {
    try expectAllNative("I8.mod_by(10, -3)", "-2");
}

test "Repl - I8.mod_by negative negative" {
    try expectAllNative("I8.mod_by(-10, -3)", "-1");
}

test "Repl - Str.is_empty" {
    try expectAllNative("Str.is_empty(\"\")", "True");
    try expectAllNative("Str.is_empty(\"a\")", "False");
}

test "Repl - lambda renders as <function>" {
    try expectAllNative("|x| x + 1", "<function>");
    try expectAllNative("|x, y| x + y", "<function>");
}

test "Repl - Str.to_utf8" {
    try expectAllNative("Str.to_utf8(\"hello\")", "[104, 101, 108, 108, 111]");
    try expectAllNative("List.len(Str.to_utf8(\"\"))", "0");
    try expectAllNative("List.len(Str.to_utf8(\"hello\"))", "5");
    try expectAllNative("List.len(Str.to_utf8(\"é\"))", "2");
    try expectAllNative("List.len(Str.to_utf8(\"🎉\"))", "4");
    try expectAllNative("List.len(Str.to_utf8(\"Hello, World!\"))", "13");
    try expectAllNative("List.len(Str.to_utf8(\"日本語\"))", "9");
    try expectAllNative("List.len(Str.to_utf8(\"a é 🎉\"))", "9");
    try expectAllNative("List.is_empty(Str.to_utf8(\"\"))", "True");
    try expectAllNative("List.is_empty(Str.to_utf8(\"x\"))", "False");
}

test "Repl - Str.from_utf8_lossy" {
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"hello\"))", "\"hello\"");
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"\"))", "\"\"");
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"🎉 party!\"))", "\"🎉 party!\"");
    try expectAllNative("Str.from_utf8_lossy(Str.to_utf8(\"abc123\"))", "\"abc123\"");
}

test "Repl - Str.from_utf8 Ok" {
    try expectAllNative("Str.from_utf8([72, 105])", "Ok(\"Hi\")");
}

test "Repl - Str.from_utf8 ok_or" {
    try expectAllNative("Str.from_utf8([72, 105]).ok_or(\"fallback\")", "\"Hi\"");
}

test "Repl - Str.from_utf8 snapshot sequence" {
    const steps = &[_][2][]const u8{
        .{ "Str.from_utf8([72, 105])", "Ok(\"Hi\")" },
        .{ "Str.from_utf8([])", "Ok(\"\")" },
        .{ "Str.from_utf8([82, 111, 99])", "Ok(\"Roc\")" },
        .{ "Str.from_utf8([240, 159, 144, 166])", "Ok(\"🐦\")" },
        .{ "Str.from_utf8([195, 169])", "Ok(\"é\")" },
        .{ "Str.from_utf8([255]).is_err()", "True" },
        .{ "Str.from_utf8([72, 105]).is_ok()", "True" },
        .{ "Str.from_utf8([72, 105]).ok_or(\"fallback\")", "\"Hi\"" },
        .{ "Str.from_utf8([255]).ok_or(\"fallback\")", "\"fallback\"" },
        .{ "Str.from_utf8([255])", "Err(BadUtf8({ index: 0, problem: InvalidStartByte }))" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
}

test "Repl - U8.from_str result format" {
    try expectAllNative("U8.from_str(\"42\")", "Ok(42)");
}

test "Repl - F32.from_str result format" {
    try expectAllNative("F32.from_str(\"3.14\")", "Ok(3.14)");
}

test "Repl - list literals" {
    try expectAllNative("List.len([1, 2, 3])", "3");
    try expectAllNative("[1, 2, 3]", "[1.0, 2.0, 3.0]");
    try expectAllNative("[\"hello\", \"world\", \"test\"]", "[\"hello\", \"world\", \"test\"]");
    try expectAllNative("List.len([\"hello\", \"world\", \"test\"])", "3");
}

test "Repl - list operations" {
    try expectAllNative("List.len(List.concat([1, 2], [3, 4]))", "4");
    try expectAllNative("List.len(List.concat([], [1, 2, 3]))", "3");
    try expectAllNative("List.len(List.concat([1, 2, 3], []))", "3");
    try expectAllNative("List.contains([1, 2, 3, 4, 5], 3)", "True");
    try expectAllNative("List.drop_if([1, 2, 3, 4, 5], |x| x > 2)", "[1.0, 2.0]");
    try expectAllNative("List.keep_if([1, 2, 3, 4, 5], |x| x > 2)", "[3.0, 4.0, 5.0]");
    try expectAllNative("List.keep_if([1, 2, 3], |_| Bool.False)", "[]");
    try expectAllNative("List.fold_rev([1.I64, 2.I64, 3.I64], 0.I64, |x, acc| acc * 10 + x)", "321");
    try expectAllNative("List.fold_rev([1], 0, |x, acc| acc * 10 + x)", "1.0");
    try expectAllNative("List.fold_rev([1, 2, 3], 0, |x, acc| acc * 10 + x)", "321.0");
    try expectAllNative("List.fold_rev([], 42, |x, acc| x + acc)", "42.0");
}

test "Repl - List.with_capacity" {
    try expectAllNative("List.with_capacity(10)", "[]");
    // TODO: List.first on empty list returns Ok({}) in dev backend instead of Err(ListWasEmpty)
    // try expectBoth("List.first(List.with_capacity(10))", "Err(ListWasEmpty)");
    try expectInterpreter("List.first(List.with_capacity(10))", "Err(ListWasEmpty)");
}

test "Repl - List.append" {
    try expectAllNative("List.append([1, 2], 3)", "[1.0, 2.0, 3.0]");
}

test "Repl - range_to" {
    // TODO: Dev backend crashes on 1.to(3) — Dec range_to not fully supported
    // try expectBoth("1.to(3)", "[1.0, 2.0, 3.0]");
    try expectInterpreter("1.to(3)", "[1.0, 2.0, 3.0]");
}

test "Repl - list_sort_with" {
    try expectAllNative("List.len(List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ))", "3");
    try expectAllNative("List.len(List.sort_with([5, 2, 8, 1, 9], |a, b| if a < b LT else if a > b GT else EQ))", "5");
    try expectAllNative("List.len(List.sort_with([], |a, b| if a < b LT else if a > b GT else EQ))", "0");
    try expectAllNative("List.len(List.sort_with([42], |a, b| if a < b LT else if a > b GT else EQ))", "1");
}

test "Repl - list fold with concat" {
    try expectAllNative("List.len(List.fold([1, 2, 3], [], |acc, x| List.concat(acc, [x])))", "3");
}

// Stateful tests (assignments, variable redefinition) - these use multi-step REPL
// sessions so we test each backend separately but with the same expectations.

fn expectStateful(backend: Backend, steps: []const [2][]const u8) !void {
    var test_env = TestEnv.init(alloc);
    defer test_env.deinit();
    var repl = switch (backend) {
        .interpreter => try Repl.init(alloc, test_env.get_ops(), null),
        .dev => try Repl.initWithBackend(alloc, test_env.get_ops(), null, .dev),
        .wasm => try Repl.initWithWasmBackend(alloc, test_env.get_ops(), null),
        .llvm => try Repl.initWithBackend(alloc, test_env.get_ops(), null, .llvm),
    };
    defer repl.deinit();

    for (steps) |step| {
        const result = try repl.step(step[0]);
        defer alloc.free(result);
        testing.expectEqualStrings(step[1], result) catch |err| {
            const backend_name = switch (backend) {
                .interpreter => "INTERPRETER",
                .dev => "DEV BACKEND",
                .wasm => "WASM BACKEND",
                .llvm => "LLVM BACKEND",
            };
            std.debug.print("{s} FAILED for: {s}\n", .{ backend_name, step[0] });
            return err;
        };
    }
}

fn expectStepsFinal(backend: Backend, steps: []const []const u8, expected: []const u8) !void {
    var test_env = TestEnv.init(alloc);
    defer test_env.deinit();
    var repl = switch (backend) {
        .interpreter => try Repl.init(alloc, test_env.get_ops(), null),
        .dev => try Repl.initWithBackend(alloc, test_env.get_ops(), null, .dev),
        .wasm => try Repl.initWithWasmBackend(alloc, test_env.get_ops(), null),
        .llvm => try Repl.initWithBackend(alloc, test_env.get_ops(), null, .llvm),
    };
    defer repl.deinit();

    for (steps, 0..) |step, i| {
        const result = try repl.step(step);
        defer alloc.free(result);

        if (i + 1 == steps.len) {
            testing.expectEqualStrings(expected, result) catch |err| {
                const backend_name = switch (backend) {
                    .interpreter => "INTERPRETER",
                    .dev => "DEV BACKEND",
                    .wasm => "WASM BACKEND",
                    .llvm => "LLVM BACKEND",
                };
                std.debug.print("{s} FAILED for: {s}\n", .{ backend_name, step });
                return err;
            };
        }
    }
}

fn expectStepsFinalInChild(backend: Backend, steps: []const []const u8, expected: []const u8) !void {
    if (builtin.os.tag == .windows) return error.SkipZigTest;

    const pid = try posix.fork();

    if (pid == 0) {
        expectStepsFinal(backend, steps, expected) catch |err| {
            std.debug.print("child expectStepsFinal error: {}\n", .{err});
            std.c._exit(1);
        };

        std.c._exit(0);
    }

    const wait_result = posix.waitpid(pid, 0);
    const status = wait_result.status;
    const termination_signal: u8 = @truncate(status & 0x7f);

    if (termination_signal != 0) {
        std.debug.print("child terminated with signal {d}\n", .{termination_signal});
        return error.TestUnexpectedResult;
    }

    const exit_code: u8 = @truncate((status >> 8) & 0xff);
    try testing.expectEqual(@as(u8, 0), exit_code);
}

test "Repl - silent assignments" {
    const steps = &[_][2][]const u8{
        .{ "x = 5", "assigned `x`" },
        .{ "x", "5.0" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
    try expectStateful(.llvm, steps);
}

test "Repl - issue 9258 opaque type param field access" {
    const steps = &[_][]const u8{
        "Wrapper(a) := { inner : a }",
        "unwrap : Wrapper(a) -> a",
        "unwrap = |w| w.inner",
        "unwrap({ inner: \"hello\" })",
    };

    try expectStepsFinal(.interpreter, steps, "\"hello\"");
    try expectStepsFinalInChild(.dev, steps, "\"hello\"");
}

test "Repl - polymorphic numeric in comparison snapshot sequence" {
    const steps = &[_][2][]const u8{
        .{ "is_positive = |x| x > 0", "assigned `is_positive`" },
        .{ "List.any([-1, 0, 1], is_positive)", "True" },
        .{ "List.any([-1, 0, -2], is_positive)", "False" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
}

test "Repl - variable redefinition" {
    const steps = &[_][2][]const u8{
        .{ "x = 5", "assigned `x`" },
        .{ "y = x + 1", "assigned `y`" },
        .{ "y", "6.0" },
        .{ "x = 3", "assigned `x`" },
        .{ "y", "4.0" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
    try expectStateful(.llvm, steps);
}

test "Repl - for loop over list" {
    const steps = &[_][2][]const u8{
        .{ "[\"hello\", \"world\", \"test\"]", "[\"hello\", \"world\", \"test\"]" },
        .{ "count = { var counter_ = 0; for _ in [\"hello\", \"world\", \"test\"] { counter_ = counter_ + 1 }; counter_ }", "assigned `count`" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
    try expectStateful(.llvm, steps);
}

test "Repl - for loop snapshots" {
    const steps = &[_][2][]const u8{
        .{ "unchanged = { var value_ = 42; for n in [] { value_ = n }; value_ }", "assigned `unchanged`" },
        .{ "result = { var allTrue_ = Bool.True; for b in [Bool.True, Bool.True, Bool.False] { if b == Bool.False { allTrue_ = Bool.False } else { {} } }; allTrue_ }", "assigned `result`" },
        .{ "count = { var counter_ = 0; for _ in [\"hello\", \"world\", \"test\"] { counter_ = counter_ + 1 }; counter_ }", "assigned `count`" },
        .{ "sum = { var total_ = 0; for n in [1, 2, 3, 4, 5] { total_ = total_ + n }; total_ }", "assigned `sum`" },
        .{ "product = { var result_ = 0; for i in [1, 2, 3] { for j in [10, 20] { result_ = result_ + (i * j) } }; result_ }", "assigned `product`" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
    try expectStateful(.llvm, steps);
}

// Non-evaluation tests that only need one backend

test "Repl - build full source with block syntax" {
    var test_env = TestEnv.init(alloc);
    defer test_env.deinit();
    var repl = try Repl.init(alloc, test_env.get_ops(), null);
    defer repl.deinit();

    try repl.addOrReplaceDefinition("x = 5", "x");
    try repl.addOrReplaceDefinition("y = x + 1", "y");

    const full_source = try repl.buildFullSource("y");
    defer alloc.free(full_source);

    const expected =
        \\{
        \\    x = 5
        \\    y = x + 1
        \\    y
        \\}
    ;
    try testing.expectEqualStrings(full_source, expected);
}

test "Repl - definition replacement" {
    var test_env = TestEnv.init(alloc);
    defer test_env.deinit();
    var repl = try Repl.init(alloc, test_env.get_ops(), null);
    defer repl.deinit();

    try repl.addOrReplaceDefinition("x = 1", "x");
    try repl.addOrReplaceDefinition("x = 2", "x");
    try repl.addOrReplaceDefinition("x = 3", "x");

    try testing.expect(repl.definitions.count() == 1);

    const full_source = try repl.buildFullSource("x");
    defer alloc.free(full_source);

    const expected =
        \\{
        \\    x = 3
        \\    x
        \\}
    ;
    try testing.expectEqualStrings(full_source, expected);
}

test "Repl - 4-arg lambda call (dev)" {
    // Regression: 4 Dec params fill all 8 arg registers on aarch64,
    // forcing roc_ops to pass-by-ptr. Previously crashed with segfault.
    const steps = &[_][2][]const u8{
        .{ "f = |a, b, c, d| a + b + c + d", "assigned `f`" },
        .{ "f(10, 20, 30, 40)", "100.0" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
    try expectStateful(.wasm, steps);
    try expectStateful(.llvm, steps);
}
