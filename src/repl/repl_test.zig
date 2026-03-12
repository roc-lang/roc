//! Tests for the REPL
const std = @import("std");
const Repl = @import("eval.zig").Repl;
const TestEnv = @import("repl_test_env.zig").TestEnv;

const testing = std.testing;

const alloc = std.testing.allocator;

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

/// Run expression on both interpreter and dev backends, assert same expected output.
fn expectBoth(expr: []const u8, expected: []const u8) !void {
    // Interpreter backend
    {
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
    // Dev backend
    {
        var test_env = TestEnv.init(alloc);
        defer test_env.deinit();
        var repl = try Repl.initWithBackend(alloc, test_env.get_ops(), test_env.crashContextPtr(), .dev);
        defer repl.deinit();
        const result = try repl.step(expr);
        defer alloc.free(result);
        testing.expectEqualStrings(expected, result) catch |err| {
            std.debug.print("DEV BACKEND FAILED for: {s}\n", .{expr});
            return err;
        };
    }
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
    try expectBoth("42", "42.0");
}

test "Repl - string expressions" {
    try expectBoth("\"Hello, World!\"", "\"Hello, World!\"");
}

test "Repl - Bool.True" {
    try expectBoth("Bool.True", "True");
}

test "Repl - Bool.False" {
    try expectBoth("Bool.False", "False");
}

test "Repl - Bool.not(False)" {
    try expectBoth("Bool.not(False)", "True");
}

test "Repl - Bool.not(Bool.True)" {
    try expectBoth("Bool.not(Bool.True)", "False");
}

test "Repl - Bool.not(Bool.False)" {
    try expectBoth("Bool.not(Bool.False)", "True");
}

test "Repl - !Bool.True" {
    try expectBoth("!Bool.True", "False");
}

test "Repl - !Bool.False" {
    try expectBoth("!Bool.False", "True");
}

test "Repl - Str.is_empty" {
    try expectBoth("Str.is_empty(\"\")", "True");
    try expectBoth("Str.is_empty(\"a\")", "False");
}

test "Repl - lambda renders as <function>" {
    try expectBoth("|x| x + 1", "<function>");
    try expectBoth("|x, y| x + y", "<function>");
}

test "Repl - Str.to_utf8" {
    try expectBoth("Str.to_utf8(\"hello\")", "[104, 101, 108, 108, 111]");
    try expectBoth("List.len(Str.to_utf8(\"\"))", "0");
    try expectBoth("List.len(Str.to_utf8(\"hello\"))", "5");
    try expectBoth("List.len(Str.to_utf8(\"é\"))", "2");
    try expectBoth("List.len(Str.to_utf8(\"🎉\"))", "4");
    try expectBoth("List.len(Str.to_utf8(\"Hello, World!\"))", "13");
    try expectBoth("List.len(Str.to_utf8(\"日本語\"))", "9");
    try expectBoth("List.len(Str.to_utf8(\"a é 🎉\"))", "9");
    try expectBoth("List.is_empty(Str.to_utf8(\"\"))", "True");
    try expectBoth("List.is_empty(Str.to_utf8(\"x\"))", "False");
}

test "Repl - Str.from_utf8_lossy" {
    try expectBoth("Str.from_utf8_lossy(Str.to_utf8(\"hello\"))", "\"hello\"");
    try expectBoth("Str.from_utf8_lossy(Str.to_utf8(\"\"))", "\"\"");
    try expectBoth("Str.from_utf8_lossy(Str.to_utf8(\"🎉 party!\"))", "\"🎉 party!\"");
    try expectBoth("Str.from_utf8_lossy(Str.to_utf8(\"abc123\"))", "\"abc123\"");
}

test "Repl - Str.from_utf8 Ok" {
    try expectBoth("Str.from_utf8([72, 105])", "Ok(\"Hi\")");
}

test "Repl - U8.from_str result format" {
    try expectBoth("U8.from_str(\"42\")", "Ok(42)");
}

test "Repl - F32.from_str result format" {
    try expectBoth("F32.from_str(\"3.14\")", "Ok(3.140000104904175)");
}

test "Repl - list literals" {
    try expectBoth("List.len([1, 2, 3])", "3");
    try expectBoth("[1, 2, 3]", "[1.0, 2.0, 3.0]");
    try expectBoth("[\"hello\", \"world\", \"test\"]", "[\"hello\", \"world\", \"test\"]");
    try expectBoth("List.len([\"hello\", \"world\", \"test\"])", "3");
}

test "Repl - list operations" {
    try expectBoth("List.len(List.concat([1, 2], [3, 4]))", "4");
    try expectBoth("List.len(List.concat([], [1, 2, 3]))", "3");
    try expectBoth("List.len(List.concat([1, 2, 3], []))", "3");
    try expectBoth("List.contains([1, 2, 3, 4, 5], 3)", "True");
    try expectBoth("List.drop_if([1, 2, 3, 4, 5], |x| x > 2)", "[1.0, 2.0]");
    try expectBoth("List.keep_if([1, 2, 3, 4, 5], |x| x > 2)", "[3.0, 4.0, 5.0]");
    try expectBoth("List.keep_if([1, 2, 3], |_| Bool.False)", "[]");
    try expectBoth("List.fold_rev([1, 2, 3], 0, |x, acc| acc * 10 + x)", "321.0");
    try expectBoth("List.fold_rev([], 42, |x, acc| x + acc)", "42.0");
}

test "Repl - List.with_capacity" {
    try expectBoth("List.with_capacity(10)", "[]");
    // TODO: List.first on empty list returns Ok({}) in dev backend instead of Err(ListWasEmpty)
    // try expectBoth("List.first(List.with_capacity(10))", "Err(ListWasEmpty)");
    try expectInterpreter("List.first(List.with_capacity(10))", "Err(ListWasEmpty)");
}

test "Repl - List.append" {
    try expectBoth("List.append([1, 2], 3)", "[1.0, 2.0, 3.0]");
}

test "Repl - range_to" {
    // TODO: Dev backend crashes on 1.to(3) — Dec range_to not fully supported
    // try expectBoth("1.to(3)", "[1.0, 2.0, 3.0]");
    try expectInterpreter("1.to(3)", "[1.0, 2.0, 3.0]");
}

test "Repl - list_sort_with" {
    try expectBoth("List.len(List.sort_with([3, 1, 2], |a, b| if a < b LT else if a > b GT else EQ))", "3");
    try expectBoth("List.len(List.sort_with([5, 2, 8, 1, 9], |a, b| if a < b LT else if a > b GT else EQ))", "5");
    try expectBoth("List.len(List.sort_with([], |a, b| if a < b LT else if a > b GT else EQ))", "0");
    try expectBoth("List.len(List.sort_with([42], |a, b| if a < b LT else if a > b GT else EQ))", "1");
}

test "Repl - list fold with concat" {
    try expectBoth("List.len(List.fold([1, 2, 3], [], |acc, x| List.concat(acc, [x])))", "3");
}

// Stateful tests (assignments, variable redefinition) - these use multi-step REPL
// sessions so we test each backend separately but with the same expectations.

fn expectStateful(backend: enum { interpreter, dev }, steps: []const [2][]const u8) !void {
    var test_env = TestEnv.init(alloc);
    defer test_env.deinit();
    var repl = switch (backend) {
        .interpreter => try Repl.init(alloc, test_env.get_ops(), null),
        .dev => try Repl.initWithBackend(alloc, test_env.get_ops(), null, .dev),
    };
    defer repl.deinit();

    for (steps) |step| {
        const result = try repl.step(step[0]);
        defer alloc.free(result);
        testing.expectEqualStrings(step[1], result) catch |err| {
            std.debug.print("{s} FAILED for: {s}\n", .{ if (backend == .interpreter) "INTERPRETER" else "DEV BACKEND", step[0] });
            return err;
        };
    }
}

test "Repl - silent assignments" {
    const steps = &[_][2][]const u8{
        .{ "x = 5", "assigned `x`" },
        .{ "x", "5.0" },
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
}

test "Repl - for loop over list" {
    const steps = &[_][2][]const u8{
        .{ "[\"hello\", \"world\", \"test\"]", "[\"hello\", \"world\", \"test\"]" },
        .{ "count = { var counter_ = 0; for _ in [\"hello\", \"world\", \"test\"] { counter_ = counter_ + 1 }; counter_ }", "assigned `count`" },
    };
    try expectStateful(.interpreter, steps);
    try expectStateful(.dev, steps);
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
