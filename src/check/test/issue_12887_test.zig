//! repro for https://github.com/roc-lang/roc/issues/12887
//! A function argument crossing a provides boundary must be directly boxed.

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

test "issue 12887: provides rejects an unboxed function argument" {
    var env = try TestEnv.init("Test",
        \\platform ""
        \\    requires {} {}
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_take_fn": take_fn_for_host }
        \\
        \\take_fn_for_host : (U64 -> U64) -> U64
        \\take_fn_for_host = |f| f(1)
    );
    defer env.deinit();

    const diagnostics = try env.module_env.getDiagnostics();
    defer env.gpa.free(diagnostics);
    try std.testing.expectEqual(@as(usize, 0), diagnostics.len);
    try env.assertOneTypeError("Hosted Function Requires Boxed Lambda");
}

test "issue 12887: host boundary requires direct boxing at every occurrence" {
    const Case = struct { ty: []const u8, valid: bool };
    const cases = [_]Case{
        .{ .ty = "U64 -> U64", .valid = false },
        .{ .ty = "Iter(U8)", .valid = false },
        .{ .ty = "Stream(U8)", .valid = false },
        .{ .ty = "Box(U64 -> U64)", .valid = true },
        .{ .ty = "Box(Iter(U8))", .valid = true },
        .{ .ty = "Box(Stream(U8))", .valid = true },
        .{ .ty = "Box({ run : U64 -> U64 })", .valid = true },
        .{ .ty = "Box({ items : Iter(U8) })", .valid = false },
        .{ .ty = "Box(List(Stream(U8)))", .valid = false },
        .{ .ty = "Box((Iter(U8), U64))", .valid = false },
        .{ .ty = "List(Box(Iter(U8)))", .valid = true },
        .{ .ty = "Box({ run : Box(U64 -> U64) })", .valid = true },
        .{ .ty = "Box((U64 -> U64) -> U64)", .valid = false },
        .{ .ty = "Box(Box(U64 -> U64) -> U64)", .valid = true },
        .{ .ty = "[Item(Iter(U8)), Empty]", .valid = false },
        .{ .ty = "[Item(Box(Iter(U8))), Empty]", .valid = true },
    };
    for (cases) |case| {
        for ([_]bool{ false, true }) |result| {
            const signature = if (result)
                try std.fmt.allocPrint(std.testing.allocator, "U64 -> ({s})", .{case.ty})
            else
                try std.fmt.allocPrint(std.testing.allocator, "({s}) -> U64", .{case.ty});
            defer std.testing.allocator.free(signature);
            const provided = try std.fmt.allocPrint(std.testing.allocator,
                \\platform ""
                \\    requires {{}} {{}}
                \\    exposes []
                \\    packages {{}}
                \\    provides {{ "roc_entry": entry }}
                \\
                \\entry : {s}
                \\entry = |_x| ...
            , .{signature});
            defer std.testing.allocator.free(provided);
            var env = try TestEnv.init("Test", provided);
            defer env.deinit();
            errdefer std.debug.print("provided case: {s}\n", .{provided});
            if (case.valid) try env.assertNoErrors() else try env.assertOneTypeError("Hosted Function Requires Boxed Lambda");
        }
        const hosted = try std.fmt.allocPrint(std.testing.allocator,
            \\Effect := [].{{
            \\    call! : ({s}) => ({s})
            \\}}
        , .{ case.ty, case.ty });
        defer std.testing.allocator.free(hosted);
        var env = try TestEnv.initHosted("Effect", hosted);
        defer env.deinit();
        errdefer std.debug.print("hosted case: {s}\n", .{hosted});
        if (case.valid) try env.assertNoErrors() else try env.assertOneTypeError("Hosted Function Requires Boxed Lambda");
    }
}

test "issue 12887: rejected growing nominals do not expand at the boundary" {
    var env = try TestEnv.init("Test",
        \\platform ""
        \\    requires {} {}
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_entry": entry }
        \\
        \\Growing(a) := [Stop, Next(Box(a), List(Growing(List(a))))]
        \\entry : Growing(U64) -> U64
        \\entry = |_x| 0
    );
    defer env.deinit();
    try env.assertOneTypeError("Invalid Recursive Type");
}

test "issue 12887: nominal parameters retain their actual boxing position" {
    const cases = [_]struct { ty: []const u8, valid: bool }{
        .{ .ty = "Safe(U64 -> U64)", .valid = true },
        .{ .ty = "Unsafe(U64 -> U64)", .valid = false },
        .{ .ty = "Box(Unsafe(U64 -> U64))", .valid = true },
        .{ .ty = "Opaque(U64 -> U64)", .valid = false },
        .{ .ty = "Box(Opaque(U64 -> U64))", .valid = true },
        .{ .ty = "Box(IteratorWrapper)", .valid = false },
        .{ .ty = "Safe(Iter(U8))", .valid = true },
        .{ .ty = "Tree(Box(U64 -> U64))", .valid = true },
        .{ .ty = "Tree(U64 -> U64)", .valid = false },
        .{ .ty = "Later(U64)", .valid = true },
        .{ .ty = "LaterSafe(U64)", .valid = true },
        .{ .ty = "Alias", .valid = true },
        .{ .ty = "{ a : Box(U64 -> U64), b : U64 -> U64 }", .valid = false },
    };
    for (cases) |case| {
        const source = try std.fmt.allocPrint(std.testing.allocator,
            \\platform ""
            \\    requires {{}} {{}}
            \\    exposes []
            \\    packages {{}}
            \\    provides {{ "roc_entry": entry }}
            \\
            \\Safe(a) := {{ value : Box(a) }}
            \\Unsafe(a) := {{ value : a }}
            \\Opaque(a) :: {{ value : a }}
            \\IteratorWrapper := {{ items : Iter(U8) }}
            \\Tree(a) := [Leaf(a), Branch(List(Tree(a)))]
            \\Later(a) := [Done, More(Box(a), List(Later(List(U64 -> U64))))]
            \\LaterSafe(a) := [Done, More(Box(a), List(LaterSafe(Box(U64 -> U64))))]
            \\Alias : Box(Iter(U8))
            \\
            \\entry : ({s}) -> U64
            \\entry = |_x| 0
        , .{case.ty});
        defer std.testing.allocator.free(source);
        var env = try TestEnv.init("Test", source);
        defer env.deinit();
        errdefer std.debug.print("case: {s}\n", .{source});
        if (case.valid) try env.assertNoErrors() else try env.assertOneTypeError("Hosted Function Requires Boxed Lambda");
    }
}

test "issue 12887: imported opaque backing does not conceal unboxed functions" {
    var wrapped = try TestEnv.init("Wrapped",
        \\Wrapped := [].{
        \\    Opaque(a) :: { run : a }
        \\    Safe(a) :: { run : Box(a) }
        \\}
    );
    defer wrapped.deinit();
    try wrapped.assertNoErrors();
    for ([_][]const u8{ "Opaque", "Safe" }, 0..) |wrapper, index| {
        const source = try std.fmt.allocPrint(std.testing.allocator,
            \\platform ""
            \\    requires {{}} {{}}
            \\    exposes []
            \\    packages {{}}
            \\    provides {{ "roc_entry": entry }}
            \\
            \\import Wrapped
            \\entry : Wrapped.{s}(U64 -> U64) -> U64
            \\entry = |_x| 0
        , .{wrapper});
        defer std.testing.allocator.free(source);
        var env = try TestEnv.initWithImport("Platform", source, "Wrapped", &wrapped);
        defer env.deinit();
        if (index == 0) {
            try env.assertOneTypeError("Hosted Function Requires Boxed Lambda");
            const violation = env.checker.problems.problems.items[0].hosted_unboxed_function;
            try std.testing.expectEqualStrings("signature -> argument 1 -> field run", env.checker.problems.getExtraString(violation.path));
        } else try env.assertNoErrors();
    }
}
