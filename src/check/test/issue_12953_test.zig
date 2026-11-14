//! repro for https://github.com/roc-lang/roc/issues/12953
//! A host may hold a boxed state whose functions are not each boxed, because
//! every function a `Box` holds is an erased callable inside that allocation.

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

test "issue 12953: provides accepts a boxed imported state that holds a function" {
    var state = try TestEnv.init("State",
        \\State := { count : U64, on_event : U64, U64 -> U64 }.{
        \\    step : State, U64 -> State
        \\    step = |s, event| { ..s, count: (s.on_event)(s.count, event) }
        \\}
    );
    defer state.deinit();
    try state.assertNoErrors();

    var env = try TestEnv.initWithImport("Platform",
        \\platform ""
        \\    requires {} {}
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_step": step_for_host }
        \\
        \\import State
        \\
        \\step_for_host : Box(State), U64 -> Box(State)
        \\step_for_host = |boxed, event| Box.box(State.step(Box.unbox(boxed), event))
    , "State", &state);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 12953: functions are accepted anywhere a host only holds them" {
    const Case = struct { ty: []const u8, valid: bool };
    const cases = [_]Case{
        // Held: the box's payload is not itself a function or sequence.
        .{ .ty = "Box(State)", .valid = true },
        .{ .ty = "Box(Generic(U64 -> U64))", .valid = true },
        .{ .ty = "Box({ inner : { run : U64 -> U64 } })", .valid = true },
        .{ .ty = "Box({ run : (U64 -> U64) -> (U64 -> U64) })", .valid = true },
        .{ .ty = "Box(List(U64 -> U64))", .valid = true },
        .{ .ty = "Box([Ready(U64 -> U64), Idle])", .valid = true },
        .{ .ty = "Box(((U64 -> U64), Str))", .valid = true },
        .{ .ty = "List(Box(State))", .valid = true },
        .{ .ty = "Box({ items : Box(Iter(U8)), run : U64 -> U64 })", .valid = true },
        // Still rejected: the host passes or reads the value itself.
        .{ .ty = "State", .valid = false },
        .{ .ty = "{ state : Box(State), run : U64 -> U64 }", .valid = false },
        .{ .ty = "List(State)", .valid = false },
        // Still rejected: a directly boxed function is called by the host.
        .{ .ty = "Box((U64 -> U64) -> U64)", .valid = false },
        .{ .ty = "Box(U64 -> State)", .valid = false },
        // Still rejected: sequences are handles only as an immediate payload.
        .{ .ty = "Box(Sequenced)", .valid = false },
        .{ .ty = "Box({ items : Iter(U8), run : U64 -> U64 })", .valid = false },
        .{ .ty = "Box(List(Stream(U8)))", .valid = false },
    };
    for (cases) |case| {
        for ([_]bool{ false, true }) |result| {
            const signature = if (result)
                try std.fmt.allocPrint(std.testing.allocator, "U64 -> ({s})", .{case.ty})
            else
                try std.fmt.allocPrint(std.testing.allocator, "({s}) -> U64", .{case.ty});
            defer std.testing.allocator.free(signature);
            const source = try std.fmt.allocPrint(std.testing.allocator,
                \\platform ""
                \\    requires {{}} {{}}
                \\    exposes []
                \\    packages {{}}
                \\    provides {{ "roc_entry": entry }}
                \\
                \\State := {{ count : U64, on_event : U64, U64 -> U64 }}
                \\Generic(a) := {{ value : a }}
                \\Sequenced := {{ items : Iter(U8), on_item : U8 -> U8 }}
                \\
                \\entry : {s}
                \\entry = |_x| ...
            , .{signature});
            defer std.testing.allocator.free(source);
            var env = try TestEnv.init("Test", source);
            defer env.deinit();
            errdefer std.debug.print("provided case: {s}\n", .{source});
            if (case.valid) try env.assertNoErrors() else try env.assertOneTypeError("Hosted Function Requires Boxed Lambda");
        }
        const hosted = try std.fmt.allocPrint(std.testing.allocator,
            \\Effect := [].{{
            \\    State := {{ count : U64, on_event : U64, U64 -> U64 }}
            \\    Generic(a) := {{ value : a }}
            \\    Sequenced := {{ items : Iter(U8), on_item : U8 -> U8 }}
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

test "issue 12953: a directly boxed function's result is not a held payload" {
    var env = try TestEnv.init("Test",
        \\platform ""
        \\    requires {} {}
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_entry": entry }
        \\
        \\State := { count : U64, on_event : U64, U64 -> U64 }
        \\
        \\entry : Box(U64 -> State) -> U64
        \\entry = |_x| 0
    );
    defer env.deinit();
    try env.assertOneTypeError("Hosted Function Requires Boxed Lambda");
    const violation = env.checker.problems.problems.items[0].hosted_unboxed_function;
    try std.testing.expectEqualStrings("signature -> argument 1 -> payload -> result -> field on_event", env.checker.problems.getExtraString(violation.path));
}
