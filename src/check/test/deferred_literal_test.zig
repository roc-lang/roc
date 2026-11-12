//! A number literal with no checking context carries no conversion
//! constraint until its type has settled.
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

fn accumulatorSource(gpa: std.mem.Allocator, count: usize) !std.ArrayList(u8) {
    var source: std.ArrayList(u8) = .empty;
    errdefer source.deinit(gpa);
    try source.appendSlice(gpa, "sum : U64 -> U64\nsum = |n| {\n");
    for (0..count) |index| {
        try source.print(gpa, "    var $t{d} = 0\n    $t{d} = $t{d} + n\n", .{ index, index, index });
    }
    try source.appendSlice(gpa, "    n\n}\n");
    return source;
}

test "a literal whose type settles to a builtin number has no conversion constraint" {
    const gpa = std.testing.allocator;
    var small: usize = 0;
    for ([_]usize{ 8, 256 }) |count| {
        var source = try accumulatorSource(gpa, count);
        defer source.deinit(gpa);
        var env = try TestEnv.init("Test", source.items);
        defer env.deinit();
        try env.assertNoErrors();
        try std.testing.expectEqual(count, env.module_env.store.literalDispatchPlans().len);
        for (env.module_env.store.literalDispatchPlans()) |plan| {
            try std.testing.expect(plan.fnVar() == null);
            try std.testing.expectEqual(.builtin_direct, plan.dispatchResolution());
        }
        const constraints = env.checker.types.static_dispatch_constraints.len();
        if (count == 8) small = constraints else try std.testing.expectEqual(small, constraints);
    }
}

test "a literal that stays undetermined gets its conversion and defaults to Dec" {
    var env = try TestEnv.init("Test",
        \\five = 5
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), env.module_env.store.literalDispatchPlans().len);
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        try std.testing.expect(plan.fnVar() != null);
    }
}

test "a literal whose type settles to a custom number type gets its conversion" {
    var env = try TestEnv.init("Test",
        \\Marker := [Marker].{
        \\    from_numeral : Numeral -> Try(Marker, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(Marker)
        \\}
        \\
        \\keep : Marker -> Marker
        \\keep = |given| {
        \\    var $current = 1
        \\    $current = given
        \\    $current
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), env.module_env.store.literalDispatchPlans().len);
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        try std.testing.expect(plan.fnVar() != null);
        try std.testing.expectEqual(.custom_dispatch, plan.dispatchResolution());
    }
}

test "a literal that does not fit the builtin type it settles to is an error" {
    var env = try TestEnv.init("Test",
        \\bump : U8 -> U8
        \\bump = |n| {
        \\    var $total = 300
        \\    $total = $total + n
        \\    $total
        \\}
    );
    defer env.deinit();
    try env.assertOneTypeError("Invalid Number");
}
