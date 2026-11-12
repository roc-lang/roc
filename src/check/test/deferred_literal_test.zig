//! A number or string literal with no checking context carries no conversion
//! constraint until its type has settled.
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

fn accumulatorSource(gpa: std.mem.Allocator, count: usize) std.mem.Allocator.Error!std.ArrayList(u8) {
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

test "a string literal whose type settles to Str has no conversion constraint" {
    const gpa = std.testing.allocator;
    var small: usize = 0;
    for ([_]usize{ 8, 256 }) |count| {
        var source: std.ArrayList(u8) = .empty;
        defer source.deinit(gpa);
        try source.appendSlice(gpa, "last : Str -> Str\nlast = |given| {\n");
        for (0..count) |index| {
            try source.print(gpa, "    var $t{d} = \"word\"\n    $t{d} = Str.concat($t{d}, given)\n", .{ index, index, index });
        }
        try source.appendSlice(gpa, "    given\n}\n");
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

test "a string literal whose type settles to a custom string type gets its conversion" {
    var env = try TestEnv.init("Test",
        \\Word := Str.{
        \\    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
        \\    from_quote = |text| Ok(Word.(text))
        \\}
        \\
        \\keep : Word -> Word
        \\keep = |given| {
        \\    var $current = "start"
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

test "a string literal and a number literal of one undetermined type are still a conflict" {
    var env = try TestEnv.init("Test",
        \\mixed = if True { 1 } else { "one" }
    );
    defer env.deinit();
    try std.testing.expect(try env.typeProblemCount() >= 1);
}

test "a rejected value assigned to a variable leaves the variable's waiting literal intact" {
    // `keep(id)` is rejected, so the reassignment's value is erroneous. The
    // `""` and the `0` are still waiting for their types then, and each one's
    // type is its variable's: an erroneous value must not become it.
    var env = try TestEnv.init("Test",
        \\keep : U64 -> Str
        \\keep = |n| n.to_str()
        \\
        \\count : U64 -> U64
        \\count = |n| n
        \\
        \\last_name : U32 -> Str
        \\last_name = |id| {
        \\    var $name = ""
        \\    $name = keep(id)
        \\    $name
        \\}
        \\
        \\last_count : U32 -> U64
        \\last_count = |id| {
        \\    var $total = 0
        \\    $total = count(id)
        \\    $total
        \\}
    );
    defer env.deinit();
    try std.testing.expectEqual(@as(usize, 2), try env.typeProblemCount());
    try std.testing.expectEqual(@as(usize, 2), env.module_env.store.literalDispatchPlans().len);
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        try std.testing.expect(plan.fnVar() == null);
        try std.testing.expectEqual(.builtin_direct, plan.dispatchResolution());
    }
}
