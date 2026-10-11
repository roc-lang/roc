//! Concrete builtin numerals do not need per-occurrence conversion signatures.
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

test "issue 11960: concrete builtin list literals allocate only their source type slots" {
    const gpa = std.testing.allocator;
    var small: usize = 0;
    for ([_]usize{ 8, 512 }) |count| {
        var source: std.ArrayList(u8) = .empty;
        defer source.deinit(gpa);
        try source.appendSlice(gpa, "values : List(U8)\nvalues = [");
        for (0..count) |_| try source.appendSlice(gpa, "7,");
        try source.appendSlice(gpa, "]\n");
        var env = try TestEnv.init("Test", source.items);
        defer env.deinit();
        try env.assertNoErrors();
        for (env.module_env.store.literalDispatchPlans()) |plan| {
            try std.testing.expect(plan.fnVar() == null);
            try std.testing.expectEqual(.builtin_direct, plan.dispatchResolution());
        }
        const descriptors = env.checker.types.descs.backing.len();
        if (count == 8) small = descriptors else try std.testing.expectEqual(@as(usize, 512 - 8), descriptors - small);
    }
}

test "issue 11960: suffixes and transparent aliases supply concrete builtin targets" {
    var env = try TestEnv.init("Test",
        \\Byte : U8
        \\values : List(Byte)
        \\values = [0, 255]
        \\signed = -128.I8
        \\fraction = 1.5.F32
    );
    defer env.deinit();
    try env.assertNoErrors();
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        try std.testing.expect(plan.fnVar() == null);
        try std.testing.expectEqual(.builtin_direct, plan.dispatchResolution());
    }
}

test "issue 11960: rejected builtin literals keep independent diagnostics" {
    var env = try TestEnv.init("Test",
        \\values : List(U8)
        \\values = [256, 257]
    );
    defer env.deinit();
    try env.assertHasTypeError("Invalid Number");
    try std.testing.expectEqual(@as(usize, 2), env.checker.problems.problems.items.len);
}

test "issue 11960: a conflicting explicit suffix still rejects the contextual target" {
    var env = try TestEnv.init("Test",
        \\values : List(U8)
        \\values = [1.U16]
    );
    defer env.deinit();
    try env.assertHasTypeError("Type Mismatch");
}

test "issue 11960: custom numeral targets keep their conversion signature" {
    var env = try TestEnv.init("Test",
        \\N := U8.{
        \\    from_numeral : Numeral -> Try(N, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(N.(7))
        \\}
        \\values : List(N)
        \\values = [300, 2.N]
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expect(env.module_env.store.literal_dispatch_plans.len() >= 2);
}

test "issue 11960: open numeral schemes retain independent concrete instantiations" {
    var env = try TestEnv.init("Test",
        \\make = |_| [1, 2]
        \\bytes : List(U8)
        \\bytes = make({})
        \\words : List(U64)
        \\words = make({})
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expect(env.module_env.store.literal_dispatch_plans.len() >= 2);
}
