//! A string literal whose context already names the builtin `Str` needs no
//! per-occurrence conversion signature.
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

test "string literals at a Str context allocate only their source type slots" {
    const gpa = std.testing.allocator;
    var small: usize = 0;
    for ([_]usize{ 8, 512 }) |count| {
        var source: std.ArrayList(u8) = .empty;
        defer source.deinit(gpa);
        try source.appendSlice(gpa, "values : List(Str)\nvalues = [");
        for (0..count) |_| try source.appendSlice(gpa, "\"word\",");
        try source.appendSlice(gpa, "]\n");
        var env = try TestEnv.init("Test", source.items);
        defer env.deinit();
        try env.assertNoErrors();
        for (env.module_env.store.literalDispatchPlans()) |plan| {
            try std.testing.expect(plan.fnVar() == null);
            try std.testing.expectEqual(.builtin_direct, plan.dispatchResolution());
        }
        // Each literal is two source nodes: the string and its one segment.
        const descriptors = env.checker.types.descs.backing.len();
        if (count == 8) small = descriptors else try std.testing.expectEqual(@as(usize, 2 * (512 - 8)), descriptors - small);
    }
}

test "a transparent alias of Str supplies the concrete target" {
    var env = try TestEnv.init("Test",
        \\Name : Str
        \\names : List(Name)
        \\names = ["a", "b"]
    );
    defer env.deinit();
    try env.assertNoErrors();
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        try std.testing.expect(plan.fnVar() == null);
        try std.testing.expectEqual(.builtin_direct, plan.dispatchResolution());
    }
}

test "a string literal with no Str context keeps its conversion and defaults to Str" {
    var env = try TestEnv.init("Test",
        \\greeting = "hello"
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expect(env.module_env.store.literal_dispatch_plans.len() >= 1);
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        try std.testing.expect(plan.fnVar() != null);
    }
}

test "a custom string literal target keeps its conversion signature" {
    var env = try TestEnv.init("Test",
        \\Word := Str.{
        \\    from_quote : Str -> Try(Word, [BadQuotedBytes(Str)])
        \\    from_quote = |text| Ok(Word.(text))
        \\}
        \\words : List(Word)
        \\words = ["a", "b"]
    );
    defer env.deinit();
    try env.assertNoErrors();
    var custom: usize = 0;
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        if (plan.fnVar() != null) custom += 1;
    }
    try std.testing.expectEqual(@as(usize, 2), custom);
}

test "a string literal at a non-Str context is still a mismatch" {
    var env = try TestEnv.init("Test",
        \\count : U64
        \\count = "three"
    );
    defer env.deinit();
    try std.testing.expect(env.checker.problems.problems.items.len != 0);
}
