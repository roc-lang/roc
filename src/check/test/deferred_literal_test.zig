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

fn patternBranchesSource(gpa: std.mem.Allocator, count: usize) std.mem.Allocator.Error!std.ArrayList(u8) {
    var source: std.ArrayList(u8) = .empty;
    errdefer source.deinit(gpa);
    try source.appendSlice(gpa, "name : U8 -> Str\nname = |byte|\n    match byte {\n");
    for (0..count) |index| try source.print(gpa, "        {d} => \"n\"\n", .{index});
    try source.appendSlice(gpa, "        _ => \"other\"\n    }\n");
    return source;
}

test "a literal pattern matched against a builtin number has no constraint" {
    const gpa = std.testing.allocator;
    var small: usize = 0;
    for ([_]usize{ 4, 64 }) |count| {
        var source = try patternBranchesSource(gpa, count);
        defer source.deinit(gpa);
        var env = try TestEnv.init("Test", source.items);
        defer env.deinit();
        try env.assertNoErrors();
        var numerals: usize = 0;
        for (env.module_env.store.literalDispatchPlans()) |plan| {
            if (plan.dispatchKind() != .numeral) continue;
            numerals += 1;
            try std.testing.expect(plan.fnVar() == null);
            try std.testing.expectEqual(.builtin_direct, plan.dispatchResolution());
        }
        try std.testing.expectEqual(count, numerals);
        // Neither a conversion nor an equality constraint per pattern.
        const constraints = env.checker.types.static_dispatch_constraints.len();
        if (count == 4) small = constraints else try std.testing.expectEqual(small, constraints);
    }
}

test "a literal pattern nested in a tag matched against a builtin payload has no constraint" {
    var env = try TestEnv.init("Test",
        \\kind : Try(U8, [Missing]) -> Str
        \\kind = |found|
        \\    match found {
        \\        Ok(0) => "zero"
        \\        Ok(_) => "other"
        \\        Err(Missing) => "missing"
        \\    }
    );
    defer env.deinit();
    try env.assertNoErrors();
    var numerals: usize = 0;
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        if (plan.dispatchKind() != .numeral) continue;
        numerals += 1;
        try std.testing.expect(plan.fnVar() == null);
        try std.testing.expectEqual(.builtin_direct, plan.dispatchResolution());
    }
    try std.testing.expectEqual(@as(usize, 1), numerals);
}

test "a string literal pattern matched against Str has no constraint" {
    var env = try TestEnv.init("Test",
        \\code : Str -> U8
        \\code = |word|
        \\    match word {
        \\        "one" => 1
        \\        "two" => 2
        \\        _ => 0
        \\    }
    );
    defer env.deinit();
    try env.assertNoErrors();
    var quotes: usize = 0;
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        try std.testing.expect(plan.fnVar() == null);
        try std.testing.expectEqual(.builtin_direct, plan.dispatchResolution());
        if (plan.dispatchKind() == .quote) quotes += 1;
    }
    try std.testing.expectEqual(@as(usize, 2), quotes);
}

test "a literal pattern matched against an undetermined type gets its conversion and equality" {
    var env = try TestEnv.init("Test",
        \\is_zero = |n|
        \\    match n {
        \\        0 => Bool.True
        \\        _ => Bool.False
        \\    }
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), env.module_env.store.literalDispatchPlans().len);
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        try std.testing.expect(plan.fnVar() != null);
    }
    try env.assertDefType("is_zero",
        \\a -> Bool
        \\  where [
        \\    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
        \\    a.is_eq : a, a -> Bool,
        \\  ]
    );
}

test "a literal pattern matched against a custom number type gets its conversion and equality" {
    var env = try TestEnv.init("Test",
        \\Marker := [Marker].{
        \\    from_numeral : Numeral -> Try(Marker, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(Marker)
        \\
        \\    is_eq : Marker, Marker -> Bool
        \\    is_eq = |_, _| Bool.True
        \\}
        \\
        \\is_one : Marker -> Bool
        \\is_one = |marker|
        \\    match marker {
        \\        1 => Bool.True
        \\        _ => Bool.False
        \\    }
    );
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), env.module_env.store.literalDispatchPlans().len);
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        try std.testing.expect(plan.fnVar() != null);
        try std.testing.expectEqual(.custom_dispatch, plan.dispatchResolution());
    }
}

test "a literal pattern that does not fit the builtin type it is matched against is an error" {
    var env = try TestEnv.init("Test",
        \\big : U8 -> Bool
        \\big = |n|
        \\    match n {
        \\        300 => Bool.True
        \\        _ => Bool.False
        \\    }
    );
    defer env.deinit();
    try std.testing.expectEqual(@as(usize, 1), try env.typeProblemCount());
}

test "a number literal pattern matched against Str is an error" {
    var env = try TestEnv.init("Test",
        \\odd : Str -> Bool
        \\odd = |word|
        \\    match word {
        \\        0 => Bool.True
        \\        _ => Bool.False
        \\    }
    );
    defer env.deinit();
    try std.testing.expect(try env.typeProblemCount() != 0);
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

const CIR = @import("can").CIR;

/// How many of the module's interpolations have a dispatch, and how many
/// have none.
fn interpolationDispatchCounts(env: *TestEnv) struct { with: usize, without: usize } {
    var with: usize = 0;
    var without: usize = 0;
    const store = &env.module_env.store;
    var raw: u32 = 0;
    while (raw < store.nodes.len()) : (raw += 1) {
        if (store.nodes.get(@fromBackingInt(@intCast(raw))).tag != .expr_interpolation) continue;
        const expr = store.getExpr(@as(CIR.Expr.Idx, @fromBackingInt(@intCast(raw))));
        if (expr.e_interpolation.constraint_fn_var == null) without += 1 else with += 1;
    }
    return .{ .with = with, .without = without };
}

test "an interpolation of Str parts where a Str is expected has no dispatch" {
    var env = try TestEnv.init("Test",
        \\greet : Str, Str -> Str
        \\greet = |greeting, name| "${greeting}, ${name}!"
    );
    defer env.deinit();
    try env.assertNoErrors();
    const counts = interpolationDispatchCounts(&env);
    try std.testing.expectEqual(@as(usize, 1), counts.without);
    try std.testing.expectEqual(@as(usize, 0), counts.with);
}

test "an interpolation with no expected type keeps its dispatch" {
    var env = try TestEnv.init("Test",
        \\greet = |name| {
        \\    shown : Str
        \\    shown = name
        \\    "hello ${shown}"
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    const counts = interpolationDispatchCounts(&env);
    try std.testing.expectEqual(@as(usize, 0), counts.without);
    try std.testing.expectEqual(@as(usize, 1), counts.with);
}

test "an interpolation with a part whose type is not yet Str keeps its dispatch" {
    var env = try TestEnv.init("Test",
        \\label : U64 -> Str
        \\label = |n| "item ${n.to_str()}"
    );
    defer env.deinit();
    try env.assertNoErrors();
    const counts = interpolationDispatchCounts(&env);
    try std.testing.expectEqual(@as(usize, 1), counts.with + counts.without);
}

test "an interpolation with a part that is not a Str is still an error where a Str is expected" {
    var env = try TestEnv.init("Test",
        \\label : U64 -> Str
        \\label = |n| "item ${n}"
    );
    defer env.deinit();
    try std.testing.expectEqual(@as(usize, 1), try env.typeProblemCount());
}

test "a waiting literal's type is not closed as an absent tag's payload" {
    // `Ok(1)` never constructs `Err`, whose payload in `Pair(_)` is the
    // literal's own type: a value's type, which is not empty.
    var env = try TestEnv.init("Test",
        \\Pair(t) : Try(t, t)
        \\
        \\shown = {
        \\    x : Pair(_)
        \\    x = Ok(1)
        \\    match x {
        \\        Ok(n) => n.to_str()
        \\        Err(n) => n.to_str()
        \\    }
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("shown", "Str");
}
