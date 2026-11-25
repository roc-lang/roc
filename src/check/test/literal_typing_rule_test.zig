//! The Literal Typing Rule (design.md): a literal checked against a fully
//! known expected type is checked by one structural walk, and each literal
//! node's type is the corresponding part of that type. Everything else keeps
//! the general path.
const std = @import("std");
const base = @import("base");
const TestEnv = @import("TestEnv.zig");

/// How many literal nodes the module checked by the general path and by the
/// rule.
const RuleCounts = struct { general: u32, rule: u32 };

fn ruleCounts(env: *const TestEnv) RuleCounts {
    const counts = env.checker.literal_rule_counts;
    return .{ .general = counts[0], .rule = counts[1] };
}

fn expectRuleCounts(env: *const TestEnv, expected: RuleCounts) error{TestExpectedEqual}!void {
    if (comptime !base.dev_diagnostics.available) return;
    try std.testing.expectEqual(expected, ruleCounts(env));
}

fn rowsSource(gpa: std.mem.Allocator, count: usize) std.mem.Allocator.Error!std.ArrayList(u8) {
    var source: std.ArrayList(u8) = .empty;
    errdefer source.deinit(gpa);
    try source.appendSlice(gpa, "rows : List({ id : U64, name : Str, pos : { x : F32, y : F32 } })\nrows = [\n");
    for (0..count) |index| {
        try source.print(gpa, "    {{ id: {d}, name: \"r{d}\", pos: {{ x: 0.1234567890123456789, y: -{d}.5 }} }},\n", .{ index, index, index });
    }
    try source.appendSlice(gpa, "]\n");
    return source;
}

test "literal typing rule: a fully known list of records creates no type variable beyond its nodes and its field kinds" {
    const gpa = std.testing.allocator;
    var vars_per_size: [2]u64 = undefined;
    var nodes_per_size: [2]u64 = undefined;
    for ([_]usize{ 4, 12 }, 0..) |count, size_index| {
        var source = try rowsSource(gpa, count);
        defer source.deinit(gpa);
        var env = try TestEnv.init("Rows", source.items);
        defer env.deinit();
        try env.assertNoErrors();
        // Each row is a record, a number, a string, a record and two numbers;
        // the list is one more literal node.
        try expectRuleCounts(&env, .{ .general = 0, .rule = @intCast(6 * count + 1) });
        for (env.module_env.store.literalDispatchPlans()) |plan| {
            try std.testing.expect(plan.fnVar() == null);
            try std.testing.expectEqual(.builtin_direct, plan.dispatchResolution());
        }
        vars_per_size[size_index] = env.checker.types.len();
        nodes_per_size[size_index] = env.module_env.store.nodes.len();
    }
    // Every further row adds its own nodes' variables and one decided kind
    // variable for each of its five record fields, and nothing else.
    try std.testing.expectEqual(nodes_per_size[1] - nodes_per_size[0] + 5 * 8, vars_per_size[1] - vars_per_size[0]);
}

test "literal typing rule: each node's type is its part of the expected type" {
    var env = try TestEnv.init("Parts",
        \\rows : List({ id : U64, tag : [Wall, Door(U8)], pair : (Str, List(I32)) })
        \\rows = [{ id: 1, tag: Wall, pair: ("a", []) }, { id: 2, tag: Door(3), pair: ("b", [1, -2]) }]
        \\empty : { a : {}, b : List(Str) }
        \\empty = { a: {}, b: [] }
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("rows", "List({ id: U64, pair: (Str, List(I32)), tag: [Door(U8), Wall] })");
    try env.assertDefType("empty", "{ a: {}, b: List(Str) }");
    // A value's annotated tag union is open (Polarity), so the rows take the
    // general path; their records' parts start the rule at their projected
    // cells, except the tags, which start no walk of their own.
    try expectRuleCounts(&env, .{ .general = 5, .rule = 14 });
}

test "literal typing rule: a mismatching literal keeps the general path and its diagnostic" {
    const cases = [_]struct { source: []const u8, title: []const u8, counts: RuleCounts }{
        // A wrong field type.
        .{ .source = "r : { a : U64, b : Str }\nr = { a: \"x\", b: \"y\" }", .title = "Type Mismatch", .counts = .{ .general = 2, .rule = 1 } },
        // A missing required field.
        .{ .source = "r : { a : U64, b : Str }\nr = { a: 1 }", .title = "Type Mismatch", .counts = .{ .general = 2, .rule = 0 } },
        // An extra field.
        .{ .source = "r : { a : U64 }\nr = { a: 1, b: \"s\" }", .title = "Type Mismatch", .counts = .{ .general = 3, .rule = 0 } },
        // A tag the union does not hold.
        .{ .source = "t : [A(U8), B]\nt = C(1)", .title = "Type Mismatch", .counts = .{ .general = 2, .rule = 0 } },
        // A payload arity the tag does not have.
        .{ .source = "t : [A(U8), B]\nt = A(1, 2)", .title = "Type Mismatch", .counts = .{ .general = 3, .rule = 0 } },
        // A number the field's type cannot hold.
        .{ .source = "r : { a : U8 }\nr = { a: 300 }", .title = "Invalid Number", .counts = .{ .general = 2, .rule = 0 } },
        // A string where a number is expected, and a number where a string is.
        .{ .source = "r : (U8, Str)\nr = (\"x\", \"y\")", .title = "Type Mismatch", .counts = .{ .general = 2, .rule = 1 } },
        .{ .source = "r : (U8, Str)\nr = (1, 2)", .title = "Type Mismatch", .counts = .{ .general = 2, .rule = 1 } },
        // A list item of the wrong type.
        .{ .source = "r : List(U8)\nr = [1, 2, \"x\", 4]", .title = "Type Mismatch", .counts = .{ .general = 2, .rule = 3 } },
    };
    for (cases) |case| {
        var env = try TestEnv.init("Mismatch", case.source);
        defer env.deinit();
        try env.assertFirstTypeError(case.title);
        try expectRuleCounts(&env, case.counts);
    }
}

test "literal typing rule: literals just outside the rule keep the general path" {
    const cases = [_]struct { source: []const u8, counts: RuleCounts }{
        // A `_` hole leaves the expected type undecided.
        .{ .source = "r : { a : U64, b : _ }\nr = { a: 1, b: \"s\" }", .counts = .{ .general = 2, .rule = 1 } },
        // An optional field, omitted or supplied.
        .{ .source = "r : { a : U64, b ?: Str }\nr = { a: 1 }", .counts = .{ .general = 1, .rule = 1 } },
        .{ .source = "r : { a : U64, b ?: Str }\nr = { a: 1, b: \"s\" }", .counts = .{ .general = 1, .rule = 2 } },
        // A record lifted into a declared nominal type.
        .{ .source = "Config := { port : U16 }\nc : Config\nc = { port: 80 }", .counts = .{ .general = 1, .rule = 1 } },
        // A field holding a function.
        .{ .source = "r : { f : U64 -> U64, n : U64 }\nr = { f: |v| v, n: 3 }", .counts = .{ .general = 1, .rule = 1 } },
        // A custom number type.
        .{
            .source =
            \\Meters := [M(I64)].{
            \\    from_numeral : Numeral -> Try(Meters, [InvalidNumeral(Str)])
            \\    from_numeral = |_n| Ok(M(0))
            \\}
            \\r : { d : Meters, n : U8 }
            \\r = { d: 5, n: 3 }
            ,
            .counts = .{ .general = 5, .rule = 1 },
        },
        // A suffixed number.
        .{ .source = "r : { a : U8 }\nr = { a: 1.U8 }", .counts = .{ .general = 1, .rule = 0 } },
    };
    for (cases) |case| {
        var env = try TestEnv.init("Outside", case.source);
        defer env.deinit();
        try env.assertNoErrors();
        try expectRuleCounts(&env, case.counts);
    }
}

test "literal typing rule: a lookup keeps its enclosing literal outside, and a nested literal starts the rule at its own cell" {
    var env = try TestEnv.init("Lookup",
        \\x = 5
        \\r : { a : U64, b : { c : Str } }
        \\r = { a: x, b: { c: "s" } }
    );
    defer env.deinit();
    try env.assertNoErrors();
    // `5` and the outer record take the general path; `{ c: "s" }` and `"s"`
    // take the rule.
    try expectRuleCounts(&env, .{ .general = 2, .rule = 2 });
}

test "literal typing rule: a call argument's context does not start the rule" {
    var env = try TestEnv.init("CallArg",
        \\f : { a : U64, b : List(Str) } -> U64
        \\f = |r| r.a
        \\n = f({ a: 1, b: ["x"] })
    );
    defer env.deinit();
    try env.assertNoErrors();
    // The argument record takes the general path; its parts start the rule
    // at the cells of its projected shape.
    try expectRuleCounts(&env, .{ .general = 1, .rule = 3 });
}

test "literal typing rule: a closed tag union is a part the walk matches" {
    var env = try TestEnv.init("Tags",
        \\f : List({ t : [A, B(U8)] }) -> U64
        \\f = |_l| 0
        \\n = f([{ t: A }, { t: B(2) }])
    );
    defer env.deinit();
    try env.assertNoErrors();
    // The argument list takes the general path, and so does `0`. Each item
    // record starts the rule at its projected cell, a copy of the parameter's
    // item type whose tag union is closed, and its tags and payload are parts.
    try expectRuleCounts(&env, .{ .general = 2, .rule = 5 });
}
