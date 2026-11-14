//! A method call on a receiver whose type is already a nominal type from
//! another module, whose method is a pure function over nominal types that
//! take no arguments, is resolved where it is checked, with no constraint and
//! no per-use function type.
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

const Counts = struct { constraints: usize, vars: usize };

fn buildSource(comptime header: []const u8, comptime item: []const u8, comptime footer: []const u8, count: usize) std.mem.Allocator.Error!std.ArrayList(u8) {
    const gpa = std.testing.allocator;
    var source: std.ArrayList(u8) = .empty;
    errdefer source.deinit(gpa);
    try source.appendSlice(gpa, header);
    for (0..count) |_| try source.appendSlice(gpa, item);
    try source.appendSlice(gpa, footer);
    return source;
}

fn countsOf(env: *TestEnv) Counts {
    return .{
        .constraints = env.checker.types.static_dispatch_constraints.len(),
        .vars = env.checker.types.len(),
    };
}

fn countsFor(comptime header: []const u8, comptime item: []const u8, comptime footer: []const u8, count: usize) TestEnv.TestEnvError!Counts {
    const gpa = std.testing.allocator;
    var source = try buildSource(header, item, footer, count);
    defer source.deinit(gpa);
    var env = try TestEnv.init("Test", source.items);
    defer env.deinit();
    try env.assertNoErrors();
    return countsOf(&env);
}

/// The type slots and constraints each repetition of `item` adds.
fn growthPerItem(comptime header: []const u8, comptime item: []const u8, comptime footer: []const u8) TestEnv.TestEnvError!Counts {
    const small = try countsFor(header, item, footer, 8);
    const large = try countsFor(header, item, footer, 512);
    const added = 512 - 8;
    try std.testing.expectEqual(@as(usize, 0), (large.constraints - small.constraints) % added);
    try std.testing.expectEqual(@as(usize, 0), (large.vars - small.vars) % added);
    return .{
        .constraints = (large.constraints - small.constraints) / added,
        .vars = (large.vars - small.vars) / added,
    };
}

const meters_source =
    \\Meters := [M(I64)].{
    \\    add : Meters, Meters -> Meters
    \\    add = |Meters.M(a), Meters.M(b)| Meters.M(a + b)
    \\
    \\    scale : Meters, I64 -> Meters
    \\    scale = |Meters.M(a), n| Meters.M(a * n)
    \\
    \\    wrap : Meters -> List(Meters)
    \\    wrap = |m| [m]
    \\
    \\    pick : Meters, a -> Meters
    \\    pick = |m, _| m
    \\
    \\    tick! : Meters => Meters
    \\    tick! = |m| m
    \\}
;

/// The growth per repetition of `item` in a module importing `Meters`.
fn importingGrowthPerItem(comptime header: []const u8, comptime item: []const u8, comptime footer: []const u8) TestEnv.TestEnvError!Counts {
    const gpa = std.testing.allocator;
    var meters = try TestEnv.init("Meters", meters_source);
    defer meters.deinit();
    var counts: [2]Counts = undefined;
    for ([_]usize{ 8, 512 }, 0..) |count, i| {
        var source = try buildSource(header, item, footer, count);
        defer source.deinit(gpa);
        var env = try TestEnv.initWithImport("Test", source.items, "Meters", &meters);
        defer env.deinit();
        try env.assertNoErrors();
        counts[i] = countsOf(&env);
    }
    const added = 512 - 8;
    try std.testing.expectEqual(@as(usize, 0), (counts[1].constraints - counts[0].constraints) % added);
    return .{
        .constraints = (counts[1].constraints - counts[0].constraints) / added,
        .vars = (counts[1].vars - counts[0].vars) / added,
    };
}

test "a closed method of a builtin type adds no constraint" {
    // Each repetition is three source nodes (the call, the receiver, and the
    // argument) and the argument's expected type.
    const growth = try growthPerItem("joins : Str, Str -> List(Str)\njoins = |s, t| [", "s.concat(t),", "]\n");
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
    try std.testing.expectEqual(@as(usize, 4), growth.vars);
}

test "a closed method with no arguments adds only its source type slots" {
    // Each repetition is two source nodes: the call and the receiver.
    const growth = try growthPerItem("names : U64 -> List(Str)\nnames = |n| [", "n.to_str(),", "]\n");
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
    try std.testing.expectEqual(@as(usize, 2), growth.vars);
}

test "a closed method of an imported type adds no constraint" {
    const growth = try importingGrowthPerItem(
        "import Meters\n\nsums : Meters, Meters -> List(Meters)\nsums = |a, b| [",
        "a.add(b),",
        "]\n",
    );
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
}

test "a closed method mixing nominal types adds no constraint" {
    const growth = try importingGrowthPerItem(
        "import Meters\n\nscaled : Meters, I64 -> List(Meters)\nscaled = |a, n| [",
        "a.scale(n),",
        "]\n",
    );
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
}

test "a method whose result type has arguments keeps its constraint" {
    const growth = try importingGrowthPerItem(
        "import Meters\n\nwraps : Meters -> List(List(Meters))\nwraps = |a| [",
        "a.wrap(),",
        "]\n",
    );
    try std.testing.expectEqual(@as(usize, 1), growth.constraints);
}

test "a polymorphic method keeps its constraint" {
    const growth = try importingGrowthPerItem(
        "import Meters\n\npicks : Meters -> List(Meters)\npicks = |a| [",
        "a.pick(1),",
        "]\n",
    );
    try std.testing.expect(growth.constraints >= 1);
}

test "a polymorphic builtin method keeps its constraint" {
    const growth = try growthPerItem("lens : List(U64) -> List(U64)\nlens = |l| [", "l.len(),", "]\n");
    try std.testing.expect(growth.constraints >= 1);
}

test "an effectful method keeps its constraint" {
    const growth = try importingGrowthPerItem(
        "import Meters\n\nticks! : Meters => List(Meters)\nticks! = |a| [",
        "a.tick!(),",
        "]\n",
    );
    try std.testing.expectEqual(@as(usize, 1), growth.constraints);
}

test "a method of a type of this module keeps its constraint" {
    const growth = try growthPerItem(
        \\Vec := { x : I64 }.{
        \\    add : Vec, Vec -> Vec
        \\    add = |a, b| { x: a.x + b.x }
        \\}
        \\
        \\sums : Vec, Vec -> List(Vec)
        \\sums = |a, b| [
    , "a.add(b),", "]\n");
    try std.testing.expectEqual(@as(usize, 1), growth.constraints);
}

test "a method call on a receiver of unknown type keeps its constraint" {
    const with_call = try countsFor("joins = |s, t| [", "s.concat(t),", "]\n", 8);
    const without_call = try countsFor("joins = |s, _t| [", "s,", "]\n", 8);
    try std.testing.expect(with_call.constraints > without_call.constraints);
}

test "a closed method called with the wrong number of arguments reports it" {
    var env = try TestEnv.init("Test",
        \\word : Str
        \\word = "a"
        \\
        \\joined : Str
        \\joined = word.concat("b", "c")
    );
    defer env.deinit();
    try std.testing.expectEqual(@as(usize, 1), try env.typeProblemCount());
}

test "each mismatched argument of a closed method is reported and leaves the method intact" {
    var env = try TestEnv.init("Test",
        \\word : Str
        \\word = "a"
        \\
        \\bad : Str
        \\bad = word.concat(1.5.F64)
        \\
        \\also_bad : Str
        \\also_bad = word.concat(2.5.F64)
        \\
        \\good : Str
        \\good = word.concat("b")
    );
    defer env.deinit();
    try env.assertTypeErrorTitles(&.{ "Type Mismatch", "Type Mismatch" });
    try env.assertDefTypeOptions("good", "Str", .{ .allow_type_errors = true });
}
