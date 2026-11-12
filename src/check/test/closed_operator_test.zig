//! An operator on a nominal type from another module whose method has
//! exactly the operator's closed signature is resolved where it is checked,
//! with no constraint and no per-use function type.
const std = @import("std");
const TestEnv = @import("TestEnv.zig");

const Counts = struct { constraints: usize, descriptors: usize };

fn countsFor(comptime header: []const u8, comptime item: []const u8, comptime footer: []const u8, count: usize) TestEnv.TestEnvError!Counts {
    const gpa = std.testing.allocator;
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(gpa);
    try source.appendSlice(gpa, header);
    for (0..count) |_| try source.appendSlice(gpa, item);
    try source.appendSlice(gpa, footer);
    var env = try TestEnv.init("Test", source.items);
    defer env.deinit();
    try env.assertNoErrors();
    for (env.module_env.store.literalDispatchPlans()) |plan| {
        try std.testing.expect(plan.fnVar() == null);
        try std.testing.expectEqual(.builtin_direct, plan.dispatchResolution());
    }
    return .{
        .constraints = env.checker.types.static_dispatch_constraints.len(),
        .descriptors = env.checker.types.descs.backing.len(),
    };
}

/// The type slots and constraints each repetition of `item` adds.
fn growthPerItem(comptime header: []const u8, comptime item: []const u8, comptime footer: []const u8) TestEnv.TestEnvError!Counts {
    const small = try countsFor(header, item, footer, 8);
    const large = try countsFor(header, item, footer, 512);
    const added = 512 - 8;
    try std.testing.expectEqual(@as(usize, 0), (large.constraints - small.constraints) % added);
    try std.testing.expectEqual(@as(usize, 0), (large.descriptors - small.descriptors) % added);
    return .{
        .constraints = (large.constraints - small.constraints) / added,
        .descriptors = (large.descriptors - small.descriptors) / added,
    };
}

test "arithmetic on a builtin number adds only its source type slots" {
    // Each repetition is two source nodes: the operator and the lookup.
    const growth = try growthPerItem("sum : U64, U64 -> U64\nsum = |a, b| a", " + b", "\n");
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
    try std.testing.expectEqual(@as(usize, 2), growth.descriptors);
}

test "a number literal operand takes the other operand's builtin type" {
    // Each repetition is two source nodes: the operator and the literal.
    const growth = try growthPerItem("bump : U64 -> U64\nbump = |a| a", " + 1", "\n");
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
    try std.testing.expectEqual(@as(usize, 2), growth.descriptors);
}

test "ordering and equality on a builtin number add only their source type slots" {
    // Each repetition is six source nodes: two operators and four lookups.
    const growth = try growthPerItem("checks : U64, U64 -> List(Bool)\nchecks = |a, b| [", "a < b, a == b,", "]\n");
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
    try std.testing.expectEqual(@as(usize, 6), growth.descriptors);
}

test "a string literal operand of equality takes the builtin Str type" {
    // Each repetition is four source nodes: the operator, the lookup, the
    // string, and its one segment.
    const growth = try growthPerItem("checks : Str -> List(Bool)\nchecks = |s| [", "s == \"word\",", "]\n");
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
    try std.testing.expectEqual(@as(usize, 4), growth.descriptors);
}

test "an operator on a type of this module keeps its constraint" {
    const growth = try growthPerItem(
        \\Vec := { x : I64 }.{
        \\    plus : Vec, Vec -> Vec
        \\    plus = |a, b| { x: a.x + b.x }
        \\}
        \\
        \\sum : Vec, Vec -> Vec
        \\sum = |a, b| a
    , " + b", "\n");
    try std.testing.expectEqual(@as(usize, 1), growth.constraints);
}

test "an operator on operands of unknown type keeps its constraint" {
    // Every use is one requirement of the same type variable.
    const with_operator = try countsFor("sum = |a, b| a", " + b", "\n", 8);
    const without_operator = try countsFor("sum = |a, _b| a", "", "\n", 0);
    try std.testing.expect(with_operator.constraints > without_operator.constraints);
}

test "equality on a type with arguments keeps its constraint" {
    const growth = try growthPerItem("checks : List(U64), List(U64) -> List(Bool)\nchecks = |a, b| [", "a == b,", "]\n");
    try std.testing.expect(growth.constraints >= 1);
}

test "equality on a closed record of builtin types adds no constraint" {
    const growth = try growthPerItem("Point : { x : U64, y : Str }\nsame : Point, Point -> List(Bool)\nsame = |a, b| [", "a == b, a != b,", "]\n");
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
}

test "equality on a closed tag union with builtin payloads adds no constraint" {
    const growth = try growthPerItem("State : [Idle, Busy(U8), Done((Str, Bool))]\nsame : State, State -> List(Bool)\nsame = |a, b| [", "a == b,", "]\n");
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
}

test "equality on a closed tag union against one of its tags adds no constraint" {
    const growth = try growthPerItem("State : [Idle, Busy(U8)]\nidle : State -> List(Bool)\nidle = |a| [", "a == Idle,", "]\n");
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
}

test "equality on a record whose field type is unknown keeps its constraint" {
    const with_operator = try countsFor("same = |a, b| [", "{ value: a } == { value: b },", "]\n", 8);
    const without_operator = try countsFor("same = |_a, _b| [", "", "]\n", 0);
    try std.testing.expect(with_operator.constraints > without_operator.constraints);
}

test "equality on a record holding a list keeps its constraint" {
    const growth = try growthPerItem("Bag : { items : List(U8) }\nsame : Bag, Bag -> List(Bool)\nsame = |a, b| [", "a == b,", "]\n");
    try std.testing.expect(growth.constraints != 0);
}

test "equality on a record holding a type of this module keeps its constraint" {
    const growth = try growthPerItem(
        \\Meters := U64.{
        \\    is_eq : Meters, Meters -> Bool
        \\    is_eq = |_, _| Bool.True
        \\}
        \\
        \\Trip : { length : Meters }
        \\same : Trip, Trip -> List(Bool)
        \\same = |a, b| [
    , "a == b,", "]\n");
    try std.testing.expect(growth.constraints != 0);
}

test "equality on a record holding a function is still an error" {
    var env = try TestEnv.init("Test",
        \\Handler : { run : U64 -> U64 }
        \\same : Handler, Handler -> Bool
        \\same = |a, b| a == b
    );
    defer env.deinit();
    try std.testing.expect(try env.typeProblemCount() != 0);
}

test "an operator on a literal whose type settles later in its scope adds no constraint" {
    // Each `$step + 1` is checked while `$step` and the `1` are one
    // undetermined literal type; `use($step)` then makes it U64.
    const growth = try growthPerItem(
        \\use : U64 -> U64
        \\use = |n| n
        \\
        \\walk : U64 -> U64
        \\walk = |seed| {
        \\    var $step = 0
        \\
    , "    $step = $step + 1\n    _ = $step == 2\n    _ = $step < 3\n",
        \\    use($step) + seed
        \\}
        \\
    );
    try std.testing.expectEqual(@as(usize, 0), growth.constraints);
}

test "an operator on a literal that stays undetermined gets its constraint and defaults to Dec" {
    var env = try TestEnv.init("Test",
        \\seconds = 60 * 60
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("seconds", "Dec");
    try std.testing.expect(env.checker.types.static_dispatch_constraints.len() != 0);
    try std.testing.expectEqual(@as(usize, 0), env.checker.pending_operators.items.len);
}

test "an operator on a literal in a function that works for any type keeps its requirement" {
    var env = try TestEnv.init("Test",
        \\double = |n| n * 2
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("double",
        \\a -> a
        \\  where [
        \\    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
        \\    a.times : a, a -> a,
        \\  ]
    );
}

test "an operator on a literal whose type settles to a type of this module gets its constraint" {
    var env = try TestEnv.init("Test",
        \\Marker := [Marker].{
        \\    from_numeral : Numeral -> Try(Marker, [InvalidNumeral(Str)])
        \\    from_numeral = |_| Ok(Marker)
        \\
        \\    plus : Marker, Marker -> Marker
        \\    plus = |a, _| a
        \\}
        \\
        \\keep : Marker -> Marker
        \\keep = |given| {
        \\    var $current = 1
        \\    $current = $current + 2
        \\    $current = given
        \\    $current
        \\}
    );
    defer env.deinit();
    try env.assertNoErrors();
}

test "an operator on a literal whose type settles to a type without the method is an error" {
    var env = try TestEnv.init("Test",
        \\name : Str -> Str
        \\name = |given| {
        \\    var $current = "a"
        \\    $current = $current - "b"
        \\    $current = given
        \\    $current
        \\}
    );
    defer env.deinit();
    try std.testing.expect(try env.typeProblemCount() != 0);
}
