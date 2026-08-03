//! LIR-shape and statement-count tests for the decision-tree match compiler
//! (src/postcheck/match_tree.zig):
//!
//! - An N-branch tag match on one scrutinee lowers to exactly ONE multiway
//!   `switch_stmt` with ONE discriminant read.
//! - The PR 9707 list-match family grows linearly in statement count.
//! - Guard, string, list, and nominal matches pass ARC certification (the
//!   harness runs the certifier).

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");

const harness = @import("lower_to_lir_harness.zig");
const expectLowersToLir = harness.expectLowersToLir;
const expectDeterministicLir = harness.expectDeterministicLir;
const expectLirInspection = harness.expectLirInspection;

const six_tag_match_app =
    \\Color := [Red, Green, Blue, Yellow, Purple, Orange]
    \\
    \\rank : Color -> I64
    \\rank = |c| match c {
    \\    Red => 1
    \\    Green => 2
    \\    Blue => 3
    \\    Yellow => 4
    \\    Purple => 5
    \\    Orange => 6
    \\}
    \\
    \\main! : List(Str) => Try({}, [Exit(I8), ..])
    \\main! = |args| {
    \\    # Depend on args so the match stays runtime code instead of being
    \\    # hoisted as a compile-time constant.
    \\    colors : List(Color)
    \\    colors = if List.len(args) > 100 { [Red] } else { [Red, Green, Blue, Yellow, Purple, Orange] }
    \\    echo!(Str.inspect(List.map(colors, rank)))
    \\    Ok({})
    \\}
;

var counted_multiway_switches: usize = 0;
var counted_total_stmts: usize = 0;

fn countShapes(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    counted_multiway_switches = 0;
    counted_total_stmts = store.cf_stmts.len();
    for (0..store.cf_stmts.len()) |i| {
        switch (store.cf_stmts.get(i)) {
            .switch_stmt => |sw| {
                if (sw.branches.len >= 5) counted_multiway_switches += 1;
            },
            else => {},
        }
    }
}

/// For every proc containing a >= 5-case switch (the `rank` specializations),
/// assert the proc reads exactly one discriminant: one multiway dispatch per
/// match, one read per tested position.
fn checkRankProcShape(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    const gpa = std.testing.allocator;
    const buf = try gpa.alloc(u8, 1 << 20);
    defer gpa.free(buf);
    var found_multiway_proc = false;
    for (0..store.proc_specs.len()) |index| {
        var writer = std.Io.Writer.fixed(buf);
        try lir.DebugPrint.writeProc(gpa, store, layouts, @enumFromInt(@as(u32, @intCast(index))), &writer);
        const text = writer.buffered();
        if (std.mem.count(u8, text, "case ") >= 5) {
            found_multiway_proc = true;
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, text, "ref.discriminant"));
        }
    }
    try std.testing.expect(found_multiway_proc);
}

test "an N-branch tag match lowers to one multiway switch with one discriminant read" {
    try expectLirInspection(six_tag_match_app, countShapes);
    // The only match with >= 6 same-scrutinee tag branches in the whole
    // program (app, echo platform wrapper, and reachable builtins) is `rank`:
    // the closed 6-variant union lowers to one 5-branch switch whose default
    // is the last arm (checker-verdict exhaustiveness).
    try std.testing.expect(counted_multiway_switches >= 1);
    try expectLirInspection(six_tag_match_app, checkRankProcShape);
}

fn listMatchApp(comptime branch_count: usize) []const u8 {
    const built = comptime blk: {
        var body: []const u8 =
            \\f : List(U64) -> U64
            \\f = |xs| match xs {
            \\
        ;
        // Branches [1, ..] => 1, [1, 2, ..] => 2, ... — the PR 9707 family
        // shape: each branch adds one more matched element.
        for (1..branch_count + 1) |n| {
            var pat: []const u8 = "    [";
            for (1..n + 1) |i| {
                if (i > 1) pat = pat ++ ", ";
                pat = pat ++ std.fmt.comptimePrint("{d}", .{i});
            }
            pat = pat ++ std.fmt.comptimePrint(", ..] => {d}\n", .{n});
            body = body ++ pat;
        }
        body = body ++
            \\    _ => 0
            \\}
            \\
            \\main! : List(Str) => Try({}, [Exit(I8), ..])
            \\main! = |args| {
            \\    # Runtime-derived list so the match is not constant-folded.
            \\    xs = List.map(args, Str.count_utf8_bytes)
            \\    echo!(Str.inspect(f(xs)))
            \\    Ok({})
            \\}
        ;
        break :blk body;
    };
    return built;
}

test "the PR 9707 list-match family stays linear in statement count" {
    try expectLirInspection(listMatchApp(3), countShapes);
    const count3 = counted_total_stmts;
    try expectLirInspection(listMatchApp(4), countShapes);
    const count4 = counted_total_stmts;
    try expectLirInspection(listMatchApp(5), countShapes);
    const count5 = counted_total_stmts;

    // PR 9707 measured 1136 -> 4272 -> 16816 statements for this family when
    // sharing was broken (~4x per added branch). Linear growth means the two
    // deltas are close and small; the second delta being 2x the first is the
    // regression alarm.
    const delta_a = count4 - count3;
    const delta_b = count5 - count4;
    try std.testing.expect(delta_b < delta_a * 2);
    try std.testing.expect(delta_a < 200);
    try std.testing.expect(delta_b < 200);
}

test "guard, string, list, and as-pattern matches pass ARC certification" {
    try expectLowersToLir(
        \\describe : Try(I64, Str), Bool -> Str
        \\describe = |v, flag| match v {
        \\    Ok(n) if flag => Str.inspect(n * 10)
        \\    Ok(0) => "zero"
        \\    Ok(n) as whole if n > 5 => Str.inspect((n, whole))
        \\    Ok(_) => "small"
        \\    Err("exact") => "lit"
        \\    Err("pre${rest}") => rest
        \\    Err("${a}!") if flag => a
        \\    Err("${b}!") => b
        \\    Err(_) => "other"
        \\}
        \\
        \\pick : List(Try(I64, Str)) -> I64
        \\pick = |xs| match xs {
        \\    [] => 0
        \\    [Ok(a)] => a
        \\    [Ok(a), .. as rest] => a + pick(rest)
        \\    [Err(_), last] => result_or(last, -1)
        \\    [_, ..] => -2
        \\}
        \\
        \\result_or : Try(I64, Str), I64 -> I64
        \\result_or = |v, fallback| match v {
        \\    Ok(n) => n
        \\    Err(_) => fallback
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    echo!(describe(Ok(7), True))
        \\    echo!(describe(Err("pretty"), False))
        \\    echo!(Str.inspect(pick([Ok(1), Ok(2), Err("x"), Ok(4)])))
        \\    Ok({})
        \\}
    );
}

test "nominal record match with declared order differing from backing order lowers" {
    try expectLowersToLir(
        \\P := { y : U8, x : U64 }
        \\
        \\get : P -> U64
        \\get = |p| match p {
        \\    P.({ y: 1, x }) => x
        \\    P.({ y, x: 2 }) => U8.to_u64(y) * 100
        \\    P.({ y, x }) => x + U8.to_u64(y)
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    a : P
        \\    a = { y: 1, x: 77 }
        \\    echo!(Str.inspect(get(a)))
        \\    Ok({})
        \\}
    );
}

test "match lowering is deterministic" {
    try expectDeterministicLir(
        \\f : Try(I64, Str) -> I64
        \\f = |v| match v {
        \\    Ok(1) => 10
        \\    Ok(n) if n > 5 => n
        \\    Ok(_) => 0
        \\    Err("x") => -1
        \\    Err(_) => -2
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    echo!(Str.inspect(f(Ok(3))))
        \\    Ok({})
        \\}
    );
}
