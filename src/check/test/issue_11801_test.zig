//! Regression tests for dispatch-state digests on unannotated method chains.
//!
//! An unannotated function whose body chains method calls (`b.map(f)`)
//! generalizes to a chain of constrained variables, each constraint's result
//! carrying the next one. Every use of the function resolves each copied
//! constraint as a new dispatch edge. A dispatch edge's canonical state digest
//! walks everything its callable reaches, which here is the rest of the chain,
//! so digesting every edge costs O(chain²) per use. A digest is only ever
//! compared along a derivation lineage against an ancestor with the same
//! target, so an edge with no such ancestor whose method name cannot be
//! minted beneath it must not compute one.
const TestEnv = @import("TestEnv.zig");
const std = @import("std");

const chain_len: usize = 40;
const use_count: usize = 5;

/// `prelude`, then `big`, which applies `call` `chain_len` times in a chain
/// starting from its argument, then `use_count` uses of `big` whose argument
/// is `arg` applied to the use's index.
fn genSource(gpa: std.mem.Allocator, prelude: []const u8, call: []const u8, arg: []const u8) std.mem.Allocator.Error![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);

    try out.appendSlice(gpa, prelude);
    try out.appendSlice(gpa, "big = |a| {\n    b0 = a\n");
    var i: usize = 1;
    while (i <= chain_len) : (i += 1) {
        const line = try std.fmt.allocPrint(gpa, "    b{d} = b{d}.{s}\n", .{ i, i - 1, call });
        defer gpa.free(line);
        try out.appendSlice(gpa, line);
    }
    const tail = try std.fmt.allocPrint(gpa, "    b{d}\n}}\n\n", .{chain_len});
    defer gpa.free(tail);
    try out.appendSlice(gpa, tail);

    var j: usize = 0;
    while (j < use_count) : (j += 1) {
        const line = try std.fmt.allocPrint(gpa, "u{d} = big(", .{j});
        defer gpa.free(line);
        try out.appendSlice(gpa, line);
        var rest = arg;
        while (std.mem.findScalar(u8, rest, '#')) |hole| {
            try out.appendSlice(gpa, rest[0..hole]);
            const index = try std.fmt.allocPrint(gpa, "{d}", .{j});
            defer gpa.free(index);
            try out.appendSlice(gpa, index);
            rest = rest[hole + 1 ..];
        }
        try out.appendSlice(gpa, rest);
        try out.appendSlice(gpa, ")\n");
    }
    return out.toOwnedSlice(gpa);
}

fn textEql(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |x, y| {
        if (x != y) return false;
    }
    return true;
}

fn replayedEdge(replayed: []const u32, edge_index: usize) bool {
    for (replayed) |index| {
        if (index == edge_index) return true;
    }
    return false;
}

fn expectNoDigestedEdges(env: *const TestEnv) error{TestUnexpectedResult}!void {
    // The first use settles every edge of the chain; the later ones replay it.
    const edges = env.checker.dispatch_target_instantiations.items;
    try std.testing.expect(edges.len >= chain_len);
    for (edges) |edge| {
        try std.testing.expect(edge.state_type_key == null);
    }
}

test "issue 11801: dispatch edges of an unannotated method chain compute no state digest" {
    const gpa = std.testing.allocator;

    const source = try genSource(gpa, "", "map(|x| x + 1)", "[#.I64]");
    defer gpa.free(source);

    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();

    // Every use resolves every copied `map` constraint to `List.map`, whose
    // scheme carries no constraints, so no edge can have a `map` descendant.
    try expectNoDigestedEdges(&env);
}

test "issue 11801: a chain of constrained targets computes no state digest when no target recurs" {
    const gpa = std.testing.allocator;

    const prelude =
        \\Wrap(a) := [W(a)].{
        \\  step : Wrap(a) -> Wrap(a) where [a.bump : a -> a]
        \\  step = |Wrap.W(x)| Wrap.W(x.bump())
        \\}
        \\
        \\Cnt := [Cnt(I64)].{
        \\  bump : Cnt -> Cnt
        \\  bump = |Cnt.Cnt(n)| Cnt.Cnt(n + 1)
        \\}
        \\
        \\
    ;
    const source = try genSource(gpa, prelude, "step()", "Wrap.W(Cnt.Cnt(#.I64))");
    defer gpa.free(source);

    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();

    // Each `step` edge mints a `bump` child, but no binding named `bump` can
    // mint `step`, so no `step` edge can have a `step` descendant.
    try expectNoDigestedEdges(&env);
}

test "issue 11801: a target that can recur beneath itself keeps its state digest" {
    // `List.is_eq` requires `is_eq` of its elements, so the outer list's
    // `is_eq` edge can have an `is_eq` descendant selecting `List.is_eq`
    // again, which compares against the outer edge's state.
    var env = try TestEnv.init("Test", "x = [[1.I64]] == [[2.I64]]");
    defer env.deinit();
    try env.assertNoErrors();

    var digested: usize = 0;
    for (env.checker.dispatch_target_instantiations.items) |edge| {
        if (edge.state_type_key != null) digested += 1;
    }
    try std.testing.expect(digested > 0);
}

test "issue 11801: imported method mint names share identity with local constraints" {
    const library =
        \\Wrap(a) := [W(a)].{
        \\    step : Wrap(a) -> Wrap(a) where [a.bump : a -> a]
        \\    step = |Wrap.W(x)| Wrap.W(x.bump())
        \\}
    ;
    var library_env = try TestEnv.init("Wrap", library);
    defer library_env.deinit();
    try library_env.assertNoErrors();

    const prelude =
        \\import Wrap
        \\
        \\Cnt := [Cnt(I64)].{
        \\    bump : Cnt -> Cnt
        \\    bump = |Cnt.Cnt(n)| Cnt.Cnt(n + 1)
        \\}
        \\
        \\
    ;
    const source = try genSource(std.testing.allocator, prelude, "step()", "Wrap.W(Cnt.Cnt(#.I64))");
    defer std.testing.allocator.free(source);
    var env = try TestEnv.initWithImport("Main", source, "Wrap", &library_env);
    defer env.deinit();
    try env.assertNoErrors();

    // `step` mints the imported `bump` requirement. That name must match
    // the local binding even though each module has its own ident store.
    try expectNoDigestedEdges(&env);
}

test "issue 11801: imported transitive mint names retain ancestor digests" {
    const library =
        \\Wrap(a) := [W(a)].{
        \\    step : Wrap(a) -> Wrap(a) where [a.bump : a -> a]
        \\    step = |Wrap.W(x)| Wrap.W(x.bump())
        \\    bump : Wrap(a) -> Wrap(a) where [a.step : a -> a]
        \\    bump = |Wrap.W(x)| Wrap.W(x.step())
        \\}
    ;
    var library_env = try TestEnv.init("Wrap", library);
    defer library_env.deinit();
    try library_env.assertNoErrors();

    const source =
        \\import Wrap
        \\
        \\Cnt := [Cnt(I64)].{
        \\    bump : Cnt -> Cnt
        \\    bump = |Cnt.Cnt(n)| Cnt.Cnt(n + 1)
        \\}
        \\x = Wrap.W(Wrap.W(Wrap.W(Cnt.Cnt(0.I64)))).step()
    ;
    var env = try TestEnv.initWithImport("Main", source, "Wrap", &library_env);
    defer env.deinit();
    try env.assertNoErrors();

    // The imported `bump` binding mints `step`, which can select the outer
    // edge's target again. Missing that cross-module name match loses its key.
    var digested: usize = 0;
    for (env.checker.dispatch_target_instantiations.items) |edge| {
        if (edge.state_type_key != null) digested += 1;
    }
    try std.testing.expect(digested > 0);
}

test "issue 11801: concrete dispatch replay reuses a settled ground target across uses" {
    const gpa = std.testing.allocator;

    const prelude =
        \\Wrap(a) := [W(a)].{
        \\  step : Wrap(a) -> Wrap(a) where [a.bump : a -> a]
        \\  step = |Wrap.W(x)| Wrap.W(x.bump())
        \\}
        \\
        \\Cnt := [Cnt(I64)].{
        \\  bump : Cnt -> Cnt
        \\  bump = |Cnt.Cnt(n)| Cnt.Cnt(n + 1)
        \\}
        \\
        \\
    ;
    const source = try genSource(gpa, prelude, "step()", "Wrap.W(Cnt.Cnt(#.I64))");
    defer gpa.free(source);

    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();

    // Every `step` edge has the receiver `Wrap(Cnt)` and the same callable
    // shape, and its instance is ground once related. Once the first one's
    // `bump` requirement settles, the later edges of the first use select
    // from it instead of instantiating `step` and resolving `bump` again; the
    // first use makes `big` replayable, the second becomes the source, and
    // the later uses replay it whole.
    try std.testing.expect(env.checker.dispatch_replayed_edges.items.len >= chain_len / 2);
    try std.testing.expectEqual(use_count - 2, useStateCount(&env, .replayed));
    // A replayed edge is a root `step` edge, and only a freshly instantiated
    // `step` resolves a `bump` requirement of its own: the replayed ones
    // publish their source's.
    var fresh_steps: usize = 0;
    var bumps: usize = 0;
    for (env.checker.dispatch_target_instantiations.items, 0..) |edge, edge_index| {
        const name = env.module_env.getIdentText(edge.method_name);
        if (replayedEdge(env.checker.dispatch_replayed_edges.items, edge_index)) {
            try std.testing.expect(edge.parent_constraint_fn_var == null);
            try std.testing.expectEqualStrings("step", name);
        } else if (textEql(name, "step")) {
            fresh_steps += 1;
        } else if (textEql(name, "bump")) {
            bumps += 1;
        }
    }
    try std.testing.expectEqual(fresh_steps, bumps);
}

test "issue 11801: concrete dispatch replay keys each call's own argument types" {
    // Both calls select `pair_with` on `Cnt`, but their callables differ in
    // the argument type each context supplies. A replay that ignored the
    // callable would give the second call the first call's `U8`.
    const source =
        \\Cnt := [Cnt(I64)].{
        \\  pair_with : Cnt, b -> (Cnt, b)
        \\  pair_with = |c, x| (c, x)
        \\}
        \\
        \\c = Cnt.Cnt(0.I64)
        \\
        \\first : (Cnt, U8)
        \\first = c.pair_with(5)
        \\
        \\second : (Cnt, I32)
        \\second = c.pair_with(5)
        \\
        \\third : (Cnt, U8)
        \\third = c.pair_with(7)
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

test "issue 11801: concrete dispatch replay never selects a method whose scheme is still being checked" {
    // Inside `bump`'s own definition its scheme is not final, so these
    // repeated concrete `bump` calls each select it afresh.
    const source =
        \\Cnt := [Cnt(I64)].{
        \\  bump : Cnt -> Cnt
        \\  bump = |Cnt.Cnt(n)| if n > 10 Cnt.Cnt(n) else Cnt.Cnt(n + 1).bump().bump().bump()
        \\}
        \\
        \\x = Cnt.Cnt(0.I64)
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), env.checker.dispatch_replayed_edges.items.len);
}

test "issue 11801: a type error in one replayed use leaves the uses sharing its instance intact" {
    // The uses of `mk` differ in their unused argument's type, so each
    // settles its own copy of `mk`'s `pair_with` requirement. `first` makes
    // the binding replayable, `second` becomes the source, and `third` and
    // `fourth` replay it, so their types share its frozen instance. `bad`
    // then relates `third` to the wrong type; poisoning that occurrence must
    // not reach the shared instance, so `fourth` keeps its type.
    const source =
        \\Cnt := [Cnt(I64)].{
        \\  pair_with : Cnt, b -> (Cnt, b)
        \\  pair_with = |c, x| (c, x)
        \\}
        \\
        \\mk = |a, _| a.pair_with(5.U8)
        \\
        \\c = Cnt.Cnt(0.I64)
        \\
        \\first = mk(c, 1.I8)
        \\
        \\second = mk(c, 1.I16)
        \\
        \\third = mk(c, 1.I32)
        \\
        \\fourth = mk(c, 1.I64)
        \\
        \\bad : (Cnt, Str)
        \\bad = third
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try std.testing.expect(env.checker.dispatch_replayed_edges.items.len >= 2);
    try std.testing.expectEqual(@as(usize, 1), try env.typeProblemCount());
    try env.assertDefTypeOptions("second", "(Cnt, U8)", .{ .allow_type_errors = true });
    try env.assertDefTypeOptions("fourth", "(Cnt, U8)", .{ .allow_type_errors = true });
}

fn useStateCount(env: *const TestEnv, state: anytype) usize {
    var count: usize = 0;
    for (env.checker.use_instances.items) |use| {
        if (use.state == state) count += 1;
    }
    return count;
}

test "issue 11801: whole-use replay settles one use and replays the rest" {
    const gpa = std.testing.allocator;
    const source = try genSource(gpa, "", "map(|x| x + 1)", "[#.I64]");
    defer gpa.free(source);

    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();

    // Every use of `big` takes a `List(I64)` and settles to the same
    // instance. The first makes `big` replayable, the second settles its
    // relations as the source, and the rest replay it.
    try std.testing.expectEqual(use_count - 2, useStateCount(&env, .replayed));
    var j: usize = 0;
    while (j < use_count) : (j += 1) {
        const name = try std.fmt.allocPrint(gpa, "u{d}", .{j});
        defer gpa.free(name);
        try env.assertDefType(name, "List(I64)");
    }
}

test "issue 11801: whole-use replay keys each use's own argument types" {
    // `first` makes `big` replayable. `second` and `third` are the sources
    // for their own shapes, and only `fourth` shares a shape with one of
    // them, so the `U8` use keeps its own element type.
    const source =
        \\big = |a| a.map(|x| x + 1).map(|x| x + 2)
        \\
        \\first = big([1.I64])
        \\
        \\second = big([2.U8])
        \\
        \\third = big([3.I64])
        \\
        \\fourth = big([4.I64])
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 1), useStateCount(&env, .replayed));
    try env.assertDefType("first", "List(I64)");
    try env.assertDefType("second", "List(U8)");
    try env.assertDefType("third", "List(I64)");
    try env.assertDefType("fourth", "List(I64)");
}

test "issue 11801: whole-use replay never takes a result its relations leave open" {
    // `wrap`'s relations leave the element type of its result open, so no
    // use is a replay source, and each use's annotation decides its own.
    const source =
        \\wrap = |a| a.map(|_| [])
        \\
        \\first : List(List(Str))
        \\first = wrap([1.I64])
        \\
        \\second : List(List(U8))
        \\second = wrap([2.I64])
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
    try std.testing.expectEqual(@as(usize, 0), useStateCount(&env, .replayed));
}

test "issue 11801: a type error in one whole-use replay leaves the uses sharing its instance intact" {
    // `first` makes `big` replayable, and `third` and `fourth` replay
    // `second`'s settled instance. `bad` relates `third` to the wrong type;
    // poisoning that occurrence must not reach the shared instance, so
    // `fourth` keeps its type.
    const source =
        \\big = |a| a.map(|x| x + 1).map(|x| x + 2)
        \\
        \\first = big([1.I64])
        \\
        \\second = big([2.I64])
        \\
        \\third = big([3.I64])
        \\
        \\fourth = big([4.I64])
        \\
        \\bad : List(Str)
        \\bad = third
    ;
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try std.testing.expectEqual(@as(usize, 2), useStateCount(&env, .replayed));
    try std.testing.expectEqual(@as(usize, 1), try env.typeProblemCount());
    try env.assertDefTypeOptions("second", "List(I64)", .{ .allow_type_errors = true });
    try env.assertDefTypeOptions("fourth", "List(I64)", .{ .allow_type_errors = true });
}
