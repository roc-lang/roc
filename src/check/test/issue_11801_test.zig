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

fn expectNoDigestedEdges(env: *const TestEnv) error{TestUnexpectedResult}!void {
    const edges = env.checker.dispatch_target_instantiations.items;
    try std.testing.expect(edges.len >= chain_len * use_count);
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
