//! Regression tests for dispatch-state digests on unannotated method chains.
//!
//! An unannotated function whose body chains method calls (`b.map(f)`)
//! generalizes to a chain of constrained variables, each constraint's result
//! carrying the next one. Every use of the function resolves each copied
//! constraint as a new dispatch edge. A dispatch edge's canonical state digest
//! walks everything its callable reaches, which here is the rest of the chain,
//! so digesting every edge costs O(chain²) per use. A digest is only ever
//! compared along a derivation lineage against an ancestor with the same
//! target, so an edge with no such ancestor whose target mints no child
//! relations must not compute one.
const TestEnv = @import("TestEnv.zig");
const std = @import("std");

const chain_len: usize = 40;
const use_count: usize = 5;

/// `big` chains `chain_len` `map` calls on its argument and is used
/// `use_count` times.
fn genSource(gpa: std.mem.Allocator) ![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);

    try out.appendSlice(gpa, "big = |a| {\n    b0 = a\n");
    var i: usize = 1;
    while (i <= chain_len) : (i += 1) {
        const line = try std.fmt.allocPrint(gpa, "    b{d} = b{d}.map(|x| x + 1)\n", .{ i, i - 1 });
        defer gpa.free(line);
        try out.appendSlice(gpa, line);
    }
    const tail = try std.fmt.allocPrint(gpa, "    b{d}\n}}\n\n", .{chain_len});
    defer gpa.free(tail);
    try out.appendSlice(gpa, tail);

    var j: usize = 0;
    while (j < use_count) : (j += 1) {
        const line = try std.fmt.allocPrint(gpa, "u{d} = big([{d}.I64])\n", .{ j, j });
        defer gpa.free(line);
        try out.appendSlice(gpa, line);
    }
    return out.toOwnedSlice(gpa);
}

test "issue 11801: leaf dispatch edges of an unannotated method chain compute no state digest" {
    const gpa = std.testing.allocator;

    const source = try genSource(gpa);
    defer gpa.free(source);

    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();

    // Every use resolves every copied `map` constraint to `List.map`, whose
    // scheme carries no constraints: no edge has a same-target ancestor and
    // none mints children, so no edge keeps a digest.
    const edges = env.checker.dispatch_target_instantiations.items;
    try std.testing.expect(edges.len >= chain_len * use_count);
    for (edges) |edge| {
        try std.testing.expect(edge.state_type_key == null);
    }
}
