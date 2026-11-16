//! Under `.boxy`, a read of a constant compile-time evaluation stored is one
//! read of immutable static data, whatever the value is built from: a list's
//! items, a record's fields, a tag's payloads, and the strings they hold are
//! parts of that data, never values of the reading body (design.md
//! "Compile-Time Evaluation Strategy"). A body that read each part would grow
//! with the constant, and so would every pass and backend that compiles it.
const std = @import("std");
const collections = @import("collections");
const lir = @import("lir");
const layout = @import("layout");
const harness = @import("lower_to_lir_harness.zig");

test "boxy reads a stored constant built from parts with one static data read" {
    try harness.expectLirInspectionWithOptions(
        \\entries : List({ name : Str, tags : List(Str), kind : [Plain, Marked(Str)] })
        \\entries = [
        \\    { name: "a first name long enough to live on the heap", tags: ["a first tag long enough to live on the heap"], kind: Plain },
        \\    { name: "a second name long enough to live on the heap", tags: [], kind: Marked("a mark long enough to live on the heap") },
        \\    { name: "a third name long enough to live on the heap", tags: ["x", "y"], kind: Plain },
        \\    { name: "a fourth name long enough to live on the heap", tags: ["z"], kind: Marked("m") },
        \\]
        \\main! = |args| {
        \\    match entries.get(args.len()) {
        \\        Ok(entry) => echo!(entry.name)
        \\        Err(_) => echo!("none")
        \\    }
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy, .proc_debug_names = true, .include_internal_static_data = true }, inspect);
}

/// `main!` reads the list, and nothing the list is built from: every other
/// static data read of its body is a type table word.
fn inspect(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    var found = false;
    for (store.getProcSpecs(), 0..) |proc, index| {
        const name = store.procDebugName(@fromBackingInt(@intCast(index))) orelse continue;
        if (!std.mem.eql(u8, name, "main!") and !std.mem.endsWith(u8, name, ".main!")) continue;
        const body = proc.body.get() orelse continue;
        found = true;
        var pending: std.ArrayList(lir.LIR.CFStmtId) = .empty;
        defer pending.deinit(std.testing.allocator);
        var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
        defer visited.deinit();
        try pending.append(std.testing.allocator, body);
        var list_reads: usize = 0;
        var part_reads: usize = 0;
        while (pending.pop()) |id| {
            if ((try visited.getOrPut(id)).found_existing) continue;
            const stmt = store.getCFStmt(id);
            if (stmt == .assign_literal and stmt.assign_literal.value == .static_data) {
                const read_layout = store.getLocal(stmt.assign_literal.target).layout_idx;
                if (read_layout != .opaque_ptr) {
                    if (layouts.getLayout(read_layout).tag == .list) list_reads += 1 else part_reads += 1;
                }
            }
            try lir.BodyClone.appendSuccessors(store, &pending, id, std.testing.allocator);
        }
        try std.testing.expectEqual(@as(usize, 1), list_reads);
        try std.testing.expectEqual(@as(usize, 0), part_reads);
    }
    try std.testing.expect(found);
}
