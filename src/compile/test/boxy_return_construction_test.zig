//! Immediately returned dynamic constructors use the caller's result storage;
//! intermediate values that are still needed retain their separate storage.
const std = @import("std");
const collections = @import("collections");
const lir = @import("lir");
const layout = @import("layout");
const harness = @import("lower_to_lir_harness.zig");

test "boxy returned constructors forward storage while a shared intermediate stays separate" {
    try harness.expectLirInspectionWithOptions(
        \\pack = |x| { value: x }
        \\wrap = |x| Wrapped(x)
        \\branch = |x, choose| if choose Left(x) else Right(x)
        \\keep = |x| {
        \\    value = { value: x }
        \\    { first: value, second: value }
        \\}
        \\main! = |args| {
        \\    echo!(Str.inspect(pack(args)))
        \\    echo!(Str.inspect(wrap(args)))
        \\    echo!(Str.inspect(branch(args, args.is_empty())))
        \\    echo!(Str.inspect(keep(args)))
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy, .proc_debug_names = true }, inspect);
}

fn inspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    const expected_names = [_][]const u8{ "pack", "wrap", "branch", "keep" };
    var found: [expected_names.len]bool = @splat(false);
    for (store.getProcSpecs(), 0..) |proc, index| {
        const proc_id: lir.LIR.LirProcSpecId = @fromBackingInt(@intCast(index));
        const name = store.procDebugName(proc_id) orelse continue;
        const matched = for (expected_names, 0..) |expected, i| {
            if (std.mem.eql(u8, name, expected) or
                (std.mem.endsWith(u8, name, expected) and name[name.len - expected.len - 1] == '.')) break i;
        } else continue;
        found[matched] = true;
        const result = proc.dyn_result orelse return error.TestUnexpectedResult;
        var work: std.ArrayList(lir.LIR.CFStmtId) = .empty;
        defer work.deinit(std.testing.allocator);
        var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
        defer visited.deinit();
        try work.append(std.testing.allocator, proc.body.?);
        var in_result: usize = 0;
        var in_frame: usize = 0;
        while (work.pop()) |id| {
            if ((try visited.getOrPut(id)).found_existing) continue;
            const stmt = store.getCFStmt(id);
            if (stmt == .dyn) {
                const op = store.stmtRow(stmt.dyn.data);
                switch (op) {
                    .record, .tag => {
                        // Generic record fields also construct Present tags;
                        // those are operands, not returned record producers.
                        const record_case = matched == 0 or matched == 3;
                        if ((record_case and op == .record) or (!record_case and op == .tag)) {
                            if (stmt.dyn.target == result) {
                                in_result += 1;
                            } else {
                                try std.testing.expectEqual(lir.LIR.DynStorage.frame, store.localDyn(stmt.dyn.target.?).?.storage);
                                in_frame += 1;
                            }
                        }
                    },
                    .copy => try std.testing.expect(stmt.dyn.target != result),
                    .frame_alloc, .desc_list, .desc_box, .desc_record, .desc_tuple, .desc_presence_slot, .desc_tag_union, .desc_args, .desc_nominal, .desc_recursive_box, .desc_recursive_open, .desc_recursive_close, .desc_note, .dict_patch, .dict_compose, .dict_record, .dict_method, .field_addr, .field_addr_named, .list_item_addr, .box_payload_addr, .tag_discriminant, .tag_discriminant_of, .tag_payload_addr, .record_update, .list, .box, .slot_alloc, .slot_store, .eq, .hash, .inspect, .call_erased, .closure, .view, .adapt => {},
                }
            }
            try lir.BodyClone.appendSuccessors(store, &work, id, std.testing.allocator);
        }
        try std.testing.expectEqual(@as(usize, if (matched == 2) 2 else 1), in_result);
        try std.testing.expectEqual(@as(usize, if (matched == 3) 1 else 0), in_frame);
    }
    for (found) |present| try std.testing.expect(present);
}
