//! Unary erased calls use independent argument storage directly, while
//! borrowed views and multiple arguments still need a complete packed block.
const std = @import("std");
const collections = @import("collections");
const lir = @import("lir");
const layout = @import("layout");
const harness = @import("lower_to_lir_harness.zig");

test "boxy unary argument blocks preserve views and omit independent packing" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\made = |f, x| f({ value: x })
            \\viewed = |f, x| f(x.value)
            \\pair = |f, x, y| f(x, y)
            \\main! = |args| {
            \\    echo!(Str.inspect(made(|x| x, args)))
            \\    echo!(Str.inspect(viewed(|x| x, { value: args })))
            \\    echo!(Str.inspect(pair(|x, y| { x, y }, args, args)))
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .proc_debug_names = true, .target_usize = width }, inspect);
    }
}

fn inspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    const names = [_][]const u8{ "made", "viewed", "pair" };
    var found: [names.len]bool = @splat(false);
    for (store.getProcSpecs(), 0..) |proc, index| {
        const name = store.procDebugName(@fromBackingInt(@intCast(index))) orelse continue;
        const matched = for (names, 0..) |expected, i| {
            if (std.mem.eql(u8, name, expected) or
                (std.mem.endsWith(u8, name, expected) and name[name.len - expected.len - 1] == '.')) break i;
        } else continue;
        found[matched] = true;
        var work: std.ArrayList(lir.LIR.CFStmtId) = .empty;
        defer work.deinit(std.testing.allocator);
        var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
        defer visited.deinit();
        try work.append(std.testing.allocator, proc.body.?);
        var records: usize = 0;
        var calls: usize = 0;
        var constructed: ?lir.LIR.LocalId = null;
        while (work.pop()) |id| {
            if ((try visited.getOrPut(id)).found_existing) continue;
            const stmt = store.getCFStmt(id);
            if (stmt == .dyn) switch (store.stmtRow(stmt.dyn.data)) {
                .record => |record| {
                    records += 1;
                    constructed = stmt.dyn.target;
                    const fields = store.getLocalSpan(record.fields);
                    try std.testing.expectEqual(@as(usize, if (matched == 2) 2 else 1), fields.len);
                    if (matched == 1) try std.testing.expectEqual(lir.LIR.DynStorage.view, (store.localDyn(collections.GuardedList.at(fields, 0)) orelse return error.TestUnexpectedResult).storage);
                },
                .call_erased => |call| {
                    calls += 1;
                    try std.testing.expectEqual((constructed orelse return error.TestUnexpectedResult), call.args.?);
                    try std.testing.expectEqual(lir.LIR.DynStorage.frame, (store.localDyn(call.args.?) orelse return error.TestUnexpectedResult).storage);
                },
                .frame_alloc, .desc_list, .desc_box, .desc_record, .desc_tuple, .desc_presence_slot, .desc_tag_union, .desc_args, .desc_nominal, .desc_recursive_box, .desc_recursive_open, .desc_recursive_close, .desc_note, .dict_patch, .dict_compose, .dict_record, .dict_method, .copy, .field_addr, .field_addr_named, .list_item_addr, .box_payload_addr, .tag_discriminant, .tag_discriminant_of, .tag_payload_addr, .record_update, .tag, .list, .box, .slot_alloc, .slot_store, .eq, .hash, .inspect, .closure, .view, .adapt => {},
            };
            try lir.BodyClone.appendSuccessors(store, &work, id, std.testing.allocator);
        }
        try std.testing.expectEqual(@as(usize, 1), records);
        try std.testing.expectEqual(@as(usize, 1), calls);
    }
    for (found) |present| try std.testing.expect(present);
}

test "boxy unary entries forward their own parameter and share exact argument layouts" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\forwarder = |f| |x| f(x)
            \\twice = |f, g, x, y| (f(x, y), g(x, y))
            \\reordered = |f, g, x, y| (f(x, y), g(y, x))
            \\main! = |args| {
            \\    echo!(Str.inspect(forwarder(|x| x)(args)))
            \\    echo!(Str.inspect(twice(|x, _| x, |_, y| y, args, "text")))
            \\    echo!(Str.inspect(reordered(|x, _| x, |x, _| x, args, "text")))
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .proc_debug_names = true, .target_usize = width }, inspectEntries);
    }
}

fn inspectEntries(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    var direct_forwarders: usize = 0;
    var shared = false;
    var ordered = false;
    for (store.getProcSpecs(), 0..) |proc, proc_index| {
        const name = store.procDebugName(@fromBackingInt(@intCast(proc_index))) orelse continue;
        const same = std.mem.eql(u8, name, "twice") or std.mem.endsWith(u8, name, ".twice");
        const reversed = std.mem.eql(u8, name, "reordered") or std.mem.endsWith(u8, name, ".reordered");
        if (!same and !reversed and proc.abi != .erased_record) continue;
        var pending: std.ArrayList(lir.LIR.CFStmtId) = .empty;
        defer pending.deinit(std.testing.allocator);
        var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
        defer visited.deinit();
        try pending.append(std.testing.allocator, proc.body.?);
        var arg_descs: usize = 0;
        const params = store.getLocalSpan(proc.args);
        const param: ?lir.LIR.LocalId = if (params.len == 2) collections.GuardedList.at(params, 0) else null;
        var forwards_param = false;
        var extracts_param = false;
        var retains_param = false;
        while (pending.pop()) |id| {
            if ((try visited.getOrPut(id)).found_existing) continue;
            const stmt = store.getCFStmt(id);
            if (stmt == .incref and stmt.incref.value == param) retains_param = true;
            if (stmt == .dyn) switch (store.stmtRow(stmt.dyn.data)) {
                .desc_args => arg_descs += 1,
                .field_addr => |field| if (field.base == param) {
                    extracts_param = true;
                },
                .call_erased => |call| if (param != null and call.args == param and store.localDyn(param.?) != null) {
                    forwards_param = true;
                },
                .frame_alloc, .desc_list, .desc_box, .desc_record, .desc_tuple, .desc_presence_slot, .desc_tag_union, .desc_nominal, .desc_recursive_box, .desc_recursive_open, .desc_recursive_close, .desc_note, .dict_patch, .dict_compose, .dict_record, .dict_method, .copy, .field_addr_named, .list_item_addr, .box_payload_addr, .tag_discriminant, .tag_discriminant_of, .tag_payload_addr, .record, .record_update, .tag, .list, .box, .slot_alloc, .slot_store, .eq, .hash, .inspect, .closure, .view, .adapt => {},
            };
            try lir.BodyClone.appendSuccessors(store, &pending, id, std.testing.allocator);
        }
        if (same or reversed) {
            try std.testing.expectEqual(@as(usize, if (same) 1 else 2), arg_descs);
            if (same) shared = true else ordered = true;
        }
        if (proc.abi == .erased_record and forwards_param) {
            try std.testing.expectEqual(lir.LIR.DynStorage.param, store.localDyn(param.?).?.storage);
            try std.testing.expect(!extracts_param);
            try std.testing.expect(!retains_param);
            direct_forwarders += 1;
        }
    }
    try std.testing.expect(shared and ordered);
    try std.testing.expect(direct_forwarders != 0);
}
