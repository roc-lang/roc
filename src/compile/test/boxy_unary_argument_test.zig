//! Unary erased calls use independent argument storage directly, while
//! borrowed views and multiple arguments still need a complete packed block.
const std = @import("std");
const collections = @import("collections");
const lir = @import("lir");
const layout = @import("layout");
const harness = @import("lower_to_lir_harness.zig");

test "boxy declared primitives lower directly at closed and open call sites" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\rotate : U32, U8 -> U32
            \\rotate = |value, bits| value.shr_wrap(bits).bitwise_or(value.shl_wrap(32 - bits))
            \\length : List(a) -> U64
            \\length = |items| items.len()
            \\main! = |args| {
            \\    echo!(Str.inspect(rotate(args.len().to_u32_wrap(), 3)))
            \\    echo!(Str.inspect(length(args)))
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .proc_debug_names = true, .target_usize = width }, struct {
            fn inspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
                var found: [2]bool = @splat(false);
                for (store.getProcSpecs(), 0..) |proc, index| {
                    const name = store.procDebugName(@fromBackingInt(@intCast(index))) orelse continue;
                    const which: usize = if (std.mem.eql(u8, name, "rotate") or std.mem.endsWith(u8, name, ".rotate")) 0 else if (std.mem.eql(u8, name, "length") or std.mem.endsWith(u8, name, ".length")) 1 else continue;
                    found[which] = true;
                    var pending: std.ArrayList(lir.LIR.CFStmtId) = .empty;
                    defer pending.deinit(std.testing.allocator);
                    var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
                    defer visited.deinit();
                    try pending.append(std.testing.allocator, proc.body.?);
                    var operations: usize = 0;
                    while (pending.pop()) |id| {
                        if ((try visited.getOrPut(id)).found_existing) continue;
                        const stmt = store.getCFStmt(id);
                        // Neither helper should call an ordinary wrapper.
                        try std.testing.expect(stmt != .assign_call);
                        if (stmt == .assign_low_level) operations += 1;
                        try lir.BodyClone.appendSuccessors(store, &pending, id, std.testing.allocator);
                    }
                    try std.testing.expect(operations >= (if (which == 0) @as(usize, 3) else 1));
                }
                for (found) |present| try std.testing.expect(present);
            }
        }.inspect);
    }
}

test "boxy worker reads the composite parameter and result descriptor once from its type table" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\keep : List(a) -> List(a)
            \\keep = |items| items
            \\main! = |args| {
            \\    echo!(Str.inspect(keep(args)))
            \\    echo!(Str.inspect(keep([1.I64, 2, 3])))
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .proc_debug_names = true, .target_usize = width }, struct {
            fn inspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
                var found = false;
                for (store.getProcSpecs(), 0..) |proc, index| {
                    const name = store.procDebugName(@fromBackingInt(@intCast(index))) orelse continue;
                    if (!std.mem.eql(u8, name, "keep") and !std.mem.endsWith(u8, name, ".keep")) continue;
                    found = true;
                    // The list and the type table, whose one List(a)
                    // descriptor the parameter and the result share.
                    const args = store.getLocalSpan(proc.args);
                    try std.testing.expectEqual(@as(usize, 2), args.len);
                    const param = store.localDyn(collections.GuardedList.at(args, 0)) orelse return error.TestUnexpectedResult;
                    const result = store.localDyn(proc.dyn_result orelse return error.TestUnexpectedResult) orelse return error.TestUnexpectedResult;
                    try std.testing.expectEqual(param.desc, result.desc);
                }
                try std.testing.expect(found);
            }
        }.inspect);
    }
}

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
                .frame_alloc, .table_entry, .copy, .field_addr, .list_item_addr, .box_payload_addr, .tag_discriminant, .tag_payload_addr, .record_update, .tag, .list, .box, .slot_alloc, .slot_store, .eq, .hash, .inspect, .closure, .view, .adapt => {},
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
        // The descriptor each record the body builds is built with, and the
        // argument blocks of its erased calls.
        var record_descs = collections.DenseMap(lir.LIR.LocalId, lir.LIR.LocalId).init(std.testing.allocator);
        defer record_descs.deinit();
        var call_args: std.ArrayList(lir.LIR.LocalId) = .empty;
        defer call_args.deinit(std.testing.allocator);
        const params = store.getLocalSpan(proc.args);
        // An erased worker's last parameter is the erased ABI's reuse input.
        const reuse_params: usize = @intFromBool(proc.erased_reuse_arg != null);
        const param: ?lir.LIR.LocalId = if (params.len == 2 + reuse_params) collections.GuardedList.at(params, 0) else null;
        var forwards_param = false;
        var extracts_param = false;
        var retains_param = false;
        while (pending.pop()) |id| {
            if ((try visited.getOrPut(id)).found_existing) continue;
            const stmt = store.getCFStmt(id);
            if (stmt == .incref and stmt.incref.value == param) retains_param = true;
            if (stmt == .dyn) switch (store.stmtRow(stmt.dyn.data)) {
                .record => |record| try record_descs.put(stmt.dyn.target.?, record.desc),
                .field_addr => |field| if (field.base == param) {
                    extracts_param = true;
                },
                .call_erased => |call| {
                    if (call.args) |args| try call_args.append(std.testing.allocator, args);
                    if (param != null and call.args == param and store.localDyn(param.?) != null) forwards_param = true;
                },
                .frame_alloc, .table_entry, .copy, .list_item_addr, .box_payload_addr, .tag_discriminant, .tag_payload_addr, .record_update, .tag, .list, .box, .slot_alloc, .slot_store, .eq, .hash, .inspect, .closure, .view, .adapt => {},
            };
            try lir.BodyClone.appendSuccessors(store, &pending, id, std.testing.allocator);
        }
        if (same or reversed) {
            // The distinct descriptors of the argument records it builds.
            var arg_descs = collections.DenseMap(lir.LIR.LocalId, void).init(std.testing.allocator);
            defer arg_descs.deinit();
            for (call_args.items) |args| if (record_descs.get(args)) |desc| try arg_descs.put(desc, {});
            try std.testing.expectEqual(@as(usize, if (same) 1 else 2), arg_descs.count());
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

test "boxy worker reads dynamic nominal backing descriptors from its type table" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\wrap : a -> Try(a, {})
            \\wrap = |value| Ok(value)
            \\main! = |args| {
            \\    echo!(Str.inspect(wrap(args)))
            \\    echo!(Str.inspect(wrap(12.U32)))
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .proc_debug_names = true, .target_usize = width }, struct {
            fn inspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
                var found = false;
                for (store.getProcSpecs(), 0..) |proc, index| {
                    const name = store.procDebugName(@fromBackingInt(@intCast(index))) orelse continue;
                    if (!std.mem.eql(u8, name, "wrap") and !std.mem.endsWith(u8, name, ".wrap")) continue;
                    found = true;
                    // The value and the type table, which holds Try(a, {})
                    // and its backing union: the body composes neither.
                    try std.testing.expectEqual(@as(usize, 2), store.getLocalSpan(proc.args).len);
                    var pending: std.ArrayList(lir.LIR.CFStmtId) = .empty;
                    defer pending.deinit(std.testing.allocator);
                    var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
                    defer visited.deinit();
                    try pending.append(std.testing.allocator, proc.body.?);
                    var table_reads: usize = 0;
                    while (pending.pop()) |id| {
                        if ((try visited.getOrPut(id)).found_existing) continue;
                        const stmt = store.getCFStmt(id);
                        if (stmt == .dyn and store.stmtRow(stmt.dyn.data) == .table_entry) table_reads += 1;
                        try lir.BodyClone.appendSuccessors(store, &pending, id, std.testing.allocator);
                    }
                    try std.testing.expect(table_reads != 0);
                }
                try std.testing.expect(found);
            }
        }.inspect);
    }
}
