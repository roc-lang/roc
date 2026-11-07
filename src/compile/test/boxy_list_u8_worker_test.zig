//! Under `.boxy`, a direct call of a `List` template whose list item variable
//! takes exactly `U8` calls the template's pre-specialized `List(U8)` worker,
//! whose body is static; every other instantiation calls the generic worker
//! (design.md "Pre-Specialized List(U8) Workers").
const std = @import("std");
const collections = @import("collections");
const lir = @import("lir");
const layout = @import("layout");
const harness = @import("lower_to_lir_harness.zig");

test "boxy List(U8) call sites target the List(U8) worker, other instantiations the generic one" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.expectLirInspectionWithOptions(
            \\pick : List(U8), U64 -> Try(U8, [OutOfBounds])
            \\pick = |bytes, index| bytes.get(index)
            \\second : List(a) -> Try(a, [OutOfBounds])
            \\second = |items| List.get(items, 1)
            \\main! = |args| {
            \\    bytes : List(U8)
            \\    bytes = Str.to_utf8("hello")
            \\    echo!(Str.inspect(pick(bytes, args.len())))
            \\    echo!(Str.inspect(second(args)))
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .proc_debug_names = true, .target_usize = width }, inspect);
    }
}

fn nameIs(name: []const u8, expected: []const u8) bool {
    return std.mem.eql(u8, name, expected) or
        (std.mem.endsWith(u8, name, expected) and name[name.len - expected.len - 1] == '.');
}

/// The debug names of the procedures `proc`'s body calls directly.
fn calledNames(store: *const lir.LirStore, proc: lir.LIR.LirProcSpec, out: *std.ArrayList([]const u8)) std.mem.Allocator.Error!void {
    var pending: std.ArrayList(lir.LIR.CFStmtId) = .empty;
    defer pending.deinit(std.testing.allocator);
    var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
    defer visited.deinit();
    try pending.append(std.testing.allocator, proc.body.?);
    while (pending.pop()) |id| {
        if ((try visited.getOrPut(id)).found_existing) continue;
        const stmt = store.getCFStmt(id);
        if (stmt == .assign_call) {
            if (store.procDebugName(stmt.assign_call.proc)) |name| try out.append(std.testing.allocator, name);
        }
        try lir.BodyClone.appendSuccessors(store, &pending, id, std.testing.allocator);
    }
}

fn inspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
    var found_pick = false;
    var found_second = false;
    var found_u8_worker = false;
    for (store.getProcSpecs(), 0..) |proc, index| {
        const name = store.procDebugName(@fromBackingInt(@intCast(index))) orelse continue;
        if (std.mem.eql(u8, name, "Builtin.List.get@U8")) {
            found_u8_worker = true;
            // Every value of the item type is static: no dynamic parameter,
            // no dynamic result, and no descriptor-guided statement.
            const args = store.getLocalSpan(proc.args);
            for (0..collections.GuardedList.borrowLen(args)) |arg_index| {
                try std.testing.expect(store.localDyn(collections.GuardedList.at(args, arg_index)) == null);
            }
            try std.testing.expect(proc.dyn_result == null);
            var pending: std.ArrayList(lir.LIR.CFStmtId) = .empty;
            defer pending.deinit(std.testing.allocator);
            var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
            defer visited.deinit();
            try pending.append(std.testing.allocator, proc.body.?);
            while (pending.pop()) |id| {
                if ((try visited.getOrPut(id)).found_existing) continue;
                try std.testing.expect(store.getCFStmt(id) != .dyn);
                try lir.BodyClone.appendSuccessors(store, &pending, id, std.testing.allocator);
            }
            continue;
        }
        const which: enum { pick, second } = if (nameIs(name, "pick")) .pick else if (nameIs(name, "second")) .second else continue;
        var called: std.ArrayList([]const u8) = .empty;
        defer called.deinit(std.testing.allocator);
        try calledNames(store, proc, &called);
        var u8_calls: usize = 0;
        var generic_calls: usize = 0;
        for (called.items) |callee| {
            if (std.mem.eql(u8, callee, "Builtin.List.get@U8")) u8_calls += 1;
            if (std.mem.eql(u8, callee, "Builtin.List.get")) generic_calls += 1;
        }
        switch (which) {
            .pick => {
                found_pick = true;
                try std.testing.expectEqual(@as(usize, 1), u8_calls);
                try std.testing.expectEqual(@as(usize, 0), generic_calls);
            },
            .second => {
                found_second = true;
                try std.testing.expectEqual(@as(usize, 0), u8_calls);
                try std.testing.expectEqual(@as(usize, 1), generic_calls);
            },
        }
    }
    try std.testing.expect(found_pick);
    try std.testing.expect(found_second);
    try std.testing.expect(found_u8_worker);
}
