//! Regression for https://github.com/roc-lang/roc/issues/13269: a zero-sized
//! capture argument needs a reaching definition just like any other argument.

const std = @import("std");
const layout = @import("layout");
const lir = @import("lir");
const GuardedList = @import("collections").GuardedList;
const harness = @import("lower_to_lir_harness.zig");

test "issue 13269: specialized call defines its zero-sized capture argument" {
    try harness.expectLirInspectionWithOptions(
        \\c = |x, f| f(x)
        \\
        \\main! = |_| {
        \\    a = {}
        \\    c(0, |_| a)
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .lss }, expectDefinedCapture);
}

test "issue 13269: a tagged callable's empty captures are defined in their arms" {
    try harness.expectLirInspectionWithOptions(
        \\c = |x, f| f(x)
        \\
        \\main! = |args| {
        \\    a = {}
        \\    f = if args.is_empty() (|_| Left(a)) else (|_| Right(a))
        \\    echo!(Str.inspect(c(0, f)))
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .lss }, expectDefinedTaggedCaptures);
}

fn expectDefinedCapture(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    try std.testing.expectEqual(@as(usize, 1), try definedCaptureCount(store, layouts));
}

fn expectDefinedTaggedCaptures(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    try std.testing.expectEqual(@as(usize, 2), try definedCaptureCount(store, layouts));
}

fn definedCaptureCount(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!usize {
    for (store.getProcSpecs(), 0..) |proc, i| {
        const id: lir.LirProcSpecId = @fromBackingInt(@as(u32, @intCast(i)));
        const name = store.procDebugName(id) orelse continue;
        if (!std.mem.eql(u8, name, "c")) continue;

        var defined: std.ArrayList(lir.LIR.LocalId) = .empty;
        defer defined.deinit(std.testing.allocator);
        const params = store.getLocalSpan(proc.args);
        for (0..params.len) |j| try defined.append(std.testing.allocator, GuardedList.at(params, j));
        return try checkPath(store, layouts, proc.body.get().?, &defined);
    }
    return error.TestUnexpectedResult;
}

// Follow each dispatch arm with its own reaching definitions. These fixtures
// contain no loops; the join body merely returns the result of dispatch.
fn checkPath(store: *const lir.LirStore, layouts: *const layout.Store, start: lir.LIR.CFStmtId, defined: *std.ArrayList(lir.LIR.LocalId)) harness.LowerToLirHarnessError!usize {
    var pc = start;
    const saved = defined.items.len;
    defer defined.shrinkRetainingCapacity(saved);
    while (true) {
        const stmt = store.getCFStmt(pc);
        if (stmt == .join) {
            pc = stmt.join.remainder;
            continue;
        }
        if (stmt == .switch_stmt) {
            const branches = store.getCFSwitchBranches(stmt.switch_stmt.branches);
            var calls: usize = 0;
            for (0..branches.len) |i| calls += try checkPath(store, layouts, GuardedList.at(branches, i).body, defined);
            return calls + try checkPath(store, layouts, stmt.switch_stmt.default_branch, defined);
        }
        if (stmt == .assign_call) {
            const args = store.getLocalSpan(stmt.assign_call.args);
            try std.testing.expectEqual(@as(usize, 2), args.len);
            const capture = GuardedList.at(args, 1);
            try std.testing.expect(layouts.isZeroSized(layouts.getLayout(store.getLocal(capture).layout_idx)));
            if (std.mem.findScalar(lir.LIR.LocalId, defined.items, capture) == null) {
                std.debug.print("c passes undefined zero-sized capture l{d} to p{d}\n", .{ @backingInt(capture), @backingInt(stmt.assign_call.proc) });
                return error.TestUnexpectedResult;
            }
            return 1;
        }
        if (stmt != .init_uninitialized) {
            if (lir.BodyClone.writtenLocal(stmt)) |dst| try defined.append(std.testing.allocator, dst);
        }
        pc = lir.BodyClone.linearNextOrNull(stmt) orelse return 0;
    }
}
