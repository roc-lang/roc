//! Regression test for roc-lang/roc#13140: boxy lowering added every worker
//! procedure without a source location, so LLVM debug info scoped each
//! procedure to `<roc-generated>` and dropped the statements' own locations.
const std = @import("std");
const collections = @import("collections");
const lir = @import("lir");
const layout = @import("layout");
const harness = @import("lower_to_lir_harness.zig");

/// The harness puts the app body after a 7-line header.
const header_lines = 7;

test "issue 13140: boxy workers lowered from source are located at their bodies" {
    try harness.expectLirInspectionWithOptions(
        \\square : U64 -> U64
        \\square = |n| n * n
        \\
        \\twice : (U64 -> U64), U64 -> U64
        \\twice = |f, x| f(f(x))
        \\
        \\main! = |args| {
        \\    n = args.len()
        \\    echo!(twice(|y| y + n, square(n)).to_str())
        \\    Ok({})
        \\}
    , .{ .specialization_strategy = .boxy }, struct {
        fn inspect(store: *const lir.LirStore, _: *const layout.Store) harness.LowerToLirHarnessError!void {
            var square: ?lir.LIR.LirProcSpecId = null;
            var lambda_located = false;
            for (store.getProcSpecs(), 0..) |_, index| {
                const id: lir.LIR.LirProcSpecId = @fromBackingInt(@intCast(index));
                const loc = store.procLoc(id);
                // The closure's body `y + n`.
                if (loc.line == 9 + header_lines and loc.column == 21) lambda_located = true;
                const name = store.procDebugName(id) orelse continue;
                if (std.mem.eql(u8, name, "square") or std.mem.endsWith(u8, name, ".square")) square = id;
            }
            try std.testing.expect(lambda_located);

            // A template worker is located at its body `n * n`, in the file
            // its statements' locations name.
            const proc_id = square orelse return error.TestUnexpectedResult;
            const proc_loc = store.procLoc(proc_id);
            try std.testing.expect(proc_loc.hasLocation());
            try std.testing.expectEqual(@as(u32, 2 + header_lines), proc_loc.line);
            try std.testing.expectEqual(@as(u32, 14), proc_loc.column);

            var pending: std.ArrayList(lir.LIR.CFStmtId) = .empty;
            defer pending.deinit(std.testing.allocator);
            var seen = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
            defer seen.deinit();
            try pending.append(std.testing.allocator, store.getProcSpec(proc_id).body.get().?);
            var located: usize = 0;
            while (pending.pop()) |stmt_id| {
                if ((try seen.getOrPut(stmt_id)).found_existing) continue;
                const loc = store.stmtLoc(stmt_id);
                if (loc.hasLocation()) {
                    try std.testing.expectEqual(proc_loc.file, loc.file);
                    located += 1;
                }
                try lir.BodyClone.appendSuccessors(store, &pending, stmt_id, std.testing.allocator);
            }
            try std.testing.expect(located != 0);
        }
    }.inspect);
}
