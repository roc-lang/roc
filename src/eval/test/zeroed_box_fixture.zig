//! Executed fixture for the compiler-internal zeroed-cell allocation contract.
//! Sharing it keeps conformance coverage independent of source optimization
//! choices while retaining the pointer-op test's allocation and leak checks.

const std = @import("std");
const base = @import("base");
const eval = @import("eval");
const layout = @import("layout");
const lir = @import("lir");

const Error = std.mem.Allocator.Error || eval.Interpreter.Error || eval.RuntimeHostEnv.LeakError || error{
    TestExpectedEqual,
    TestUnexpectedResult,
};

/// Allocate a zero-filled cell, read it, overwrite it through its payload
/// pointer, read it again, and release it. The two reads must sum to seven.
pub fn run(allocator: std.mem.Allocator) Error!void {
    var store = lir.LirStore.init(allocator);
    defer store.deinit();
    var layouts = try layout.Store.init(allocator, base.target.TargetUsize.native);
    defer layouts.deinit();
    var runtime_env = eval.RuntimeHostEnv.init(allocator);
    defer runtime_env.deinit();

    const box_u64 = try layouts.insertBox(.u64);
    const ptr_u64 = try layouts.insertPtr(.u64);
    const cell = try store.addLocal(.{ .layout_idx = box_u64 });
    const p = try store.addLocal(.{ .layout_idx = ptr_u64 });
    const pre = try store.addLocal(.{ .layout_idx = .u64 });
    const v = try store.addLocal(.{ .layout_idx = .u64 });
    const st = try store.addLocal(.{ .layout_idx = .zst });
    const post = try store.addLocal(.{ .layout_idx = .u64 });
    const sum = try store.addLocal(.{ .layout_idx = .u64 });

    const ret = try store.addCFStmt(.{ .ret = .{ .value = sum } });
    const drop_cell = try store.addCFStmt(.{ .decref = .{
        .value = cell,
        .rc = .{ .concrete = .{ .op = .decref, .layout_idx = box_u64 } },
        .next = ret,
    } });
    const add = try lowLevelStmt(&store, sum, .num_int_add_wrap, &.{ pre, post }, drop_cell);
    const load_post = try lowLevelStmt(&store, post, .ptr_load, &.{p}, add);
    const store_v = try lowLevelStmt(&store, st, .ptr_store, &.{ p, v }, load_post);
    const v_lit = try store.addCFStmt(.{ .assign_literal = .{
        .target = v,
        .value = .{ .i64_literal = .{ .value = 7, .layout_idx = .u64 } },
        .next = store_v,
    } });
    const load_pre = try lowLevelStmt(&store, pre, .ptr_load, &.{p}, v_lit);
    const cast = try lowLevelStmt(&store, p, .ptr_cast, &.{cell}, load_pre);
    const alloc = try lowLevelStmt(&store, cell, .box_alloc_zeroed, &.{}, cast);
    const proc = try store.addProcSpec(.{
        .name = store.freshSyntheticSymbol(),
        .args = try store.addLocalSpan(&.{}),
        .frame_locals = try store.addLocalSpan(&.{ cell, p, pre, v, st, post, sum }),
        .body = alloc,
        .ret_layout = .u64,
    });

    var interp = try eval.Interpreter.init(allocator, &store, &layouts, runtime_env.get_ops(), .preserve);
    defer interp.deinit();
    const result = try interp.eval(.{ .proc_id = proc, .arg_layouts = &.{} });
    const value: *const u64 = @ptrCast(@alignCast(result.value.ptr));
    try std.testing.expectEqual(@as(u64, 7), value.*);
    try std.testing.expectEqual(@as(u32, 1), runtime_env.allocationCallCount());
    try runtime_env.checkForLeaks();
}

fn lowLevelStmt(
    store: *lir.LirStore,
    target: lir.LIR.LocalId,
    op: base.LowLevel,
    args: []const lir.LIR.LocalId,
    next: lir.LIR.CFStmtId,
) Error!lir.LIR.CFStmtId {
    return store.addCFStmt(.{ .assign_low_level = .{
        .target = target,
        .op = op,
        .rc_effect = op.rcEffect(),
        .args = try store.addLocalSpan(args),
        .next = next,
    } });
}
